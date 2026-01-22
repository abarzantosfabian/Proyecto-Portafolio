###############################################################################
# SCRIPT 3 — REBALANCEO (Backtest + Diagnóstico + Recomendación)
# -----------------------------------------------------------------------------
# OBJETIVO
# Simular y comparar políticas de rebalanceo usando el portafolio objetivo
# construido en el Script 2. Este script produce:
#   - Backtest de wealth/retornos por política
#   - Métricas de performance (Return/Vol/Sharpe/Drawdown)
#   - Métricas operativas (frecuencia de rebalanceo y turnover)
#   - Benchmark + Tracking Error / Tracking Difference
#   - Recomendación final “audit-friendly” basada en un Decision Score
#
# POLÍTICAS EVALUADAS
# A) Periodic: rebalanceo a calendario (ej. trimestral)
# B) Bands: rebalanceo mensual, pero solo si se rompen bandas +/- band
# C) GlidePath + Bands (opcional): cambia el target equity/defensivo por horizonte
#    (requiere que exista el activo libre de riesgo en returns, ej. "AGG")
#
#
# SUPUESTOS IMPORTANTES
# - returns_log proviene del Script 2 y por defecto son retornos log (cfg_reb$returns_are_log=TRUE)
# - El backtest no modela trading real (slippage, impuestos, fricciones completas).
#   Se usan proxies: turnover y tc_bps como señal para “cost-aware” (diagnóstico).
#
# ENTRADAS
# - Objeto `port` del Script 2:
#     port$returns_log      (xts)
#     port$weights_target   (vector nombrado)
#
# SALIDAS
# - Tabla de resultados: tbl
# - Mejor política sugerida: best_policy
# - Gráfico comparativo: charts.PerformanceSummary
#
# INSTRUCCIONES DE USO
# 1) Ejecuta Script 1 y Script 2 para crear `sel` y `port`.
# 2) Edita el bloque CONFIG (cfg_reb) si quieres:
#    - banda, periodicidad, benchmark, glide path, etc.
# 3) Ejecuta este script completo.
# 4) Revisa:
#    - tbl (métricas + TE/TD + KPIs + Decision_Score)
#    - best_policy (recomendación)
#    - charts.PerformanceSummary (comparación visual)
###############################################################################

###############################################################################
# 0) PRE-CHECKS + PAQUETES (CARGA CONTROLADA)
###############################################################################
if (!exists("port")) stop("No existe objeto 'port'. Ejecuta Script 2 primero.")

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
  library(dplyr)
  library(quantmod)
})

returns_log <- port$returns_log
w_target    <- port$weights_target

# Validación básica (evita errores silenciosos)
stopifnot(is.xts(returns_log))
stopifnot(is.numeric(w_target), !is.null(names(w_target)))
stopifnot(all(names(w_target) %in% colnames(returns_log)))

###############################################################################
# 1) CONFIGURACIÓN (EDITA AQUÍ)
###############################################################################
cfg_reb <- list(
  # returns_are_log:
  # - TRUE si tus retornos vienen en log (lo recomendado para tu pipeline)
  # - FALSE si trabajas con retornos discretos
  returns_are_log = TRUE,
  
  # Bandas (por ejemplo 0.05 = 5 puntos porcentuales absolutos)
  band = 0.05,
  
  # Frecuencias (k=3 trimestral, k=1 mensual; endpoints usa "months")
  k_periodic = 3,
  k_bands    = 1,
  
  # GlidePath (solo si hay activo libre de riesgo)
  use_glide = TRUE,
  free_risk_ticker = "AGG",
  horizon_years = 20,
  start_equity  = 0.80,
  end_equity    = 0.30,
  k_glide = 3,
  use_bands_glide = TRUE,
  
  # Benchmark
  benchmark = "60_40",     # "SPY" o "Estrategia 60%/40% Renta Variable/Fija"
  bench_from = "2022-01-01",
  
  # Diagnóstico/Cost proxies (no trading real)
  tc_bps = 10,             # costos de transacción proxy (puntos base)
  risk_window_days = 126,  # ~6 meses hábiles (si luego deseas riesgo rolling)
  risk_tol = 0.02,         # tolerancia vol anual (placeholder para extensión)
  n_sims = 1500            # placeholder para extensión (prob rebalanceo ex-ante)
)

###############################################################################
# 2) HELPERS: WEALTH, DRIFT, CONVERSIONES
###############################################################################
# wealth_step():
# - Actualiza la riqueza acumulada en un paso.
# - Si returns_are_log=TRUE: wealth <- wealth * exp(r)
# - Si returns_are_log=FALSE: wealth <- wealth * (1 + r)
wealth_step <- function(wealth, port_ret, returns_are_log) {
  if (returns_are_log) wealth * exp(port_ret) else wealth * (1 + port_ret)
}

# weight_drift():
# - Actualiza pesos por deriva tras observar retornos del periodo.
# - Se re-normaliza para que sumen 1.
weight_drift <- function(w, r_vec, returns_are_log) {
  gross <- if (returns_are_log) exp(r_vec) else (1 + r_vec)
  w_new <- w * gross
  w_new / sum(w_new)
}

# Convertir wealth a retornos discretos para métricas estándar
to_discrete_returns_from_wealth <- function(wealth_xts) {
  na.omit(Return.calculate(wealth_xts, method = "discrete"))
}

###############################################################################
# 3) REGLAS DE REBALANCEO (DECISIONES)
###############################################################################
# Regla A: Periodic (siempre rebalancea en fecha programada)
rebalance_periodic <- function(current_w, target_w) {
  list(
    rebalance = TRUE,
    new_w = target_w,
    reason = "Periódico (fecha programada)",
    max_dev = max(abs(current_w - target_w))
  )
}

# Regla B: Bands (rebalancea solo si rompe banda)
rebalance_bands <- function(current_w, target_w, band) {
  dev <- abs(current_w - target_w)
  mx <- max(dev)
  need <- any(dev > band)
  list(
    rebalance = need,
    new_w = if (need) target_w else current_w,
    reason = if (need) paste0("Bandas: max dev=", round(mx,4), " > ", band)
    else      paste0("Bandas: max dev=", round(mx,4), " <= ", band),
    max_dev = mx
  )
}

###############################################################################
# 4) BACKTEST BASE (PERIODIC / BANDS) + LOGS (AUDITORÍA)
###############################################################################
# backtest_rebalance():
# - Simula wealth y la deriva de pesos.
# - En fechas programadas (endpoints) evalúa la regla y registra log:
#   * rebalanced (TRUE/FALSE)
#   * max_dev (desviación máxima vs target)
#   * turnover (aprox: sum(|Δw|)/2)
#   * reason (texto audit-friendly)
backtest_rebalance <- function(returns, w_target, rule = c("periodic","bands"),
                               k = 1, band = 0.05,
                               returns_are_log = TRUE, verbose = FALSE) {
  
  rule <- match.arg(rule)
  returns <- returns[, names(w_target)]
  w <- as.numeric(w_target); names(w) <- names(w_target)
  
  wealth <- 1
  wealth_xts <- xts(rep(NA_real_, nrow(returns)), order.by = index(returns))
  
  ep <- endpoints(returns, on = "months", k = k)
  reb_idx <- rep(FALSE, nrow(returns))
  reb_idx[ep] <- TRUE
  reb_idx[1] <- TRUE
  
  log_df <- data.frame(
    date=as.Date(character()),
    rule=character(),
    rebalanced=logical(),
    max_dev=numeric(),
    turnover=numeric(),
    reason=character(),
    stringsAsFactors = FALSE
  )
  
  for (t in 1:nrow(returns)) {
    r <- as.numeric(returns[t, ])
    port_ret <- sum(w * r)
    
    wealth <- wealth_step(wealth, port_ret, returns_are_log)
    wealth_xts[t] <- wealth
    
    # deriva de pesos post-retorno
    w <- weight_drift(w, r, returns_are_log)
    
    if (reb_idx[t]) {
      d <- as.Date(index(returns)[t])
      
      dec <- if (rule == "periodic") {
        rebalance_periodic(w, w_target)
      } else {
        rebalance_bands(w, w_target, band)
      }
      
      turnover <- if (dec$rebalance) sum(abs(dec$new_w - w))/2 else 0
      w <- dec$new_w
      
      log_df <- rbind(log_df, data.frame(
        date=d, rule=rule, rebalanced=dec$rebalance,
        max_dev=dec$max_dev, turnover=turnover, reason=dec$reason,
        stringsAsFactors = FALSE
      ))
      
      if (verbose) cat(sprintf("[%s] %s | reb=%s\n", d, dec$reason, ifelse(dec$rebalance,"SÍ","NO")))
    }
  }
  
  colnames(wealth_xts) <- paste0("Wealth_", rule)
  list(wealth = wealth_xts, log = log_df)
}

###############################################################################
# 5) GLIDEPATH + BANDAS (TARGET DINÁMICO POR HORIZONTE)
###############################################################################
# Glide path: define el peso total en equity como función del tiempo
glide_path_equity_weight <- function(t, T_total, start_equity, end_equity) {
  w <- start_equity + (end_equity - start_equity) * (t / T_total)
  pmin(pmax(w, 0), 1)
}

# Construye vector de pesos target (equity iguales + defensivo)
make_target_weights_glide <- function(equity_tickers, free_risk_ticker, equity_total_weight) {
  eq_n <- length(equity_tickers)
  w <- rep(0, eq_n + 1)
  names(w) <- c(equity_tickers, free_risk_ticker)
  w[equity_tickers] <- equity_total_weight / eq_n
  w[free_risk_ticker] <- 1 - equity_total_weight
  w
}

# backtest_glidepath():
# - Similar al backtest base, pero el target cambia con el tiempo.
# - Puede aplicar bandas para evitar rebalanceos excesivos.
backtest_glidepath <- function(returns, equity_tickers, free_risk_ticker,
                               horizon_years, start_equity, end_equity,
                               k = 3, band = 0.05, use_bands = TRUE,
                               returns_are_log = TRUE, verbose = FALSE) {
  
  stopifnot(free_risk_ticker %in% colnames(returns))
  returns <- returns[, c(equity_tickers, free_risk_ticker)]
  
  ep <- endpoints(returns, on="months", k=k)
  reb_idx <- rep(FALSE, nrow(returns))
  reb_idx[ep] <- TRUE
  reb_idx[1] <- TRUE
  
  start_date <- as.Date(index(returns)[1])
  t_years <- as.numeric(as.Date(index(returns)) - start_date) / 365.25
  
  w_target <- make_target_weights_glide(
    equity_tickers, free_risk_ticker,
    glide_path_equity_weight(0, horizon_years, start_equity, end_equity)
  )
  w <- as.numeric(w_target); names(w) <- names(w_target)
  
  wealth <- 1
  wealth_xts <- xts(rep(NA_real_, nrow(returns)), order.by=index(returns))
  
  log_df <- data.frame(
    date=as.Date(character()),
    equity_target=numeric(),
    rebalanced=logical(),
    max_dev=numeric(),
    turnover=numeric(),
    reason=character(),
    stringsAsFactors=FALSE
  )
  
  for (i in 1:nrow(returns)) {
    r <- as.numeric(returns[i, ])
    port_ret <- sum(w * r)
    
    wealth <- wealth_step(wealth, port_ret, returns_are_log)
    wealth_xts[i] <- wealth
    
    w <- weight_drift(w, r, returns_are_log)
    
    if (reb_idx[i]) {
      d <- as.Date(index(returns)[i])
      
      eq_target <- glide_path_equity_weight(t_years[i], horizon_years, start_equity, end_equity)
      w_new_target <- make_target_weights_glide(equity_tickers, free_risk_ticker, eq_target)
      
      dev <- abs(w - w_new_target)
      mx <- max(dev)
      
      do_reb <- if (use_bands) any(dev > band) else TRUE
      reason <- if (use_bands) {
        if (do_reb) paste0("Glide+bandas: max dev=", round(mx,4), " > ", band, " | eq=", round(eq_target,3))
        else       paste0("Glide+bandas: max dev=", round(mx,4), " <= ", band, " | eq=", round(eq_target,3))
      } else {
        paste0("Glide periódico | eq=", round(eq_target,3))
      }
      
      turnover <- if (do_reb) sum(abs(w_new_target - w))/2 else 0
      if (do_reb) w <- w_new_target
      
      log_df <- rbind(log_df, data.frame(
        date=d, equity_target=eq_target, rebalanced=do_reb,
        max_dev=mx, turnover=turnover, reason=reason,
        stringsAsFactors=FALSE
      ))
      
      if (verbose) cat(sprintf("[%s] %s\n", d, reason))
    }
  }
  
  colnames(wealth_xts) <- "Wealth_Glide"
  list(wealth=wealth_xts, log=log_df)
}

###############################################################################
# 6) BENCHMARK + TRACKING ERROR / TRACKING DIFFERENCE
###############################################################################
# Descarga precios benchmark y construye retornos discretos
get_prices_yahoo <- function(tickers, from) {
  env <- new.env()
  suppressWarnings(getSymbols(tickers, src="yahoo", from=from, env=env, auto.assign=TRUE))
  plist <- lapply(tickers, function(tk) Ad(get(tk, envir=env)))
  p <- do.call(merge, plist); colnames(p) <- tickers
  na.omit(p)
}
prices_to_returns <- function(prices, method="discrete") na.omit(Return.calculate(prices, method=method))

# Tracking Error anualizado: sd(port - bench) * sqrt(252)
tracking_error_annualized <- function(port_ret, bench_ret, scale=252) {
  x <- na.omit(merge(port_ret, bench_ret))
  active <- x[,1] - x[,2]
  as.numeric(sd(active) * sqrt(scale))
}

# Tracking Difference anualizado: mean(port - bench) * 252
tracking_difference_annualized <- function(port_ret, bench_ret, scale=252) {
  x <- na.omit(merge(port_ret, bench_ret))
  active <- x[,1] - x[,2]
  as.numeric(mean(active) * scale)
}

###############################################################################
# 7) KPIs OPERATIVOS + MÉTRICAS DE PERFORMANCE
###############################################################################
# KPIs de rebalanceo: frecuencia, turnover, desviación promedio
rebalance_kpis <- function(log_df) {
  if (is.null(log_df) || nrow(log_df) == 0) {
    return(data.frame(Rebalance_Count=0, Rebalance_Rate=0, Avg_Turnover=0, Avg_MaxDev=NA_real_))
  }
  data.frame(
    Rebalance_Count = sum(log_df$rebalanced),
    Rebalance_Rate  = mean(log_df$rebalanced),
    Avg_Turnover    = mean(log_df$turnover[log_df$rebalanced], na.rm=TRUE),
    Avg_MaxDev      = mean(log_df$max_dev, na.rm=TRUE),
    stringsAsFactors = FALSE
  )
}

# Resumen performance clásico
portfolio_summary <- function(ret_xts, name) {
  ret_xts <- na.omit(ret_xts)
  data.frame(
    Portfolio = name,
    Annualized_Return = as.numeric(Return.annualized(ret_xts)),
    Annualized_Vol    = as.numeric(StdDev.annualized(ret_xts)),
    Sharpe            = as.numeric(SharpeRatio.annualized(ret_xts, Rf=0)),
    Max_Drawdown      = as.numeric(maxDrawdown(ret_xts)),
    stringsAsFactors = FALSE
  )
}

###############################################################################
# 8) EJECUCIÓN DE ESTRATEGIAS (RUN)
###############################################################################
# A) Periodic (ej. trimestral)
res_periodic <- backtest_rebalance(
  returns_log, w_target,
  rule="periodic", k=cfg_reb$k_periodic, band=cfg_reb$band,
  returns_are_log=cfg_reb$returns_are_log, verbose=FALSE
)

# B) Bands (ej. mensual, rebalancea solo si rompe banda)
res_bands <- backtest_rebalance(
  returns_log, w_target,
  rule="bands", k=cfg_reb$k_bands, band=cfg_reb$band,
  returns_are_log=cfg_reb$returns_are_log, verbose=FALSE
)

# C) GlidePath (opcional)
res_glide <- NULL
if (cfg_reb$use_glide && cfg_reb$free_risk_ticker %in% colnames(returns_log)) {
  equity_tickers <- setdiff(colnames(returns_log), cfg_reb$free_risk_ticker)
  res_glide <- backtest_glidepath(
    returns_log, equity_tickers, cfg_reb$free_risk_ticker,
    cfg_reb$horizon_years, cfg_reb$start_equity, cfg_reb$end_equity,
    k=cfg_reb$k_glide, band=cfg_reb$band, use_bands=cfg_reb$use_bands_glide,
    returns_are_log=cfg_reb$returns_are_log, verbose=FALSE
  )
}

###############################################################################
# 9) WEALTH -> RETORNOS DISCRETOS (PARA MÉTRICAS)
###############################################################################
r_periodic <- to_discrete_returns_from_wealth(res_periodic$wealth)
r_bands    <- to_discrete_returns_from_wealth(res_bands$wealth)
r_glide    <- if (!is.null(res_glide)) to_discrete_returns_from_wealth(res_glide$wealth) else NULL

###############################################################################
# 10) BENCHMARK (SPY o 60/40)
###############################################################################
bench_spy <- prices_to_returns(get_prices_yahoo("SPY", cfg_reb$bench_from), "discrete")
colnames(bench_spy) <- "SPY"

bench_6040_prices <- get_prices_yahoo(c("SPY", cfg_reb$free_risk_ticker), cfg_reb$bench_from)
bench_6040_rets <- prices_to_returns(bench_6040_prices, "discrete")

bench_6040 <- na.omit(0.60*bench_6040_rets[,"SPY"] + 0.40*bench_6040_rets[,cfg_reb$free_risk_ticker])
colnames(bench_6040) <- "Bench_60_40"

bench_use <- if (cfg_reb$benchmark == "SPY") bench_spy else bench_6040

###############################################################################
# 11) TABLA DE RESULTADOS (PERFORMANCE + TE/TD + KPIs)
###############################################################################
tbl <- bind_rows(
  cbind(portfolio_summary(r_periodic, "Periodic"), rebalance_kpis(res_periodic$log)),
  cbind(portfolio_summary(r_bands,    "Bands"),    rebalance_kpis(res_bands$log)),
  if (!is.null(r_glide)) cbind(portfolio_summary(r_glide, "GlidePath"), rebalance_kpis(res_glide$log)) else NULL
) %>%
  mutate(
    TE_Ann = c(
      tracking_error_annualized(r_periodic, bench_use),
      tracking_error_annualized(r_bands,    bench_use),
      if (!is.null(r_glide)) tracking_error_annualized(r_glide, bench_use) else NULL
    ),
    TD_Ann = c(
      tracking_difference_annualized(r_periodic, bench_use),
      tracking_difference_annualized(r_bands,    bench_use),
      if (!is.null(r_glide)) tracking_difference_annualized(r_glide, bench_use) else NULL
    )
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n===== RESULTADOS =====\n")
print(tbl)

###############################################################################
# 12) RECOMENDACIÓN “PROFESIONAL” (SCORE COST-AWARE)
###############################################################################
# Idea: no solo “mejor Sharpe”, sino mejor trade-off entre:
# - Sharpe (beneficio)
# - Tracking Error (desvío vs benchmark)
# - Rebalance_Rate y Avg_Turnover (carga operativa/costos proxy)
tbl <- tbl %>%
  mutate(
    Decision_Score = Sharpe - 0.5*TE_Ann - 0.1*Rebalance_Rate - 0.2*Avg_Turnover,
    Decision_Score = round(Decision_Score, 4)
  )

best_policy <- tbl %>%
  arrange(desc(Decision_Score)) %>%
  slice(1) %>%
  pull(Portfolio)

cat("\n===== RECOMENDACIÓN =====\n")
print(tbl[, c("Portfolio","Sharpe","TE_Ann","Rebalance_Rate","Avg_Turnover","Decision_Score")])
cat("Política recomendada:", best_policy, "\n")

###############################################################################
# 13) VISUALIZACIÓN COMPARATIVA
###############################################################################
R_comp <- na.omit(merge(r_periodic, r_bands, bench_use))
colnames(R_comp) <- c("Periodic","Bands","Benchmark")

if (!is.null(r_glide)) {
  R_comp <- na.omit(merge(r_periodic, r_bands, r_glide, bench_use))
  colnames(R_comp) <- c("Periodic","Bands","GlidePath","Benchmark")
}

charts.PerformanceSummary(R_comp, main="Estrategias vs Benchmark")
###############################################################################
