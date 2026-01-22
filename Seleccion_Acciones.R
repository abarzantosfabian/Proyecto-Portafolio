###############################################################################
# SCRIPT 1 — SELECCIÓN DE ACTIVOS (Yahoo Finance)
# -----------------------------------------------------------------------------
# OBJETIVO
# Rankear un universo de acciones y seleccionar un subconjunto (top_n) para
# construir un portafolio posteriormente.
#
# ENFOQUE (PROFESIONAL / ROBUSTO CON YAHOO)
# 1) Señal principal (estable): MOMENTUM 12–1, calculado desde precios históricos.
#    - momentum 12–1: retorno (t-1 mes) / (t-12 meses) - 1
# 2) Señales complementarias ("fundamental-lite") desde Yahoo vía getQuote:
#    - P/E, Market Cap, Beta y Dividend Yield (cuando estén disponibles).
#
# SALIDAS
# - sel$ranked_table: tabla ordenada con métricas + score
# - sel$tickers_selected: vector de tickers top_n (insumo del Script 2)
#
# INSTRUCCIONES DE USO
# 1) Edita el bloque CONFIG: universe, top_n, from_date_prices, weights.
# 2) Ejecuta el script completo.
# 3) Si aparece warning por pocos datos de soporte, el ranking se basará más
#    en momentum (esto es intencional para robustez).
###############################################################################

###############################################################################
# 0) PAQUETES (CARGA CONTROLADA)
###############################################################################
# Esta sección carga los paquetes requeridos. En un entorno profesional se
# recomienda fijar versiones vía renv, pero aquí lo mantenemos simple.
suppressPackageStartupMessages({
  library(quantmod)
  library(PerformanceAnalytics)
  library(xts)
  library(zoo)
  library(dplyr)
  library(tibble)
})

###############################################################################
# 1) CONFIGURACIÓN (EDITA AQUÍ)
###############################################################################
# universe: universo de acciones a rankear
# top_n: cantidad final de seleccionadas
# from_date_prices: fecha de inicio para precios (>=2 años recomendado)
# trading_days_month: aproximación de días hábiles por mes (21 típico)
# score_weights: ponderaciones del score (pueden sumar 1 o no)

cfg_sel <- list(
  universe = c("AAPL","MSFT","AMZN","GOOGL","NVDA","META","JPM","V","MA","UNH",
               "XOM","COST","PEP","KO","AVGO","LLY","HD","PG","ADBE","TSLA"),
  top_n = 8,
  from_date_prices = "2022-01-01",
  trading_days_month = 21,
  score_weights = list(
    value = 0.35,      # P/E bajo
    momentum = 0.35,   # momentum alto
    size = 0.10,       # market cap alto(proxy de “quality/mega cap”)
    risk = 0.10,       # beta baja
    yield = 0.10       # dividend yield alto
  )
)

###############################################################################
# 2) UTILIDADES (PARSING + NORMALIZACIÓN)
###############################################################################
# Estas funciones convierten strings típicos de Yahoo ("2.3T", "1.2B", "3.5%")
# y entregan medidas robustas (winsorize, zscore_safe) para scoring.
parse_suffix_number <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- as.character(x)
  x <- gsub(",", "", x)
  if (is.na(x) || x == "" || x == "N/A") return(NA_real_)
  
  mult <- 1
  last <- substr(x, nchar(x), nchar(x))
  if (last %in% c("T","B","M","K")) {
    mult <- switch(last, T=1e12, B=1e9, M=1e6, K=1e3)
    x <- substr(x, 1, nchar(x) - 1)
  }
  val <- suppressWarnings(as.numeric(x))
  if (is.na(val)) NA_real_ else val * mult
}

parse_percent <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- as.character(x)
  x <- gsub("%", "", x)
  x <- gsub(",", "", x)
  val <- suppressWarnings(as.numeric(x))
  if (is.na(val)) NA_real_ else val / 100
}

winsorize <- function(x, p = 0.01) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(x)
  qs <- stats::quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  x[x < qs[1]] <- qs[1]
  x[x > qs[2]] <- qs[2]
  x
}

zscore_safe <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

###############################################################################
# 3) EXTRACCIÓN DE DATOS (YAHOO)
###############################################################################
# 3.1) Precios ajustados desde Yahoo (Adjusted Close)
# - Se usa para construir momentum (señal principal)
get_prices_yahoo <- function(tickers, from) {
  env <- new.env()
  suppressWarnings(getSymbols(tickers, src = "yahoo", from = from, env = env, auto.assign = TRUE))
  plist <- lapply(tickers, function(tk) Ad(get(tk, envir = env)))
  p <- do.call(merge, plist)
  colnames(p) <- tickers
  na.omit(p)
}

# 3.2) “Fundamental-lite” desde Yahoo vía getQuote
# - Yahoo no garantiza cobertura completa. Por eso se parsea con tolerancia.
# - Nota: las columnas pueden cambiar (dependen de la fuente).
yahoo_get_quote <- function(tickers) {
  q <- suppressWarnings(getQuote(tickers, src = "yahoo"))
  q <- tibble::rownames_to_column(as.data.frame(q), var = "ticker")
  colnames(q) <- gsub("\\.", "_", colnames(q))
  
  mc_col   <- intersect(c("Market_Cap","MarketCap"), names(q))[1]
  pe_col   <- intersect(c("PE","P_E"), names(q))[1]
  beta_col <- intersect(c("Beta"), names(q))[1]
  yld_col  <- intersect(c("Yield"), names(q))[1]
  
  q %>%
    mutate(
      market_cap = if (!is.na(mc_col)) parse_suffix_number(.data[[mc_col]]) else NA_real_,
      pe_ttm     = if (!is.na(pe_col)) suppressWarnings(as.numeric(.data[[pe_col]])) else NA_real_,
      beta       = if (!is.na(beta_col)) suppressWarnings(as.numeric(.data[[beta_col]])) else NA_real_,
      div_yield  = if (!is.na(yld_col)) parse_percent(.data[[yld_col]]) else NA_real_
    ) %>%
    select(ticker, market_cap, pe_ttm, beta, div_yield)
}

###############################################################################
# 4) SEÑAL PRINCIPAL: MOMENTUM 12–1
###############################################################################
# momentum 12–1:
# - compara precio a 1 mes vs precio a 12 meses (aprox).
# - requiere suficiente historia (>= ~13 meses de precios).
compute_momentum_12_1 <- function(prices_xts, trading_days_month = 21) {
  n <- nrow(prices_xts)
  need <- 12 * trading_days_month + trading_days_month + 5
  if (n < need) stop("Insuficientes datos para momentum 12–1. Usa from_date_prices más antigua.")
  idx_12m <- 12 * trading_days_month
  idx_1m  <- 1  * trading_days_month
  p_1m  <- as.numeric(prices_xts[n - idx_1m, ])
  p_12m <- as.numeric(prices_xts[n - idx_12m, ])
  mom <- (p_1m / p_12m) - 1
  names(mom) <- colnames(prices_xts)
  mom
}

###############################################################################
# 5) SCORING + SELECCIÓN (ROBUSTO CON NA)
###############################################################################
# Reglas de selección profesional:
# - Exige momentum (señal core).
# - Exige al menos 1 métrica adicional si existe; si quedan pocos tickers,
#   relaja filtro automáticamente a solo momentum.
# - Penaliza tickers con demasiados NA (suave, no destructivo).
select_assets_yahoo <- function(cfg) {
  
  tickers <- unique(cfg$universe)
  
  message("[1/4] Descargando métricas Yahoo (getQuote)...")
  q <- yahoo_get_quote(tickers)
  
  message("[2/4] Descargando precios Yahoo para momentum...")
  prices <- get_prices_yahoo(tickers, from = cfg$from_date_prices)
  
  # Solo podemos rankear tickers con precios descargados correctamente
  tickers_ok_prices <- colnames(prices)
  q <- q %>% filter(ticker %in% tickers_ok_prices)
  
  if (nrow(q) == 0) stop("No se pudo obtener getQuote para ningún ticker con precios válidos.")
  
  message("[3/4] Calculando momentum 12–1...")
  mom <- compute_momentum_12_1(prices[, q$ticker], cfg$trading_days_month)
  mom_df <- data.frame(ticker = names(mom), momentum_12_1 = as.numeric(mom), stringsAsFactors = FALSE)
  
  df <- left_join(q, mom_df, by = "ticker")
  
  message("[4/4] Diagnóstico de NA (cobertura de datos):")
  print(colSums(is.na(df)))
  
  # Filtro principal: momentum + al menos 1 métrica soporte
  df2 <- df %>%
    mutate(
      has_mom = !is.na(momentum_12_1),
      n_support = rowSums(!is.na(across(c(pe_ttm, market_cap, beta, div_yield))))
    ) %>%
    filter(has_mom, n_support >= 1) %>%
    select(-has_mom, -n_support)
  
  # Fallback: si queda poco, usamos solo momentum (degradación controlada)
  if (nrow(df2) < max(5, cfg$top_n)) {
    warning(
      paste0(
        "Pocos tickers con datos de soporte (", nrow(df2), "). ",
        "Relajando filtro: ranking se basará principalmente en Momentum + lo disponible."
      )
    )
    df2 <- df %>% filter(!is.na(momentum_12_1))
    if (nrow(df2) == 0) stop("Ni siquiera hay momentum disponible. Revisa tickers o from_date_prices.")
  }
  
  w <- cfg$score_weights
  
  ranked <- df2 %>%
    mutate(
      # Robustez ante outliers
      pe_w   = winsorize(pe_ttm),
      cap_w  = winsorize(market_cap),
      beta_w = winsorize(beta),
      yld_w  = winsorize(div_yield),
      mom_w  = winsorize(momentum_12_1),
      
      # Señales normalizadas
      z_value = -zscore_safe(pe_w),      # menor P/E => mejor
      z_size  =  zscore_safe(cap_w),     # mayor market cap => mejor
      z_risk  = -zscore_safe(beta_w),    # menor beta => mejor
      z_yield =  zscore_safe(yld_w),     # mayor yield => mejor
      z_mom   =  zscore_safe(mom_w),     # mayor momentum => mejor
      
      # Score final (ponderado)
      score = w$value    * z_value +
        w$size     * z_size  +
        w$risk     * z_risk  +
        w$yield    * z_yield +
        w$momentum * z_mom,
      
      # Penalización suave por falta de datos fundamentales
      n_na = rowSums(is.na(across(c(pe_ttm, market_cap, beta, div_yield)))),
      score = score - 0.05 * n_na
    ) %>%
    arrange(desc(score))
  
  selected <- head(ranked$ticker, cfg$top_n)
  
  list(
    raw_table = df2,
    ranked_table = ranked,
    tickers_selected = selected
  )
}

###############################################################################
# 6) EJECUCIÓN (RUN)
###############################################################################
# Este bloque ejecuta el pipeline completo y deja los outputs en `sel`.
sel <- select_assets_yahoo(cfg_sel)

cat("\n===== TOP RANKING (primeras 15 filas) =====\n")
print(head(sel$ranked_table, 15))

cat("\n===== TICKERS SELECCIONADOS =====\n")
print(sel$tickers_selected)

tickers_eq <- sel$tickers_selected
###############################################################################