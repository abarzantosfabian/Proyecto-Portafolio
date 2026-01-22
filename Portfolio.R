###############################################################################
# SCRIPT 2 — CONSTRUCCIÓN DEL PORTAFOLIO (Optimización Media–Varianza)
# -----------------------------------------------------------------------------
# OBJETIVO
# Construir un portafolio “objetivo” (pesos estratégicos) a partir de los tickers
# seleccionados en el Script 1. Este portafolio objetivo será el punto de
# referencia para las reglas de rebalanceo del Script 3.
#
# MÉTODO
# - Optimización media–varianza (MV) con:
#   - Long-only (no cortos)
#   - Full investment: suma de pesos = 1
#   - Box constraints: min/max por activo
#   - Solver: DEoptim (robusto para problemas no convexos / objetivos múltiples)
#
# NOTAS IMPORTANTES
# - Los retornos se calculan como "log" por defecto, lo que facilita el backtest
#   multiplicativo posterior. (Script 3 convierte a discretos para métricas)
# - Se incluye opcionalmente un activo libre de riesgo (ej. AGG) con piso mínimo.
#
# SALIDAS
# - port$weights_target : pesos objetivo (vector nombrado)
# - port$returns_log    : retornos (xts) usados para optimización/backtest
# - port$prices         : precios (xts)
# - port$portfolio_spec : especificación de restricciones y objetivos (auditoría)
# - port$opt            : objeto optimizado completo (diagnóstico)
#
# INSTRUCCIONES DE USO
# 1) Ejecuta Script 1 para crear `sel$tickers_selected`.
# 2) Edita el bloque CONFIG (cfg_port) si quieres:
#    - cambiar min/max por activo
#    - incluir o no el activo defensivo (AGG)
#    - ajustar aversión al riesgo
# 3) Ejecuta este script de arriba hacia abajo.
# 4) Usa `port$weights_target` como input del Script 3.
###############################################################################

###############################################################################
# 0) PAQUETES (CARGA CONTROLADA)
###############################################################################
suppressPackageStartupMessages({
  library(quantmod)
  library(PerformanceAnalytics)
  library(xts)
  library(zoo)
  library(PortfolioAnalytics)
  library(DEoptim)
})

###############################################################################
# 1) CONFIGURACIÓN (EDITA AQUÍ)
###############################################################################
cfg_port <- list(
  from_date_prices = "2022-01-01",  # Fecha inicial para extracción de datos
  returns_method = "log",           # "log" recomendado para backtest con exp()
  min_w = 0.00,                     # Ponderación mínima por activo
  max_w = 0.35,                     # Ponderación máxima por activo
  risk_aversion = 10,               # >0: mayor = más aversión al riesgo
  seed = 123,                       # Reproducibilidad DEoptim
  include_defensive = TRUE,         # Incluir activo libre de riesgo (TRUE/FALSE)
  free_risk_ticker = "AGG",         # Proxy libre de riesgo (bonos)
  free_risk_min_w = 0.05            # Piso mínimo del defensivo (0 si no quieres piso)
)

###############################################################################
# 2) INPUTS (DESDE SCRIPT 1)
###############################################################################
# Este script asume que ya ejecutaste el Script 1 y existe el objeto `sel`.
if (!exists("sel")) stop("No existe objeto 'sel'. Ejecuta Script 1 primero (sel$tickers_selected).")

tickers_eq <- sel$tickers_selected

# Universo final para optimización: acciones seleccionadas + libre de riesgo opcional
tickers <- if (cfg_port$include_defensive) {
  unique(c(tickers_eq, cfg_port$free_risk_ticker))
} else {
  tickers_eq
}

###############################################################################
# 3) DATOS (PRECIOS Y RETORNOS DESDE YAHOO)
###############################################################################
# get_prices_yahoo()
# - Descarga precios ajustados (Adjusted Close) de Yahoo Finance.
# - Devuelve xts con una columna por ticker.
get_prices_yahoo <- function(tickers, from) {
  env <- new.env()
  suppressWarnings(
    getSymbols(tickers, src = "yahoo", from = from, env = env, auto.assign = TRUE)
  )
  plist <- lapply(tickers, function(tk) Ad(get(tk, envir = env)))
  p <- do.call(merge, plist)
  colnames(p) <- tickers
  na.omit(p)
}

# prices_to_returns()
# - Convierte precios a retornos ("log" o "discrete").
# - Se omiten NA (primer periodo siempre es NA por definición de Return.calculate)
prices_to_returns <- function(prices, method = "log") {
  na.omit(Return.calculate(prices, method = method))
}

# Ejecutar descarga y cálculo de retornos
prices  <- get_prices_yahoo(tickers, from = cfg_port$from_date_prices)
returns <- prices_to_returns(prices, method = cfg_port$returns_method)

# Validación mínima (práctica profesional)
if (nrow(returns) < 60) warning("Pocos datos de retornos (<60 obs). Resultados pueden ser inestables.")
if (anyNA(returns)) returns <- na.omit(returns)

###############################################################################
# 4) OPTIMIZACIÓN (PORTFOLIOANALYTICS + DEoptim)
###############################################################################
# build_target_portfolio_mv()
# - Construye especificación del portafolio y optimiza pesos.
# - Incluye:
#   * Full investment: suma(w)=1
#   * Long-only: w>=0
#   * Box: min/max por activo
#   * Piso libre de riesgo opcional (ej. AGG >= 5%)
#
# Nota de robustez:
# - En algunos entornos, PortfolioAnalytics puede fallar con ".storage not found".
#   Por eso se inicializa .storage si no existe.
build_target_portfolio_mv <- function(returns,
                                      min_w,
                                      max_w,
                                      risk_aversion,
                                      seed,
                                      free_risk_ticker = NULL,
                                      free_risk_min_w = 0.0) {
  
  set.seed(seed)
  assets <- colnames(returns)
  
  # 1) Definir portafolio
  port <- PortfolioAnalytics::portfolio.spec(assets)
  
  # 2) Restricciones “core”
  port <- PortfolioAnalytics::add.constraint(port, type = "full_investment") # suma = 1
  port <- PortfolioAnalytics::add.constraint(port, type = "long_only")       # w >= 0
  port <- PortfolioAnalytics::add.constraint(port, type = "box", min = min_w, max = max_w)
  
  # 3) Restricción libre de riesgo (piso mínimo) si aplica
  if (!is.null(free_risk_ticker) &&
      free_risk_ticker %in% assets &&
      free_risk_min_w > 0) {
    
    port <- PortfolioAnalytics::add.constraint(
      port,
      type = "box",
      min = ifelse(assets == free_risk_ticker, free_risk_min_w, min_w),
      max = max_w
    )
  }
  
  # 4) Objetivos
  # - Riesgo: StdDev (volatilidad)
  # - Retorno: mean
  port <- PortfolioAnalytics::add.objective(port, type = "risk",   name = "StdDev", multiplier = risk_aversion)
  port <- PortfolioAnalytics::add.objective(port, type = "return", name = "mean",   multiplier = 1)
  
  # 5) Workaround de robustez
  if (!exists(".storage", envir = .GlobalEnv)) .storage <<- new.env(parent = emptyenv())
  
  # 6) Optimización (DEoptim)
  opt <- PortfolioAnalytics::optimize.portfolio(
    R = returns,
    portfolio = port,
    optimize_method = "DEoptim",
    trace = FALSE
  )
  
  # 7) Extraer pesos
  w <- PortfolioAnalytics::extractWeights(opt)
  w <- w / sum(w)
  
  list(
    opt = opt,
    weights_target = w,
    portfolio_spec = port
  )
}

###############################################################################
# 5) EJECUCIÓN (RUN)
###############################################################################
port <- build_target_portfolio_mv(
  returns = returns,
  min_w = cfg_port$min_w,
  max_w = cfg_port$max_w,
  risk_aversion = cfg_port$risk_aversion,
  seed = cfg_port$seed,
  free_risk_ticker = if (cfg_port$include_defensive) cfg_port$free_risk_ticker else NULL,
  free_risk_min_w = cfg_port$free_risk_min_w
)

###############################################################################
# 6) SALIDAS Y CHEQUEOS (AUDITORÍA)
###############################################################################
cat("\n===== PESOS OBJETIVO (weights_target) =====\n")
print(round(port$weights_target, 4))
cat("Suma pesos:", round(sum(port$weights_target), 6), "\n")

# Guardar data para el Script 3
port$prices <- prices
port$returns_log <- returns
port$tickers <- tickers

# Chequeos típicos (mejor práctica)
if (any(port$weights_target < -1e-8)) warning("Existen pesos negativos (no esperado en long-only).")
if (abs(sum(port$weights_target) - 1) > 1e-6) warning("Los pesos no suman 1 (revisa restricciones).")

cat("\nListo. Usa port$weights_target en el Script 3.\n")
###############################################################################
