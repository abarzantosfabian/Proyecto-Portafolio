📊 Quantitative Portfolio Engine (R)

Motor cuantitativo en R para selección de activos, construcción de portafolios y rebalanceo, utilizando datos de Yahoo Finance y prácticas habituales en la industria de gestión de activos.

El objetivo es construir un portafolio óptimo y recomendar una política de rebalanceo en función de desempeño, riesgo, tracking error y carga operativa. 

🧩 Estructura del Proyecto

├── Seleccion_Acciones.R   # Selección y ranking de activos    
├── Portfolio.R            # Optimización y construcción del portafolio  
├── Rebalanceo.R           # Backtest y recomendación de rebalanceo  
└── README.md

🚀 Flujo de Uso

Ejecuta los scripts en orden.

1️⃣ Selección de Activos (Seleccion_Acciones.R)

- Rankea un universo de acciones usando:
- Momentum 12–1 (señal principal)
- Métricas “fundamental-lite” (P/E, Market Cap, Beta, Dividend Yield)
- Maneja datos faltantes de forma robusta
- Selecciona los top N activos

2️⃣ Construcción del Portafolio (Portfolio.R)

- Descarga precios históricos
- Calcula retornos (log)
- Optimiza pesos mediante media–varianza
- Aplica restricciones realistas: Long-only, límites por activo, activo libre de riesgo (ej. AGG)

3️⃣ Rebalanceo y Recomendación (Rebalanceo.R)

- Evalúa políticas de rebalanceo:
- Periódico
- Bandas
- Glide Path + Bandas
- Compara contra benchmark (SPY o 60/40)

Calcula:

- Retorno, volatilidad, Sharpe
- Tracking Error y Tracking Difference
- Turnover y frecuencia de rebalanceo
- Recomienda la política con mejor Decision Score

📈 Benchmarks

- SPY (S&P 500)
- Portafolio 60/40 (SPY / AGG)

⚠️ Notas Importantes

- No incluye impuestos ni costos de ejecución reales
- Costos de transacción se aproximan vía turnover
- No constituye recomendación financiera

🎯 Uso Esperado

Este proyecto está orientado a:
- Análisis cuantitativo
- Evaluación de políticas de rebalanceo
- Investigación aplicada en gestión de activos

Para más detalles, utilizar Instrucciones_Proyecto_Portfolio

