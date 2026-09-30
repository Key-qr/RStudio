# Análisis Estadístico en R — Dataset Bankloan

Scripts de R para el análisis de un dataset de créditos bancarios: limpieza 
de datos, estadística descriptiva, probabilidad, pruebas de hipótesis y ANOVA.

## Contenido

| Archivo | Descripción |
|---|---|
| `Limpieza_de_datos.R` | Limpieza de valores faltantes/erróneos, imputación (mediana/moda), descriptivos univariados y bivariados |
| `Probabilidades_pruebas_y_ANOVA.R` | Probabilidad binomial/hipergeométrica, pruebas de normalidad, prueba t de una muestra, ANOVA con Tukey y Kruskal-Wallis |
| `matriz_dispersion_pairs.R` | Matriz de dispersión (pairs) entre variables numéricas, coloreada por variable de impago |

## Técnicas

- Imputación de datos faltantes según el tipo y asimetría de cada variable
- Estadística descriptiva univariada y bivariada
- Distribución binomial e hipergeométrica
- Pruebas de normalidad (Shapiro-Wilk) e intervalos de confianza
- Prueba t de una muestra
- ANOVA, Kruskal-Wallis y comparaciones múltiples (Tukey, Dunn)

## Herramientas
R · ggplot2 · dplyr · car · nortest
