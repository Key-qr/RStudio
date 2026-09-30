# Análisis Estadístico en R — Dataset Bankloan

Análisis de un dataset de créditos bancarios (700 clientes): limpieza de 
datos, estadística descriptiva, probabilidad, pruebas de hipótesis y ANOVA.

## Contenido

| Archivo | Descripción |
|---|---|
| `Limpieza_de_datos.R` | Limpieza de valores faltantes/erróneos, imputación (mediana/moda), descriptivos univariados y bivariados |
| `Probabilidades_pruebas_y_ANOVA.R` | Probabilidad binomial/hipergeométrica, pruebas de normalidad, prueba t de una muestra, ANOVA con Tukey y Kruskal-Wallis |
| `matriz_dispersion_pairs.R` | Matriz de dispersión (pairs) entre variables numéricas, coloreada por impago |
| `data/Bankloan.txt` | Dataset original |
| `plots/` | Gráficos generados por los scripts |

## Principales hallazgos

- 26.1% de los clientes está en impago
- Ingreso, deuda de tarjeta y otras deudas están fuertemente sesgadas a la derecha
- Los clientes en impago tienen en promedio menos antigüedad laboral y mayor endeudamiento
- El ingreso varía significativamente según el nivel educativo (ANOVA + Kruskal-Wallis, p < 0.001)

## Cómo correrlo

1. Clonar el repositorio y abrir en RStudio
2. Instalar paquetes:
```r
   install.packages(c("ggplot2", "dplyr", "car", "nortest", "gridExtra"))
```
3. Correr `Limpieza_de_datos.R` primero (genera el CSV limpio que usan los otros scripts)
4. Correr `Probabilidades_pruebas_y_ANOVA.R`
5. Correr `matriz_dispersion_pairs.R`

## Herramientas
R · ggplot2 · dplyr · car · nortest
