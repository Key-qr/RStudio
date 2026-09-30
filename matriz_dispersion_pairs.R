# =================================================================
# PAIRS AVANZADO - bankloan_limpio.csv
# Matriz de dispersion con correlaciones, histogramas y color por impago
# =================================================================

setwd("C:/EXAMEN")
df_limpio <- read.csv("bankloan_limpio.csv", stringsAsFactors = FALSE)
df_limpio$impago <- factor(df_limpio$impago, levels = c("No", "Si"))

vars_num <- c("age", "employ", "address", "income", "debtinc", "creddebt", "othdebt")
colores  <- c("No" = "#2c7fb8", "Si" = "#de2d26")
col_pts  <- colores[df_limpio$impago]

# --- Panel superior: coeficiente de correlacion (tamaño según fuerza) ---
panel.cor <- function(x, y, digits = 2, cex.cor = 1.4, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  txt <- format(round(r, digits), nsmall = digits)
  color_txt <- ifelse(abs(r) > 0.5, "#de2d26", "black")
  text(0.5, 0.5, txt, cex = cex.cor * (0.4 + abs(r) * 1.5), col = color_txt)
}

# --- Diagonal: histograma de cada variable ---
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "#a6cee3", border = "white")
}

# --- Panel inferior: dispersion coloreada por impago + linea suavizada ---
panel.puntos <- function(x, y, ...) {
  points(x, y, pch = 19, cex = 0.5, col = col_pts)
  lines(lowess(x, y), col = "black", lwd = 1.5)
}

# --- Grafico final ---
pairs(df_limpio[, vars_num],
      main = "Matriz de dispersion avanzada - segun impago",
      upper.panel = panel.cor,
      diag.panel  = panel.hist,
      lower.panel = panel.puntos,
      gap = 0.3)

legend("bottom", legend = levels(df_limpio$impago), col = colores,
       pch = 19, horiz = TRUE, bty = "n", inset = -0.02, xpd = TRUE)
