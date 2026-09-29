# =================================================================
# PREGUNTA 2: Probabilidad, normalidad e IC, prueba t, ANOVA/Kruskal-Wallis
# =================================================================

setwd("C:/EXAMEN")

# ---- 0. LIBRERIAS ----
suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(car)        # leveneTest()
  library(dunn.test)  # dunn.test() - post-hoc no parametrico
})

set.seed(123)
dir.create("plots", showWarnings = FALSE)

# ---- 1. CARGA DE LA BASE YA LIMPIA (generada en la Pregunta 1) ----
df_limpio <- read.csv("bankloan_limpio.csv", stringsAsFactors = FALSE)

niveles_ed <- c("Basica", "Secundaria", "Superior incompleta",
                "Superior completa", "Postgrado")
df_limpio$ed_f     <- factor(df_limpio$ed, levels = 1:5, labels = niveles_ed, ordered = TRUE)
df_limpio$impago   <- factor(df_limpio$default, levels = c(0, 1), labels = c("No", "Si"))


# =================================================================
# 2.1 PROBABILIDAD DEL SORTEO DE BECAS
# =================================================================
# El banco sortea 10 becas entre sus 700 clientes para terminar una carrera
# que dejaron trunca. Se asume que "estar en posibilidad de usar la beca"
# corresponde a los clientes con nivel educativo "Superior incompleta"
# (empezaron estudios superiores pero no los terminaron).

N     <- nrow(df_limpio)                          # tamanio de la poblacion (700)
K     <- sum(df_limpio$ed_f == "Superior incompleta")  # clientes con carrera trunca
n_sel <- 10                                        # becas sorteadas
k     <- 4                                         # clientes de interes entre los seleccionados

cat("== PREGUNTA 2.1: Probabilidad del sorteo ==\n")
cat("N (poblacion) =", N, "\n")
cat("K (con superior incompleta) =", K, " -> p =", round(K / N, 4), "\n")

# Modelo exacto: hipergeometrica (muestreo SIN reposicion sobre poblacion finita)
p_hiper <- dhyper(k, m = K, n = N - K, k = n_sel)

# Modelo aproximado: binomial (valido porque n_sel << N)
p_binom <- dbinom(k, size = n_sel, prob = K / N)

cat("P(X = 4) [Hipergeometrica, exacta] =", round(p_hiper, 5), "\n")
cat("P(X = 4) [Binomial, aproximada]     =", round(p_binom, 5), "\n\n")


# =================================================================
# 2.2 PRUEBAS DE NORMALIDAD E INTERVALOS DE CONFIANZA AL 95%
# =================================================================

vars_deuda <- c("debtinc", "creddebt", "othdebt")
etiquetas_deuda <- c(debtinc = "Deuda/Ingreso (%)",
                     creddebt = "Deuda tarjeta credito (miles US$)",
                     othdebt = "Otras deudas (miles US$)")

cat("== PREGUNTA 2.2: Normalidad e IC 95% ==\n")

resultados_norm <- lapply(vars_deuda, function(v) {
  x <- df_limpio[[v]]
  
  # Shapiro-Wilk (prueba de normalidad)
  sw <- shapiro.test(x)
  
  # Intervalo de confianza al 95% para la media (t de Student, valido por TCL, n grande)
  ic <- t.test(x, conf.level = 0.95)$conf.int
  
  data.frame(variable = etiquetas_deuda[v],
             media = round(mean(x), 3),
             shapiro_W = round(sw$statistic, 4),
             shapiro_p = signif(sw$p.value, 4),
             IC95_inf = round(ic[1], 3),
             IC95_sup = round(ic[2], 3))
})
resultados_norm <- do.call(rbind, resultados_norm)
print(resultados_norm, row.names = FALSE)

# Graficos Q-Q para verificar normalidad visualmente
png("plots/qqplots_deudas.png", width = 1200, height = 450, res = 130)
par(mfrow = c(1, 3))
for (v in vars_deuda) {
  qqnorm(df_limpio[[v]], main = etiquetas_deuda[v])
  qqline(df_limpio[[v]], col = "red")
}
dev.off()
par(mfrow = c(1, 1))
cat("\n")


# =================================================================
# 2.3 PRUEBA T DE UNA MUESTRA: ingresos de clientes con Titulo Superior
# =================================================================
# H0: mu <= 58   (el ingreso promedio NO supera los 58 mil dolares)
# H1: mu >  58   (el ingreso promedio SI supera los 58 mil dolares)
# alfa = 0.05, prueba unilateral derecha

cat("== PREGUNTA 2.3: Prueba t de una muestra (Titulo Superior) ==\n")

ingresos_titulo <- df_limpio$income[df_limpio$ed_f == "Superior completa"]
cat("n =", length(ingresos_titulo), " | media =", round(mean(ingresos_titulo), 2),
    " | sd =", round(sd(ingresos_titulo), 2), "\n")

test_t <- t.test(ingresos_titulo, mu = 58, alternative = "greater")
print(test_t)

if (test_t$p.value < 0.05) {
  cat("Conclusion: p-valor <", 0.05, "-> se rechaza H0. Si hay evidencia de que la media supera 58 mil.\n\n")
} else {
  cat("Conclusion: p-valor >=", 0.05, "-> no se rechaza H0. No hay evidencia suficiente de que",
      "la media supere los 58 mil dolares.\n\n")
}


# =================================================================
# 2.4 ANOVA: ingresos segun nivel educativo
# =================================================================
# H0: las medias de ingreso son iguales en todos los niveles educativos
# H1: al menos una media difiere
# alfa = 0.05

cat("== PREGUNTA 2.4: ANOVA ingresos ~ nivel educativo ==\n")

modelo_anova <- aov(income ~ ed_f, data = df_limpio)
print(summary(modelo_anova))

# --- Verificacion de supuestos ---
cat("\n-- Supuesto 1: Normalidad de los residuales (Shapiro-Wilk) --\n")
residuales <- residuals(modelo_anova)
print(shapiro.test(residuales))

cat("\n-- Supuesto 2: Homogeneidad de varianzas (Levene) --\n")
print(leveneTest(income ~ ed_f, data = df_limpio))

# Graficos de diagnostico de residuales
png("plots/anova_diagnostico.png", width = 1000, height = 450, res = 130)
par(mfrow = c(1, 2))
qqnorm(residuales, main = "QQ-plot de residuales"); qqline(residuales, col = "red")
plot(fitted(modelo_anova), residuales,
     xlab = "Valores ajustados", ylab = "Residuales",
     main = "Residuales vs. ajustados")
abline(h = 0, col = "red", lty = 2)
dev.off()
par(mfrow = c(1, 1))

# --- Como se incumplen los supuestos, se corrobora con Kruskal-Wallis ---
cat("\n-- Alternativa no parametrica: Kruskal-Wallis --\n")
kw <- kruskal.test(income ~ ed_f, data = df_limpio)
print(kw)

# --- Comparaciones multiples ---
cat("\n-- Comparaciones multiples de Tukey (HSD) sobre el ANOVA --\n")
tukey <- TukeyHSD(modelo_anova)
print(tukey)

cat("\n-- Comparaciones multiples de Dunn (post-hoc de Kruskal-Wallis, Bonferroni) --\n")
dunn.test(df_limpio$income, df_limpio$ed_f, method = "bonferroni")

# Boxplot final: ingresos por nivel educativo
p_ingr_ed <- ggplot(df_limpio, aes(x = ed_f, y = income, fill = ed_f)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Ingresos segun nivel educativo", x = "Nivel educativo",
       y = "Ingreso (miles US$)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
print(p_ingr_ed)
ggsave("plots/boxplot_ingresos_ed.png", p_ingr_ed, width = 7, height = 4.5, dpi = 130)

cat("\nOK - Pregunta 2 terminada\n")
