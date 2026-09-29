# =================================================================
# PREGUNTA 1: Limpieza de datos + descriptivos univariados y bivariados
# =================================================================

setwd("C:/EXAMEN")

# ---- 0. LIBRERIAS ----
suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
})

set.seed(123)
dir.create("plots", showWarnings = FALSE)

# ---- 1. CARGA DE DATOS ----
df <- read.csv("Bankloan.txt", stringsAsFactors = FALSE)

cat("== Antes de limpiar ==\n")
cat("Filas:", nrow(df), "\n")
print(colSums(is.na(df)))
cat("Valores unicos de default (crudo):", paste(unique(df$default), collapse = " | "), "\n")
cat("Max de age:", max(df$age, na.rm = TRUE), "\n\n")


# =================================================================
# 2. LIMPIEZA DE DATOS
# =================================================================

# 2.1 default: viene con basura de captura ('0' y ":0"). Se limpia a 0/1.
df$default <- gsub("[':]", "", df$default)
df$default <- as.integer(df$default)
df$impago  <- factor(df$default, levels = c(0, 1), labels = c("No", "Si"))

# 2.2 age: 136 anios es un error de digitacion (limite fisiologico), se marca
#     como NA antes de imputar, junto con los NA originales
df$age[df$age > 100] <- NA

# 2.3 ed: es ordinal (nivel educativo 1-5), se deja como factor ordenado
niveles_ed <- c("Basica", "Secundaria", "Superior incompleta",
                "Superior completa", "Postgrado")
df$ed_f <- factor(df$ed, levels = 1:5, labels = niveles_ed, ordered = TRUE)

# 2.4 Diagnostico de asimetria (decide el metodo de imputacion)
cat("== Diagnostico de asimetria (skewness) ==\n")
skew_age    <- mean((df$age - mean(df$age, na.rm = TRUE))^3, na.rm = TRUE) /
  sd(df$age, na.rm = TRUE)^3
skew_income <- mean((df$income - mean(df$income, na.rm = TRUE))^3, na.rm = TRUE) /
  sd(df$income, na.rm = TRUE)^3
cat("Skewness age:   ", round(skew_age, 2), "\n")
cat("Skewness income:", round(skew_income, 2), "\n\n")

# 2.5 IMPUTACION
# age: distribucion razonablemente simetrica -> MEDIANA
mediana_age <- median(df$age, na.rm = TRUE)
df$age_imp  <- ifelse(is.na(df$age), mediana_age, df$age)

# income: fuerte asimetria a la derecha -> MEDIANA (robusta a outliers)
mediana_income <- median(df$income, na.rm = TRUE)
df$income_imp  <- ifelse(is.na(df$income), mediana_income, df$income)

# ed: categorica ordinal -> MODA (categoria mas frecuente)
moda_ed      <- as.integer(names(sort(table(df$ed), decreasing = TRUE))[1])
df$ed_imp    <- ifelse(is.na(df$ed), moda_ed, df$ed)
df$ed_imp_f  <- factor(df$ed_imp, levels = 1:5, labels = niveles_ed, ordered = TRUE)

cat("Mediana usada para age:", mediana_age, "\n")
cat("Mediana usada para income:", mediana_income, "\n")
cat("Moda usada para ed:", moda_ed, "\n\n")

cat("== Despues de limpiar (NAs restantes) ==\n")
print(colSums(is.na(df[, c("age_imp", "ed_imp", "employ", "address", "income_imp",
                           "debtinc", "creddebt", "othdebt", "default")])))

# 2.6 Dataset final limpio
df_limpio <- df %>%
  transmute(age = age_imp, ed = ed_imp, ed_f = ed_imp_f, employ, address,
            income = income_imp, debtinc, creddebt, othdebt,
            default, impago)

write.csv(df_limpio, "bankloan_limpio.csv", row.names = FALSE)


# =================================================================
# 3. DESCRIPTIVOS UNIVARIADOS
# =================================================================

tema <- theme_minimal(base_size = 10)

vars_num <- c("age", "employ", "address", "income", "debtinc", "creddebt", "othdebt")
etiquetas <- c(age = "Edad (anios)",
               employ = "Anios en el empleo actual",
               address = "Anios en la direccion actual",
               income = "Ingreso (miles US$)",
               debtinc = "Deuda/Ingreso (%)",
               creddebt = "Deuda tarjeta credito (miles US$)",
               othdebt = "Otras deudas (miles US$)")

# --- 3.1 Panel con los 7 histogramas juntos ---
lista_hist <- lapply(vars_num, function(v) {
  ggplot(df_limpio, aes(x = .data[[v]])) +
    geom_histogram(fill = "#2c7fb8", color = "white", bins = 25) +
    labs(title = etiquetas[v], x = NULL, y = "Frecuencia") +
    tema
})
panel_hist <- grid.arrange(grobs = lista_hist, ncol = 3)
ggsave("plots/panel_histogramas.png", panel_hist, width = 13, height = 9, dpi = 130)

# --- 3.2 Panel con los 7 boxplots juntos ---
lista_box <- lapply(vars_num, function(v) {
  ggplot(df_limpio, aes(y = .data[[v]])) +
    geom_boxplot(fill = "#a6cee3") +
    labs(title = etiquetas[v], y = NULL) +
    tema + theme(axis.text.x = element_blank())
})
panel_box <- grid.arrange(grobs = lista_box, ncol = 3)
ggsave("plots/panel_boxplots.png", panel_box, width = 13, height = 9, dpi = 130)

# --- 3.3 Panel con las 2 graficas de barras (categoricas) ---
p_ed <- ggplot(df_limpio, aes(x = ed_f)) +
  geom_bar(fill = "#2c7fb8") +
  labs(title = "Distribucion del nivel educativo", x = NULL, y = "Frecuencia") +
  tema + theme(axis.text.x = element_text(angle = 20, hjust = 1))

p_default <- ggplot(df_limpio, aes(x = impago)) +
  geom_bar(fill = c("#2c7fb8", "#de2d26")) +
  labs(title = "Distribucion de impago", x = NULL, y = "Frecuencia") +
  tema

panel_cat <- grid.arrange(p_ed, p_default, ncol = 2)
ggsave("plots/panel_categoricas.png", panel_cat, width = 9, height = 4.5, dpi = 130)

# --- 3.4 Tablas resumen univariado (se imprimen en consola) ---
cat("\n========== RESUMEN UNIVARIADO ==========\n")
cat("\n-- Resumen numerico --\n")
print(summary(df_limpio[, vars_num]))
cat("\n-- Desviacion estandar --\n")
print(sapply(df_limpio[, vars_num], sd))
cat("\n-- Distribucion nivel educativo --\n")
print(table(df_limpio$ed_f))
print(round(prop.table(table(df_limpio$ed_f)) * 100, 1))
cat("\n-- Distribucion impago --\n")
print(table(df_limpio$impago))
print(round(prop.table(table(df_limpio$impago)) * 100, 1))


# =================================================================
# 4. DESCRIPTIVOS BIVARIADOS: impago vs las demas variables
# =================================================================

cat("\n========== RESUMEN BIVARIADO (impago vs demas variables) ==========\n")

cat("\n-- Medias y medianas por grupo de impago --\n")
tab <- df_limpio %>%
  group_by(impago) %>%
  summarise(across(all_of(vars_num), list(media = mean, mediana = median),
                   .names = "{.col}_{.fn}"))
print(as.data.frame(tab))

cat("\n-- Tabla de contingencia: nivel educativo vs impago (conteo y % fila) --\n")
tc <- table(df_limpio$ed_f, df_limpio$impago)
print(tc)
print(round(prop.table(tc, 1) * 100, 1))

# --- 4.1 Panel con los 7 boxplots bivariados (impago vs cada variable) ---
lista_biv <- lapply(vars_num, function(v) {
  ggplot(df_limpio, aes(x = impago, y = .data[[v]], fill = impago)) +
    geom_boxplot() +
    labs(title = etiquetas[v], x = NULL, y = NULL) +
    scale_fill_manual(values = c("#2c7fb8", "#de2d26")) +
    tema + theme(legend.position = "none")
})
panel_biv <- grid.arrange(grobs = lista_biv, ncol = 3)
ggsave("plots/panel_bivariado.png", panel_biv, width = 13, height = 9, dpi = 130)

# --- 4.2 Impago por nivel educativo (grafico individual) ---
p_biv_ed <- ggplot(df_limpio, aes(x = ed_f, fill = impago)) +
  geom_bar(position = "fill") +
  labs(title = "Proporcion de impago por nivel educativo", x = "Nivel educativo",
       y = "Proporcion") +
  scale_fill_manual(values = c("#2c7fb8", "#de2d26")) +
  tema + theme(axis.text.x = element_text(angle = 20, hjust = 1))
print(p_biv_ed)
ggsave("plots/biv_ed.png", p_biv_ed, width = 6, height = 4, dpi = 130)

cat("\nOK - Pregunta 1 terminada\n")














