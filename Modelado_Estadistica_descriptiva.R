# CL2 - Lenguaje de Ciencia de Datos III
# Pregunta 1

library(ggplot2)
library(dplyr)
library(caret)
library(gridExtra)

# Ruta de trabajo (carpeta donde está titanic.csv)
setwd("C:/Examen_T2")

titanic <- read.csv("titanic.csv", na.strings = c("", "NA"))

# Carpeta para gráficos
if (!dir.exists("graficos")) dir.create("graficos")

# Exploración
dim(titanic)
str(titanic)
summary(titanic)

colSums(is.na(titanic))
round(colMeans(is.na(titanic)) * 100, 2)

table(titanic$Survived)
round(prop.table(table(titanic$Survived)) * 100, 2)

titanic$Survived_f <- factor(titanic$Survived, levels = c(0, 1), labels = c("No", "Si"))
colores <- c("No" = "#d9534f", "Si" = "#5cb85c")

# ------------------------------------------------------------
# a) Descriptivos bivariados: Survived vs demás variables
# ------------------------------------------------------------

# Categóricas: tabla, % por fila y chi-cuadrado
bivariado_cat <- function(var, simular = FALSE) {
  tab <- table(titanic[[var]], titanic$Survived_f)
  print(addmargins(tab))
  print(round(prop.table(tab, 1) * 100, 2))
  prueba <- chisq.test(tab, simulate.p.value = simular, B = 10000)
  print(prueba)

  df <- as.data.frame(prop.table(tab, 1))
  names(df) <- c("Categoria", "Survived", "Prop")
  g <- ggplot(df, aes(Categoria, Prop, fill = Survived)) +
    geom_col(position = "fill") +
    geom_text(aes(label = paste0(round(Prop * 100), "%")),
              position = position_fill(vjust = 0.5), size = 2.5) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colores) +
    labs(title = paste("Survived vs", var),
         subtitle = paste("p-valor chi2 =", format.pval(prueba$p.value, digits = 3)),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 9) + theme(legend.position = "none")
  list(p = prueba$p.value, g = g)
}

# Numéricas: resumen por grupo y Wilcoxon
bivariado_num <- function(var) {
  print(titanic %>%
          filter(!is.na(.data[[var]])) %>%
          group_by(Survived_f) %>%
          summarise(n = n(),
                    media   = mean(.data[[var]]),
                    mediana = median(.data[[var]]),
                    de      = sd(.data[[var]]),
                    min     = min(.data[[var]]),
                    max     = max(.data[[var]])))
  prueba <- wilcox.test(titanic[[var]] ~ titanic$Survived_f)
  print(prueba)

  g <- ggplot(titanic %>% filter(!is.na(.data[[var]])),
              aes(Survived_f, .data[[var]], fill = Survived_f)) +
    geom_boxplot(alpha = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 2.5) +
    scale_fill_manual(values = colores) +
    labs(title = paste(var, "vs Survived"),
         subtitle = paste("p-valor Wilcoxon =", format.pval(prueba$p.value, digits = 3)),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 9) + theme(legend.position = "none")
  list(p = prueba$p.value, g = g)
}

# Edad agrupada
titanic$GrupoEdad <- cut(titanic$Age, breaks = c(0, 12, 18, 35, 60, 80),
                         labels = c("0-12", "13-18", "19-35", "36-60", "61-80"),
                         include.lowest = TRUE)

# Cabin: con registro vs sin registro
titanic$TieneCabina <- ifelse(is.na(titanic$Cabin), "Sin cabina", "Con cabina")

r_pclass   <- bivariado_cat("Pclass")
r_sex      <- bivariado_cat("Sex")
r_embarked <- bivariado_cat("Embarked")
r_sibsp    <- bivariado_cat("SibSp", simular = TRUE)  # celdas < 5
r_parch    <- bivariado_cat("Parch", simular = TRUE)  # celdas < 5
r_gedad    <- bivariado_cat("GrupoEdad")
r_cabina   <- bivariado_cat("TieneCabina")
r_age      <- bivariado_num("Age")
r_fare     <- bivariado_num("Fare")

# Ticket
length(unique(titanic$Ticket))

# Todos los gráficos bivariados en una sola hoja (rojo = No, verde = Si)
graficos_biv <- list(r_pclass$g, r_sex$g, r_embarked$g,
                     r_sibsp$g, r_parch$g, r_gedad$g,
                     r_cabina$g, r_age$g, r_fare$g)

grid.arrange(grobs = graficos_biv, ncol = 3,
             top = "Survived vs variables (rojo = No, verde = Si)")

hoja <- arrangeGrob(grobs = graficos_biv, ncol = 3,
                    top = "Survived vs variables (rojo = No, verde = Si)")
ggsave("graficos/P1_01_bivariados.png", hoja, width = 13, height = 11)

# Resumen de p-valores
resumen <- data.frame(
  Variable = c("Pclass", "Sex", "Embarked", "SibSp", "Parch", "Age", "GrupoEdad", "Fare", "Cabina"),
  Prueba   = c("Chi2", "Chi2", "Chi2", "Chi2", "Chi2", "Wilcoxon", "Chi2", "Wilcoxon", "Chi2"),
  p_valor  = c(r_pclass$p, r_sex$p, r_embarked$p, r_sibsp$p, r_parch$p,
               r_age$p, r_gedad$p, r_fare$p, r_cabina$p)
)
resumen$Significativa <- ifelse(resumen$p_valor < 0.05, "Si", "No")
resumen

# ------------------------------------------------------------
# b) Preparación de la data
# ------------------------------------------------------------

# Se eliminan:
# PassengerId -> identificador
# Name        -> texto único por pasajero
# Ticket      -> 681 valores distintos
# Cabin       -> 77% de NA
data_modelo <- titanic %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(Survived = factor(Survived, levels = c(0, 1), labels = c("No", "Si")),
         Pclass   = factor(Pclass),
         Sex      = factor(Sex),
         Embarked = factor(Embarked))

str(data_modelo)
colSums(is.na(data_modelo))

# Partición 70/30 estratificada
set.seed(123)
idx   <- createDataPartition(data_modelo$Survived, p = 0.7, list = FALSE)
train <- data_modelo[idx, ]
test  <- data_modelo[-idx, ]

# Imputación con valores de train
# Age: mediana por Pclass y Sex
med_age <- train %>%
  group_by(Pclass, Sex) %>%
  summarise(Age_med = median(Age, na.rm = TRUE), .groups = "drop")
med_age

imputar <- function(df) {
  df %>%
    left_join(med_age, by = c("Pclass", "Sex")) %>%
    mutate(Age = ifelse(is.na(Age), Age_med, Age)) %>%
    select(-Age_med)
}

# Embarked: moda
moda_emb <- names(which.max(table(train$Embarked)))
moda_emb

train <- imputar(train)
test  <- imputar(test)
train$Embarked[is.na(train$Embarked)] <- moda_emb
test$Embarked[is.na(test$Embarked)]   <- moda_emb

colSums(is.na(train))
colSums(is.na(test))

dim(train)
dim(test)
round(prop.table(table(train$Survived)) * 100, 2)
round(prop.table(table(test$Survived)) * 100, 2)

# Distribución de Survived: data completa, train y test
particion <- rbind(data.frame(Conjunto = "Total", Survived = data_modelo$Survived),
                   data.frame(Conjunto = "Train", Survived = train$Survived),
                   data.frame(Conjunto = "Test",  Survived = test$Survived))
particion$Conjunto <- factor(particion$Conjunto, levels = c("Total", "Train", "Test"))
g <- ggplot(particion, aes(Conjunto, fill = Survived)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            position = position_fill(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = colores) +
  labs(title = "Proporción de Survived: total, train y test", x = NULL, y = "%") +
  theme_minimal()
print(g)
ggsave("graficos/P1_02_survived_particion.png", g, width = 7, height = 5)

# Guardar para la pregunta 2
saveRDS(list(train = train, test = test), "titanic_tratado.rds")
