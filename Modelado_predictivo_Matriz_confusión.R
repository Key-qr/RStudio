# CL2 - Lenguaje de Ciencia de Datos III
# Pregunta 2

library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(MASS)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(gridExtra)

# Misma ruta de la pregunta 1
setwd("C:/Examen_T2")

if (!dir.exists("graficos")) dir.create("graficos")

# Data tratada en la pregunta 1
datos <- readRDS("titanic_tratado.rds")
train <- datos$train
test  <- datos$test

dim(train)
dim(test)

formula_mod <- Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked

# ------------------------------------------------------------
# a) Modelos
# Estadísticos:    regresión logística y LDA
# No estadísticos: árbol de decisión y random forest
# ------------------------------------------------------------

# 1. Regresión logística
mod_log <- glm(formula_mod, data = train, family = binomial)
summary(mod_log)

or_log <- data.frame(
  OR      = exp(coef(mod_log)),
  IC_inf  = exp(confint.default(mod_log))[, 1],
  IC_sup  = exp(confint.default(mod_log))[, 2],
  p_valor = summary(mod_log)$coefficients[, 4]
)
round(or_log, 4)

or_plot <- or_log[-1, ]
or_plot$Variable <- rownames(or_plot)
g_or <- ggplot(or_plot, aes(reorder(Variable, OR), OR)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2, color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() + scale_y_log10() +
  labs(title = "Regresión logística - Odds Ratios (IC 95%)", x = NULL, y = "OR") +
  theme_minimal()

prob_log <- predict(mod_log, test, type = "response")
pred_log <- factor(ifelse(prob_log >= 0.5, "Si", "No"), levels = c("No", "Si"))

# 2. LDA
mod_lda <- lda(formula_mod, data = train)
mod_lda

prob_lda <- predict(mod_lda, test)$posterior[, "Si"]
pred_lda <- factor(ifelse(prob_lda >= 0.5, "Si", "No"), levels = c("No", "Si"))

lda_train <- data.frame(LD1 = predict(mod_lda, train)$x[, 1], Survived = train$Survived)
g_lda <- ggplot(lda_train, aes(LD1, fill = Survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("No" = "#d9534f", "Si" = "#5cb85c")) +
  labs(title = "LDA - Función discriminante (train)", x = "LD1", y = "Densidad") +
  theme_minimal()

# 3. Árbol de decisión
set.seed(123)
mod_arbol <- rpart(formula_mod, data = train, method = "class",
                   control = rpart.control(cp = 0.001, minsplit = 20, xval = 10))
printcp(mod_arbol)

# Poda con el cp de menor xerror
cp_opt <- mod_arbol$cptable[which.min(mod_arbol$cptable[, "xerror"]), "CP"]
cp_opt
mod_arbol <- prune(mod_arbol, cp = cp_opt)
mod_arbol$variable.importance

rpart.plot(mod_arbol, type = 2, extra = 104, fallen.leaves = TRUE,
           box.palette = "RdGn", main = "Árbol de decisión")

png("graficos/P2_01_arbol.png", width = 2400, height = 1600, res = 250)
rpart.plot(mod_arbol, type = 2, extra = 104, fallen.leaves = TRUE,
           box.palette = "RdGn", main = "Árbol de decisión")
dev.off()

prob_arbol <- predict(mod_arbol, test, type = "prob")[, "Si"]
pred_arbol <- predict(mod_arbol, test, type = "class")

# 4. Random forest
set.seed(123)
mod_rf <- randomForest(formula_mod, data = train, ntree = 500, mtry = 2, importance = TRUE)
mod_rf

err_rf <- data.frame(Arboles = 1:mod_rf$ntree, mod_rf$err.rate)
err_rf <- pivot_longer(err_rf, -Arboles, names_to = "Tipo", values_to = "Error")
g_err <- ggplot(err_rf, aes(Arboles, Error, color = Tipo)) +
  geom_line() +
  scale_color_manual(values = c("OOB" = "black", "No" = "#d9534f", "Si" = "#5cb85c")) +
  labs(title = "Random Forest - Error vs número de árboles", x = "Árboles", y = "Error") +
  theme_minimal()

imp_rf <- data.frame(Variable = rownames(importance(mod_rf)),
                     Gini = importance(mod_rf)[, "MeanDecreaseGini"])
imp_rf
g_imp <- ggplot(imp_rf, aes(reorder(Variable, Gini), Gini)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Random Forest - Importancia de variables", x = NULL, y = "MeanDecreaseGini") +
  theme_minimal()

prob_rf <- predict(mod_rf, test, type = "prob")[, "Si"]
pred_rf <- predict(mod_rf, test, type = "class")

# Gráficos de los modelos en una sola hoja
grid.arrange(g_or, g_lda, g_err, g_imp, ncol = 2)

hoja_modelos <- arrangeGrob(g_or, g_lda, g_err, g_imp, ncol = 2)
ggsave("graficos/P2_02_modelos.png", hoja_modelos, width = 13, height = 9)

# ------------------------------------------------------------
# b) Matriz de confusión y métricas en test (positivo = "Si")
# ------------------------------------------------------------
cm_log   <- confusionMatrix(pred_log,   test$Survived, positive = "Si", mode = "everything")
cm_lda   <- confusionMatrix(pred_lda,   test$Survived, positive = "Si", mode = "everything")
cm_arbol <- confusionMatrix(pred_arbol, test$Survived, positive = "Si", mode = "everything")
cm_rf    <- confusionMatrix(pred_rf,    test$Survived, positive = "Si", mode = "everything")

cm_log
cm_lda
cm_arbol
cm_rf

roc_log   <- roc(test$Survived, prob_log,   levels = c("No", "Si"), direction = "<")
roc_lda   <- roc(test$Survived, prob_lda,   levels = c("No", "Si"), direction = "<")
roc_arbol <- roc(test$Survived, prob_arbol, levels = c("No", "Si"), direction = "<")
roc_rf    <- roc(test$Survived, prob_rf,    levels = c("No", "Si"), direction = "<")

metricas <- function(cm, roc_obj, modelo) {
  data.frame(Modelo      = modelo,
             Accuracy    = cm$overall["Accuracy"],
             Kappa       = cm$overall["Kappa"],
             Sensitivity = cm$byClass["Sensitivity"],
             Specificity = cm$byClass["Specificity"],
             Precision   = cm$byClass["Precision"],
             F1          = cm$byClass["F1"],
             AUC         = as.numeric(auc(roc_obj)),
             row.names   = NULL)
}

comparacion <- rbind(metricas(cm_log,   roc_log,   "Logística"),
                     metricas(cm_lda,   roc_lda,   "LDA"),
                     metricas(cm_arbol, roc_arbol, "Árbol"),
                     metricas(cm_rf,    roc_rf,    "Random Forest"))
comparacion[, -1] <- round(comparacion[, -1], 4)
comparacion <- comparacion[order(-comparacion$F1), ]
comparacion

write.csv(comparacion, "graficos/P2_comparacion_modelos.csv", row.names = FALSE)

# Matrices de confusión
cm_df <- rbind(data.frame(cm_log$table,   Modelo = "Logística"),
               data.frame(cm_lda$table,   Modelo = "LDA"),
               data.frame(cm_arbol$table, Modelo = "Árbol"),
               data.frame(cm_rf$table,    Modelo = "Random Forest"))
cm_df$Resultado <- ifelse(cm_df$Prediction == cm_df$Reference, "Correcto", "Error")
cm_df$Modelo <- factor(cm_df$Modelo, levels = c("Logística", "LDA", "Árbol", "Random Forest"))

g_cm <- ggplot(cm_df, aes(Reference, Prediction, fill = Resultado)) +
  geom_tile(color = "white", alpha = 0.85) +
  geom_text(aes(label = Freq), size = 6) +
  facet_wrap(~ Modelo, ncol = 2) +
  scale_fill_manual(values = c("Correcto" = "#5cb85c", "Error" = "#d9534f")) +
  labs(title = "Matrices de confusión - test", x = "Real", y = "Predicción", fill = NULL) +
  theme_minimal()

# Comparación de métricas
met_largo <- pivot_longer(comparacion, -Modelo, names_to = "Metrica", values_to = "Valor")
g_met <- ggplot(met_largo, aes(Metrica, Valor, fill = Modelo)) +
  geom_col(position = position_dodge(0.85), width = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Valor)),
            position = position_dodge(0.85), vjust = -0.3, size = 2.4) +
  ylim(0, 1.05) +
  labs(title = "Comparación de métricas - test", x = NULL, y = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# Curvas ROC
roc_df <- rbind(
  data.frame(FPR = 1 - roc_log$specificities,   TPR = roc_log$sensitivities,
             Modelo = sprintf("Logística (AUC = %.3f)", auc(roc_log))),
  data.frame(FPR = 1 - roc_lda$specificities,   TPR = roc_lda$sensitivities,
             Modelo = sprintf("LDA (AUC = %.3f)", auc(roc_lda))),
  data.frame(FPR = 1 - roc_arbol$specificities, TPR = roc_arbol$sensitivities,
             Modelo = sprintf("Árbol (AUC = %.3f)", auc(roc_arbol))),
  data.frame(FPR = 1 - roc_rf$specificities,    TPR = roc_rf$sensitivities,
             Modelo = sprintf("Random Forest (AUC = %.3f)", auc(roc_rf)))
)
roc_df <- roc_df[order(roc_df$Modelo, roc_df$FPR, roc_df$TPR), ]
g_roc <- ggplot(roc_df, aes(FPR, TPR, color = Modelo)) +
  geom_line(linewidth = 1) +
  geom_abline(linetype = "dashed", color = "grey50") +
  labs(title = "Curvas ROC - test", x = "1 - Especificidad", y = "Sensibilidad", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2))

# Evaluación en una sola hoja: matrices y ROC arriba, métricas abajo
grid.arrange(g_cm, g_roc, g_met, layout_matrix = rbind(c(1, 2), c(3, 3)))

hoja_eval <- arrangeGrob(g_cm, g_roc, g_met, layout_matrix = rbind(c(1, 2), c(3, 3)))
ggsave("graficos/P2_03_evaluacion.png", hoja_eval, width = 14, height = 12)

# Mejor modelo (mayor F1)
comparacion[1, ]
