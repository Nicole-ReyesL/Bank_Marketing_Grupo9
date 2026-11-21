banco <- read.csv("datos/bank-full.csv", sep = ";")

banco$contrato <- ifelse(banco$y == "yes", 1, 0)

table(banco$contrato)

mean(banco$contrato)

modelo_mpl <- lm(contrato ~ age + balance, data = banco)

summary(modelo_mpl)

confint(modelo_mpl, level = 0.95)

banco$prob_mpl <- predict(modelo_mpl)
summary(banco$prob_mpl)
sum(banco$prob_mpl < 0 | banco$prob_mpl > 1)

write.csv(
  as.data.frame(summary(modelo_mpl)$coefficients),
  "informe/resultados_mpl.csv",
  row.names = TRUE
)

library(sandwich)
library(lmtest)

coeftest(modelo_mpl, vcov. = vcovHC(modelo_mpl, type = "HC1"))

resultados_robustos <- coeftest(
  modelo_mpl,
  vcov. = vcovHC(modelo_mpl, type = "HC1")
)

tabla_robustos <- data.frame(
  variable = rownames(resultados_robustos),
  estimacion = resultados_robustos[, 1],
  error_estandar_robusto = resultados_robustos[, 2],
  estadistico_t = resultados_robustos[, 3],
  valor_p = resultados_robustos[, 4],
  row.names = NULL
)

write.csv(
  tabla_robustos,
  "informe/resultados_mpl_robustos.csv",
  row.names = FALSE
)

banco$error_mpl <- banco$contrato - banco$prob_mpl

ecm_mpl <- mean(banco$error_mpl^2)

ecm_mpl

resultados_ecm <- data.frame(
  modelo = "Modelo de Probabilidad Lineal",
  ECM = ecm_mpl
)

write.csv(
  resultados_ecm,
  "informe/resultados_ecm_mpl.csv",
  row.names = FALSE
)


summary(modelo_mpl)$r.squared

summary(modelo_mpl)$adj.r.squared


coef(modelo_mpl)

coef(modelo_mpl)["age"] * 100

coef(modelo_mpl)["balance"] * 1000 * 100

indicadores_mpl <- data.frame(
  modelo = "Modelo de Probabilidad Lineal",
  R2 = summary(modelo_mpl)$r.squared,
  R2_ajustado = summary(modelo_mpl)$adj.r.squared,
  ECM = ecm_mpl
)

indicadores_mpl

write.csv(
  indicadores_mpl,
  "informe/indicadores_mpl.csv",
  row.names = FALSE
)

probabilidad_general <- mean(banco$contrato)

ecm_basico <- mean((banco$contrato - probabilidad_general)^2)

comparacion_ecm <- data.frame(
  modelo = c("Prediccion basica", "MPL"),
  ECM = c(ecm_basico, ecm_mpl)
)

comparacion_ecm

write.csv(
  comparacion_ecm,
  "informe/comparacion_ecm_mpl.csv",
  row.names = FALSE
)
