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