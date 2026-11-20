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