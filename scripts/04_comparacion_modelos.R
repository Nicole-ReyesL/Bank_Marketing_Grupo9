modelo_logit <- glm(
  contrato ~ age + balance,
  data = banco,
  family = binomial(link = "logit")
)

banco$prob_logit <- predict(
  modelo_logit,
  type = "response"
)

ecm_logit <- mean(
  (banco$contrato - banco$prob_logit)^2
)

comparacion_final <- data.frame(
  modelo = c("MPL", "Logit"),
  ECM = c(ecm_mpl, ecm_logit)
)

comparacion_final

write.csv(
  comparacion_final,
  "informe/comparacion_final_modelos.csv",
  row.names = FALSE
)