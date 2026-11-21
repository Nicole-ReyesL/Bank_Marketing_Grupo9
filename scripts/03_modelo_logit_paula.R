
# PROYECTO FINAL - ANALITICA DE DATOS
# Grupo 9
# Modelo Logit - Paula

# 1. Cargar la base de datos
banco <- read.csv(file.choose(), sep = ";")

# 2. Comprobar la base de datos
dim(banco)
names(banco)

# 3. Crear la variable dependiente
# 1 = Acepta el deposito
# 0 = No acepta el deposito

banco$contrato <- ifelse(banco$y == "yes", 1, 0)

# 4. Revisar la variable dependiente
table(banco$contrato)
prop.table(table(banco$contrato))

# 5. Estimar el modelo Logit
modelo_logit <- glm(
  contrato ~ age + balance,
  data = banco,
  family = binomial(link = "logit")
)

# 6. Mostrar los resultados
summary(modelo_logit)

# 7. Calcular las probabilidades estimadas
banco$prob_logit <- predict(modelo_logit, type = "response")

# Revisar las probabilidades
summary(banco$prob_logit)

# 8. Calcular probabilidades para valores representativos
# Se mantienen las variables en sus valores promedio

valores_referencia <- data.frame(
  age = mean(banco$age),
  balance = mean(banco$balance)
)

probabilidad_referencia <- predict(
  modelo_logit,
  newdata = valores_referencia,
  type = "response"
)

probabilidad_referencia

# 9. Evaluar el efecto de aumentar la edad en un año
valores_mas_edad <- valores_referencia
valores_mas_edad$age <- valores_mas_edad$age + 1

probabilidad_mas_edad <- predict(
  modelo_logit,
  newdata = valores_mas_edad,
  type = "response"
)

# Diferencia en probabilidad
probabilidad_mas_edad - probabilidad_referencia

# 10. Evaluar el efecto de aumentar el saldo en 1000
valores_mas_saldo <- valores_referencia
valores_mas_saldo$balance <- valores_mas_saldo$balance + 1000

probabilidad_mas_saldo <- predict(
  modelo_logit,
  newdata = valores_mas_saldo,
  type = "response"
)

# Diferencia en probabilidad
probabilidad_mas_saldo - probabilidad_referencia

# 11. Guardar los resultados
tabla_logit <- as.data.frame(coef(summary(modelo_logit)))
tabla_logit$variable <- rownames(tabla_logit)
rownames(tabla_logit) <- NULL

write.csv(
  tabla_logit,
  "resultados_logit_paula.csv",
  row.names = FALSE
)

# 12. Guardar las probabilidades estimadas
write.csv(
  data.frame(probabilidad = banco$prob_logit),
  "probabilidades_logit_paula.csv",
  row.names = FALSE
)

summary(modelo_logit)

probabilidad_referencia

probabilidad_mas_edad - probabilidad_referencia

probabilidad_mas_saldo - probabilidad_referencia

# 13. Revisar coeficientes del modelo Logit

coeficientes_logit <- coef(summary(modelo_logit))

coeficientes_logit

# 14. Guardar tabla de coeficientes

write.csv(
  as.data.frame(coeficientes_logit),
  "coeficientes_logit_paula.csv"
)


# 15. Preparar variables de control

# Convertir variables categoricas en factores
banco$job <- as.factor(banco$job)
banco$marital <- as.factor(banco$marital)
banco$education <- as.factor(banco$education)
banco$default <- as.factor(banco$default)
banco$housing <- as.factor(banco$housing)
banco$loan <- as.factor(banco$loan)
banco$contact <- as.factor(banco$contact)
banco$month <- as.factor(banco$month)
banco$poutcome <- as.factor(banco$poutcome)

# 16. Estimar el modelo Logit ampliado

modelo_logit_ampliado <- glm(
  contrato ~ age + balance + job + marital + education +
    default + housing + loan + contact + month +
    campaign + previous + poutcome,
  data = banco,
  family = binomial(link = "logit")
)

# 17. Mostrar resultados del modelo ampliado

summary(modelo_logit_ampliado)

# 18. Guardar los coeficientes del modelo ampliado

coeficientes_logit_ampliado <- coef(summary(modelo_logit_ampliado))

write.csv(
  as.data.frame(coeficientes_logit_ampliado),
  "coeficientes_logit_ampliado_paula.csv"
)

# 19. Calcular probabilidades estimadas

banco$prob_logit_ampliado <- predict(
  modelo_logit_ampliado,
  type = "response"
)

summary(banco$prob_logit_ampliado)

# 20. Guardar probabilidades estimadas

write.csv(
  data.frame(probabilidad = banco$prob_logit_ampliado),
  "probabilidades_logit_ampliado_paula.csv",
  row.names = FALSE
)

# 21. Comparar el modelo inicial y el ampliado

AIC(modelo_logit, modelo_logit_ampliado)

# 22. Grafico de probabilidades estimadas segun la edad

edades <- seq(
  min(banco$age),
  max(banco$age),
  by = 1
)

datos_grafico_edad <- data.frame(
  age = edades,
  balance = mean(banco$balance),
  job = factor(names(which.max(table(banco$job))),
               levels = levels(banco$job)),
  marital = factor(names(which.max(table(banco$marital))),
                   levels = levels(banco$marital)),
  education = factor(names(which.max(table(banco$education))),
                     levels = levels(banco$education)),
  default = factor(names(which.max(table(banco$default))),
                   levels = levels(banco$default)),
  housing = factor(names(which.max(table(banco$housing))),
                   levels = levels(banco$housing)),
  loan = factor(names(which.max(table(banco$loan))),
                levels = levels(banco$loan)),
  contact = factor(names(which.max(table(banco$contact))),
                   levels = levels(banco$contact)),
  month = factor(names(which.max(table(banco$month))),
                 levels = levels(banco$month)),
  campaign = mean(banco$campaign),
  previous = mean(banco$previous),
  poutcome = factor(names(which.max(table(banco$poutcome))),
                    levels = levels(banco$poutcome))
)

datos_grafico_edad$probabilidad <- predict(
  modelo_logit_ampliado,
  newdata = datos_grafico_edad,
  type = "response"
)

plot(
  datos_grafico_edad$age,
  datos_grafico_edad$probabilidad,
  type = "l",
  xlab = "Edad del cliente",
  ylab = "Probabilidad estimada de contratar",
  main = "Modelo Logit ampliado: probabilidad y edad"
)

# 23. Guardar el grafico

png("../graficos/04_logit_edad.png",
    width = 900, height = 600)

plot(
  datos_grafico_edad$age,
  datos_grafico_edad$probabilidad,
  type = "l",
  xlab = "Edad del cliente",
  ylab = "Probabilidad estimada de contratar",
  main = "Modelo Logit ampliado: probabilidad y edad"
)

dev.off()

summary(modelo_logit_ampliado)

AIC(modelo_logit, modelo_logit_ampliado)

# 24. Grafico de probabilidades segun el saldo

saldos <- seq(
  quantile(banco$balance, 0.01),
  quantile(banco$balance, 0.99),
  length.out = 100
)

datos_grafico_saldo <- datos_grafico_edad[
  rep(1, length(saldos)), ]

datos_grafico_saldo$balance <- saldos

datos_grafico_saldo$probabilidad <- predict(
  modelo_logit_ampliado,
  newdata = datos_grafico_saldo,
  type = "response"
)

plot(
  datos_grafico_saldo$balance,
  datos_grafico_saldo$probabilidad,
  type = "l",
  xlab = "Saldo bancario",
  ylab = "Probabilidad estimada de contratar",
  main = "Modelo Logit ampliado: probabilidad y saldo"
)

# 25. Guardar el grafico

png(
  "../graficos/05_logit_saldo.png",
  width = 900,
  height = 600
)

plot(
  datos_grafico_saldo$balance,
  datos_grafico_saldo$probabilidad,
  type = "l",
  xlab = "Saldo bancario",
  ylab = "Probabilidad estimada de contratar",
  main = "Modelo Logit ampliado: probabilidad y saldo"
)

dev.off()

list.files("../graficos")


# 26. Estimar el modelo de probabilidad lineal ampliado

modelo_mpl_ampliado <- lm(
  contrato ~ age + balance + job + marital + education +
    default + housing + loan + contact + month +
    campaign + previous + poutcome,
  data = banco
)

# 27. Mostrar resultados del MPL ampliado

summary(modelo_mpl_ampliado)

# 28. Guardar coeficientes del MPL ampliado

coeficientes_mpl_ampliado <- coef(summary(modelo_mpl_ampliado))

write.csv(
  as.data.frame(coeficientes_mpl_ampliado),
  "coeficientes_mpl_ampliado_paula.csv"
)

# 29. Calcular predicciones del MPL ampliado

banco$prob_mpl_ampliado <- predict(modelo_mpl_ampliado)

summary(banco$prob_mpl_ampliado)

# 30. Revisar predicciones fuera del intervalo de 0 a 1

sum(
  banco$prob_mpl_ampliado < 0 |
    banco$prob_mpl_ampliado > 1
)

summary(modelo_mpl_ampliado)

coef(summary(modelo_mpl_ampliado))


# 31. Comparacion de edad y saldo entre MPL y Logit

comparacion_principal <- data.frame(
  Variable = c("Edad", "Saldo"),
  MPL_coeficiente = coef(modelo_mpl_ampliado)[c("age", "balance")],
  MPL_valor_p = coef(summary(modelo_mpl_ampliado))[c("age", "balance"), 4],
  Logit_coeficiente = coef(modelo_logit_ampliado)[c("age", "balance")],
  Logit_valor_p = coef(summary(modelo_logit_ampliado))[c("age", "balance"), 4]
)

comparacion_principal

# 32. Guardar la comparacion

write.csv(
  comparacion_principal,
  "comparacion_mpl_logit_paula.csv",
  row.names = FALSE
)

list.files()


# 33. Interpretacion del Logit mediante probabilidades

# Crear un perfil de referencia
perfil <- banco[1, , drop = FALSE]

perfil$age <- mean(banco$age)
perfil$balance <- mean(banco$balance)

# Probabilidad inicial
prob_base <- predict(
  modelo_logit_ampliado,
  newdata = perfil,
  type = "response"
)

# Probabilidad con un año adicional de edad
perfil_edad <- perfil
perfil_edad$age <- perfil$age + 1

prob_edad <- predict(
  modelo_logit_ampliado,
  newdata = perfil_edad,
  type = "response"
)

# Probabilidad con 1000 unidades monetarias adicionales de saldo
perfil_saldo <- perfil
perfil_saldo$balance <- perfil$balance + 1000

prob_saldo <- predict(
  modelo_logit_ampliado,
  newdata = perfil_saldo,
  type = "response"
)

# Mostrar resultados
data.frame(
  escenario = c(
    "Perfil de referencia",
    "Un año adicional de edad",
    "1000 unidades adicionales de saldo"
  ),
  probabilidad = c(
    prob_base,
    prob_edad,
    prob_saldo
  )
)


# 34. Guardar probabilidades de los escenarios

tabla_probabilidades <- data.frame(
  escenario = c(
    "Perfil de referencia",
    "Un año adicional de edad",
    "1000 unidades adicionales de saldo"
  ),
  probabilidad = c(
    as.numeric(prob_base),
    as.numeric(prob_edad),
    as.numeric(prob_saldo)
  )
)

write.csv(
  tabla_probabilidades,
  "probabilidades_escenarios_logit_paula.csv",
  row.names = FALSE
)

# 34. Crear tabla final de resultados del modelo Logit

tabla_logit_final <- read.csv(
  "coeficientes_logit_paula.csv",
  row.names = 1
)

tabla_logit_final <- data.frame(
  Variable = rownames(tabla_logit_final),
  Estimacion = tabla_logit_final$Estimate,
  Error_estandar = tabla_logit_final$Std..Error,
  Estadistico_z = tabla_logit_final$z.value,
  Valor_p = tabla_logit_final$Pr...z..
)

# Redondear resultados para facilitar su lectura
tabla_logit_final[, 2:5] <- round(
  tabla_logit_final[, 2:5],
  6
)

# Mostrar la tabla en RStudio
View(tabla_logit_final)

# Guardar la tabla organizada
write.csv(
  tabla_logit_final,
  "tabla_final_logit_paula.csv",
  row.names = FALSE
)

# 35. Organizar tabla final del modelo Logit ampliado

coeficientes_ampliados <- as.data.frame(
  coef(summary(modelo_logit_ampliado))
)

tabla_logit_ampliado <- data.frame(
  Variable = rownames(coeficientes_ampliados),
  Estimacion = coeficientes_ampliados$Estimate,
  Error_estandar = coeficientes_ampliados$`Std. Error`,
  Estadistico_z = coeficientes_ampliados$`z value`,
  Valor_p = coeficientes_ampliados$`Pr(>|z|)`
)

tabla_logit_ampliado[, 2:5] <- round(
  tabla_logit_ampliado[, 2:5],
  6
)

View(tabla_logit_ampliado)

write.csv(
  tabla_logit_ampliado,
  "tabla_final_logit_ampliado_paula.csv",
  row.names = FALSE
)

# 36. Organizar tabla final del modelo MPL ampliado

coeficientes_mpl <- as.data.frame(
  coef(summary(modelo_mpl_ampliado))
)

tabla_mpl_ampliado <- data.frame(
  Variable = rownames(coeficientes_mpl),
  Estimacion = coeficientes_mpl$Estimate,
  Error_estandar = coeficientes_mpl$`Std. Error`,
  Estadistico_t = coeficientes_mpl$`t value`,
  Valor_p = coeficientes_mpl$`Pr(>|t|)`
)

# Redondear resultados
tabla_mpl_ampliado[, 2:5] <- round(
  tabla_mpl_ampliado[, 2:5],
  6
)

# Mostrar tabla
View(tabla_mpl_ampliado)

# Guardar tabla
write.csv(
  tabla_mpl_ampliado,
  "tabla_final_mpl_ampliado_paula.csv",
  row.names = FALSE
)

# 37. Comparación de los modelos ampliados

comparacion_ampliada <- data.frame(
  Variable = c("Edad", "Saldo"),
  
  MPL_coeficiente = coef(modelo_mpl_ampliado)[
    c("age", "balance")
  ],
  
  MPL_valor_p = coef(summary(modelo_mpl_ampliado))[
    c("age", "balance"), 4
  ],
  
  Logit_coeficiente = coef(modelo_logit_ampliado)[
    c("age", "balance")
  ],
  
  Logit_valor_p = coef(summary(modelo_logit_ampliado))[
    c("age", "balance"), 4
  ]
)

# Redondear resultados
comparacion_ampliada[, 2:5] <- round(
  comparacion_ampliada[, 2:5],
  6
)

# Mostrar tabla
View(comparacion_ampliada)

# Guardar tabla
write.csv(
  comparacion_ampliada,
  "comparacion_final_modelos_ampliados_paula.csv",
  row.names = FALSE
)

# 38. Comparación del ajuste de los modelos Logit

comparacion_aic <- AIC(
  modelo_logit,
  modelo_logit_ampliado
)

View(comparacion_aic)

write.csv(
  comparacion_aic,
  "comparacion_aic_logit_paula.csv",
  row.names = TRUE
)