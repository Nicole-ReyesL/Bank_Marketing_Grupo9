banco <- read.csv("datos/bank-full.csv", sep = ";")

dim(banco)

names(banco)

head(banco)


str(banco)
summary(banco)
colSums(is.na(banco))
table(banco$y)
round(prop.table(table(banco$y)) * 100, 2)


summary(banco$age)
sd(banco$age)
summary(banco$balance)
sd(banco$balance)
aggregate(age ~ y, data = banco, FUN = mean)
aggregate(balance ~ y, data = banco, FUN = mean)


library(ggplot2)
ggplot(banco, aes(x = y, y = age)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Edad según contratación del depósito",
    x = "Contrató el depósito",
    y = "Edad (años)"
  )

ggplot(banco, aes(x = y, y = balance)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Saldo según contratación del depósito",
    x = "Contrató el depósito",
    y = "Saldo bancario (euros)"
  )

ggplot(banco, aes(x = y, y = balance)) +
  geom_boxplot(fill = "lightgreen") +
  coord_cartesian(ylim = c(-2000, 10000)) +
  labs(
    title = "Distribución del saldo bancario",
    subtitle = "Vista ampliada entre -2.000 y 10.000 euros",
    x = "Contrató el depósito",
    y = "Saldo bancario (euros)"
  )

aggregate(age ~ y, data = banco, FUN = median)
aggregate(balance ~ y, data = banco, FUN = median)


grafico_edad <- ggplot(banco, aes(x = y, y = age)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Edad según contratación del depósito",
    x = "Contrató el depósito",
    y = "Edad (años)"
  )

grafico_saldo <- ggplot(banco, aes(x = y, y = balance)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Saldo según contratación del depósito",
    x = "Contrató el depósito",
    y = "Saldo bancario (euros)"
  )

grafico_saldo_zoom <- grafico_saldo +
  coord_cartesian(ylim = c(-2000, 10000)) +
  labs(
    subtitle = "Vista ampliada entre -2.000 y 10.000 euros"
  )

ggsave("graficos/01_edad.png",
       plot = grafico_edad, width = 8, height = 5)

ggsave("graficos/02_saldo.png",
       plot = grafico_saldo, width = 8, height = 5)

ggsave("graficos/03_saldo_ampliado.png",
       plot = grafico_saldo_zoom, width = 8, height = 5)
