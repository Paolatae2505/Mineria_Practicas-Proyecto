unlink("/home/paola/R/x86_64-pc-linux-gnu-library/4.1/00LOCK-Rcpp", recursive = TRUE)
install.packages(c("curl", "Rcpp", "RcppEigen", "RcppArmadillo", "later", 
                   "ggrepel", "ggforce", "graphlayouts", "promises", 
                   "httpuv", "ggraph", "plotly", "DT", "arulesViz"))

# Intentar instalar arulesViz nuevamente
install.packages("arulesViz")
install.packages("arules")
# Cargar las bibliotecas necesarias
library(arules)
library(arulesViz)
gtd_data <- read.csv("/home/paola/Documentos/SeptimoSemestre/MYAD/Practicas/PGIT/Mineria_Practicas-Proyecto/ProyectoFinal/globalterrorismdb_0718dist.csv")

# Crear un objeto de transacciones (asegúrate de que tu conjunto de datos tenga la estructura adecuada)
transacciones <- as(gtd_data [, -1], "transactions")

# Lista de posibles valores de niveles de soporte y confianza
soporteLev <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2)
confianzaLev <- c(0.8, 0.7, 0.6, 0.5, 0.4, 0.3)

# Listas vacías para almacenar la cantidad de reglas creadas
rules_sup03 <- integer(length = 6)
rules_sup04 <- integer(length = 6)
rules_sup07 <- integer(length = 6)
rules_sup1 <- integer(length = 6)
rules_sup10 <- integer(length = 6)
rules_sup25 <- integer(length = 6)

# Algoritmo Apriori para cada valor de soporte y confianza
for (i in 1:length(confianzaLev)) {
  rules_sup03[i] <- length(
    apriori(
      transacciones,
      parameter = list(sup = soporteLev[1], conf = confianzaLev[i], target = "rules")
    )
  )
}

for (i in 1:length(confianzaLev)) {
  rules_sup04[i] <- length(
    apriori(
      transacciones,
      parameter = list(sup = soporteLev[2], conf = confianzaLev[i], target = "rules")
    )
  )
}

for (i in 1:length(confianzaLev)) {
  rules_sup07[i] <- length(
    apriori(
      transacciones,
      parameter = list(sup = soporteLev[3], conf = confianzaLev[i], target = "rules")
    )
  )
}

for (i in 1:length(confianzaLev)) {
  rules_sup1[i] <- length(
    apriori(
      transacciones,
      parameter = list(sup = soporteLev[4], conf = confianzaLev[i], target = "rules")
    )
  )
}

for (i in 1:length(confianzaLev)) {
  rules_sup10[i] <- length(
    apriori(
      transacciones,
      parameter = list(sup = soporteLev[5], conf = confianzaLev[i], target = "rules")
    )
  )
}

for (i in 1:length(confianzaLev)) {
  rules_sup25[i] <- length(
    apriori(
      transacciones,
      parameter = list(sup = soporteLev[6], conf = confianzaLev[i], target = "rules")
    )
  )
}

# Trama para cada escenario
library(gridExtra)
library(ggplot2)
plot1 <- qplot(
  confianzaLev, rules_sup03,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 0.3%"
) + theme_bw()

plot2 <- qplot(
  confianzaLev, rules_sup04,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 0.4%"
) + theme_bw()

plot3 <- qplot(
  confianzaLev, rules_sup07,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 0.7%"
) + theme_bw()

plot4 <- qplot(
  confianzaLev, rules_sup1,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 1%"
) + theme_bw()

plot5 <- qplot(
  confianzaLev, rules_sup10,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 10%"
) + theme_bw()

plot6 <- qplot(
  confianzaLev, rules_sup25,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 25%"
) + theme_bw()

library(gridExtra)

grid.arrange(plot1, plot2, plot3, plot4, plot5,plot6, ncol = 2)

# Data frame
num_reglas <- data.frame(
  rules_sup03, rules_sup04, rules_sup07, rules_sup1,
  rules_sup10, rules_sup25, confianzaLev
)

# Número de reglas encontradas con diferentes niveles de soporte
ggplot(data = num_reglas, aes(x = confianzaLev)) +
  geom_line(aes(y = rules_sup03, colour = "Soporte del 0.3%")) +
  geom_point(aes(y = rules_sup03, colour = "Soporte del 0.3%")) +
  geom_line(aes(y = rules_sup04, colour = "Soporte del 0.4%")) +
  geom_point(aes(y = rules_sup04, colour = "Soporte del 0.4%")) +
  geom_line(aes(y = rules_sup07, colour = "Soporte del 0.7%")) +
  geom_point(aes(y = rules_sup07, colour = "Soporte del 0.7%")) +
  geom_line(aes(y = rules_sup1, colour = "Soporte del 1%")) +
  geom_point(aes(y = rules_sup1, colour = "Soporte del 1%")) +
  geom_line(aes(y = rules_sup10, colour = "Soporte del 10%")) +
  geom_point(aes(y = rules_sup10, colour = "Soporte del 10%")) +
  geom_line(aes(y = rules_sup25, colour = "Soporte del 25%")) +
  geom_point(aes(y = rules_sup25, colour = "Soporte del 25%")) +
  labs(x="Niveles de Confianza", y="N?mero de reglas encontradas", 
       title="Algoritmo Apriori con diferentes niveles de Soporte") +
  theme_bw() +
  theme(legend.title=element_blank())


reglas <- apriori(data = transacciones,
                  parameter = list(support = soporteLev[5],
                                   confidence = confianzaLev[1],
                                   # Se especifica que se creen reglas
                                   target = "rules"))
str(gtd_data)
summary(reglas)
library(tibble)
r <- as_tibble(as(reglas, Class = "data.frame"))
r

# Inspeccionamos las reglas, ordenando por confianza
inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
library(dplyr)
# Podemos ordenar el tibble
r %>% arrange(desc(confidence))
#############################################################################
# EVALUACIÓN DE REGLAS DE ASOCIACIÓN #
#############################################################################
metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest"),
                            transactions = transacciones)

metricas <- interestMeasure(reglas, measure = c("fishersExactTest"),
                            transactions = transacciones)
metricas

#Estas nuevas m?tricas pueden a?adirse al objeto que contiene las reglas.

quality(reglas) <- cbind(quality(reglas), metricas)
# inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
df_reglas <- as(reglas, Class = "data.frame") 
df_reglas %>% as.tibble() %>% arrange(desc(confidence)) %>% head()