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
gtd_data <- read.csv("datosPrepTerrorismoNormalizadoSome.csv")

###########################################################################
                      # MÉTODO APRIORI 
###########################################################################

porcentaje_muestreo <- 0.1
tamano_muestra <- round(nrow(gtd_data) * porcentaje_muestreo)

set.seed(123)
gtd_data <- gtd_data[sample(nrow(gtd_data), tamano_muestra), ]

num_filas <- nrow(gtd_data)
print(num_filas)

# Crear un objeto de transacciones (asegúrate de que tu conjunto de datos tenga la estructura adecuada)
transacciones <- as(gtd_data [, -1], "transactions")

# Lista de posibles valores de niveles de soporte y confianza
soporteLev <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2)
confianzaLev <- c(0.8, 0.7, 0.6, 0.5, 0.4, 0.3)

# Listas vacías para almacenar la cantidad de reglas creadas
rules_sup01 <- integer(length = 6)
rules_sup02<- integer(length = 6)
rules_sup03 <- integer(length = 6)
rules_sup04 <- integer(length = 6)
rules_sup05 <- integer(length = 6)
rules_sup06 <- integer(length = 6)

# Algoritmo Apriori para cada valor de soporte y confianza
for (i in 1:length(confianzaLev)) {
  rules_sup01[i] <- length(
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
  rules_sup03[i] <- length(
    apriori(
      transacciones,
      parameter = list(sup = soporteLev[3], conf = confianzaLev[i], target = "rules")
    )
  )
}

for (i in 1:length(confianzaLev)) {
  rules_sup04[i] <- length(
    apriori(
      transacciones,
      parameter = list(sup = soporteLev[4], conf = confianzaLev[i], target = "rules")
    )
  )
}

for (i in 1:length(confianzaLev)) {
  rules_sup05[i] <- length(
    apriori(
      transacciones,
      parameter = list(sup = soporteLev[5], conf = confianzaLev[i], target = "rules")
    )
  )
}

for (i in 1:length(confianzaLev)) {
  rules_sup06[i] <- length(
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
  confianzaLev, rules_sup01,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 0.01%"
) + theme_bw()

plot2 <- qplot(
  confianzaLev, rules_sup02,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 0.02%"
) + theme_bw()

plot3 <- qplot(
  confianzaLev, rules_sup03,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 0.03%"
) + theme_bw()

plot4 <- qplot(
  confianzaLev, rules_sup04,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 0.05%"
) + theme_bw()

plot5 <- qplot(
  confianzaLev, rules_sup05,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 0.1%"
) + theme_bw()

plot6 <- qplot(
  confianzaLev, rules_sup06,
  geom = c("point", "line"),
  xlab = "Nivel de Confianza",
  ylab = "Número de reglas encontradas",
  main = "Apriori con soporte del 0.2%"
) + theme_bw()

library(gridExtra)

grid.arrange(plot1, plot2, plot3, plot4, plot5,plot6, ncol = 2)

# Data frame
num_reglas <- data.frame(
  rules_sup01, rules_sup04, rules_sup03, rules_sup04,
  rules_sup05, rules_sup06, confianzaLev
)

# Número de reglas encontradas con diferentes niveles de soporte
ggplot(data = num_reglas, aes(x = confianzaLev)) +
  geom_line(aes(y = rules_sup01, colour = "Soporte del 0.01%")) +
  geom_point(aes(y = rules_sup01, colour = "Soporte del 0.01%")) +
  geom_line(aes(y = rules_sup04, colour = "Soporte del 0.02%")) +
  geom_point(aes(y = rules_sup04, colour = "Soporte del 0.02%")) +
  geom_line(aes(y = rules_sup03, colour = "Soporte del 0.03%")) +
  geom_point(aes(y = rules_sup03, colour = "Soporte del 0.03%")) +
  geom_line(aes(y = rules_sup04, colour = "Soporte del 0.05%")) +
  geom_point(aes(y = rules_sup04, colour = "Soporte del 0.05%")) +
  geom_line(aes(y = rules_sup05, colour = "Soporte del 0.1%")) +
  geom_point(aes(y = rules_sup05, colour = "Soporte del 0.1%")) +
  geom_line(aes(y = rules_sup06, colour = "Soporte del 0.2%")) +
  geom_point(aes(y = rules_sup06, colour = "Soporte del 0.2%")) +
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

##########################################################################
# Filtrado de reglas con Succes 
#################################################################3
reglas_succes <- apriori(data = transacciones,
                            parameter = list(support = soporteLev[5],
                                             confidence = confianzaLev[1],
                                             # Se especifica que se creen reglas
                                             target = "rules"),
                            appearance= list(rhs="sucess"))
#############################################################################
# MÉTODO ECLAT
#############################################################################

itemsets = eclat(data = transacciones,
                 parameter = list(support = soporteLev[1]))
for (i in 1:length(confianzaLev)){
  rules_sup01[i] <- length(ruleInduction(itemsets, transacciones, 
                                          confidence = confianzaLev[i]))
}

itemsets = eclat(data = transacciones,
                 parameter = list(support = soporteLev[1]))
for (i in 1:length(confianzaLev)){
  rules_sup02[i] <- length(ruleInduction(itemsets, transacciones, 
                                         confidence = confianzaLev[i]))
}

itemsets = eclat(data = transacciones,
                 parameter = list(support = soporteLev[1]))
for (i in 1:length(confianzaLev)){
  rules_sup03[i] <- length(ruleInduction(itemsets, transacciones, 
                                         confidence = confianzaLev[i]))
}

itemsets = eclat(data = transacciones,
                 parameter = list(support = soporteLev[1]))
for (i in 1:length(confianzaLev)){
  rules_sup04[i] <- length(ruleInduction(itemsets, transacciones, 
                                         confidence = confianzaLev[i]))
}

itemsets = eclat(data = transacciones,
                 parameter = list(support = soporteLev[1]))
for (i in 1:length(confianzaLev)){
  rules_sup05[i] <- length(ruleInduction(itemsets, transacciones, 
                                         confidence = confianzaLev[i]))
}

itemsets = eclat(data = transacciones,
                 parameter = list(support = soporteLev[1]))
for (i in 1:length(confianzaLev)){
  rules_sup06[i] <- length(ruleInduction(itemsets, transacciones, 
                                         confidence = confianzaLev[i]))
}



library(gridExtra)

# N?mero de reglas encontradas con un soporte del 0.19%
plot1_1 <- qplot(confianzaLev, rules_sup01, geom=c("point", "line"), 
               xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
               main="ECLAT con soporte del 0.19%") +
  theme_bw()

# N?mero de reglas encontradas con un soporte del 0.3%
plot2 <- qplot(confianzaLev, rules_sup02, geom=c("point", "line"), 
               xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
               main="ECLAT con soporte del 0.3%") + 
  theme_bw()

# N?mero de reglas encontradas con un soporte del 0.4%
plot3 <- qplot(confianzaLev, rules_sup03, geom=c("point", "line"), 
               xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
               main="ECLAT con soporte del 0.4%") + 
  theme_bw()

# N?mero de reglas encontradas con un soporte del 0.7%
plot4 <- qplot(confianzaLev, rules_sup04, geom=c("point", "line"), 
               xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
               main="ECLAT con soporte del 0.7%") + 
  theme_bw()

# N?mero de reglas encontradas con un soporte del 0.8%
plot5 <- qplot(confianzaLev, rules_sup05, geom=c("point", "line"), 
               xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
               main="ECLAT con soporte del 0.8%") + 
  theme_bw()

# N?mero de reglas encontradas con un soporte del 1%
plot6 <- qplot(confianzaLev, rules_sup06, geom=c("point", "line"), 
               xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
               main="ECLAT con soporte del 1%") + 
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)

# Estos puntos se trazaron en un gr?fico de l?neas m?ltiples. Cada l?nea 
# representaba un valor de soporte.

# Data frame
num_reglas <- data.frame(rules_sup01, rules_sup02, rules_sup03, rules_sup04, 
                         rules_sup05, rules_sup06, confianzaLev)

# N?mero de reglas encontradas con soportes del 0.19%, 0.3%, 0.4%, 0.7%, 0.8%, 1%
ggplot(data=num_reglas, aes(x=confianzaLev)) +
  
  # GRaficar l?neas y puntos (soporte del 0.19%)
  geom_line(aes(y=rules_sup01, colour="Soporte del 0.01%")) + 
  geom_point(aes(y=rules_sup01, colour="Soporte del 0.01%")) +
  
  # GRaficar l?neas y puntos (soporte del 0.3%)
  geom_line(aes(y=rules_sup02, colour="Soporte del 0.02%")) + 
  geom_point(aes(y=rules_sup02, colour="Soporte del 0.02%")) +
  
  # GRaficar l?neas y puntos (soporte del 0.4%)
  geom_line(aes(y=rules_sup03, colour="Soporte del 0.03%")) + 
  geom_point(aes(y=rules_sup03, colour="Soporte del 0.03%")) +
  
  # GRaficar l?neas y puntos (soporte del 0.7%)
  geom_line(aes(y=rules_sup04, colour="Soporte del 0.05%")) + 
  geom_point(aes(y=rules_sup04, colour="Soporte del 0.05%")) +
  
  # GRaficar l?neas y puntos (soporte del 0.8%)
  geom_line(aes(y=rules_sup05, colour="Soporte del 0.1%")) + 
  geom_point(aes(y=rules_sup05, colour="Soporte del 0.1%")) +
  
  # GRaficar l?neas y puntos (soporte del 1%)
  geom_line(aes(y=rules_sup06, colour="Soporte del 0.2%")) + 
  geom_point(aes(y=rules_sup06, colour="Soporte del 0.2%")) +
  
  # Labs and theme
  labs(x="Niveles de Confianza", y="N?mero de reglas encontradas", 
       title="Algoritmo ECLAT con diferentes niveles de Soporte") +
  theme_bw() +
  theme(legend.title=element_blank())

# Podemos apoyarnos en la gr?fica anterior, para determinar los niveles de confianza
# y soporte que permitan analizar una cantidad razonable de reglas. 
# Por lo tanto, para el modelo final se utiliz? un nivel de soporte del 0.7% y un 
# nivel de confianza del 50%.

# Para crear las reglas de asociaci?n se sigue el mismo proceso que para obtener itemsets
# frecuentes pero, adem?s de especificar un soporte m?nimo, se tiene que establecer una
# confianza m?nima para que una regla se incluya en los resultados. En este caso, se
# emplea una confianza m?nima del 50%.


# Para crear las reglas de asociaci?n se utiliza el m?todo ruleInduction, donde debemos
# establecer una confianza m?nima para que una regla se incluya en los resultados. 
# En este caso, se emplea una confianza m?nima del 70%.
itemsets = eclat(data = transacciones,
                 parameter = list(support = soporteLev[5]))
reglas <- ruleInduction(itemsets, transacciones, confidence = confianzaLev[1])
reglas
summary(reglas)

# Se han identificado un total de 24725 reglas, la mayor?a de ellas formadas por 4 items
# en el antecedente (parte izquierda de la regla).

inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))

# ==============================================================================
# EVALUACI?N DE LAS REGLAS DE ASOCIACI?N
# ==============================================================================

metricas <- interestMeasure(reglas, measure = c("fishersExactTest"),
                            transactions = transacciones)
metricas

#Estas nuevas m?tricas pueden a?adirse al objeto que contiene las reglas.

quality(reglas) <- cbind(quality(reglas), metricas)
# inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
df_reglas <- as(reglas, Class = "data.frame") 
df_reglas %>% as_tibble() %>% arrange(desc(confidence)) %>% head()


##########################################################################
# Filtrado de reglas con Succes 
#################################################################3
reglas_succes <- apriori(data = transacciones,
                         parameter = list(support = soporteLev[5],
                                          confidence = confianzaLev[1],
                                          # Se especifica que se creen reglas
                                          target = "rules"),
                         appearance= list(rhs="sucess"))
