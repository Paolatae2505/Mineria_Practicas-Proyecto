# Cargar las librerías necesarias
library(ggplot2)

# Cargar los datos de los archivos CSV
grupoA <- read.csv("./grupoA.csv")
grupoB <- read.csv("./grupoB.csv")

# 2. Mostrar los datos de la variable Estatura en un histograma para cada grupo
hist(grupoA$Estatura, main="Histograma de Estatura - Grupo A", xlab="Estatura", col="pink")
hist(grupoB$Estatura, main="Histograma de Estatura - Grupo B", xlab="Estatura", col="thistle")