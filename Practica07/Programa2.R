# Cargar las librerías necesarias
library(ggplot2)

# Cargar los datos de los archivos CSV
grupoA <- read.csv("./grupoA.csv")
grupoB <- read.csv("./grupoB.csv")

# 2. Mostrar los datos de la variable Estatura en un histograma para cada grupo
hist(grupoA$Estatura, main="Histograma de Estatura - Grupo A", xlab="Estatura", col="pink")
hist(grupoB$Estatura, main="Histograma de Estatura - Grupo B", xlab="Estatura", col="thistle")

# 3. box plot de variable edad para ambos grupos
box_A_edad <- boxplot(grupoA$Edad, main="Grupo A: Edad")
box_B__edad <- boxplot(grupoB$Edad, main="Grupo B: Edad")
