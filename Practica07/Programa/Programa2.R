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

# 4. ¿Cuál grupo tiene un promedio de altura mayor? Y ¿Cuál tiene una altura mediana menor?
meanA = mean(grupoA$Estatura)
meanB = mean(grupoB$Estatura)
mayor = max(meanA, meanB)
print(paste("Promedio de altura grupo A: ", meanA))
print(paste("Promedio de altura grupo B: ",meanB))
print(paste("Promedio de altura mayor: ",mayor))

medianA = median(grupoA$Estatura)
medianB = median(grupoB$Estatura)
menor = min(meanA, meanB)
print(paste("Mediana de altura grupo A: ", medianA))
print(paste("Mediana de altura grupo B: ",medianB))
print(paste("Mediana de altura menor: ", menor))
