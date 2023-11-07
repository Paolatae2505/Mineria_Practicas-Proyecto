# Cargar las librerías necesarias
library(ggplot2)

# Cargar los datos de los archivos CSV
grupoA <- read.csv("./grupoA.csv")
grupoB <- read.csv("./grupoB.csv")

# 1. Muestra la distribución de tipos sanguíneos de la variable Gr_Sang mediante una gráfica de pay.
dist_grupoA <- prop.table(table(grupoA$Gr_sang))
pie(dist_grupoA, col = rainbow(length(dist_grupoA)), main = "Distribución de Tipos Sanguíneos - Grupo A")
legend("topright", legend = paste(names(dist_grupoA), ": ", round(100 * dist_grupoA, 2), "%"), 
       fill = rainbow(length(dist_grupoA)), cex = 0.8)

dist_grupoB <- prop.table(table(grupoB$Gr_sang))
pie(dist_grupoB, col = rainbow(length(dist_grupoB)), main = "Distribución de Tipos Sanguíneos - Grupo B")
legend("topright", legend = paste(names(dist_grupoB), ": ", round(100 * dist_grupoB, 2), "%"), 
       fill = rainbow(length(dist_grupoB)), cex = 0.8)

# 2. Mostrar los datos de la variable Estatura en un histograma para cada grupo
hist(grupoA$Estatura, main="Histograma de Estatura - Grupo A", xlab="Estatura", col="pink")
hist(grupoB$Estatura, main="Histograma de Estatura - Grupo B", xlab="Estatura", col="thistle")

# 3. box plot de variable edad para ambos grupos
box_A_edad <- boxplot(grupoA$Edad, main="Grupo A: Edad")
box_B__edad <- boxplot(grupoB$Edad, main="Grupo B: Edad")

# 4. ¿Cuál grupo tiene un promedio de altura mayor? Y ¿Cuál tiene una altura mediana menor?
grupoA <- na.omit(grupoA)
grupoB <- na.omit(grupoB)
meanA = mean(grupoA$Estatura)
meanB = mean(grupoB$Estatura)
mayor = max(meanA, meanB)
print(paste("Promedio de altura grupo A: ", meanA))
print(paste("Promedio de altura grupo B: ",meanB))
print(paste("Promedio de altura mayor: ",mayor))

medianA = median(grupoA$Estatura)
medianB = median(grupoB$Estatura)
menor = min(medianA, medianB)
print(paste("Mediana de altura grupo A: ", medianA))
print(paste("Mediana de altura grupo B: ",medianB))
print(paste("Mediana de altura menor: ", menor))
