# instalar y cargar paquetes
install.packages("FSelector")
install.packages("ade4")
install.packages("GGally")
install.packages("HSAUR")
install.packages("car")
install.packages("FactoMineR")
install.packages("factoextra")
library(FSelector)
library(ade4)
library(GGally)

# leer datos
data(olympic)
head(olympic)

# EJERCICIO D

# Hacemos una copia en la que solo tomamos los valores de tab para el decatlón
decathlon <- data.frame(olympic$tab)
head(decathlon)

#Obtenemos un resumen de los datos del decatlón
summary(decathlon)

#Vamos a realizar un diagrama de dispersión
library(car)

#Recodificamos para tener todas en la misma dirección
decathlon$X100 <- max(decathlon$X100)-decathlon$X100
decathlon$long <- max(decathlon$long)-decathlon$long
decathlon$poid <- max(decathlon$poid)-decathlon$poid
decathlon$haut <- max(decathlon$haut)-decathlon$haut
decathlon$X400 <- max(decathlon$X400)-decathlon$X400
decathlon$X110 <- max(decathlon$X110)-decathlon$X110
decathlon$disq <- max(decathlon$disq)-decathlon$disq
decathlon$perc <- max(decathlon$perc)-decathlon$perc
decathlon$jave <- max(decathlon$jave)-decathlon$jave
decathlon$X1500 <- max(decathlon$X1500)-decathlon$X1500

#Dibujamos el diagrama de dispersión correspondiente
scatterplotMatrix(decathlon,diagonal = list(method ="histogram"),smooth = FALSE)

#Comprobemos los resultados con la matriz de correlaciones
round(cor(decathlon),2)

# EJERCICIO E

# hacemos una copia en la cual cambiamos el formato de olympyc para facilitar 
# usar las funciones de FSelector
olymic_combined <- data.frame(olympic$tab, olympic$score)
head(olymic_combined)

# selección de características
# utilizando chi.squared
pesos <- chi.squared(olympic.score~.,data=olymic_combined)
subconjunto_chi <- cutoff.k(pesos,5)
subconjunto_chi
# utilizando oneR 
pesos <- oneR(olympic.score~.,data=olymic_combined)
subconjunto_oneR <- cutoff.k(pesos,5)
subconjunto_oneR
# utilizando gain.ratio
pesos <- gain.ratio(olympic.score~.,data=olymic_combined)
subconjunto_gain <- cutoff.k(pesos,5)
subconjunto_gain
# utilizando information.gain 
pesos <- information.gain(olympic.score~.,data=olymic_combined)
subconjunto_info <- cutoff.k(pesos,5)
subconjunto_info

#noramlización min-max
min.max <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# aplicamos la función
normalizados <- as.data.frame(lapply(olympic$tab,min.max))
head(normalizados)

# creamos el diagrama de dispersión en forma de matriz
# para los datos sin normalizar
ggpairs(olympic$tab, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")
# para los datos normalizados
ggpairs(normalizados, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")
