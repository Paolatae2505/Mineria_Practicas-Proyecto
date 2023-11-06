# instalar y cargar paquetes
install.packages("FSelector")
install.packages("ade4")
install.packages("GGally")
library(FSelector)
library(ade4)
library(GGally)

# leer datos
data(olympic)
head(olympic)
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
#fución para normalizar
min.max <- function(x) {
  return (((x - min(x)) / (max(x) - min(x))*(2))-1)
}
#Aplicamos la función
normalizados <- as.data.frame(lapply(olympic$tab,min.max))
head(normalizados)

#Creamos el diagrama de dispersión en forma de matriz, con regresión lineal
ggpairs(normalizados, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")
