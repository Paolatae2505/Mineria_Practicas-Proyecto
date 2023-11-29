# --- A CAMBIAR UNA VEZ QUE SE TENGA EL csv DE LOS PREPROCESADOS ---
gtd_data <- read.csv("/home/paola/Documentos/SeptimoSemestre/MYAD/ProyectoFinal/globalterrorismdb_0718dist.csv")

# NA's
gtd_data_m<-data.frame(gtd_data)
for (var in 1:ncol(gtd_data_m)) {
    if (class(gtd_data_m[,var])=="numeric") {
        gtd_data_m[is.na(gtd_data_m[,var]),var] <- mean(gtd_data_m[,var], na.rm = TRUE)
    } else if (class(gtd_data_m[,var]) %in% c("character", "factor")) {
        gtd_data_m[is.na(gtd_data_m[,var]),var] <- mode(gtd_data_m[,var])
    } else if (class(gtd_data_m[,var]) == "integer"){
        gtd_data_m[is.na(gtd_data_m[,var]),var] <- median(gtd_data_m[,var], na.rm = TRUE)
    }
}
summary(gtd_data_m)

# Muestreo del 1% (motivos de hardware)
porcentaje_muestreo <- 0.01
tamano_muestra <- round(nrow(gtd_data) * porcentaje_muestreo)
# Configuramos una semilla para reproducibilidad
set.seed(123)
# Realizamos el muestreo
gtd_data <- gtd_data[sample(nrow(gtd_data), tamano_muestra), ]

# convertir a numeric los character y factor
gtd_data_numeric<-data.frame(gtd_data_m)
cols_names <- colnames(gtd_data)
j <- 1
for(i in gtd_data_numeric) {
  n = nlevels(i)
  if(class(i) == 'character'){
      x <- as.factor(i)
      gtd_data_numeric[cols_names[j]]<- as.numeric(x)
  }
  if(class(i) == 'factor'){
    gtd_data_numeric[cols_names[j]]<- as.numeric(i) #as.factor(sample(i, n, replace=TRUE))
  }
  j <-j+1
}

#noramlización min-max
min.max <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# aplicamos la función
normalizados <- as.data.frame(lapply(gtd_data_numeric,min.max))

normalizados <- subset(normalizados, select = -c(gsubname3, weaptype4, weaptype4_txt, weapsubtype4, weapsubtype4_txt,guncertain3,ransompaidus,claimmode3))
head(normalizados)

# --- APLICACION ALGORTIMO k-means 

# Dependencias
library(readxl)
library(tibble)
library(tidyverse)
library(cluster)
#install.packages("textshape")
library(textshape)
#install.packages("NbClust")
library(NbClust)
library(tidyr)
#install.packages("factoextra")
library(factoextra)


#calcular la matriz de distacias
#el método aceptado también puede ser: "maximum", "manhattan", "canberra",
#"binary", "minkowski", "pearson", "spearman" o "kendall"
m.distancia <- get_dist(normalizados, method = "euclidean")
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers
#el método aceptado puede ser: wss, elbow, silhouette o gap_stat
fviz_nbclust(normalizados, kmeans, method = "wss")
fviz_nbclust(normalizados, kmeans, method = "silhouette")
fviz_nbclust(normalizados, kmeans, method = "gap_stat")

#con esta función se pueden calcular los siguientes métodos: ARREGLAR
#"kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew",
#"friedman", "rubin", "cindex","db", "silhouette", "duda", "pseudot2", "beale",
#"ratkowsky", "ball","ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus",
#"tau", "dunn","hubert", "sdindex", "dindex", "sdbw",
#"all" (all indices except GAP, Gamma, Gplus and Tau)
#"alllong" (es decir, todos los antes citados).
resnumclust<-NbClust(normalizados, distance = "manhattan", min.nc=2, max.nc=5, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

#con el resultado anterior observamos que el mejor candidato es 3
#por lo tanto, calculamos a dos clústers
k3 <- kmeans(normalizados, centers = 3, nstart = 25)
str(k3)

#ploteamos los cluster
fviz_cluster(k3, data = normalizados)

#regresamos al df original para poder continuar el analisis
normalizados %>%
  mutate(Cluster = k3$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df <- normalizados
head(df)
#aplicamos la función factor al cluster obtenidos
df$clus<-as.factor(k3$cluster)

#volvemos a iniciar el df con los datos originales
df <- normalizados
#estandarizamos el df
df <- scale(df)
df<- as.data.frame(df)
#volvemos a aplicar factor
df$clus<-as.factor(k2$cluster)
#y mostramos el resultado
#head(df)

df$clus<-factor(df$clus)
#del df lo modificamos las columnas para agregar el cluster al que pertenece
data_long <- gather(df, caracteristica, valor, success:1.0000, factor_key=TRUE)

#por último, mostramos los resultados de la modificación anterior ARREGLAR
ggplot(data_long, aes(as.factor(x = caracteristica),
  y = valor,group=clus, colour = clus)
    + stat_summary(fun = mean, geom="pointrange", size = 1)
    + stat_summary(geom="line"))
