# importamos los datos, todos en formato numerico y normalizados
gtd_data <- read.csv("/gtd_data_normalizado_all.csv")

# importar dependencias
install.packages("readxl")
library(readxl)
library(tibble)
#install.packages("tidyverse")
library(tidyverse)
library(cluster)
library(magrittr) 
library(dplyr)    
#install.packages("textshape")
library(textshape)
#install.packages("NbClust")
library(NbClust)
library(tidyr)
#install.packages("factoextra")
library(factoextra)


# calcular la matriz de distacias
m.distancia <- get_dist(gtd_data, method = "euclidean")
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers
fviz_nbclust(gtd_data, kmeans, method = "wss")
fviz_nbclust(gtd_data, kmeans, method = "silhouette")
fviz_nbclust(gtd_data, kmeans, method = "gap_stat")

# Con el resultado anterior observamos que el mejor candidato es 3
# Calculamos 3 clústers
k3 <- kmeans(gtd_data, centers = 3, nstart = 25)
str(k3)

# Ploteamos los cluster
fviz_cluster(k3, data = gtd_data)

# Regresamos al data frames original para continuar el analisis
gtd_data %>%
  mutate(Cluster = k3$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
gtd_data_clusters <- gtd_data
head(gtd_data_clusters)

# Aplicamos la función factor a los clusters obtenidos
gtd_data_clusters$clus <- as.factor(k3$cluster)
head(gtd_data_clusters)

# Volvemos a iniciar el gtd_data_clusters con los datos originales
gtd_data_clusters <- gtd_data
# Estandarizamos el gtd_data_clusters
gtd_data_clusters <- scale(gtd_data_clusters)
gtd_data_clusters <- as.data.frame(gtd_data_clusters)
#Volvemos a aplicar factor
gtd_data_clusters$clus<-as.factor(k3$cluster)
head(gtd_data_clusters)

gtd_data_clusters$clus <- factor(gtd_data_clusters$clus)
# gtd_data_clusters: modificamos las columnas para agregar el cluster al que pertenece cada tupla
data_long <- gather(gtd_data_clusters, caracteristica, valor, success:1.0000, factor_key=TRUE)
head(data_long)
# Mostramos los resultados de la modificación anterior 
ggplot(data_long, aes(x = as.factor(caracteristica),
                      y = valor,group=clus, colour = clus))+
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")
