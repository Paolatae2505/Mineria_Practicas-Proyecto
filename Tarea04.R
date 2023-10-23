library(tidyverse)

library(ggplot2)

star_data <- read.csv("/home/paola/Documentos/SeptimoSemestre/MYAD/Tareas/6 class csv.csv")

#d. Gráfica de barras de la variable Spectral class
ggplot(star_data, aes(x = Spectral.Class)) +
  geom_bar() +
  labs(title = "Distribución de Spectral Class") 

