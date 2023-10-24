library(tidyverse)

library(ggplot2)

star_data <- read.csv("/home/paola/Documentos/SeptimoSemestre/MYAD/Tareas/6 class csv.csv")

#a. Variables categóricas
star_data <- star_data %>% mutate_if(is.character, as.factor)
star_data$Star.type <- as.factor(star_data$Star.type)
head(star_data)

#b. Resúmen
summary(star_data)

#d. Gráfica de barras de la variable Spectral class
ggplot(star_data, aes(x = Spectral.Class)) +
  geom_bar() +
  labs(title = "Distribución de Spectral Class") 

# Histogramas
# install.packages("DataExplorer")
library(DataExplorer)
plot_histogram(star_data)

# Boxplots
tem <- boxplot(star_data$Temperature..K., main="Temperature (K)")
lum <- boxplot(star_data$Luminosity.L.Lo., main="Luminosity.L.Lo.")
rad <- boxplot(star_data$Radius.R.Ro., main="Radius.R.Ro.")
ab <- boxplot(star_data$Absolute.magnitude.Mv., main="Absolute.magnitude.Mv.")
read <- boxplot(star_data$Star.type, main="Star.type")
