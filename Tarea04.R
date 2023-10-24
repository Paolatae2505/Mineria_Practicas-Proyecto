require(tidyverse)

library(ggplot2)

star_data <- read.csv("/home/paola/Documentos/SeptimoSemestre/MYAD/Practicas/PGIt/Mineria_Practicas-Proyecto/6 class csv.csv")

#a. Variables categóricas
star_data <- star_data %>% mutate_if(is.character, as.factor)
star_data$Star.type <- as.factor(star_data$Star.type)
head(star_data)

#b. Resúmen
summary(star_data)

# c.Histogramas
require(DataExplorer)
plot_histogram(star_data)

# Boxplots
tem <- boxplot(star_data$Temperature..K., main="Temperature (K)")
lum <- boxplot(star_data$Luminosity.L.Lo., main="Luminosity.L.Lo.")
rad <- boxplot(star_data$Radius.R.Ro., main="Radius.R.Ro.")
ab <- boxplot(star_data$Absolute.magnitude.Mv., main="Absolute.magnitude.Mv.")
read <- boxplot(star_data$Star.type, main="Star.type")

#d. Gráfica de barras de la variable Spectral class
ggplot(star_data, aes(x = Spectral.Class)) +
  geom_bar() +
  labs(title = "Distribución de Spectral Class") 
  
# e.Diagrama de dispersión
pairs(star_data[, c("Temperature..K.", "Luminosity.L.Lo.", "Radius.R.Ro.", "Absolute.magnitude.Mv.","Star.type")])
names(star_data)

#f.Gráfica de correlación
correlation_matrix <- cor(star_data[, c("Temperature..K.", "Luminosity.L.Lo.", "Radius.R.Ro.", "Absolute.magnitude.Mv.", "Star.type")])
corrplot(correlation_matrix, method = "color")
