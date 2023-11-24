# Carga de datos
gtd_data <- read.csv("/home/paola/Documentos/SeptimoSemestre/MYAD/ProyectoFinal/globalterrorismdb_0718dist.csv")

# Muestreo del 10% de tus datos
porcentaje_muestreo <- 0.1
tamano_muestra <- round(nrow(gtd_data) * porcentaje_muestreo)

# Configuramos una semilla para reproducibilidad
set.seed(123)

# Realizamos el muestreo
muestra <- gtd_data[sample(nrow(gtd_data), tamano_muestra), ]

# Muestra el resumen de la muestra
summary(muestra)

# Eliminación de valores atípicos en cada columna numérica
columnas_numericas <- sapply(muestra, is.numeric)

for (columna in colnames(muestra)[columnas_numericas]) {
  # Calcular el primer y tercer cuartil
  Q1 <- quantile(muestra[, columna], 0.25)
  Q3 <- quantile(muestra[, columna], 0.75)
  
  # Calcular el rango intercuartíl (IQR)
  IQR_valor <- Q3 - Q1
  
  # Definir el umbral basado en el IQR
  umbral_atipico <- 1.5 * IQR_valor
  
  # Identificar índices de valores atípicos
  indices_atipicos <- which(muestra[, columna] < (Q1 - umbral_atipico) | muestra[, columna] > (Q3 + umbral_atipico))
  
  # Filtrar el conjunto de datos para quitar los valores atípicos
  muestra <- muestra[-indices_atipicos, ]
}

# Muestra el resumen después de la eliminación de valores atípicos
summary(muestra)
