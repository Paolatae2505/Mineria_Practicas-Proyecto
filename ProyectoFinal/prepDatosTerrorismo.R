
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



#Elimincación de valores atipicos
columnas_numericas <- sapply(muestra, is.numeric)
muestra_numericas <- muestra[, columnas_numericas]

# Eliminar filas con valores atípicos
muestra <- muestra[!apply(muestra_numericas, 2, function(x) any(abs(scale(x)) > 3)), ]