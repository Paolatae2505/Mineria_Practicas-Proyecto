# Carga de datos
gtd_data <- read.csv("/home/paola/Documentos/SeptimoSemestre/MYAD/ProyectoFinal/globalterrorismdb_0718dist.csv")

# Muestreo del 10%
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

#---- TRATAMIENTO DE VALORES PERDIDOS -----
# remplazando por media / moda / mediana
gtd_data_m<-data.frame(muestra)
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

# reemplazando usando aprendizaje automatico
install.packages("missForest")
library(missForest)

# convertir a numeric los character y factor
gtd_data_numeric<-data.frame(muestra)
cols_names <- colnames(gtd_data)
j <- 1
set.seed(1)
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

# Imputar los valores perdidos, usando los parámetros con valores default
gtd_data_numeric_imp <- missForest(gtd_data_numeric)
summary(gtd_data_numeric_imp)
