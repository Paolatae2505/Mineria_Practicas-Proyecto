# Carga de datos
gtd_data <- read.csv("globalterrorismdb_0718dist.csv")

# Muestreo del 10%
porcentaje_muestreo <- 0.1
tamano_muestra <- round(nrow(gtd_data) * porcentaje_muestreo)

# Configuramos una semilla para reproducibilidad
set.seed(123)

# Realizamos el muestreo
muestra <- gtd_data[sample(nrow(gtd_data), tamano_muestra), ]

# Muestra el resumen de la muestra
summary(muestra)

#---- SELECCION DE ATRIBUTOS -----
library(dplyr)
library(mlbench)

# Cargar biblioteca especializada en selección de características:
library(FSelector)

# Utilizando el enfoque de filtros para reducir dimensiones, calcular los pesos
pesos <- chi.squared(success~.,data=muestra)
# pesos <- chi.squared(success~.,data=gtd_data)
pesos #Visualizar

#Graficar los pesos en el orden de importancia
orden <- order(pesos$attr_importance)
dotchart(pesos$attr_importance[orden],labels=rownames(pesos)[orden],xlab="Importancia")
subconjunto <- cutoff.k(pesos,69)

# Crea muestra con las variables que nos sirven
muestra_seleccion <- muestra[, subconjunto]
# Paso eventid al principio:
muestra_seleccion <- muestra_seleccion %>% select(eventid, everything())
summary(muestra_seleccion)
str(muestra_seleccion)


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


#---- ELIMINACIÓN DE VALORES ATÍPICOS -----
# Crear una función para eliminar valores atípicos
eliminar_atipicos <- function(datos, umbral = 1.5) {
  # Identificar columnas numéricas
  columnas_numericas <- sapply(datos, is.numeric)
  
  # Inicializar el conjunto de datos resultante
  datos_limpios <- datos
  
  # Iterar sobre las columnas numéricas
  for (columna in colnames(datos)[columnas_numericas]) {
    # Calcular el primer y tercer cuartil
    Q1 <- quantile(datos[, columna], 0.25)
    Q3 <- quantile(datos[, columna], 0.75)
    
    # Calcular el rango intercuartílico (IQR)
    IQR_valor <- Q3 - Q1
    
    # Definir el umbral basado en el IQR
    umbral_atipico <- umbral * IQR_valor
    
    # Identificar índices de valores atípicos
    indices_atipicos <- which(datos[, columna] < (Q1 - umbral_atipico) | datos[, columna] > (Q3 + umbral_atipico))
    
    # Filtrar el conjunto de datos para quitar los valores atípicos
    datos_limpios <- datos_limpios[-indices_atipicos, ]
  }
  
  return(datos_limpios)
}

# Ejemplo de uso
muestra <- eliminar_atipicos(muestra)


# Muestra el resumen después de la eliminación de valores atípicos
summary(muestra)

#---- DISCRETIZACIÓN -----
library(dplyr)
library(ggplot2) 

# Discretizar por rango
discretizar_por_rango <- function(data, num_bins = 10) { #PENDIENTE: VER NUM DE BINS A DIVIDIR
  # Identificar columnas numéricas
  columnas_numericas <- sapply(data, is.numeric)

  for (columna in names(data)[columnas_numericas]) {
    # Verificar el rango de la columna antes de discretizar
    cat("Rango de", columna, ":", range(data[[columna]]), "\n")
    
    data[[paste0(columna, "_disc")]] <- cut_interval(data[[columna]], n = num_bins, dig.lab = 9)
  }

  return(data)
}

datos_discretizados <- discretizar_por_rango(data.frame(muestra))
head(datos_discretizados)
summary(datos_discretizados)

