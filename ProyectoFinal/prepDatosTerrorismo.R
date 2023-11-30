# Carga de datos
gtd_data <- read.csv("globalterrorismdb_0718dist.csv")

# Indices de columas con valores -9 y 9
with_nines = c(17, 23, 29, 31, 72, 75, 81, 105, 110, 117, 131, 132, 133, 134)
# Sustitución de -9 y 9 con NA
for (i in with_nines) {
  gtd_data[, i][gtd_data[, i] == 9 | gtd_data[, i] == -9] <- NA
}

eliminar_columnas_valores_perdidos <- function(datos, umbral = 90) {
    # Calcula el porcentaje de valores perdidos por columna
    porcentaje_perdido <- colMeans(is.na(datos)) * 100
  
    # Encuentra las columnas que superan el umbral
    columnas_a_eliminar <- names(porcentaje_perdido[porcentaje_perdido > umbral])
  
    # Elimina las columnas identificadas
    datos_limpio <- datos[, setdiff(names(datos), columnas_a_eliminar)]
  
    return(datos_limpio)
}

# Ejemplo de uso con tus datos
gtd_data_limpiado <- eliminar_columnas_valores_perdidos(gtd_data, 90)
summary(gtd_data_limpiado)
ncol(gtd_data_limpiado)

# Suponiendo que tus datos están en un objeto llamado 'datos'
columnas_txt <- grep("_txt$", names(gtd_data_limpiado), value = TRUE)
# Mostrar los nombres de las columnas encontradas
print(columnas_txt)

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
library(FSelector)

selecciona_atributos <- function(data){
    # Utilizando el enfoque de filtros por chi cuadrada
    pesos <- chi.squared(success~.,data=muestra)
    pesos
    
    # Pesos en orden de importancia
    orden <- order(pesos$attr_importance)
    # dotchart(pesos$attr_importance[orden],labels=rownames(pesos)[orden],xlab="Importancia") # Visualizar
    subconjunto <- rownames(pesos)[pesos$attr_importance > 0]
    
    # Crea muestra con las variables que nos sirven
    data_seleccion <- muestra[, subconjunto]
    # Paso eventid al principio:
    data_seleccion <- data_seleccion %>% select(eventid, everything())
    # Añado el atributo success al final
    success <- (muestra$success)
    data_seleccion <- cbind(data_seleccion, success)
    return(data_seleccion)
}

#---- TRATAMIENTO DE VALORES PERDIDOS -----

# remplazando por media / moda / mediana
imputacion <- function(data){
    for (var in 1:ncol(data)) {
        if (class(data[,var])=="numeric") {
            data[is.na(data[,var]),var] <- mean(data[,var], na.rm = TRUE)
        } else if (class(data[,var]) %in% c("character", "factor")) {
            data[is.na(data[,var]),var] <- mode(data[,var])
        } else if (class(data[,var]) == "integer"){
            data[is.na(data[,var]),var] <- median(data[,var], na.rm = TRUE)
        }
    }
    return(data)
}

# -- reemplazando usando aprendizaje automatico --
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



#---- DISCRETIZACIÓN -----
library(dplyr)
library(ggplot2) 

# Discretizar por rango
discretizar_por_rango <- function(data, num_bins = 10) { # Usar sturges para la cantidad de bins
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



# Juntar todo ----------------------
head(muestra)
muestra_seleccion <- selecciona_atributos(muestra)
summary(muestra_seleccion)
muestra_m <- imputacion(muestra_seleccion)
head(muestra_m)
muestra_sin_atipicos <- eliminar_atipicos(muestra_m)
head(muestra_sin_atipicos) # no sirve
