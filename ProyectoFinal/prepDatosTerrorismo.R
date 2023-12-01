# Carga de datos
gtd_data <- read.csv("globalterrorismdb_0718dist.csv")
str(gtd_data)

#borrar <- c("alternative_txt")
#gtd_data <- gtd_data[ , !(names(gtd_data) %in% borrar)]

# ------------- LIMPIEZA PREVIA A LA SELECCIÓN -------------
# -------- Quitamos versión de texto de los datos ----------

columnas_txt <- grep("_txt$", names(gtd_data), value = TRUE)
print(columnas_txt)

quitar_terminacion_txt <- function(columna_txt) {
  col_sin_txt <- sub("_txt$", "", columna_txt)
  return(col_sin_txt)
}

# Crear una función para asociar las filas únicas y llenar valores faltantes
crear_asociacion_unica_y_llenar <- function(gtd_data, variable1, variable2) {
    
  # Inicializar un diccionario vacío
    asociacion_unica <- list()
    index1 <- which(names(gtd_data) == variable1)
    index2 <- which(names(gtd_data) == variable2)

    gtd_data[, variable2] <- as.character(gtd_data[, variable2])
    gtd_data[, variable2][gtd_data[, variable2] == ""] <- NA
    gtd_data[, variable2][gtd_data[, variable2] == " "] <- NA
    
  # Iterar sobre las filas del data frame
  for (i in 1:nrow(gtd_data)) {
    # Verificar si la asociación ya existe en el diccionario
    #print(gtd_data[i, index1])
    if (!is.na(gtd_data[i, index1]) && !is.null(gtd_data[i,index2]) &&
        !gtd_data[i,index1] %in% names(asociacion_unica)) {
        # Agregar la asociación al diccionario
        asociacion_unica[[as.character(gtd_data[i, index1])]] <- gtd_data[i, index2]
    }
  }

  for (i in 1:nrow(gtd_data)) {
    #Llenar valores faltantes en variable1 usando la asociación
    if (is.na(gtd_data[i, index1]) && !is.na(gtd_data[i, index2])) {
      num_asociado <- names(asociacion_unica)[asociacion_unica == gtd_data[i, index2]]
      gtd_data[i, index1] <- as.numeric(num_asociado)
    }
  }
  return(gtd_data)
}


# Recorrer las columnas que terminan en "_txt"
for (columna in columnas_txt) {
    col_sin_txt <- quitar_terminacion_txt(columna)
    print(col_sin_txt)
    print(columna)
    gtd_data <- crear_asociacion_unica_y_llenar(gtd_data, col_sin_txt, columna)
}

str(gtd_data)

# Eliminar columnas con terminación en _txt
for (columna in columnas_txt) {
  gtd_data <- gtd_data[, -which(names(gtd_data) == columna), drop = FALSE]
}

summary(gtd_data)
ncol(gtd_data)

# ---- Eliminación de columnas con más del 90% de val perdidos: -----

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

# quitarle las columnas eventid y success al data set muestra
muestra_m2 <- muestra_m[, !(colnames(muestra_m) %in% c('eventid', 'success'))]

# discretizar con la funcion discretizar_por_rango 
muestra_discretizada <- discretizar_por_rango(muestra_m2)

# Añadir 'eventid' como primera columna y 'success' como última
# muestra_sin_atipicos <- eliminar_atipicos(na.omit(muestra))
eventid <- (muestra$eventid)
muestra_discretizada2 <- cbind(eventid, muestra_discretizada)
success <- (muestra$success)
muestra_discretizada3 <- cbind(muestra_discretizada2, success)
head(muestra_discretizada3)
summary(muestra_discretizada3)
