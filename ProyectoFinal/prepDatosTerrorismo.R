# Carga de datos
gtd_data <- read.csv("globalterrorismdb_0718dist.csv")
str(gtd_data)

# ------------- LIMPIEZA PREVIA A LA SELECCIÓN -------------

# Convertimos a NA los espacios en blanco:

library(dplyr)
gtd_data_limpieza <- gtd_data %>% 
    mutate_all(~ ifelse(. %in% c("", " ", "Unknown", -9,-99), NA, .))
str(gtd_data_limpieza)
# Quitamos versión _txt de los datos:

columnas_txt <- grep("_txt$", names(gtd_data_limpieza), value = TRUE)
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
    gtd_data_limpieza <- crear_asociacion_unica_y_llenar(gtd_data_limpieza, col_sin_txt, columna)
}

# Eliminar columnas con terminación en _txt
for (columna in columnas_txt) {
  gtd_data_limpieza <- gtd_data_limpieza[, -which(names(gtd_data_limpieza) == columna), drop = FALSE]
}

# Vemos el resultado:
str(gtd_data_limpieza)
ncol(gtd_data_limpieza)


# -- Eliminación de columnas con más del 90% de valores perdidos: --

eliminar_columnas_valores_perdidos <- function(datos, umbral = 90) {
    # Transforma cadenas vacias a NAs
    gtd_data_con_na <- datos %>% 
    mutate_all(~ ifelse(. %in% c("", " "), NA, .))
  
    # Calcula el porcentaje de valores perdidos por columna
    porcentaje_perdido <- colMeans(is.na(gtd_data_con_na)) * 100
  
    # Encuentra las columnas que superan el umbral
    columnas_a_eliminar <- names(porcentaje_perdido[porcentaje_perdido > umbral])
  
    # Elimina las columnas identificadas
    datos_limpio <- datos[, setdiff(names(datos), columnas_a_eliminar)]
  
    return(datos_limpio)
}

# Aplicamos a gtd_data
gtd_data_limpieza <- eliminar_columnas_valores_perdidos(gtd_data_limpieza, 90)
str(gtd_data_limpieza)
ncol(gtd_data_limpieza)

# Eliminamos columnas de forma manual
columnas_a_eliminar <- c("location", "summary", "nwound", "propcomment", "addnotes", "scite2", "scite3", "related" )
gtd_data_limpieza <- gtd_data_limpieza[, !(names(gtd_data_limpieza) %in% columnas_a_eliminar)]
str(gtd_data_limpieza)

# Utilizamos un muestreo del 10% para facilitar la ejecución
#porcentaje_muestreo <- 0.1
#tamano_muestra <- round(nrow(gtd_data_limpieza) * porcentaje_muestreo)
#set.seed(123)
#muestra <- gtd_data_limpiado[sample(nrow(gtd_data_limpieza), tamano_muestra), ]
# Observamos la muestra
#summary(muestra)

# ---- IGUALAMOS LA CANTIDAD DE INSTANCIAS DE SUCCESS = 0 CON las = a 1 ---- 
set.seed(123)
gtd_data_s1 <- gtd_data_limpieza[gtd_data_limpieza$success == 1,]
gtd_data_s0 <- gtd_data_limpieza[gtd_data_limpieza$success == 0,]
occurrences_s0 <- nrow(gtd_data_s0)
gtd_data_s1 <- gtd_data_s1[sample(nrow(gtd_data_s1), occurrences_s0),]
muestra <- bind_rows(gtd_data_s1, gtd_data_s0)

# ---- SELECCION DE ATRIBUTOS CON CHI^2 -----

library(dplyr)
library(mlbench)
library(FSelector)

# correlaciones <- cor(gtd_data[, -1], gtd_data$success)
# Imprimir las correlaciones
# print(correlaciones)

# Utilizando el enfoque de filtros por chi cuadrada
pesos <- chi.squared(success ~ ., data = muestra)
pesos
    
# Pesos en orden de importancia
orden <- order(pesos$attr_importance)
dotchart(pesos$attr_importance[orden],labels=rownames(pesos)[orden],xlab="Importancia") # Visualizar
subconjunto <- rownames(pesos)[pesos$attr_importance > 0]

# Crea muestra con las variables que nos sirven
gtd_data_seleccion <- muestra[, subconjunto]
# Paso eventid al principio:
gtd_data_seleccion <- gtd_data_seleccion %>% select(eventid, everything())
# Añado el atributo success al final
success <- (muestra$success)
gtd_data_seleccion <- cbind(gtd_data_seleccion, success)

str(gtd_data_seleccion)
ncol(gtd_data_seleccion)

#---- TRATAMIENTO DE VALORES PERDIDOS -----

# remplazando por media / moda / mediana :

mode2 <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
imputacion <- function(data){
    for (var in 1:ncol(data)) {
        if (class(data[,var])=="numeric") {
            data[is.na(data[,var]),var] <- mean(data[,var], na.rm = TRUE)
        } else if (class(data[,var]) %in% c("character", "factor")) {
            no_empty <- na.omit(data[,var][data[,var] != ""])
            m <- mode2(no_empty)
            data[is.na(data[,var]),var] <- m
            data[data[,var]== "",var] <- m
        } else if (class(data[,var]) == "integer"){
            data[is.na(data[,var]),var] <- median(data[,var], na.rm = TRUE)
        }
    }
    return(data)
}

# Aplicamos:
gtd_data_sin_vp <- imputacion(gtd_data_seleccion)
str(gtd_data_sin_vp)

# reemplazando usando aprendizaje automatico :

install.packages("missForest")
library(missForest)
# Utilizamos un muestreo del 1% debido al bajo poder de procesamiento
# con el que contamos
porcentaje_muestreo <- 0.01
tamano_muestra <- round(nrow(muestra) * porcentaje_muestreo)
muestra <- muestra[sample(nrow(muestra), tamano_muestra),]
# Observamos la muestra
summary(muestra)
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
gtd_data_sin_vp2 <- missForest(gtd_data_numeric)
summary(gtd_data_sin_vp2)


#---- ELIMINACIÓN DE VALORES ATÍPICOS -----
columnas_con_nas <- colSums(is.na(gtd_data_sin_vp)) > 0
columnas_con_nas <- names(columnas_con_nas[columnas_con_nas])
print(columnas_con_nas)
quitar_atipicos <- function(columna, threshold = 1.5) {
  # Calcular el rango intercuartílico para la columna
  iqr <- IQR(columna, na.rm = TRUE)
  
  # Calcular los límites para identificar valores atípicos
  lower_limit <- median(columna, na.rm = TRUE) - threshold * iqr
  upper_limit <- median(columna, na.rm = TRUE) + threshold * iqr
  
  # Filtrar los valores atípicos
  columna_filtrada <- ifelse(columna < lower_limit | columna > upper_limit, NA, columna)
  
  return(columna_filtrada)
}

columnas_con_nans <- colSums(is.na(gtd_data_sin_vp)) > 0

# Mostrar las columnas con valores faltantes
print(columnas_con_nans)
# Aplicamos a nuestro dataset:
str(gtd_data_sin_vp)
columnas_numericas <- gtd_data_sin_vp[sapply(gtd_data_sin_vp, is.numeric)]
str(columnas_numericas)
print(dim(gtd_data_sin_vp))
hay_infinitos <- sapply(columnas_numericas, function(col) any(is.infinite(col)))
# Imprimir el resultado
print(hay_infinitos)

hay_nas <- sapply(columnas_numericas, function(col) any(is.na(col)))

# Imprimir el resultado
print(hay_nas)
# Verificar las columnas seleccionadas y sus tipos
print(sapply(columnas_numericas, class))
for (nombre_columna in names(columnas_numericas)) {
  gtd_data_sin_vp[[nombre_columna]] <- quitar_atipicos(gtd_data_sin_vp[[nombre_columna]])
}


#gtd_data_sin_atipicos <- quitar_atipicos(gtd_data_sin_vp)
head(gtd_data_sin_vp)
summary(gtd_data_sin_vp)

hay_nas_en_data <- any(is.na(gtd_data_sin_vp))
print(hay_nas_en_data)

columnas_con_nas <- colSums(is.na(gtd_data_sin_vp)) > 0
columnas_con_nas <- names(columnas_con_nas[columnas_con_nas])
print(columnas_con_nas)

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
    
    data[[columna]] <- cut_interval(data[[columna]], n = num_bins, dig.lab = 9)
  }

  return(data)
}

# quitarle las columnas eventid y success al data set muestra
gtd_data_discretizado <- gtd_data_sin_vp[, !(colnames(gtd_data_sin_vp) %in% c('eventid', 'success'))]
str(gtd_data_discretizado)
# discretizar con la funcion discretizar_por_rango 
gtd_data_discretizado2 <- discretizar_por_rango(gtd_data_discretizado)
str(gtd_data_discretizado2)
eventid <- as.factor(gtd_data_sin_vp$eventid)
gtd_data_discretizado2 <- cbind(eventid, gtd_data_discretizado2)
success <- as.factor(gtd_data_sin_vp$success)
gtd_data_discretizado2 <- cbind(gtd_data_discretizado2, success)

head(gtd_data_discretizado)
str(gtd_data_sin_vp)
str(gtd_data_discretizado2)

# ----- GENERACION CSV CON COLUMNAS NUMERICAS DISCRETIZADAS -----

head(gtd_data_discretizado)
write.csv(gtd_data_discretizado, "gtd_data_discretizado.csv", row.names = TRUE)



#---- NORMALIZACIÓN -----

# Normalización min-max
min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# ----- GENERACION CSV CON ÚNICAMENTE COLUMNAS NUMÉRICAS NORMALIZADAS -----

# Seleccionar solo las columnas numéricas
columnas_numericas <- sapply(gtd_data_sin_vp, is.numeric)
# quitarle las columnas eventid y success al data set muestra
gtd_data_num <- gtd_data_sin_vp[, !(colnames(gtd_data_sin_vp) %in% c('eventid', 'success'))]
str(gtd_data_num)
# Aplicar normalización solo a las columnas numéricas
gtd_data_normalizado_some <- as.data.frame(lapply(gtd_data_num, function(x) if (is.numeric(x)) min_max(x) else x))
str(gtd_data_normalizado_some)
eventid <- (gtd_data_sin_vp$eventid)
gtd_data_normalizado <- cbind(eventid, gtd_data_normalizado_some)
success <- (gtd_data_sin_vp$success)
gtd_data_normalizado_some <- cbind(gtd_data_normalizado, success)

head(gtd_data_normalizado_some)
str(gtd_data_discretizado)
str(gtd_data_normalizado_some)
                                             
write.csv(gtd_data_normalizado_some, "gtd_data_normalizado_some.csv", row.names = TRUE)

# ----- GENERACION CSV CON TODAS LAS COLUMNAS NUMERICAS Y NORMALIZADAS -----

# Convertir todo a numérico
convertir_a_num <- function(data){
    cols_names <- colnames(data)
    j <- 1
    set.seed(1)
    for(i in data) {
      n = nlevels(i)
      if(class(i) == 'character'){
          x <- as.factor(i)
          data[cols_names[j]] <- as.numeric(x)
      }
      if(class(i) == 'factor'){
        data[cols_names[j]] <- as.numeric(i)
      }
      j <- j + 1
    }
    return(data)
}

gtd_data_numerico <- convertir_a_num(gtd_data_sin_vp)
gtd_data_num <- gtd_data_numerico[, !(colnames(gtd_data_numerico) %in% c('eventid', 'success'))]
gtd_data_normalizado_all <- as.data.frame(lapply(gtd_data_num,min_max))
eventid <- (gtd_data_numerico$eventid)
gtd_data_normalizado_all2 <- cbind(eventid, gtd_data_normalizado_all)
success <- (gtd_data_numerico$success)
gtd_data_normalizado_all <- cbind(gtd_data_normalizado_all2, success)
str(gtd_data_normalizado_all)

write.csv(gtd_data_normalizado_all, "gtd_data_normalizado_all.csv", row.names = TRUE)
