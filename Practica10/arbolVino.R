#Importamos los paquetes
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

# Datos
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", "wine.data")

# Información
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.names", "wine.names")

# lee las primeras 10 lineas del archivo de wine.data, vemos que todos son valores
# numericos, algunos son enteros y otro flotantes
readLines("wine.data", n = 10)

# guardda en un data frame: vino, los datos del archivo wine.data
vino <- read.table("wine.data", sep = ",", header = FALSE)
 
# muestra el dataframe vino: cada una de las tuplas, de las 14 caracteristicas
vino

# lee las primeras 10 lineas del archivo de wine.names
readLines("wine.names", n = 10)

# Crea una copia del aricho wine.names llamada wine_names de tipo .txt
# y después nos muestra su contenido vemos en geneal info sobre cada una de las columnas
# como los NA's pero también información más general como la fuente de donde vienen los datos
file.copy(from = "wine.names", to = "wine_names.txt")
file.show("wine_names.txt")

# nos da un resumen sobre las carateristicas de cada una de la columna del dataframe vino
# nos mustra los cuartiles, los NA's (o en este caso su inexistencia), la media, el mínimo y 
# máximo valor
summary(vino)

# Obtenemos, del archivo wine_names, los nombres de cada una de las caracteríticas
#  listados en el dataframe vino, esto en lugar de solo nombrarlas como V1, V2, etc.
nombres <- 
  readLines("wine_names.txt")[58:70] %>% 
  gsub("[[:cntrl:]].*\\)", "", .) %>% 
  trimws() %>% 
  tolower() %>% 
  gsub(" |/", "_", .) %>% 
  # Agregamos el nombre "tipo", para nuestra primera columna con los tipos de vino
  c("tipo", .)

# Asigna los nombres que obtuvimos en la anterior seccion a las columnas de dataframe 
# vino
names(vino) <- nombres 

# Cambia a factor la clase de la columna de nombre tipo 
vino <- vino %>% 
  mutate_at("tipo", factor)

# Creamos una muestra de los datos guardados en vino: vino_entrenamiento. 
# especificamos la semilla en 1649, de manera que la muestra resultante será siempre
# misma, en este caso estamos creando una muesta con el 70% de los datos, por eso
# el .7 pasado como argumento al sample_frac
set.seed(1649)
vino_entrenamiento <- sample_frac(vino, .7)

# Agrega los datos que no se agregaron a vino_entrenamiento a vino_prueba
vino_prueba <- setdiff(vino, vino_entrenamiento)

# Hacemos un árbol de decisión, arbol_1 , usando a los datos de vino_entrenamiento
# donde la variable a predecir es la columna tipo 
arbol_1 <- rpart(formula = tipo ~ ., data = vino_entrenamiento)

# nos muestra la estructura del arbol_1
arbol_1

# nos muestra a traves de un greafico la estructura del arbol_1
rpart.plot(arbol_1)

# nos da la prediccion del tipo de los datos de vino_prueba
prediccion_1 <- predict(arbol_1, newdata = vino_prueba, type = "class")

# nos da la matriz de confusion que nos indica el tipo que predijo vs el que 
# realmente era. Esto segun nuestro modelo arbol_1 para los datos vino_prueba
confusionMatrix(prediccion_1, vino_prueba[["tipo"]])

# Repetimos el proceso de crear conjuntos con los datos de entrenamiento y prueba
# tenieendo igual el primero 70% de los datos. Luego hacemos el árbol de decisión, arbol_2,
# y calculamos la predicción. Lo único que cambia el valor de la semilla, a  8476, que  a su vez
# cambia el resultado de los procesos aletatorios como lo son la selección de que datos serán de 
# entrenamiento
set.seed(7439)
vino_entrenamiento_2 <- sample_frac(vino, .7)
vino_prueba_2 <- setdiff(vino, vino_entrenamiento_2)
arbol_2 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_2)
prediccion_2 <- predict(arbol_2, newdata = vino_prueba_2, type = "class")

# nos muestra a traves de un greafico la estructura del arbol_2
rpart.plot(arbol_2)

# nos da la matriz de confusion que nos indica el tipo que predijo vs el que 
# realmente era. Esto segun nuestro modelo arbol_2 para los datos vino_prueba
confusionMatrix(prediccion_2, vino_prueba_2[["tipo"]])

# Repetimos el proceso de crear conjuntos con los datos de entrenamiento y prueba
# tenieendo igual el primero 70% de los datos. Luego hacemos el árbol de decisión, arbol_3,
# y calculamos la predicción. Lo único que cambia el valor de la semilla, a  8476, que  a su vez
# cambia el resultado de los procesos aletatorios como lo son la selección de que datos serán de 
# entrenamiento
set.seed(8476)
vino_entrenamiento_3 <- sample_frac(vino, .7)
vino_prueba_3 <- setdiff(vino, vino_entrenamiento)
arbol_3 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_3)
prediccion_3 <- predict(arbol_3, newdata = vino_prueba_3, type = "class")
rpart.plot(arbol_3)

# nos da la matriz de confusion que nos indica el tipo que predijo vs el que 
# realmente era. Esto segun nuestro modelo arbol_3 para los datos vino_prueba
confusionMatrix(prediccion_3, vino_prueba_3[["tipo"]])

#Definimos un conjunto de funciones para generar arboles de manera repetitiva
crear_sets <- function(datos, proporcion = .7) {
  sets <- list()
  
  sets[["entrenamiento"]] <- sample_frac(datos, proporcion)
  sets[["prueba"]] <- setdiff(datos, sets[["entrenamiento"]])
  
  sets
}

entrenar_arbol <- function(sets, objetivo, predictores = ".", mi_cp = .01) {
  if(length(predictores > 1)) {
    predictores <- paste0(predictores, collapse = "+")
  }
  mi_formula <- paste0(objetivo, " ~ ", predictores) %>% as.formula()
  
  arbol <- list()
  arbol[["modelo"]] <- 
    rpart(data = sets[["entrenamiento"]], formula = mi_formula, 
          control = rpart.control(cp = mi_cp, xval = 35, minsplit = 5))
  arbol[["prediccion"]] <- predict(arbol[["modelo"]], sets[["prueba"]], type = "class")
  arbol[["referencia"]] <- sets[["prueba"]][[objetivo]]
  
  arbol
}

obtener_diagnostico <- function(arbol, objetivo, mi_cp = 0.01) {
  diagnostico <- list()
  diagnostico[["matriz"]] <- confusionMatrix(data = arbol[["prediccion"]], 
                                             reference = arbol[["referencia"]])
  
  cp <- with(arbol[["modelo"]], cptable[which.min(cptable[, "xerror"]), "CP"])
  cp_original <- mi_cp
  podar <- if(cp < mi_cp) "SI" else "NO"
  diagnostico[["mincp"]] <- data.frame("CP mínimo" = cp, "CP original" = cp_original, "Podar" = podar)
  
  diagnostico
} 

crear_arbol <- function(datos, objetivo, predictores = ".", mi_cp = 0.01) {
  resultado <- list()
  resultado[["sets"]] <- crear_sets(datos)
  resultado[["arbol"]] <- entrenar_arbol(resultado[["sets"]], objetivo, predictores, mi_cp)
  resultado[["diagnostico"]] <- obtener_diagnostico(resultado[["arbol"]], objetivo, mi_cp)
  
  resultado
}

# Repetimos el proceso que hemos venido haciendo hasta ahora para tres árbolde pero esta vez
# estandarizamos cada una de las partes en una función: tenemos una función que hace los conjuntos de entrenamiento
# y pruna, otra que los entrena y otra que nos dice que tan bueno fue el modelo resultante. Siendo que 
# al final las tres funciones son usadas en conjunto en una cuarta función: crear_arbol
set.seed(1986)
unarbol <- crear_arbol(vino, "tipo", mi_cp = 0.005)

unarbol[["diagnostico"]]

#



