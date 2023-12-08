# -----------------------------------------------------------------------------
# ------------------------------- Árbol CART ----------------------------------
# -----------------------------------------------------------------------------

gtd_data <- read.csv("gtd_data_discretizado.csv")


# ---CONSTRUCCION DEL ARBOL ---
# Instalar dependencias
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(dplyr) 
library(rpart.plot)
library(caret)

# Convertir a character los factor
gtd_data_factor <- gtd_data
cols_names <- colnames(gtd_data)
j <- 1
for(i in gtd_data_factor) {
  n = nlevels(i)
  if(class(i) == 'character'){
      x <- as.factor(i)
      gtd_data_factor[cols_names[j]]<- as.numeric(x)
  }
  j <-j+1
}
gtd_data<- gtd_data_factor

# Creamos conjuntos de entrenamiento y prueba
set.seed(1649)
gtd_entrenamiento <- sample_frac(gtd_data, .7)
gtd_prueba <- setdiff(gtd_data, gtd_entrenamiento)
str(gtd_entrenamiento)

# Creamos el árbol con RPART
gtd.rpart <- rpart(formula = success ~ ., data = gtd_entrenamiento,method = "class")

# Mostramos el árbol de decisión
rpart.plot(gtd.rpart)

# --- PREDICCIONES DEL ARBOL ---
# nos da la prediccion de exito (success) para gtd_prueba
prediccion_1 <- predict(gtd.rpart, newdata = gtd_prueba, type = "class")
# obtenemos la matriz de confusión de las predicciones
confusionMatrix(prediccion_1, as.factor(gtd_prueba[["success"]]))

# --- AJUSTE DE HIPERPARAMETROS ---
#Ajustamos los parámetros con minsplit y podado .09
arbol1 <- rpart(success ~ .,
               control = rpart.control(minsplit = 30,maxdepth = 10),
               data = gtd_entrenamiento,method = "class")

arbol1 <- prune(arbol1, cp = 0.1) # podando árbol
rpart.plot(arbol1,main = "Exito de ataque")
prediccion1 <- predict(arbol1, newdata = gtd_prueba, type = "class")
confusionMatrix(prediccion1, as.factor(gtd_prueba[["success"]]))

# Ajustamos los parámetros 2
arbol2 <- rpart(success ~ ., 
               control = rpart.control(cp = 0),
               data = gtd_entrenamiento,method = "class")
rpart.plot(arbol2,main = "Exito de ataque")
prediccion_2 <- predict(arbol2, newdata = gtd_prueba,type = "class")
confusionMatrix(prediccion_2, as.factor(gtd_prueba[["success"]]))


# Ajuste de parámetros 3 (ajustando la profundidad maxima)
arbol3 <- rpart(success ~ .,
                control = rpart.control(maxdepth = 5),
                data = gtd_entrenamiento, method = "class")

rpart.plot(arbol3, main = "Árbol con profundidad máxima ajustada a 5")
prediccion_3 <- predict(arbol2, newdata = gtd_prueba,type = "class")
confusionMatrix(prediccion_3, as.factor(gtd_prueba[["success"]]))

# --- EVALUCION FINAL ---
# cambiar según quien tenga al finalizar los mejores valores
conf_matrix <- confusionMatrix(prediccion_1, as.factor(gtd_prueba[["success"]]))
TN <- conf_matrix[["table"]][1]
FP <- conf_matrix[["table"]][2]
FN <- conf_matrix[["table"]][3]
TP <- conf_matrix[["table"]][4]
source("Medidas.R") #cambiar según ruta del git
medidas <- tabla_medidas(TP,TN,FP,FN)



# -----------------------------------------------------------------------------
# ------------------------------ Red Neuronal ---------------------------------
# -----------------------------------------------------------------------------

gtd_data_orig <- read.csv("datosPrepTerrorismoNumericos.csv")
ncol(gtd_data_orig)

# Muestreo del 10%
porcentaje_muestreo <- 0.1
tamano_muestra <- round(nrow(gtd_data_orig) * porcentaje_muestreo)

# Configuramos una semilla para reproducibilidad
set.seed(123)

# Realizamos el muestreo
gtd_data <- gtd_data_orig[sample(nrow(gtd_data_orig), tamano_muestra), ]
ncol(gtd_data)
# Muestra el resumen de la muestra
summary(gtd_data)


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
gtd_data <-imputacion(gtd_data)
gtd_data$success <- as.factor(gtd_data$success)
summary(gtd_data)
str(gtd_data)


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

gtd_data_num <- convertir_a_num(gtd_data)
head(gtd_data_num)

# ------- Creamos conjuntos de entrenamiento y prueba ---------
install.packages("caTools")
library(caTools)
set.seed(123)

# Quitar los eventid y success
borrar <- c("eventid")
gtd_data2 <- gtd_data_num[ , !(names(gtd_data_num) %in% borrar)]
gtd_data2$success <- as.factor(gtd_data2$success)
str(gtd_data2)

split = sample.split(gtd_data2$success, SplitRatio = 0.66)
summary(split)

entrena = subset(gtd_data2, split == TRUE)
prueba = subset(gtd_data2, split == FALSE)

str(entrena)
summary(prueba)


# Crear formula para pasarle a la red 
# Usamos las características obtenidas en preprocesamiento o usamos todo??

# caracteristicas de preprocesamiento: ------------------------
library(dplyr)
#library(mlbench)
#library(FSelector)
# Utilizando el enfoque de filtros para reducir dimensiones, calcular los pesos
#pesos <- chi.squared(success~.,data=gtd_data)
#pesos
# Pesos en orden de importancia
#orden <- order(pesos$attr_importance)
#subconjunto <- cutoff.k(orden,50)


caracteristicas <- names(gtd_data2)
caracteristicas

caracteristicas <- caracteristicas[-which(names(gtd_data2) %in% 
                                            c("success"))]
caracteristicas

# Concatenamos las cadenas
form <- paste(caracteristicas,collapse=' + ')
form

#Hacemos coincidir con el dataset de entrenamiento
form <- paste('success ~',form)
form

# Convertir a formula
form <- as.formula(form)
form

# Instalamos el paquete para redes neuronales
# install.packages("neuralnet")
library(neuralnet)

#Entrenamos las red neuronales:
# Params: atributos, conjunto de entrenamiento, num de neuronas capa oculta , false categ/ true numerica
# https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf (pag. 7)

# Red Neuronal 1
red1 <- neuralnet(form,entrena,hidden=9,linear.output=FALSE)

# Red Neuronal 2 : 2 capas ocultas con 12 nodos
red2 <- neuralnet(form, entrena, hidden = c(12, 12), linear.output = FALSE)

# Red Neuronal 3 : tasa de aprend. de 0.01
red3 <- neuralnet(form, entrena, hidden = 9, linear.output = FALSE, learningrate = 0.01)

# Red Neuronal 4
red4 <- neuralnet(form, entrena, hidden = 9, linear.output = FALSE, algorithm = "rprop+")

# Red Neuronal 5 : se repite 50 veces
red5 <- neuralnet(form, entrena, hidden = 9, linear.output = FALSE, rep = 50)

# falta red con momentum modificado

graficar_y_evaluar_red <- function(nombre, red, conjunto_entrenamiento, conjunto_prueba, labels = c("success", "no success")) {
    print(nombre)
  # Graficamos la red neuronal
  plot(red)
  summary(red)

  # Veamos los resultados del entrenamiento
  out <- cbind(red$covariate, +red$net.result[[1]])

  # Ponemos nombres de columnas
  out <- out %>% as.data.frame %>% rename(NO = 16, YES = 17)

  # Probamos el modelo entrenado
  prediccion <- predict(red, conjunto_prueba)
  print(head(round(prediccion, 2)))

  prediccion_c <- round(prediccion, 2)
  
  # Creamos una matriz de confusión simple
  prediction_label <- data.frame(max.col(prediccion_c)) %>%
    mutate(prediccion_c = labels[max.col.prediccion_c.]) %>%
    select(2) %>%
    unlist()
  
  confusion_matrix <- table(conjunto_prueba$success, prediction_label)
  print(confusion_matrix)
  
  # Puedes retornar o imprimir cualquier otra métrica que desees evaluar
  
  # También podrías retornar la matriz de confusión para su posterior análisis
  return(confusion_matrix)
}

confusion_red1 <- graficar_y_evaluar_red("red1", red1, entrena, prueba)
confusion_red2 <- graficar_y_evaluar_red("red2", red2, entrena, prueba)
confusion_red3 <- graficar_y_evaluar_red("red3", red3, entrena, prueba)
confusion_red4 <- graficar_y_evaluar_red("red4", red4, entrena, prueba)
confusion_red5 <- graficar_y_evaluar_red("red5", red5, entrena, prueba)

# --- EVALUACION ---
# Guardar las variables para la evaluación
TP <- 1
TN <- 2
FP <- 3
FN <- 4
confusionRedNeuronal <- c(TP, TN, FP, FN)
save(confusionRedNeuronal, file = "confusionRedNeuronal.RData")
