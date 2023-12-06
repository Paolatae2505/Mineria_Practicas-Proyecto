# --- A CAMBIAR UNA VEZ QUE SE TENGA EL csv DE LOS PREPROCESADOS ---
# Importar data set con normalizacion en las valiables numeric e integer
gtd_data <- read.csv("/gtd_data_normalizado_some.csv") #cambiar segun ruta del git 

# ---CONSTRUCCION DEL ARBOL ---
# Importar dependencias
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(dplyr) 
library(rpart.plot)
library(caret)

# Convertir a factor los character
gtd_data_factor <- data.frame(gtd_data)
cols_names <- colnames(gtd_data)
j <- 1
for(i in gtd_data_factor) {
  n = nlevels(i)
  if(class(i) == 'character'){
      x <- as.factor(i)
      gtd_data_factor[cols_names[j]]<- x
  }
  j <-j+1
}
gtd_data<-data.frame(gtd_data_factor)

# Creamos conjuntos de entrenamiento y prueba
set.seed(1649)
gtd_entrenamiento <- sample_frac(gtd_data, .66)
gtd_prueba <- setdiff(gtd_data, gtd_entrenamiento)

# Creamos el árbol con RPART
gtd.rpart <- rpart(formula = success ~ ., data = gtd_entrenamiento,method = "class")

# Mostramos el árbol de decisión
rpart.plot(gtd.rpart)

# --- PREDICCIONES DEL ARBOL ---
# Nos da la prediccion de exito (success) para gtd_prueba
prediccion <- predict(gtd.rpart, newdata = gtd_prueba, type = "class")
# Obtenemos la matriz de confusión de las predicciones: prediccion
confusionMatrix(prediccion, as.factor(gtd_prueba[["success"]]))

# --- AJUSTE DE HIPERPARAMETROS ---
#Ajustamos los par�metros 1
arbol1 <- rpart(success ~ .,
               control = rpart.control(minsplit = 5,maxdepth = 10),
               data = gtd_entrenamiento,method = "class")
arbol1
# Nos da la prediccion de exito (success) para gtd_prueba
prediccion_1 <- predict(arbol1, newdata = gtd_prueba, type = "class")
# obtenemos la matriz de confusión de las predicciones: prediccion_1
confusionMatrix(prediccion_1, as.factor(gtd_prueba[["success"]]))

# Ajustamos los par�metros 2
arbol2 <- rpart(success ~ ., 
               control = rpart.control(cp = 0),
               data = gtd_entrenamiento,method = "class")
rpart.plot(arbol2,main = "Exito de ataque")
# Obtenemos la matriz de confusión de las predicciones: prediccion_2
prediccion_2 <- predict(arbol2, newdata = gtd_prueba,type = "class")

# -- EVALUACION FINAL ---
# cambiar según quien tenga al finalizar los mejores valores
conf_matrix <- confusionMatrix(prediccion_1, as.factor(gtd_prueba[["success"]]))
TN <- conf_matrix[["table"]][1]
FP <- conf_matrix[["table"]][2]
FN <- conf_matrix[["table"]][3]
TP <- conf_matrix[["table"]][4]
source("Medidas.R") #cambiar según ruta del git
medidas <- tabla_medidas(TP,TN,FP,FN)
