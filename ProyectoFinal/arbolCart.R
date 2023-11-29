# --- A CAMBIAR UNA VEZ QUE SE TENGA EL csv DE LOS PREPROCESADOS ---

gtd_data <- read.csv("/content/drive/MyDrive/Escuela/Almacenes/globalterrorismdb_0718dist.csv")

# MA's
gtd_data_m<-data.frame(gtd_data)
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
gtd_data<-data.frame(gtd_data_m)

# Muestreo del 10%
#porcentaje_muestreo <- 0.1
#tamano_muestra <- round(nrow(gtd_data) * porcentaje_muestreo)

# Configuramos una semilla para reproducibilidad
#set.seed(123)

# Realizamos el muestreo
#gtd_data <- gtd_data[sample(nrow(gtd_data), tamano_muestra), ]

# ---CONSTRUCCION DEL ARBOL ---
# Instalar dependencias
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(dplyr) 
library(rpart.plot)
library(caret)

# Convertir a character los factor
gtd_data_factor <- data.frame(gtd_data)
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
gtd_data<-data.frame(gtd_data_factor)

# Creamos conjuntos de entrenamiento y prueba
set.seed(1649)
gtd_entrenamiento <- sample_frac(gtd_data, .7)
gtd_prueba <- setdiff(gtd_data, gtd_entrenamiento)

# Creamos el árbol con RPART
gtd.rpart <- rpart(formula = success ~ ., data = gtd_entrenamiento,method = "class")

# Mostramos el árbol de decisión
rpart.plot(gtd.rpart)

# --- PREDICCIONES DEL ARBOL ---
# nos da la prediccion de exiyto (success) para gtd_prueba
prediccion_1 <- predict(gtd.rpart, newdata = gtd_prueba, type = "class")
# obtenemos la matriz de confusión de las predicciones
confusionMatrix(prediccion_1, as.factor(gtd_prueba[["success"]]))

