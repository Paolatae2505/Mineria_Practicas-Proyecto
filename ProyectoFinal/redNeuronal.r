gtd_data <- read.csv("globalterrorismdb_0718dist.csv")

gtd_data$success <- as.factor(gtd_data$success)
summary(gtd_data)
str(gtd_data)

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

gtd_data_num <- convertir_a_num(muestra)
head(gtd_data_num)

# ------- Creamos conjuntos de entrenamiento y prueba ---------
install.packages("caTools")
library(caTools)
set.seed(123)

# Quitar los eventid y success
borrar <- c("eventid")
gtd_data2 <- gtd_data[ , !(names(gtd_data) %in% borrar)]
gtd_data2$success <- as.factor(gtd_data2$success)
str(gtd_data2)

split = sample.split(gtd_data$success, SplitRatio = 0.66)
summary(split)

entrena = subset(gtd_data, split == TRUE)
prueba = subset(gtd_data, split == FALSE)

summary(entrena)
summary(prueba)


# Crear formula para pasarle a la red 
# Usamos las características obtenidas en preprocesamiento o usamos todo??

# caracteristicas de preprocesamiento: ------------------------
#library(dplyr)
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

#Entrenamos la red neuronal
# Params: atributos, conjunto de entrenamiento, num de neuronas capa oculta , false categ/ true numerica
red <- neuralnet(form,entrena,hidden=9,linear.output=FALSE) # pasar a numericos para que funcione

#Graficamos la red neuronal
plot(red)
red
summary(red)

# Veamos los resultados del entrenamiento
out <- cbind(red$covariate, +red$net.result[[1]])
out

#Vamos a poner los nombres de columnas
# library(dplyr)
out <- out %>% as.data.frame %>% rename(NO = 16, YES = 17)

head(out)
colnames(out)

# Probamos el modelo entrenado
# Calcular las predicciones sobre el conjunto de prueba
prediccion <- predict(red,prueba)

prediccion

# Verificar los resultados
print(head(round(prediccion,2)))


