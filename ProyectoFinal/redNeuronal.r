gtd_data_orig <- read.csv("globalterrorismdb_0718dist.csv")

# Muestreo del 10%
porcentaje_muestreo <- 0.1
tamano_muestra <- round(nrow(gtd_data_orig) * porcentaje_muestreo)

# Configuramos una semilla para reproducibilidad
set.seed(123)

# Realizamos el muestreo
gtd_data <- gtd_data[sample(nrow(gtd_data_orig), tamano_muestra), ]

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

prediccion_c <-round(prediccion,2)
# Ahora vamos a crear una matriz de confusión simple:
labels <- c("success", "no success")

# Red 1
prediction_label <- data.frame(max.col(prediccion_c)) %>%
  mutate(prediccion_c=labels[max.col.prediccion_c.]) %>%
  select(2) %>%
  unlist()
table(prueba$success, prediction_label)
