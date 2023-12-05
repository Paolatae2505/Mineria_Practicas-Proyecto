exactitud <- function (TP,TN, FP,FN){
  return ((TP + TN) / (TP + FN +FP + TN))
}
error_clasificaccion <- function (TP,TN, FP,FN){
  return ((FP + FN) / (TP + FN +FP + TN))
}
sensibilidad<- function (TP,FN){
  return (TP / (TP + FN ))
}
especificidad<- function (TN, FP){
  return (TN / (TN + FP))
}
presicion <-function (TP, FP){
  return (TP / (TP + FP))
}
NPV <- function (TN, FN){
  return (TN / ( TN + FN))
}
FP_rate <- function (FP,TN){
  return  (FP / (FP + TN))
}
FDR<- function (TP, FP){
  return  (FP / (FP + TP))
}
FN_rate <- function (TP,FN){
  return  (FN / (FN + TP)) 
}
BACC <- function (TP,TN, FP,FN){
  return  (((TP / (TP + FN)) + (TN / (FP + TN))) / 2)
}
F_measure<- function (p,r){
  return ((2*r*p)/(r + p))
}
kappa  <- function (TP,TN, FP,FN,E){
  return ((TP + TN - E) / (TP + FN +FP + TN - E))
}

tabla_medidas  <- function(TP,TN, FP, FN){
  num_filas <- 1
  num_columnas <- 12
  medidas <- matrix(nrow = num_filas, ncol = num_columnas)
  
  colnames(medidas) <- c("Exactitud" ,"Error de clasificación","Sensibilidad","Especificidad",
                                "Precisión", "NPV", "Falsas alarmas", "FDR",  "FN rate", "BACC",
                                "F-measure", "Índice kappa")
  medidas[1, 1] = exactitud(TP,TN, FP, FN)
  medidas[1, 2] = error_clasificaccion (TP,TN, FP, FN)
  medidas[1, 3] = sensibilidad(TP,FN)
  medidas[1, 4] = especificidad(TN, FP)
  medidas[1, 5] = presicion(TP, FP)
  medidas[1, 6] = NPV (TN, FN)
  medidas[1, 7] = FP_rate(TP, FP)
  medidas[1, 8] = FDR(TP, FP)
  medidas[1, 9] = FN_rate(TP,FN)
  medidas[1, 10] = BACC(TP,TN, FP, FN)
  r <- medidas[1, 3]
  p <- medidas[1, 5]
  medidas[1, 11] = F_measure(r,p)
  E <- 1#revisar E(TP+TN)
  medidas[1, 12] = kappa(TP,TN, FP, FN,E)
  return(medidas)
}
tabla_medidas(1,2,3,4)
