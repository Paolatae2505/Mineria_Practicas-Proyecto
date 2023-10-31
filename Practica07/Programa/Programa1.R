
# Crear un vector de tamaño 10000 con valores entre -5000 y 5000
random_vec <- round(runif(n=10000, min=-5000, max=5000))

# Vector donde guardaremos los resultados de las funciones
summaryFalso <- c()
summaryFalso <- append(summaryFalso,min(random_vec))
summaryFalso <- append(summaryFalso,quantile(random_vec, probs = 0.25))
summaryFalso <- append(summaryFalso,median(random_vec))
summaryFalso <- append(summaryFalso,mean(random_vec))
summaryFalso <- append(summaryFalso,quantile(random_vec, probs = 0.75))
summaryFalso <- append(summaryFalso,max(random_vec))
print(summaryFalso)

# Función para comprobar que todos los valores de dos vectores sean iguales
iguales <- function(x, y){
  comparaValores <- summaryFalso == summary(random_vec)
  all(comparaValores == TRUE)
}

# Vemos que el summary sea igual a nuestro summary hecho
iguales(summaryFalso, summary(random_vec))