# Crear una matriz con los datos
matriz_datos <- matrix(c(
  6, 4, 0, 0,
  3, 6, 1, 0,
  0, 2, 5, 3,
  0, 1, 2, 7
), ncol = 4, byrow = TRUE)

# Calcular la matriz de correlación de Pearson
matriz_correlacion <- cor(matriz_datos)

# Imprimir la matriz de correlación
print(matriz_correlacion)

# Crear una tabla simple a partir de la tabla de doble entrada.
test <- data.frame(
  xi = c(20,20,30,30,30,30,40,40,40,50,50),
  yi = c(30,40,30,40,50,60,40,50,60,50,60),
  fi = c(6,3,4,6,2,1,1,5,2,3,7)
)

# Imprimir la tabla simple
print(test)

#Permite aceder directamente a las variables de test
attach(test)
#Graficar la habilidad verbal en función del razonamiento abstracto
plot(yi, xi)
#Añadimos la recta ajustada
abline(lm(xi ~ yi))

# Guardar la información de la recta
test.lm = lm(xi ~ yi)
# Ootener un resumen del ajuste
summary(test.lm)