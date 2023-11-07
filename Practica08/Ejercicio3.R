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
