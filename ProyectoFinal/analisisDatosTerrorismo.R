require(tidyverse)

gtd_data <- read.csv("/home/paola/Documentos/SeptimoSemestre/MYAD/ProyectoFinal/globalterrorismdb_0718dist.csv")

# Visualizar las primeras filas del conjunto de datos
head(gtd_data)

# Resumen estadístico de las variables numéricas
summary(gtd_data)

# Información sobre el conjunto de datos
str(gtd_data)

# Crear una tabla con información detallada para cada atributo
columnas_categoricas <- sapply(gtd_data, function(x) is.factor(x) | is.character(x))

# Convertir columnas categóricas a factores
gtd_data[, columnas_categoricas] <- lapply(gtd_data[, columnas_categoricas], as.factor)

# Calcular métricas
Valores_Perdidos = sapply(gtd_data, function(x) sum(is.na(x)) / length(x) * 100)
Niveles = sapply(gtd_data, function(x) ifelse(is.factor(x), paste(levels(x), collapse = ", "), NA))
Frecuencia = sapply(gtd_data, function(x) ifelse(is.factor(x), table(x), NA))


sapply(gtd_data, function(x) is.factor(x))

#obtener variables numericas
cols_numeric <- colnames(gtd_data[,sapply(gtd_data,is.numeric)])
numeric_gtd_data <- gtd_data[cols_numeric]

#obtener sus histrogramas
j <-0
for(i in numeric_gtd_data) {
      hist((i), main = paste("Histograma de " , cols_numeric[j]), xlab = cols_numeric[j])
      j <-j+1
}


resumen_categoricas <- summary(gtd_data)

# Filtrar solo las variables categóricas
categoricas <- resumen_categoricas[unlist(lapply(resumen_categoricas, class)) %in% c("character", "factor")]

# Obtener niveles y frecuencia para cada variable categórica
resultados <- lapply(categoricas, function(var) {
  niveles <- ifelse(is.factor(var), paste(levels(var), collapse = ", "), NA)
  frecuencia <- ifelse(is.factor(var), table(var), NA)
  return(data.frame(Niveles = niveles, Frecuencia = frecuencia))
})

# Combina los resultados en un solo marco de datos
resultados_df <- do.call(rbind, resultados)

# Agrega el nombre de la variable como una columna
resultados_df$Variable <- rownames(resultados_df)

resultados_df

Valores_Perdidos
Niveles
Frecuencia

Valores_Perdidos_2 = colMeans(is.na(gtd_data)) * 100
Valores_Perdidos_2

# Identificar las variables categóricas
variables_categoricas <- sapply(gtd_data, function(columna) is.factor(columna) | is.character(columna))

# Mostrar las variables categóricas
nombres_categoricos <- names(variables_categoricas[variables_categoricas])
print(nombres_categoricos)
nombres_categoricos


sapply(gtd_data, class)


factores <- sapply(gtd_data, is.factor)
columnas_factores <- names(factores[factores])


# Supongamos que 'gtd_data' es tu conjunto de datos

# Obtener las columnas de caracteres
columnas_caracter <- sapply(gtd_data, is.character)

# Inicializar un vector para almacenar las columnas que podrían ser factores
columnas_factores <- character()

# Definir un umbral para el número máximo de niveles permitidos
umbral_niveles <- 10

# Iterar sobre las columnas de caracteres
for (col in names(gtd_data[columnas_caracter])) {
  # Contar los niveles únicos en la columna
  num_niveles <- length(unique(gtd_data[[col]]))
  
  # Verificar si el número de niveles es menor que el umbral
  if (num_niveles <= umbral_niveles) {
    columnas_factores <- c(columnas_factores, col)
  }
}

# Mostrar las columnas identificadas como factores
print(columnas_factores)


gtd_data[columnas_factores] <- lapply(gtd_data[columnas_factores], as.factor)

# Obtener la frecuencia y niveles de los factores
frecuencia <- sapply(gtd_data[columnas_factores], function(x) table(x))
niveles <- sapply(gtd_data[columnas_factores], function(x) paste(levels(x), collapse = ", "))

Niveles = sapply(gtd_data, function(x) ifelse(is.factor(x), paste(levels(x), collapse = ", "), NA))
Frecuencia = sapply(gtd_data, function(x) ifelse(is.factor(x), table(x), NA))



# ------------- Creación de tabla (matriz) ----------------
num_filas <- length(names(gtd_data))
num_columnas <- 11
info_atributos <- matrix(nrow = num_filas, ncol = num_columnas)
                    
colnames(info_atributos) <- c("Nombre", "Tipo", "ValoresPermitidos", "ValPerdidos", "Min", "Max", "Mean", "DevEstandar", "TipoDist", "NivelesYFrecuencia", "Atipicos")
nombres_gtd_data = names(gtd_data)

for (i in 1:num_filas) {
    atributo = gtd_data[[nombres_gtd_data[i]]]
    # Col 1: Nombre del atributo
    info_atributos[i, 1] = nombres_gtd_data[i]
    # Col 2: Tipo de atributo (nominal, ordinal, numérico, etc.).
    info_atributos[i, 2] = class(atributo)
    # Col 3: Valores permitidos (si aplica)

    # Col 4: Porcentaje de valores perdidos.
    info_atributos[i, 4] = mean(is.na(atributo)) * 100

    # Para estadísticas:
    numeric <- is.numeric(atributo)
    # Col 5: Valor mínimo (si aplica)
    if (numeric) {
        info_atributos[i, 5] = min(atributo, na.rm = TRUE)
    # Col 6: máximo (si aplica)
        info_atributos[i, 6] = max(atributo, na.rm = TRUE)
    # Col 7: media (si aplica)
        info_atributos[i, 7] = mean(atributo, na.rm = TRUE)
    # Col 8: desviación estándar (si aplica).
        info_atributos[i, 8] = sd(atributo, na.rm = TRUE)
    }
    # Col 9: Si es numérico, indicar el tipo de distribución que parece seguir (p.e. normal).

    # Col 10: Si es categórico, los niveles y frecuencia de cada uno.

    # Col 11: Indicar si el atributo presenta valores atípicos.
}


# Mostrar resultado
print(info_atributos)
