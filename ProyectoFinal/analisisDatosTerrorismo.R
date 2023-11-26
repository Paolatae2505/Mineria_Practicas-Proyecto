require(tidyverse)
library(corrplot)

gtd_data <- read.csv("/home/paola/Documentos/SeptimoSemestre/MYAD/ProyectoFinal/globalterrorismdb_0718dist.csv")

# Visualizar las primeras filas del conjunto de datos
head(gtd_data)

# Resumen estadístico de las variables numéricas
summary(gtd_data)

# Información sobre el conjunto de datos
str(gtd_data)

# Vector donde almacenaremos la distribución de las variable categoricas o NA
# Vara las que no aplique
distribucion <- vector( length=length(names(gtd_data)))
cols_names <- colnames(gtd_data)
j <- 1
for(i in gtd_data) {
  if (class(i) == "numeric"){
    # Para saber en que posición guardar la distribución que observemos
    print(j)
    # Gráficamos histogramas de variables categoricas para aproximar distribución
    hist((i), main = paste("Histograma de " , cols_names[j]), xlab = cols_names[j])
  }else{
      distribucion[j] = "NA"
  }
  j <-j+1
}
distribucion[1] = "beta"
distribucion[14] = "normal"
distribucion[15] = "beta"
distribucion[71] = "lognormal"
distribucion[102] = "lognormal"
distribucion[108] = "lognormal"
distribucion[113] = "lognormal"
distribucion[118] = "lognormal"
distribucion[119] = "lognormal"
distribucion[120] = "lognormal"
                                
# Calcular métricas
Valores_Perdidos = sapply(gtd_data, function(x) sum(is.na(x)) / length(x) * 100)
#Niveles = sapply(gtd_data, function(x) ifelse(is.factor(x), paste(levels(x), collapse = ", "), NA))
#Frecuencia = sapply(gtd_data, function(x) ifelse(is.factor(x), table(x), NA))

#Verificar cuales atributos son factores
sapply(gtd_data, function(x) is.factor(x))

#obtener variables numericas
cols_numeric <- colnames(gtd_data[,sapply(gtd_data,is.numeric)])
numeric_gtd_data <- gtd_data[cols_numeric]
#Ver las variables categoricas
resumen_categoricas <- summary(gtd_data)


# Identificar las variables categóricas
variables_categoricas <- sapply(gtd_data, function(columna) is.factor(columna) | is.character(columna))

# Mostrar las variables categóricas
nombres_categoricos <- names(variables_categoricas[variables_categoricas])
print(nombres_categoricos)
nombres_categoricos

#Clase de las variables
sapply(gtd_data, class)
# Verificamos cuales columnas son factores
factores <- sapply(gtd_data, is.factor)
columnas_factores <- names(factores[factores])
#---------------------------------------------------------------------
 #Programa para obtener Factores de las Categoricas 
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

allowed_values <-  vector( length=length(names(gtd_data)))

j <- 1
for(i in gtd_data) {
    # Obtenemos sus valores únicos
    unique_values <- unique(i)
    # Omitimos valores nulos
    unique_values <- na.omit(unique_values)

    # Verificamos que no sean más de 50 valores diferentes para considerarlos
    # valores permitidos
    if (length(unique_values) < 50) {
        if (is.numeric(i)){
            allowed_values[j] = paste(min(unique_values), "-", max(unique_values))
        } else {
            allowed_values[j] = paste(unique_values, collapse = ", ")
        }
    } else {
        allowed_values[j] = "NA"
    }
    j <- j+1
}

# Mostrar los valores permitidos para cada columna
print(allowed_values)

cols_names <- colnames(gtd_data)

# Vector donde almacenaremos si una variable cuenta con valores atípicos
atypical <- vector( length=length(names(gtd_data)))

j <-1
for(i in gtd_data) {
  if(class(i) == "numeric") {
      # Graficamos el bloxpot de las variables numéricas y lo guardamos
      box <- boxplot(i, main = paste("Boxplot de " , cols_names[j]), xlab = cols_names[j], plot = FALSE)
      # Obtenemos los valores que identifica como atípicos
      valores_atipicos <- box$out

      atypical[j] <- ifelse(length(valores_atipicos) > 0, "yes", "no")

  } else {
      atypical[j] <- "no"
  }

  j <-j+1
}
print(atypical)
Niveles = sapply(gtd_data, function(x) ifelse(is.factor(x), paste(levels(x), collapse = ", "), NA))
Frecuencia = sapply(gtd_data, function(x) ifelse(is.factor(x), table(x), NA))

#_---------------------------------------------------------
gtd_data[columnas_factores] <- lapply(gtd_data[columnas_factores], as.factor)

# ------------- Creación de tabla (matriz) ----------------
num_filas <- length(names(gtd_data))
num_columnas <- 12
info_atributos <- matrix(nrow = num_filas, ncol = num_columnas)
                    
colnames(info_atributos) <- c("Nombre", "Tipo", "ValoresPermitidos", "ValPerdidos", "Min", "Max", "Mean", "DevEstandar", "TipoDist", "Niveles","Frecuencia" ,"Atipicos")
nombres_gtd_data = names(gtd_data)

for (i in 1:num_filas) {
    atributo = gtd_data[[nombres_gtd_data[i]]]
    # Col 1: Nombre del atributo
    info_atributos[i, 1] = nombres_gtd_data[i]
    # Col 2: Tipo de atributo (nominal, ordinal, numérico, etc.).
    info_atributos[i, 2] = class(atributo)
    # Col 3: Valores permitidos (si aplica)
    info_atributos[i, 3] = allowed_values[i]
    # Col 4: Porcentaje de valores perdidos.
    Valores_Perdidos = sum(is.na(atributo)) / length(atributo) * 100
    info_atributos[i, 4] = Valores_Perdidos

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
    info_atributos[i, 9] =  distribucion[i]
    # Col 10 y 11: Si es categórico, los niveles y frecuencia de cada uno.
    if (is.factor(atributo)) {
      info_atributos[i, 10] = Niveles[i]
      info_atributos[i, 11] = Frecuencia[i]
    } else {
      info_atributos[i, 10] = NA
      info_atributos[i, 11] = NA
    }
    # Col 12: Indicar si el atributo presenta valores atípicos.
    info_atributos[i, 12] = atypical[i]
    
}


# Mostrar resultado
print(info_atributos)
 #-----------------------------------------------------------------------------------------
                                #CORRELACIÓN (Preguntas)
#------------------------------------------------------------------------------------------

#convertir factor y char a numeric
cols_names <- colnames(gtd_data)
j <- 1
set.seed(1)
for(i in gtd_data) {
  n = nlevels(i)
  if(class(i) == 'character'){
      x <- as.factor(i)
      gtd_data[cols_names[j]]<- as.numeric(x)
  }
  if(class(i) == 'factor'){
    gtd_data[cols_names[j]]<- as.numeric(i) #as.factor(sample(i, n, replace=TRUE))
  }
  j <-j+1
}
                    
correlation_matrix <- cor(gtd_data)


print(correlation_matrix)
#Variables objetivo
subset_data <- gtd_data[c("success", "city", "attacktype1","attacktype2","attacktype3")]
correlation_matrix_subset <- cor(subset_data)
print(correlation_matrix_subset)



umbral <- 0.7  

# Encontrar pares de variables con correlación superior al umbral
correlation_pairs <- which(abs(correlation_matrix) > umbral & correlation_matrix != 1, arr.ind = TRUE)

# Filtrar las variables únicas de los pares encontrados
variables_con_correlacion_fuerte <- unique(c(row.names(correlation_pairs), colnames(correlation_pairs)))



# Variables más relacionadas con success
cor_success <- correlation_matrix["success", ]

# Variables más relacionadas con city
cor_city <- correlation_matrix["city", ]

# Variables más relacionadas con attacktype1
cor_attacktype1 <- correlation_matrix["attacktype1", ]

# Variables más relacionadas con attacktype2
cor_attacktype2 <- correlation_matrix["attacktype2", ]

# Variables más relacionadas con attacktype3
cor_attacktype3 <- correlation_matrix["attacktype3", ]

# Imprimir los resultados
print("Correlaciones con success:")
print(cor_success)

print("Correlaciones con city:")
print(cor_city)

print("Correlaciones con attacktype1:")
print(cor_attacktype1)

print("Correlaciones con attacktype2:")
print(cor_attacktype2)

print("Correlaciones con attacktype3:")
print(cor_attacktype3)
