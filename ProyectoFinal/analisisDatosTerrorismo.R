require(tidyverse)

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
    #info_atributos[i, 9] =  distribucion[i]
    # Col 10 y 11: Si es categórico, los niveles y frecuencia de cada uno.
    if (is.factor(atributo)) {
      info_atributos[i, 10] = paste(levels(atributo), collapse = ", ")
      info_atributos[i, 11] = paste(table(atributo), collapse = ", ")
    } else {
      info_atributos[i, 10] = NA
      info_atributos[i, 11] = NA
    }
    
}


# Mostrar resultado
print(info_atributos)
