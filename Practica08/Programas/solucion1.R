# Instalación del paquete y carga de bibliotecas 
# pararellenando los datos faltantes
install.packages("missForest")
library(missForest)
install.packages("GGally")
library(GGally)
library(MASS)
# leer dataset
sales <- read.csv('/content/drive/MyDrive/Escuela/Almacenes/vgsales.csv')
head(sales)

# Vemos que sustituciones deberán ser realizadas para los campos no numerios
unique(sales$Platform)
unique(sales$Genre)
unique(sales$Publisher)

#Sustituimos errores de dedo
sales[sales=="intendo"]<-"Nintendo"
sales[sales=="nintndo"]<-"Nintendo"
sales[sales=="ation"]<-"Action"
sales[sales=="ACTON"]<-"Action"
sales[sales=="WI"]<-"WII"
sales[sales=="wII"]<-"WII"
sales[sales=="wi"]<-"WII"
sales[sales=="Wi"]<-"WII"
sales[sales=="wii"]<-"WII"
sales[sales=="Wii"]<-"WII"
sales[sales=="PStation2"]<-"PS2"
sales[sales=="PlayStation2"]<-"PS2"
sales[sales=="PlayS2"]<-"PS2"
sales[sales=="xbox360"]<-"XBOX360"
sales[sales=="X360"]<-"XBOX360"
sales[sales==""]<-"NA"

# manejamos solo a los valores numericos
sales_numeric<- data.frame(sales$Year, sales$NA_Sales, sales$EU_Sales, sales$JP_Sales, sales$Other_Sales, sales$Global_Sales)

# imputar los valores perdidos
sales.imp <- missForest(sales_numeric)
# guardamos los valores imputados en su propio dataframe
sales_ximp<-sales.imp$ximp

# corrigiendo los errores :Z- SCORE 
# aplicar el algoritmo Z-score
sales.normalizados <- scale(sales_ximp)
sales.normalizados
ventas_norm <- as.data.frame(sales.normalizados)
# observar los primeros valores
head(ventas_norm)

# volvemos a juntar los datos numericos con los que no lo son
sales_limpio <- data.frame(sales$Rank, sales$Name, sales$Platform,
                               ventas_norm$sales.Year, sales$Genre,sales$Publisher,
                               ventas_norm$sales.NA_Sales, ventas_norm$sales.EU_Sales,
                               ventas_norm$sales.JP_Sales, ventas_norm$sales.Other_Sales,
                               ventas_norm$sales.Global_Sales)
names(sales_limpio) <- colnames(sales)#c('ID', 'items', 'store', 'price')

# visualizamos primeros valores de dataset limpio
head(sales_limpio)
# escribimos el dataset en un csv
write.csv(sales_limpio, "\vsalesLimpio.csv")

# Vemos  el resumen de los datos ya limpios para ver los efectos de la normalización y
# tratado de valores faltantes
summary(sales_limpio)


# prueba χ2 entre Genre y el Publisher
# crear tabla de contingencia
tabla <- table(sales$Genre,sales$Publisher)
tabla
# realizar la prueba
chisq.test(tabla)

