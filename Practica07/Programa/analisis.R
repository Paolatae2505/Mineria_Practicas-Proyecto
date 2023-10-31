library(ggplot2)
library(corrplot)
sales <- read.csv('/home/paola/Documentos/SeptimoSemestre/MYAD/Practicas/PGIt/Mineria_Practicas-Proyecto/Practica07/DataSet/vgsales.csv',header=T)
head(sales)

# Correlación entre los atributos.
correlation_matrix <- cor(sales[, c("Year",  "NA_Sales", "EU_Sales","JP_Sales","Other_Sales", "Global_Sales")])
corrplot(correlation_matrix, method = "color")

plot_correlation(sales, maxcat = 5L)

# Datos atipicos.
box_Year <- boxplot(sales$Year, main="Year")
box_NA_Sales <- boxplot(sales$NA_Sales, main="NA_Sales")
box_EU_Sales <- boxplot(sales$EU_Sales, main="EU_Sales")
box_JP_Sales <- boxplot(sales$JP_Sales, main="JP_Sales")
box_Other_Sales <- boxplot(sales$Other_Sales, main="Other_Sales")
box_Global_Sales <- boxplot(sales$Global_Sales, main="Global_Sales")


# Valores se repiten más.
hist_Year <- hist((sales$Year), main="Histograma de Year")
hist_NA_Sales <- hist((sales$NA_Sales), main="Histograma de NA_Sales")
hist_EU_Sales <- hist((sales$EU_Sales), main="Histograma de EU_Sales")
hist_JP_Sales <- hist((sales$JP_Sales), main="Histograma de JP_Sales")
hist_Other_Sales <- hist((sales$Other_Sales), main="Histograma de Other_Sales")
hist_Global_Sales <- hist((sales$Global_Sales), main="Histograma de Global_Sales")


# Valores faltantes, o errores en los datos.

# Contar los valores faltantes
sum(is.na(sales))
# Contar los casos completos de la tabla
sum(complete.cases(sales))
# Omitir los casos faltantes y cuenta los que están completos
nrow(na.omit(sales))
# Creamos nueva tabla solo con casos completos
sales2 <- na.omit(sales)


head(sales2)