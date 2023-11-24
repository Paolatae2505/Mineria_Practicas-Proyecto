# Instalar y cargar paquetes
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Enumerar las opciones , en este caso son 2
opciones <- c("Opción 1", "Opción 2")

# Enlistar las alternativas de decisión y sus estados asociados
alternativas <- list(
  "Opción 1" = c("Estado 1: 80% de probabilidad de 70% de productos correctos", "Estado 2: 20% de probabilidad de 50% de productos correctos"),
  "Opción 2" = c("Estado 1: 70% de probabilidad de 70% de productos correctos", "Estado 2: 30% de probabilidad de 50% de productos correctos")
)

cat("Opciones:\n")
cat(paste("* ", opciones, "\n"))

cat("\nAlternativas de decisión y sus estados asociados:\n")
for (opcion in opciones) {
  cat(paste(opcion, ":\n"))
  cat(paste("  * ", alternativas[[opcion]], "\n"))
}

# Datos simulados para ejemplo
datos <- data.frame(
  Opcion = rep(c("Opción 1", "Opción 2"), each = 100),
  Probabilidad = c(runif(100, 0.7, 0.9), runif(100, 0.6, 0.8)),
  PorcentajeCorrecto = c(runif(100, 0.6, 0.8), runif(100, 0.5, 0.7))
)

# Crear un modelo de árbol de decisión
modelo_arbol <- rpart(Opcion ~ Probabilidad + PorcentajeCorrecto, data = datos, method = "class")

# Visualizar el árbol de decisión
rpart.plot(modelo_arbol, main = "Árbol de Decisión")

# Ajustar los costos
costo_opcion1 <- 450000  # costo original
costo_opcion2 <- 600000  # costo original

# Ajustar las probabilidades a priori
prob_priori_estado1 <- 0.5
prob_priori_estado2 <- 0.5

# Calcular beneficio de cada rama
beneficio_rama_opcion1_estado1 <- prob_priori_estado1 * (0.8 * 0.7 * 250 - costo_opcion1) + prob_priori_estado2 * (0.2 * 0.5 * 250 - costo_opcion1)
beneficio_rama_opcion1_estado2 <- prob_priori_estado1 * (0.8 * 0.3 * 250 - costo_opcion1) + prob_priori_estado2 * (0.2 * 0.5 * 250 - costo_opcion1)

beneficio_rama_opcion2_estado1 <- prob_priori_estado1 * (0.7 * 0.7 * 250 - costo_opcion2) + prob_priori_estado2 * (0.3 * 0.5 * 250 - costo_opcion2)
beneficio_rama_opcion2_estado2 <- prob_priori_estado1 * (0.7 * 0.3 * 250 - costo_opcion2) + prob_priori_estado2 * (0.3 * 0.5 * 250 - costo_opcion2)

# Resolver el árbol de decisión de derecha a izquierda
beneficio_opcion1 <- max(beneficio_rama_opcion1_estado1, beneficio_rama_opcion1_estado2)
beneficio_opcion2 <- max(beneficio_rama_opcion2_estado1, beneficio_rama_opcion2_estado2)

# Exponer cuál es el mejor resultado
if (beneficio_opcion1 > beneficio_opcion2) {
  cat("La mejor opción es la Opción 1 con un beneficio de", round(beneficio_opcion1, 2), "\n")
} else {
  cat("La mejor opción es la Opción 2 con un beneficio de", round(beneficio_opcion2, 2), "\n")
}

