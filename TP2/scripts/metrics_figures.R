# metrics_figures.R
# Busco calcular la frecuencia de términos del corpus
#           de comunicados de la OEA y generar una figura
#           con los 5 términos más frecuentes.

# Cargo las librerías necesarias
library(tidyverse)   # dplyr + ggplot2 + readr 
library(here)        # rutas relativas al proyecto

# Ruta del input 
input_file <- here("TP2", "output", "processed_text.rds")

# Ruta donde vamos a guardar la figura
output_file <- here("TP2", "output", "frecuencia_terminos.png")

# Leo la tabla de lemmas que generó processing.R
procesado <- read_rds(input_file)

# Cuento cuántas veces aparece cada lemma en todo el corpus
# sort = TRUE deja la tabla ordenada de la palabra más frecuente a la menos frecuente
frecuencia <- procesado |> 
  count(lemma, sort = TRUE)

# Miro los primeros 15 términos más frecuentes para entender el corpus
print(head(frecuencia, 15))

# Me quedo con los 5 términos más frecuentes
# Estos representan los temas centrales de los comunicados de la OEA
top_terminos <- frecuencia |> 
  head(5)

# Construyo el gráfico de barras con ggplot2
grafico <- ggplot(top_terminos, aes(x = reorder(lemma, n), y = n)) +
  geom_col(fill = "steelblue") +       # barras de color azul
  coord_flip() +                         # las barras van horizontales
  labs(
    title = "Términos más frecuentes en los comunicados de la OEA",
    subtitle = "Enero a abril de 2026",
    x = "Término",
    y = "Frecuencia"
  ) +
  theme_minimal()                        # tema limpio, sin fondo gris

# Guardo la figura en TP2/output/
ggsave(
  filename = output_file,
  plot = grafico,
  width = 8,
  height = 5,
  dpi = 300
)

# Mensaje para confirmar que terminó
message("Figura guardada en: ", output_file)