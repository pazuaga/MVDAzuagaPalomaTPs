# processing.R
# ------------------------------------------------------------------------------
# Busco tomar la tabla de comunicados crudos y devolver una tabla
#           lematizada y limpia, lista para calcular la DTM.
#           - Limpia el texto (puntuación, números, caracteres especiales, espacios)
#           - Lematiza con udpipe en español
#           - Se queda con sustantivos, verbos y adjetivos en minúscula
#           - Saca stopwords 


# Librerías
library(tidyverse)
library(here)
library(udpipe)
library(stopwords)

# Crear la carpeta output si no existe 
output_dir <- here("TP2", "output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Creando el directorio: ", output_dir)
} else {
  message("El directorio ya existe: ", output_dir)
}

# Leemos la tabla generada por scraping_oea.R 
data_path <- here("TP2", "data", "comunicados_oea.rds")
comunicados <- read_rds(data_path)

message("Comunicados cargados: ", nrow(comunicados))

# Limpieza inicial del cuerpo 

comunicados_limpio <- comunicados |>
  mutate(
    # Saca saltos de línea, tabulaciones y retornos de carro
    cuerpo_limpio = str_replace_all(cuerpo, "[\\r\\n\\t]+", " "),
    # Saca comillas y caracteres especiales (misma lista que en clase)
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[\\\"'\u201c\u201d\u2018\u2019\u00ab\u00bb`\u00b4%()]", " "),
    # Saca todos los signos de puntuación (clase POSIX [:punct:])
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[[:punct:]]", " "),
    # Saca números (clase POSIX [:digit:])
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[[:digit:]]", " "),
    # Normaliza espacios múltiples
    cuerpo_limpio = str_squish(cuerpo_limpio)
  )

# Descargamos el modelo de udpipe en español
m_es <- udpipe_download_model(language = "spanish", overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)

# Lematizamos 
# udpipe_annotate devuelve una fila por cada palabra del texto, con el lema
# (lemma) y la categoría gramatical (upos: NOUN, VERB, ADJ, etc.).
message("Lematizando los comunicados (esto puede tardar unos minutos)...")

lemas <- udpipe_annotate(
  modelo_es,
  x = comunicados_limpio$cuerpo_limpio,
  doc_id = comunicados_limpio$id
) |>
  as.data.frame() |>
  mutate(id = as.integer(doc_id)) |>
  select(id, lemma, upos)

# Agregamos los títulos para tenerlos como contexto (left_join)
lemas <- lemas |>
  left_join(
    comunicados_limpio |> select(id, titulo),
    by = "id"
  )

message("Lematización terminada. Tokens totales: ", nrow(lemas))

# Filtramos sustantivos, verbos y adjetivos 
lemas <- lemas |> filter(upos %in% c("NOUN", "VERB", "ADJ"))

# Pasamos todo a minúscula 
lemas <- lemas |> mutate(lemma = str_to_lower(lemma))

message("Tras filtrar NOUN/VERB/ADJ y pasar a minúscula: ",
        nrow(lemas), " tokens.")

# Eliminación de stopwords
# Cargamos stopwords en español e inglés (por si aparecen términos en inglés
# en algún comunicado)
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_words <- tibble(lemma = c(stop_es, stop_en))

# Guardamos cuántos tokens había antes para informar por consola
n_antes <- nrow(lemas)

lemas <- lemas |>
  anti_join(stop_words, by = "lemma") |>
  # También sacamos lemmas que sean solo números 
  # y palabras muy cortas. 
  filter(
    !str_detect(lemma, "^\\d+$"),
    nchar(lemma) > 2
  )

n_despues <- nrow(lemas)
message("Stopwords eliminadas: ", n_antes - n_despues,
        ". Quedan ", n_despues, " tokens.")

# Guardamos el resultado 
# attr() para dejar registro de cuándo corrimos el procesamiento.
attr(lemas, "fecha_proceso") <- Sys.time()

write_rds(lemas, file = file.path(output_dir, "processed_text.rds"))

message("Archivo guardado en: ", file.path(output_dir, "processed_text.rds"))