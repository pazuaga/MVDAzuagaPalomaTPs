# scraping_oea.R

# Buscaré scrapear los comunicados de prensa de la OEA para los meses
#           Enero, Febrero, Marzo y Abril de 2026 y armar una tabla con
#           columnas id, titulo y cuerpo.

# Librerías 
# tidyverse para manipulación, rvest para scraping,
# robotstxt para chequear permisos, here para rutas relativas, xml2 para guardar html.
library(tidyverse)
library(rvest)
library(robotstxt)
library(here)
library(xml2)

# Chequeo de permisos de scraping 
# antes de scrapear, paths_allowed()
# verifica que el robots.txt del sitio nos deje scrapear.
url_base <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp"

allowed <- paths_allowed(paths = url_base, bot = "*")
message("¿Podemos scrapear oas.org? ", allowed)

# Crawl-delay del robots.txt de oas.org = 3 segundos
# (revisado manualmente en https://oas.org/robots.txt)
crawl_delay <- 3

# Crear la carpeta data si no existe 
data_dir <- here("TP2", "data")

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message("Creando el directorio: ", data_dir)
} else {
  message("El directorio ya existe: ", data_dir)
}

# Función para scrapear una página mensual de OEA
# Dado un mes y un año, armo la URL, bajo el HTML, lo guardo en /data y
# devuelvo un tibble con los títulos y urls de los comunicados del mes.
scrapear_mes_oea <- function(mes, anio) {
  
  # Pausa según crawl-delay para no sobrecargar el servidor
  Sys.sleep(crawl_delay)
  
  # Construyo la URL del mes con paste0 
  url <- paste0(
    "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp",
    "?nMes=", mes, "&nAnio=", anio
  )
  message("Scrapeando: ", url)
  
  # Leo el HTML de la página
  pagina_html <- read_html(url)
  
  # Guardo el HTML crudo en /data con registro de fecha de descarga
  attr(pagina_html, "fecha_descarga") <- Sys.time()
  
  nombre_archivo <- paste0(
    "comunicados_mes_", mes, "_", anio, "_",
    format(Sys.Date(), "%Y-%m-%d"), ".html"
  )
  write_html(pagina_html, file = file.path(data_dir, nombre_archivo))
  
  # Extracción de los títulos y urls 
  # Selector verificado con SelectorGadget: agarra los 21 títulos del mes.
  selector_titulos <- ".itemmenulink"
  
  titulos_nodos <- pagina_html |>
    html_elements(selector_titulos)
  
  # Texto de cada título
  titulos <- titulos_nodos |>
    html_text2() |>
    str_trim()
  
  # URL de cada comunicado
  urls <- titulos_nodos |>
    html_attr("href")
  
  # Nos quedamos con los que tengan url y título no vacíos
  mantener <- !is.na(urls) & titulos != ""
  titulos <- titulos[mantener]
  urls <- urls[mantener]
  
  # Completamos rutas relativas con la raíz del sitio
  urls <- if_else(
    str_starts(urls, "http"),
    urls,
    paste0("https://www.oas.org/es/centro_noticias/", urls)
  )
  
  # Devolvemos un tibble como en scraping_nlp.qmd
  tibble(
    mes = mes,
    anio = anio,
    titulo = titulos,
    url = urls
  )
}

# Función para extraer el cuerpo de un comunicado
# Recibe la URL de un comunicado, baja la página y devuelve el texto del cuerpo
# como un único string 
extraer_cuerpo_oea <- function(url) {
  
  Sys.sleep(crawl_delay)  # Respetamos el crawl-delay igual que arriba
  
  html_comunicado <- read_html(url)
  
  # Selector verificado con SelectorGadget sobre los comunicados individuales:
  # agarra todos los párrafos del cuerpo y nada del menú/encabezado.
  selector_cuerpo <- "p:nth-child(5)"
  
  cuerpo <- html_comunicado |>
    html_elements(selector_cuerpo) |>
    html_text2() |>
    str_trim()
  
  # Concatenamos todos los párrafos en un solo string 
  cuerpo <- str_c(cuerpo, collapse = " ")
  
  # Limpieza básica: sacamos saltos de línea, comillas y espacios dobles.
  # Las tres líneas de abajo son idénticas a las de extraer_cuerpo_noticia().
  cuerpo <- str_replace_all(cuerpo, "[\\r\\n\\t]+", " ")
  cuerpo <- str_replace_all(cuerpo, "[\\\"'\u201c\u201d\u2018\u2019\u00ab\u00bb`\u00b4%()]", "")
  cuerpo <- str_squish(cuerpo)
  
  return(cuerpo)
}

# Scraping de los 4 meses
# Iteramos sobre enero (1), febrero (2), marzo (3) y abril (4) de 2026.
meses <- 1:4
anio <- 2026

comunicados <- tibble()

for (m in meses) {
  comunicados_mes <- scrapear_mes_oea(mes = m, anio = anio)
  comunicados <- bind_rows(comunicados, comunicados_mes)
  message("Mes ", m, " listo. Total comunicados hasta ahora: ", nrow(comunicados))
}

# Asignamos un id único a cada comunicado
comunicados <- comunicados |>
  mutate(id = row_number()) |>
  select(id, titulo, url, mes, anio)

message("Cantidad total de comunicados scrapeados: ", nrow(comunicados))

# Extraemos el cuerpo de cada comunicado
# map_chr aplica la función a cada url y devuelve un vector de strings.
message("Ahora bajamos el cuerpo de cada comunicado (esto puede tardar)...")

comunicados <- comunicados |>
  mutate(cuerpo = map_chr(url, extraer_cuerpo_oea))

# Tabla final con las 3 columnas
tabla_final <- comunicados |>
  select(id, titulo, cuerpo)

# Guardamos la tabla final en .rds
# attr() para dejar registro de cuándo se generó
attr(tabla_final, "fecha_descarga") <- Sys.time()
attr(tabla_final, "source_url") <- url_base

write_rds(tabla_final, file = file.path(data_dir, "comunicados_oea.rds"))

message("Tabla guardada en: ", file.path(data_dir, "comunicados_oea.rds"))
message("Scraping terminado. Filas: ", nrow(tabla_final))

