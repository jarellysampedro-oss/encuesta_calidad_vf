# ETAPA 0 — Configuración, conceptos y funciones comunes.
# Objetivo: preparar rutas y ayudas que se reutilizan sin modificar los CSV.
# Una sola vez, si faltan paquetes:
# install.packages(c('here','readr','dplyr','tidyr','ggplot2','knitr'))
suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(ggplot2)
})

for (carpeta in c('resultados', 'resultados/tablas', 'resultados/figuras')) {
  dir.create(here::here(carpeta), recursive = TRUE, showWarnings = FALSE)
}

# Los identificadores y las categorías se importan inicialmente como texto.
leer_texto <- function(ruta) {
  if (!file.exists(ruta)) stop('Archivo no encontrado: ', ruta)
  readr::read_csv(ruta,
                  col_types = readr::cols(.default = readr::col_character()),
                  na = 'NA', trim_ws = FALSE, show_col_types = FALSE)
}

# NA, blanco y los marcadores didácticos son ausencia; cero NO es ausencia.
normalizar_ausencia <- function(x) {
  y <- trimws(as.character(x))
  y[!is.na(y) & toupper(y) %in% c('', 'NA', 'N/D', 'SIN DATO')] <- NA_character_
  y
}

# Convierte texto a número solo en una columna DERIVADA; no toca la fuente.
a_numero <- function(x) suppressWarnings(as.numeric(normalizar_ausencia(x)))

# Para registrar alertas, los índices se calculan ANTES de crear el tibble.
# Así se evita el conflicto de nombres con una columna llamada 'variable'.
crear_alerta <- function(tabla, condicion, regla_id, columna_origen,
                        descripcion, severidad = 'REVISAR') {
  if (!columna_origen %in% names(tabla)) stop('No existe: ', columna_origen)
  if (length(condicion) != nrow(tabla)) stop('Condición con longitud incorrecta')
  idx <- which(!is.na(condicion) & condicion)
  valores <- as.character(tabla[[columna_origen]][idx])
  tibble::tibble(
    fila_csv = tabla$fila_csv[idx],
    id_encuesta = tabla$id_encuesta[idx],
    id_persona = tabla$id_persona[idx],
    periodo_referencia = tabla$periodo_referencia[idx],
    regla_id = rep(regla_id, length(idx)),
    variable = rep(columna_origen, length(idx)),
    valor_observado = valores,
    severidad = rep(severidad, length(idx)),
    descripcion = rep(descripcion, length(idx))
  )
}

message('Etapa 0: configuración disponible. Trabaje en el proyecto .Rproj.')
