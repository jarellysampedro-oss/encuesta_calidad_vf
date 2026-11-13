# ETAPA 4 — Normalización controlada, escenarios de imputación (Módulo 6).
# Entregables: propuesta SIN aprobación, bitácora, 04_escenarios.rds.
source(here::here('R', '00_configuracion.R'))
personas <- readRDS(here::here('resultados', '01_datos.rds'))$personas

# Solo se canonizan etiquetas inequívocas del catálogo aprobado para el taller.
canon_prov <- c('pichincha'='Pichincha','guayas'='Guayas',
                'azuay'='Azuay','cotopaxi'='Cotopaxi','manabi'='Manabi')
canon_edu <- c('sin instruccion'='Sin instruccion','primaria'='Primaria',
               'secundaria'='Secundaria','superior'='Superior','posgrado'='Posgrado')
canon_zona <- c('urbana'='Urbana','rural'='Rural')
canonizar <- function(x, mapa) {
  key <- tolower(trimws(as.character(x)))
  propuesta <- unname(mapa[key])
  # coalesce() conserva la entrada original si no se reconoce con certeza.
  dplyr::coalesce(propuesta, as.character(x))
}

personas_propuesta <- personas %>%
  mutate(provincia = canonizar(provincia, canon_prov),
         zona = canonizar(zona, canon_zona),
         nivel_educacion = canonizar(nivel_educacion, canon_edu))

# Comparar por id_encuesta, que no cambia aun si id_persona está duplicado.
registrar_cambios <- function(origen, propuesta, columna) {
  anterior <- as.character(origen[[columna]])
  posterior <- as.character(propuesta[[columna]])
  cambiado <- !(is.na(anterior) & is.na(posterior)) &
    (is.na(anterior) != is.na(posterior) |
       (!is.na(anterior) & !is.na(posterior) & anterior != posterior))
  idx <- which(cambiado)
  tibble::tibble(
    id_encuesta = origen$id_encuesta[idx],
    periodo_referencia = origen$periodo_referencia[idx],
    variable = rep(columna, length(idx)),
    valor_original = anterior[idx],
    valor_propuesto = posterior[idx],
    regla = rep('NORMALIZACION_ETIQUETA', length(idx)),
    estado = rep('PENDIENTE DE AUTORIZACION', length(idx)),
    responsable = rep('Ejercicio didactico', length(idx)),
    fecha_propuesta = rep(as.character(Sys.Date()), length(idx))
  )
}
bitacora <- bind_rows(
  registrar_cambios(personas, personas_propuesta, 'provincia'),
  registrar_cambios(personas, personas_propuesta, 'zona'),
  registrar_cambios(personas, personas_propuesta, 'nivel_educacion')
)

# Ensayo de imputación separado: mediana por período y zona.
# NO se imputan formatos inválidos ni ingresos negativos; NO se usa para
# clasificar oficialmente pobreza o para reportar una base corregida.
personas_propuesta <- personas_propuesta %>%
  group_by(periodo_referencia, zona) %>%
  mutate(mediana_grupo = median(ingreso_n[!is.na(ingreso_n) & ingreso_n >= 0],
                                na.rm = TRUE),
         ingreso_imputado_demo = if_else(
           is.na(normalizar_ausencia(ingreso_mensual_usd)),
           mediana_grupo, ingreso_n)) %>%
  ungroup()

readr::write_csv(bitacora, here::here('resultados','tablas','04_bitacora_propuesta.csv'))
saveRDS(list(original = personas, propuesta = personas_propuesta,
             bitacora_propuesta = bitacora),
        here::here('resultados', '04_escenarios.rds'))
message('Solo PROPUESTA. Conserve el CSV y los RDS originales.')
