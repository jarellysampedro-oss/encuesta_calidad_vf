# ETAPA 1 — Importación conservadora y perfilado inicial (Módulos 1–3).
# Entregables: 01_diagnostico_variables.csv y 01_datos.rds.
source(here::here('R', '00_configuracion.R'))

personas_raw <- leer_texto(here::here('data_raw', 'encuesta_socioeconomica_2026.csv'))
hogares_raw <- leer_texto(here::here('data_raw', 'hogares_sinteticos_2026.csv'))

# Se añade el número de la línea física del CSV: encabezado = línea 1.
personas <- personas_raw %>%
  mutate(fila_csv = dplyr::row_number() + 1L,
         edad_n = a_numero(edad),
         ingreso_n = a_numero(ingreso_mensual_usd),
         horas_n = a_numero(horas_trabajo_semana),
         miembros_n = a_numero(miembros_hogar),
         ingreso_hogar_n = a_numero(ingreso_hogar_mensual_usd),
         fecha_d = suppressWarnings(as.Date(fecha_entrevista, format = '%Y-%m-%d')))

# count() responde cuántos registros hay en cada categoría.
print(personas %>% count(periodo_referencia, zona, name = 'registros'))
print(personas %>% count(nivel_educacion, sort = TRUE))

# vapply() aplica la misma cuenta a cada columna y conserva un vector numérico.
diagnostico <- tibble::tibble(
  variable = names(personas_raw),
  n_ausentes = vapply(personas_raw, function(x) sum(is.na(normalizar_ausencia(x))), integer(1)),
  n_distintos = vapply(personas_raw, function(x) dplyr::n_distinct(normalizar_ausencia(x), na.rm = TRUE), integer(1))
) %>% mutate(pct_ausentes = round(100 * n_ausentes / nrow(personas_raw), 2))

readr::write_csv(diagnostico, here::here('resultados', 'tablas', '01_diagnostico_variables.csv'))
saveRDS(list(personas = personas, hogares_raw = hogares_raw, personas_raw = personas_raw),
        here::here('resultados', '01_datos.rds'))
message('ETAPA 1: inspeccione resultados/tablas/01_diagnostico_variables.csv')
