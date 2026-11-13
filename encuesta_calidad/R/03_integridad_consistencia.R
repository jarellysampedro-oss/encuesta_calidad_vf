# ETAPA 3 — Unicidad, referencias entre personas/hogares y reglas lógicas (Módulo 5).
# Entregable: 03_alertas_integridad.csv + tabla de claves huérfanas.
source(here::here('R', '00_configuracion.R'))
hito <- readRDS(here::here('resultados', '01_datos.rds'))
personas <- hito$personas
hogares <- hito$hogares_raw

# add_count() conserva todas las filas y les agrega su frecuencia.
dup <- personas %>%
  filter(!is.na(normalizar_ausencia(id_persona))) %>%
  add_count(periodo_referencia, id_persona, name = 'repeticiones') %>%
  filter(repeticiones > 1)

# anti_join() devuelve personas cuyo hogar NO aparece en el archivo maestro.
huerfanos <- personas %>%
  filter(!is.na(normalizar_ausencia(id_hogar))) %>%
  anti_join(hogares, by = c('id_hogar', 'periodo_referencia'))

# Se seleccionan explícitamente las filas; no se borra ningún registro.
ids_dups <- dup$id_encuesta
ids_huerfanos <- huerfanos$id_encuesta

# inner_join() adjunta atributos del hogar para contrastarlos con cada persona.
cruce <- personas %>%
  inner_join(hogares %>% select(id_hogar, periodo_referencia,
                               provincia_hogar = provincia, zona_hogar = zona,
                               miembros_hogar_maestro = miembros_hogar,
                               pobreza_maestra = pobreza_didactica),
             by = c('id_hogar', 'periodo_referencia'))

# Regla condicional laboral. No se afirma que todo cero de horas es un error.
act <- normalizar_ausencia(personas$condicion_actividad)
cons_ocupado <- act == 'Ocupado' & !is.na(personas$horas_n) & personas$horas_n == 0
cons_noocupado <- act %in% c('Desocupado', 'Inactivo') &
  !is.na(personas$horas_n) & personas$horas_n > 0
cons_periodo <- !is.na(personas$fecha_d) &
  format(personas$fecha_d, '%Y-%m') != personas$periodo_referencia

alertas_integridad <- bind_rows(
  crear_alerta(personas, personas$id_encuesta %in% ids_dups,
               'I01', 'id_persona', 'ID de persona repetido en su período'),
  crear_alerta(personas, personas$id_encuesta %in% ids_huerfanos,
               'I02', 'id_hogar', 'Hogar sin correspondencia en catálogo'),
  crear_alerta(personas, is.na(normalizar_ausencia(personas$id_hogar)),
               'I03', 'id_hogar', 'ID de hogar ausente'),
  crear_alerta(personas, cons_ocupado,
               'I04', 'horas_trabajo_semana', 'Ocupado con cero horas'),
  crear_alerta(personas, cons_noocupado,
               'I05', 'horas_trabajo_semana', 'No ocupado con horas positivas'),
  crear_alerta(personas, cons_periodo,
               'I06', 'fecha_entrevista', 'Fecha fuera del período de referencia')
)

# Comparación entre campos replicados y tabla maestra; puede haber NA.
alertas_cruce <- bind_rows(
  crear_alerta(cruce, !is.na(normalizar_ausencia(cruce$provincia)) &
                trimws(tolower(cruce$provincia)) != trimws(tolower(cruce$provincia_hogar)),
               'I07', 'provincia', 'Provincia de persona difiere de hogar'),
  crear_alerta(cruce, !is.na(normalizar_ausencia(cruce$zona)) &
                trimws(tolower(cruce$zona)) != trimws(tolower(cruce$zona_hogar)),
               'I08', 'zona', 'Zona de persona difiere de hogar'),
  crear_alerta(cruce, !is.na(a_numero(cruce$miembros_hogar)) &
                a_numero(cruce$miembros_hogar) != a_numero(cruce$miembros_hogar_maestro),
               'I09', 'miembros_hogar', 'Tamaño registrado difiere de catálogo'),
  crear_alerta(cruce, normalizar_ausencia(cruce$pobreza_didactica) !=
                normalizar_ausencia(cruce$pobreza_maestra),
               'I10', 'pobreza_didactica', 'Clasificación no coincide con catálogo de hogar')
)
alertas_integridad <- bind_rows(alertas_integridad, alertas_cruce)
print(alertas_integridad %>% count(regla_id, name = 'total'))
readr::write_csv(huerfanos, here::here('resultados', 'tablas', '03_huerfanos.csv'))
readr::write_csv(alertas_integridad, here::here('resultados', 'tablas', '03_alertas_integridad.csv'))
saveRDS(alertas_integridad, here::here('resultados', '03_alertas_integridad.rds'))
