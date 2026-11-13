# ETAPA 2 — Reglas de obligatoriedad, dominio, rango y formato (Módulo 4).
# Entregable: 02_alertas_variables.csv. Una alerta NO confirma error.
source(here::here('R', '00_configuracion.R'))
hito <- readRDS(here::here('resultados', '01_datos.rds'))
personas <- hito$personas
prov_validas <- readr::read_csv(here::here('diccionario', 'catalogo_provincias.csv'),
                               show_col_types = FALSE)$provincia
edu_validas <- readr::read_csv(here::here('diccionario', 'catalogo_educacion.csv'),
                              show_col_types = FALSE)$nivel_educacion

x_edad <- normalizar_ausencia(personas$edad)
x_ingreso <- normalizar_ausencia(personas$ingreso_mensual_usd)
x_edu <- normalizar_ausencia(personas$nivel_educacion)
x_prov <- normalizar_ausencia(personas$provincia)
x_zona <- normalizar_ausencia(personas$zona)
x_id <- normalizar_ausencia(personas$id_persona)

# !is.na(x) distingue el formato inválido de un campo no respondido.
alertas_variables <- dplyr::bind_rows(
  crear_alerta(personas, is.na(x_id), 'V01', 'id_persona', 'Clave ausente'),
  crear_alerta(personas, !is.na(x_edad) & (is.na(personas$edad_n) |
                  personas$edad_n < 18 | personas$edad_n > 110),
               'V02', 'edad', 'Edad no interpretable o fuera del criterio didáctico'),
  crear_alerta(personas, is.na(x_edad), 'V03', 'edad', 'Edad sin respuesta'),
  crear_alerta(personas, !is.na(x_ingreso) & is.na(personas$ingreso_n),
               'V04', 'ingreso_mensual_usd', 'Formato de ingreso no numérico'),
  crear_alerta(personas, !is.na(personas$ingreso_n) & personas$ingreso_n < 0,
               'V05', 'ingreso_mensual_usd', 'Ingreso negativo, requiere revisión'),
  crear_alerta(personas, is.na(x_ingreso), 'V06', 'ingreso_mensual_usd', 'Ingreso no declarado'),
  crear_alerta(personas, !is.na(x_edu) & !(x_edu %in% edu_validas),
               'V07', 'nivel_educacion', 'Categoría educativa fuera de catálogo literal'),
  crear_alerta(personas, !is.na(x_prov) & !(x_prov %in% prov_validas),
               'V08', 'provincia', 'Provincia no coincide con catálogo literal'),
  crear_alerta(personas, !is.na(x_zona) & !(x_zona %in% c('Urbana', 'Rural')),
               'V09', 'zona', 'Zona fuera del dominio literal'),
  crear_alerta(personas, is.na(personas$fecha_d),
               'V10', 'fecha_entrevista', 'Fecha ISO no interpretable')
)
print(alertas_variables %>% count(regla_id, severidad, name = 'total'))
readr::write_csv(alertas_variables, here::here('resultados', 'tablas', '02_alertas_variables.csv'))
saveRDS(alertas_variables, here::here('resultados', '02_alertas_variables.rds'))
