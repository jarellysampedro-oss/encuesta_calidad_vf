# ETAPA 5 — Indicadores, resúmenes y comparaciones (Módulo 7).
# Entregables: tablas CSV verificables y 05_tablas.rds.
source(here::here('R','00_configuracion.R'))
hito <- readRDS(here::here('resultados', '04_escenarios.rds'))
alertas <- dplyr::bind_rows(readRDS(here::here('resultados','02_alertas_variables.rds')),
                           readRDS(here::here('resultados','03_alertas_integridad.rds')))
provs <- readr::read_csv(here::here('diccionario','catalogo_provincias.csv'),
                         show_col_types = FALSE)$provincia
edus <- readr::read_csv(here::here('diccionario','catalogo_educacion.csv'),
                        show_col_types = FALSE)$nivel_educacion

# Indicadores educativos (no oficiales): definiciones y denominadores explícitos.
calcular_indicadores <- function(df) {
  n <- nrow(df)
  oblig <- c('id_persona','edad','nivel_educacion','provincia','zona','ingreso_mensual_usd')
  pres <- vapply(df[oblig], function(x) sum(!is.na(normalizar_ausencia(x))), integer(1))
  num_complet <- sum(pres); den_complet <- n * length(oblig)

  edad <- a_numero(df$edad)
  ingreso <- a_numero(df$ingreso_mensual_usd)
  fecha <- suppressWarnings(as.Date(df$fecha_entrevista, format='%Y-%m-%d'))
  raw_age <- normalizar_ausencia(df$edad)
  raw_ing <- normalizar_ausencia(df$ingreso_mensual_usd)
  raw_edu <- normalizar_ausencia(df$nivel_educacion)
  raw_zona <- normalizar_ausencia(df$zona)
  raw_prov <- normalizar_ausencia(df$provincia)
  aplic <- cbind(!is.na(raw_age), !is.na(raw_ing), !is.na(raw_edu),
                !is.na(raw_zona), !is.na(raw_prov))
  valid <- cbind(!is.na(edad) & edad >= 18 & edad <= 110,
                 !is.na(ingreso) & ingreso >= 0,
                 !is.na(raw_edu) & raw_edu %in% edus,
                 !is.na(raw_zona) & raw_zona %in% c('Urbana','Rural'),
                 !is.na(raw_prov) & raw_prov %in% provs)
  num_valid <- sum(valid & aplic, na.rm=TRUE)
  den_valid <- sum(aplic)

  id <- normalizar_ausencia(df$id_persona)
  elegible <- !is.na(id)
  clave <- paste(df$periodo_referencia, id, sep='|')
  id_unico <- !duplicated(clave) & !duplicated(clave, fromLast=TRUE)
  num_unic <- sum(id_unico & elegible); den_unic <- sum(elegible)

  cond <- normalizar_ausencia(df$condicion_actividad)
  horas <- a_numero(df$horas_trabajo_semana)
  aplica_lab <- !is.na(cond) & !is.na(horas) &
    cond %in% c('Ocupado','Desocupado','Inactivo')
  regla_lab <- (cond=='Ocupado' & horas>0) |
    (cond %in% c('Desocupado','Inactivo') & horas==0)
  aplica_fecha <- !is.na(fecha) & !is.na(normalizar_ausencia(df$periodo_referencia))
  regla_fecha <- format(fecha,'%Y-%m') == df$periodo_referencia
  num_cons <- sum(aplica_lab & regla_lab, na.rm=TRUE) +
    sum(aplica_fecha & regla_fecha, na.rm=TRUE)
  den_cons <- sum(aplica_lab) + sum(aplica_fecha)

  numeradores <- c(num_complet,num_valid,num_unic,num_cons)
  denominadores <- c(den_complet,den_valid,den_unic,den_cons)
  tibble::tibble(indicador=c('Completitud','Validez','Unicidad','Consistencia'),
                 numerador=numeradores,denominador=denominadores,
                 porcentaje=ifelse(denominadores>0,
                                  round(100*numeradores/denominadores,2),NA_real_))
}

indicadores <- bind_rows(
  calcular_indicadores(hito$original) %>% mutate(escenario='Original'),
  calcular_indicadores(hito$propuesta) %>% mutate(escenario='Propuesta')
)
# Split por período; aplicar exactamente las mismas reglas y denominadores.
partes_originales <- lapply(split(hito$original, hito$original$periodo_referencia),
  function(d) calcular_indicadores(d) %>%
    mutate(periodo=unique(d$periodo_referencia), escenario='Original'))
partes_propuestas <- lapply(split(hito$propuesta, hito$propuesta$periodo_referencia),
  function(d) calcular_indicadores(d) %>%
    mutate(periodo=unique(d$periodo_referencia), escenario='Propuesta'))
por_periodo <- dplyr::bind_rows(c(partes_originales, partes_propuestas))

edu_zona <- hito$original %>%
  count(periodo_referencia, zona, nivel_educacion, name='registros')
pobreza_zona <- hito$original %>%
  count(periodo_referencia, zona, pobreza_didactica, name='registros')

# Valores negativos y formatos ilegibles se excluyen del resumen, no se borran.
resumen_ingresos <- hito$original %>%
  group_by(periodo_referencia, zona) %>%
  summarise(registros=n(),
            datos_validos=sum(!is.na(ingreso_n) & ingreso_n>=0),
            media=if (any(!is.na(ingreso_n) & ingreso_n>=0))
              mean(ingreso_n[!is.na(ingreso_n) & ingreso_n>=0]) else NA_real_,
            mediana=if (any(!is.na(ingreso_n) & ingreso_n>=0))
              median(ingreso_n[!is.na(ingreso_n) & ingreso_n>=0]) else NA_real_,
            .groups='drop')
resumen_alertas <- alertas %>% count(regla_id,severidad,name='incidencias',sort=TRUE)

TABLAS <- list(indicadores=indicadores,indicadores_periodo=por_periodo,
               educacion_zona=edu_zona,pobreza_zona=pobreza_zona,
               resumen_ingresos=resumen_ingresos,resumen_alertas=resumen_alertas)
for (nombre in names(TABLAS)) {
  readr::write_csv(TABLAS[[nombre]],
                   here::here('resultados','tablas',paste0('05_',nombre,'.csv')))
}
saveRDS(list(tablas=TABLAS, datos=hito$original, propuesta=hito$propuesta,
             alertas=alertas, bitacora=hito$bitacora_propuesta),
        here::here('resultados','05_tablas.rds'))
message('Tablas guardadas. Revise numerador/denominador antes de graficar.')
