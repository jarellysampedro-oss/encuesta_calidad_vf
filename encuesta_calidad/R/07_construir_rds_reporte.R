# ETAPA 7 — CREAR EL ENTREGABLE RDS (Módulos 7 y 8).
# RDS = archivo binario nativo de R: conserva tablas/listas con sus tipos.
# Esta es la interfaz entre análisis y reportes reproducibles.
source(here::here('R','00_configuracion.R'))
ruta_tablas <- here::here('resultados','05_tablas.rds')
if (!file.exists(ruta_tablas)) stop('Ejecute el PASO 5 para generar las tablas')
res <- readRDS(ruta_tablas)
figuras <- file.path('resultados','figuras',
                     paste0(c('06_indicadores','06_educacion','06_ingresos',
                              '06_alertas','06_pobreza'),'.png'))
if (any(!vapply(figuras, function(f) file.exists(here::here(f)), logical(1)))) {
  stop('Faltan figuras; ejecute R/06_graficos_seguimiento.R')
}

# El objeto de reporte contiene información suficiente sin depender del
# Environment de RStudio ni de los scripts abiertos.
paquete_reporte <- list(
  metadatos=list(titulo='Encuesta socioeconómica sintética 2026',
                 origen='Datos 100% ficticios — no resultados INEC',
                 fecha_generacion=as.character(Sys.Date()),
                 periodos=c('2026-10','2026-11'),
                 filas_original=nrow(res$datos),
                 umbral_pobreza_solo_didactico=185,
                 estado_correcciones='PROPUESTAS NO APROBADAS'),
  tablas=res$tablas,
  alertas=res$alertas,
  bitacora=res$bitacora,
  figuras=figuras
)
ruta_salida <- here::here('resultados','07_paquete_reporte.rds')
saveRDS(paquete_reporte,ruta_salida,compress='gzip')

# Prueba de lectura desde cero (importante para asegurar su integridad).
comprobacion <- readRDS(ruta_salida)
stopifnot(is.list(comprobacion),
          all(c('metadatos','tablas','alertas','bitacora','figuras') %in%
                names(comprobacion)),
          nrow(comprobacion$tablas$indicadores)==8)
message('RDS listo: ',ruta_salida)
