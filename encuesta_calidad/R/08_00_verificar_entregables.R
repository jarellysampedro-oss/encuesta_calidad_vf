# ETAPA 8 — Auditoría técnica final ANTES de publicar (Módulos 8 y 9).
source(here::here('R','00_configuracion.R'))
requeridos <- c(
  'resultados/01_datos.rds',
  'resultados/tablas/01_diagnostico_variables.csv',
  'resultados/tablas/02_alertas_variables.csv',
  'resultados/tablas/03_alertas_integridad.csv',
  'resultados/tablas/04_bitacora_propuesta.csv',
  'resultados/tablas/05_indicadores.csv',
  'resultados/tablas/05_indicadores_periodo.csv',
  'resultados/figuras/06_indicadores.png',
  'resultados/figuras/06_educacion.png',
  'resultados/figuras/06_ingresos.png',
  'resultados/figuras/06_alertas.png',
  'resultados/figuras/06_pobreza.png',
  'resultados/07_paquete_reporte.rds')
check <- tibble::tibble(archivo=requeridos,
                        existe=file.exists(here::here(requeridos)))
print(check)
if (!all(check$existe)) stop('Faltan entregables. Revise los pasos anteriores.')
r <- readRDS(here::here('resultados','07_paquete_reporte.rds'))
print(r$tablas$indicadores)
message('Revisión de insumos correcta. Ahora realice render de Quarto.')
