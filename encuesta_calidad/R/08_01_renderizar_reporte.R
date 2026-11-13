# =============================================================================
# ETAPA 8.1 | RENDERIZAR EL REPORTE QUARTO DESDE UN SCRIPT DE R
# Taller integrador: encuesta socioeconómica sintética 2026
# =============================================================================
# OBJETIVO
# Transformar el documento reportes/reporte_calidad.qmd en HTML y/o Word.
# Todos los datos de entrada proceden del RDS generado en la ETAPA 7.
# Este script NO ejecuta ni modifica los CSV, NI aplica imputaciones.
#
# SECUENCIA ANTES DE EJECUTAR ESTE SCRIPT:
#   1) Abra encuesta_calidad.Rproj en RStudio.
#   2) Complete y compruebe R/00_configuracion.R ... R/07_construir_rds_reporte.R.
#   3) Verifique que exista resultados/07_paquete_reporte.rds.
#   4) Instale Quarto CLI desde https://quarto.org/ (una sola vez).
#   5) Instale paquetes si faltan (una sola vez):
#      install.packages(c("quarto", "here", "knitr", "dplyr", "rmarkdown"))
#   6) Ejecute el presente archivo POR BLOQUES o con Source.
#
# PARA PERSONALIZAR:
#   formatos <- c("html")           # Solo reporte web HTML
#   formatos <- c("html", "docx")   # HTML y documento Word
#   renderizar_sitio <- TRUE         # También construye web/_site/
# =============================================================================

# PASO 1. CONFIGURAR LOS FORMATOS A GENERAR.
# Es un vector de textos: cada elemento es un formato admitido por el .qmd.
formatos <- c("html", "docx")
renderizar_sitio <- FALSE

# PASO 2. COMPROBAR LAS DEPENDENCIAS.
# requireNamespace() permite verificar paquetes SIN cargarlos en el entorno.
paquetes_requeridos <- c("here", "quarto", "knitr", "dplyr", "rmarkdown")
paquetes_faltantes <- paquetes_requeridos[
  !vapply(paquetes_requeridos, requireNamespace, logical(1), quietly = TRUE)
]

if (length(paquetes_faltantes) > 0L) {
  stop(
    "Faltan paquetes de R: ", paste(paquetes_faltantes, collapse = ", "),
    ". Instale primero: install.packages(c(\"",
    paste(paquetes_faltantes, collapse = "\", \""), "\"))",
    call. = FALSE
  )
}

# quarto_path() localiza el programa Quarto CLI: no basta tener el paquete R.
ruta_quarto <- quarto::quarto_path()
if (is.null(ruta_quarto) || !nzchar(ruta_quarto)) {
  stop(
    "No se encontró Quarto CLI. Instálelo desde https://quarto.org/ ",
    "y reinicie RStudio antes de ejecutar este archivo.",
    call. = FALSE
  )
}
message("Quarto CLI encontrado: ", ruta_quarto)

# PASO 3. DEFINIR RUTAS DEL PROYECTO, NO RUTAS ABSOLUTAS DEL EQUIPO.
# here::here() parte de la carpeta raíz identificada por .Rproj.
ruta_qmd <- here::here("reportes", "reporte_calidad.qmd")
ruta_rds <- here::here("resultados", "07_paquete_reporte.rds")
ruta_sitio <- here::here("web")

# file.exists() comprueba que exista el archivo antes de intentar utilizarlo.
if (!file.exists(ruta_qmd)) {
  stop("No existe la plantilla Quarto: ", ruta_qmd, call. = FALSE)
}
if (!file.exists(ruta_rds)) {
  stop(
    "Falta el archivo RDS: ", ruta_rds, "\n",
    "Primero ejecute R/07_construir_rds_reporte.R.", call. = FALSE
  )
}

# PASO 4. VALIDAR LA ESTRUCTURA DEL RDS.
# readRDS() recupera un objeto de R sin depender del panel Environment.
paquete <- tryCatch(
  readRDS(ruta_rds),
  error = function(e) stop("No se pudo leer el RDS: ", conditionMessage(e),
                            call. = FALSE)
)

componentes <- c("metadatos", "tablas", "alertas", "bitacora", "figuras")
if (!is.list(paquete) || !all(componentes %in% names(paquete))) {
  stop("El RDS no contiene los componentes esperados: ",
       paste(componentes, collapse = ", "), call. = FALSE)
}

# Las tablas siguientes son referenciadas en el documento .qmd.
tablas_requeridas <- c(
  "indicadores", "indicadores_periodo", "resumen_ingresos", "resumen_alertas"
)
if (!is.list(paquete$tablas) ||
    !all(tablas_requeridas %in% names(paquete$tablas))) {
  stop("Faltan tablas para el reporte: ",
       paste(setdiff(tablas_requeridas, names(paquete$tablas)), collapse = ", "),
       call. = FALSE)
}

# PASO 5. VERIFICAR LAS FIGURAS QUE EL .QMD INSERTARÁ.
# Son archivos PNG creados durante la ETAPA 6; no se regeneran aquí.
nombres_figuras <- c(
  "06_indicadores.png", "06_educacion.png", "06_ingresos.png",
  "06_pobreza.png", "06_alertas.png"
)
rutas_figuras <- here::here("resultados", "figuras", nombres_figuras)
figuras_faltantes <- rutas_figuras[!file.exists(rutas_figuras)]
if (length(figuras_faltantes) > 0L) {
  stop(
    "No se encontraron las figuras:\n- ",
    paste(figuras_faltantes, collapse = "\n- "), "\n",
    "Primero ejecute R/06_graficos_seguimiento.R.",
    call. = FALSE
  )
}
message("Comprobación correcta: RDS y figuras disponibles.")

# PASO 6. RENDERIZAR EL REPORTE.
# quarto_render() invoca Quarto desde R. Para cada formato vuelve a ejecutar
# los bloques {r} del archivo .qmd y genera un documento nuevo.
# as_job = FALSE permite comprobar el resultado en la misma consola.
formatos_admitidos <- c("html", "docx")
if (!length(formatos) || !all(formatos %in% formatos_admitidos)) {
  stop("Seleccione únicamente html y/o docx en la variable 'formatos'.",
       call. = FALSE)
}

for (formato in unique(formatos)) {
  message("\n========== GENERANDO ", toupper(formato), " ==========")
  tryCatch(
    quarto::quarto_render(
      input = ruta_qmd,
      output_format = formato,
      execute = TRUE,
      quiet = FALSE,
      as_job = FALSE
    ),
    error = function(e) {
      stop("Falló el renderizado ", formato, ": ", conditionMessage(e),
           call. = FALSE)
    }
  )

  # Los documentos se generan junto al .qmd (en la carpeta reportes/).
  ruta_resultado <- file.path(dirname(ruta_qmd),
                              paste0("reporte_calidad.", formato))
  if (!file.exists(ruta_resultado)) {
    stop("Quarto terminó sin generar el archivo esperado: ", ruta_resultado,
         call. = FALSE)
  }
  message("ARCHIVO GENERADO: ", ruta_resultado)
}

# PASO 7 (OPCIONAL). GENERAR TAMBIÉN EL SITIO WEB QUARTO.
# Active renderizar_sitio <- TRUE al principio del archivo.
# No equivale a publicar en Internet: solo crea los archivos web locales.
if (isTRUE(renderizar_sitio)) {
  archivo_config <- file.path(ruta_sitio, "_quarto.yml")
  if (!file.exists(archivo_config)) {
    stop("No existe la configuración del sitio: ", archivo_config,
         call. = FALSE)
  }
  message("\n========== GENERANDO SITIO WEB ==========")
  tryCatch(
    quarto::quarto_render(
      input = ruta_sitio,
      output_format = "html",
      execute = TRUE,
      quiet = FALSE,
      as_job = FALSE
    ),
    error = function(e) {
      stop("Falló el sitio web: ", conditionMessage(e), call. = FALSE)
    }
  )
  ruta_index <- file.path(ruta_sitio, "_site", "index.html")
  if (!file.exists(ruta_index)) {
    stop("No se encontró la página principal: ", ruta_index,
         call. = FALSE)
  }
  message("SITIO WEB GENERADO: ", ruta_index)
}

# PASO 8. CONFIRMACIÓN FINAL.
message("\nProceso completado. Revise los archivos dentro de 'reportes/'.")
message("Nota: las cifras proceden del RDS sintético; no son datos oficiales.")

# PREGUNTAS PARA EL ESTUDIANTE:
# 1) ¿Por qué el informe puede generarse sin tener objetos en Environment?
# 2) ¿Qué ocurre si falta resultados/07_paquete_reporte.rds?
# 3) ¿Qué diferencia existe entre el HTML del informe y web/_site/index.html?
# 4) ¿Por qué se debe comprobar la existencia de cada figura antes del render?
