# ETAPA 6 — Gráficos de calidad, pobreza, educación e ingresos (Módulo 7).
# Entregable: PNG reutilizables en HTML y Word.
source(here::here('R','00_configuracion.R'))
h <- readRDS(here::here('resultados','05_tablas.rds'))
archivo_figura <- function(nombre) here::here('resultados','figuras',paste0(nombre,'.png'))

# Gráfico 1: comparar indicadores Original/Propuesta.
g_ind <- ggplot(h$tablas$indicadores,
                aes(x=indicador,y=porcentaje,fill=escenario)) +
  geom_col(position='dodge') +
  geom_text(aes(label=paste0(porcentaje,'%')),
            position=position_dodge(width=.9),vjust=-.3,size=3) +
  coord_cartesian(ylim=c(0,108)) +
  labs(title='Indicadores de calidad por escenario',x='Dimensión',y='Porcentaje') +
  theme_minimal()
ggsave(archivo_figura('06_indicadores'),g_ind,width=9,height=5,dpi=170)

# Gráfico 2: nivel educativo por zona. Los conteos NO se interpretan como tasas nacionales.
g_edu <- ggplot(h$tablas$educacion_zona,
                aes(x=nivel_educacion,y=registros,fill=zona)) +
  geom_col(position='dodge') +
  facet_wrap(~periodo_referencia) +
  labs(title='Nivel educativo por zona y período',x='Nivel educativo',y='Registros') +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=32,hjust=1))
ggsave(archivo_figura('06_educacion'),g_edu,width=11,height=5,dpi=170)

# Gráfico 3: distribución de ingresos, excluyendo NA y negativos del histograma.
# coord_cartesian hace zoom para visualizar 0–3.000 USD sin alterar observaciones.
g_ing <- h$datos %>% filter(!is.na(ingreso_n),ingreso_n>=0) %>%
  ggplot(aes(x=ingreso_n)) +
  geom_histogram(bins=45) +
  facet_wrap(~periodo_referencia) +
  coord_cartesian(xlim=c(0,3000)) +
  labs(title='Distribución de ingresos mensuales',
       subtitle='Zoom visual: 0–3.000 USD; extremos no se borran',
       x='Ingreso personal (USD)',y='Registros') +
  theme_minimal()
ggsave(archivo_figura('06_ingresos'),g_ing,width=10,height=5,dpi=170)

# Gráfico 4: incidencias por regla; una fila puede registrar varias alertas.
g_alt <- ggplot(h$tablas$resumen_alertas,
                aes(x=reorder(regla_id,incidencias),y=incidencias)) +
  geom_col() + coord_flip() +
  labs(title='Alertas detectadas por regla',x='Regla',y='Incidencias') +
  theme_minimal()
ggsave(archivo_figura('06_alertas'),g_alt,width=9,height=7,dpi=170)

# Gráfico 5: pobreza didáctica por zona, sin pretensión de inferencia nacional.
g_pob <- h$tablas$pobreza_zona %>%
  ggplot(aes(x=zona,y=registros,fill=pobreza_didactica)) +
  geom_col(position='fill') + facet_wrap(~periodo_referencia) +
  scale_y_continuous(labels=function(x) paste0(round(100*x),'%')) +
  labs(title='Composición según pobreza didáctica',
       subtitle='Umbral ficticio de USD 185 per cápita/mes; no es pobreza oficial',
       x='Zona',y='Porcentaje dentro de cada zona') +
  theme_minimal()
ggsave(archivo_figura('06_pobreza'),g_pob,width=9,height=5,dpi=170)
message('Gráficos guardados en resultados/figuras; abrir y revisar visualmente.')
