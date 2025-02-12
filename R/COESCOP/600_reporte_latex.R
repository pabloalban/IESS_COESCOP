# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

#Lectura de archivos--------------------------------------------------------------------------------
#source( 'R/COESCOP/106_lectura_metropolitanos.R', encoding = 'UTF-8', echo = FALSE )


#Mineria de datos-----------------------------------------------------------------------------------
#source( 'R/COESCOP/106_lectura_metropolitanos.R', encoding = 'UTF-8', echo = FALSE )
#source( 'R/COESCOP/200_mineria_datos_cargos.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos genéricos -------------------------------------------------------------------------------
source( 'R/402_graf_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicas de COESCOP ------------------------------------------------------------------
source( 'R/COESCOP/400_graf_analisis_demografico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/COESCOP/401_graf_analisis_demografico_concordia.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/COESCOP/402_graf_analisis_demografico_2.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas ---------------------------------------------------------------------------------
source( 'R/501_tab_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas de COESCOP --------------------------------------------------------------------
source( 'R/COESCOP/500_tab_analisis_demografico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/COESCOP/501_tab_resultados.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/COESCOP/502_tab_analisis_demografico_concordia.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/COESCOP/504_tab_analisis_demografico_2.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel -----------------------------------------------------------------------------------
