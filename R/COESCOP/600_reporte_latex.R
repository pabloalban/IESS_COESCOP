# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )


# Gráficos específicas de COESCOP ----------------------------------------------------------------------
source( 'R/COESCOP/400_graf_analisis_demografico.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas de COESCOP ------------------------------------------------------------------------
 source( 'R/COESCOP/500_tab_analisis_demografico.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel -----------------------------------------------------------------------------------
