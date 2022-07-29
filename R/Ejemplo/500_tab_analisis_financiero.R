message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura tabla información finaciera ' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Activo Fondo
message( '\tLectura activo del fondo' )
aux <- as.data.table(activo_del_fondo)
aux[, Año:=as.character(Año)]
aux <- clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_activo_del_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Análisis Componentes del Activo-------------------------------------------------------------------
message( '\tTabla Análisis Componentes del Activo' )
aux <- as.data.table( analisis_componentes_activo[ , -c(2,3) ] )
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,0,0,0,0,0,0,0,0,0) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_componentes_activo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Análisis Horizontal Activo------------------------------------------------------------------------
message( '\tTabla Análisis Horizontal Activo' )
aux <- as.data.table(analisis_horizontal_activo[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2,2,2) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_activo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))
# Análisis vertical del activo----------------------------------------------------------------------
message( '\tTabla Análisis Vertical del Activo' )
aux <- as.data.table(analisis_vertical_activo[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_activo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Cuentas por Cobrar Fondo RT-----------------------------------------------------------------------
message( '\tTabla Análisis Cuentas por Cobrar' )
aux <- as.data.table(cuentas_cobrar_fondo)
aux[, Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_cobrar_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Pasivos del Fondo---------------------------------------------------------------------------------
message( '\tTabla Pasivos del Fondo' )
aux <- as.data.table(pasivos_fondo)
aux[, Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_pasivos_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Componentes del Pasivo del Fondo------------------------------------------------------------------
message( '\tTabla Componentes del Pasivo del Fondo' )
aux <- as.data.table(componentes_pasivos_fondo[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,0,0,0,0,0,0,0,0,0))
print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_pasivos_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Análisis Horizontal del Pasivo--------------------------------------------------------------------
message( '\tTabla Análisis Horizontal del Pasivo' )
aux <- as.data.table(analisis_horizontal_pasivos[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_pasivos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Análisis Vertical del Pasivo----------------------------------------------------------------------
message( '\tTabla Análisis Vertical del Pasivo' )
aux <- as.data.table(analisis_vertical_pasivos[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_pasivos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Cuentas por Pagar del Fondo-----------------------------------------------------------------------
message( '\tTabla Cuentas por Pagar del Fondo' )
aux <- as.data.table(cuentas_pagar_fondo)
aux[,Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_pagar_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Patrimonio del Fondo------------------------------------------------------------------------------
message( '\tTabla Patrimonio del Fondo' )
aux <- as.data.table(patrimonio_fondo)
aux[,Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Componentes del Patrimonio del Fondo--------------------------------------------------------------
message( '\tTabla Componentes del Patrimonio del Fondo' )
aux <- as.data.table(componentes_patrimonio_fondo[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,0,0,0,0,0,0,0,0,0))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_patrimonio_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Análisis Horizontal del Patrimonio----------------------------------------------------------------
message( '\tTabla  Análisis Horizontal del Patrimonio' )
aux <- as.data.table(analisis_horizontal_patrimonio[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_patrimonio_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Análisis Vertical del Patrimonio------------------------------------------------------------------
message( '\tTabla Análisis Vertical del Patrimonio' )
aux <- as.data.table(analisis_vertical_patrimonio[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_patrimonio_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Ingresos del Fondo--------------------------------------------------------------------------------
message( '\tTabla Ingresos del Fondo' )
aux <- as.data.table(ingresos_fondo)
aux[,Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Componentes de los Ingresos-----------------------------------------------------------------------
message( '\tTabla Componentes de los Ingresos' )
aux <- as.data.table(componentes_ingresos[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,0,0,0,0,0,0,0,0,0))
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_ingresos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Análisis Horizontal del Ingreso-------------------------------------------------------------------
message( '\tTabla Análisis Horizontal del Ingreso' )
aux <- as.data.table( analisis_horizontal_ingresos[ , -c(2,3) ] )
aux<-clean_names( aux )
aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_ingresos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Análisis Vertical del Ingreso---------------------------------------------------------------------
message( '\tTabla Análisis Vertical del Ingreso' )
aux <- as.data.table(analisis_vertical_ingresos[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 2,2,2,2,2,2,2))
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_ingresos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Ingresos por Aportes------------------------------------------------------------------------------
message( '\tTabla Ingresos por Aportes' )
aux <- as.data.table(ingresos_aportes)
aux[,Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 ,2 ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_aportes_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#TABLA 20 Otros Ingresos--------------------------------------------------
message( '\tTabla Otros Ingresos' )
aux <- as.data.table(otros_ingresos)
aux[,Anio:=as.character(Anio)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,0,0,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_otros_ingresos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#TABLA 21 Gastos--------------------------------------------------
message( '\tTabla Gastos' )
aux <- as.data.table(gastos)
aux[,Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_gastos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#tabla ingresos vs egresos--------------------------------------------------------------------------
message( '\tTabla ingresos vs egresos' )
aux <- as.data.table(ingresos_vs_gastos)
aux[,Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_gastos_vs_ingresos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#tabla ingresos por aportes vs gastos administrativos-----------------------------------------------
message( '\tTabla aportes vs gastos administrativos' )
aux <- as.data.table(ingresos_vs_gtos_adm)
aux[,Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_gastos_admi_vs_ingresos_afi_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#TABLA 22 Componentes del Gasto--------------------------------------------------
message( '\tTabla Componentes del Gasto' )
aux <- as.data.table(componentes_gastos[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,0,0,0,0,0,0,0,0,0))
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_gastos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

#TABLA 23 Análisis Horizontal del Gasto--------------------------------------------------
message( '\tTabla Análisis Horizontal del Gasto' )
aux <- as.data.table(analisis_horizontal_gastos[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2,2,2))
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_gastos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

#TABLA 24 Análisis Vertical del Gasto--------------------------------------------------
message( '\tTabla Análisis Vertical del Gasto' )
aux <- as.data.table(analisis_vertical_gastos[ , -c(2,3) ])
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2,2,2,2) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_gastos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

#TABLA 25 Gastos Prestacionales--------------------------------------------------
message( '\tTabla Gastos Prestacionales' )
aux <- as.data.table( gastos_prestacionales )
aux[,AÑO:=as.character(AÑO)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_gastos_prestacionales_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#TABLA 26 Otros Gastos Prestacionales--------------------------------------------------
message( '\tTabla Otros Gastos Prestacionales' )
aux <- as.data.table(otros_gastos_prestacionales)
aux[,AÑO:=as.character(AÑO)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_otros_gastos_prestacionales_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#TABLA 27 Otros Gastos Prestacionales--------------------------------------------------
message( '\tTabla Otros Gastos ' )
aux <- as.data.table(gast_adm)
aux[,AÑO:=as.character(AÑO)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_otros_gastos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Relación patrimonio gasto y beneficio de pensiones
message( '\tTabla relación patrimonio gasto y beneficio de pensiones' )
aux <- as.data.table( relacion_patrimonio_beneficio )
aux[, anio:=as.character(anio)]
aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 2) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_relacion_patrimonio_beneficio_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )
#-----------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

