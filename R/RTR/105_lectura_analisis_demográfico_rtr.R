message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo población afiliada activa inicial SGO del IESS' )

col_nom <- c( 'anio', 'Afiliados', 'Tasa')

col_tip <- c( 'character', 'numeric', 'numeric' )

file <- paste0( parametros$Data, 'IESS_poblacion_afiliada_jubilada_inicial.xlsx' )

pob_afi_ini<-read_excel(file,sheet="Afiliados"
                        ,col_names=TRUE,guess_max = 24000)
pob_afi_ini <- as.data.table( pob_afi_ini )
setnames( pob_afi_ini, col_nom )

# Masa Salarial
message( '\tLeyendo masa salarial inicial del SGO del IESS' )

col_nom <- c( 'anio', 'Masa_Anual', 'Masa_Mensual','Crecimiento_anual','Tasa')

col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data, 'IESS_poblacion_afiliada_jubilada_inicial.xlsx' )

masa_salarial_ini<-read_excel(file,sheet="Masa_Salarial"
                              ,col_names=TRUE,guess_max = 24000)
masa_salarial_ini <- as.data.table( masa_salarial_ini )
setnames( masa_salarial_ini, col_nom )

#Población afiliada por edad y sexo al SGO
message( '\tLeyendo población afiliada y pensionistas por sexo y edad activa inicial SGO del IESS' )
col_nom <- c( 'sexo', 'edad', 'n', 'cat' )
col_tip <- c( 'character', 'numeric', 'numeric', 'character'  )
file <- paste0( parametros$Data, 'IESS_poblacion_inicial_edad_sexo.xlsx' )
pob_afi_edad_sexo_ini<-read_excel(file,sheet="datos"
                                  ,col_names=TRUE,guess_max = 24000)
pob_afi_edad_sexo_ini <- as.data.table( pob_afi_edad_sexo_ini )
setnames( pob_afi_edad_sexo_ini, col_nom )

#Masa Salarial por edad y sexo
message( '\tLeyendo masa salarial por sexo y monto del SGO del IESS' )
col_nom <- c( 'sexo', 'monto', 'n', 'cat')
col_tip <- c( 'character', 'numeric', 'numeric', 'character' )
file <- paste0( parametros$Data, 'IESS_poblacion_inicial_edad_sexo.xlsx' )

masa_sal_edad_monto_ini <-read_excel(file,sheet="D.Masa_Salarial"
                                     ,col_names=TRUE,guess_max = 24000)
masa_sal_edad_monto_ini <- as.data.table( masa_sal_edad_monto_ini )
setnames( masa_sal_edad_monto_ini, col_nom )

#--------------------Tiempo de aportación
message( '\tLeyendo tiempo de aportación de afiliados al SGO del IESS' )
col_nom <- c( 'edad', '[0,5]', '[5,10]', '[10,15]', '[15,20]', '[20,25]', '[25,30]', '[30,35]', '[35,40]', '[40,45]', '[45,50]', '>50','Total')
col_tip <- c( 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character',
              'character', 'character', 'character', 'character', 'character' )
file <- paste0( parametros$Data, 'IESS_afiliados_tiempo_aportacion.xlsx' )
afi_tiempo_aportacion <-read_excel(file,sheet="tiempo"
                                   ,col_names=TRUE,guess_max = 24000)
afi_tiempo_aportacion <- as.data.table( afi_tiempo_aportacion )
setnames( afi_tiempo_aportacion, col_nom )

#------------------- PRESTACIONES DE RT ---------------------------------------------------------
#----------Subsidios
message( '\tLeyendo subsidios de RT' )
col_nom <- c( 'anio', 'subsidio','creci_sub', 'monto',  'creci_monto')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

subsidios <-read_excel(file,sheet="subsidios"
                                     ,col_names=TRUE,guess_max = 24000)
subsidios <- as.data.table( subsidios )
setnames( subsidios, col_nom )
#----------Subsidios por edad y sexo
message( '\tLeyendo subsidios por edad y sexo de RT' )
col_nom <- c( 'personas', 'edad', 'sexo', 'anio')
col_tip <- c( 'numeric', 'numeric', 'numeric', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

subsidios_edad_sexo <-read_excel(file,sheet="subsidios_edad_sexo"
                       ,col_names=TRUE,guess_max = 24000)
subsidios_edad_sexo <- as.data.table( subsidios_edad_sexo )
setnames( subsidios_edad_sexo, col_nom )

#----------Montos Subsidios por edad y sexo
message( '\tLeyendo montos subsidios por edad y sexo de RT' )
col_nom <- c( 'personas', 'grupo', 'sexo', 'anio')
col_tip <- c( 'numeric', 'numeric', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

montos_subsidios_sexo <-read_excel(file,sheet="montos_subsidios_sexo"
                                 ,col_names=TRUE,guess_max = 24000)
montos_subsidios_sexo <- as.data.table( montos_subsidios_sexo )
setnames( montos_subsidios_sexo, col_nom )

#----------Indemnizaciones del fondo de RT
message( '\tLeyendo indemizaciones del fondo de RT' )
col_nom <- c( 'anio', 'numero', 'creci_num', 'monto', 'creci_monto')
col_tip <- c( 'numeric','numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

indemnizaciones <-read_excel(file,sheet="indemnizacion"
                                   ,col_names=TRUE,guess_max = 24000)
indemnizaciones <- as.data.table( indemnizaciones )
setnames( indemnizaciones, col_nom )

#----------Indemnizaciones por edad y sexo del fondo de RT
message( '\tLeyendo indemizaciones por edad y sexo  del fondo de RT' )
col_nom <- c( 'personas', 'edad', 'sexo', 'anio')
col_tip <- c( 'numeric','numeric', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

indemnizaciones_edad_sexo <-read_excel(file,sheet="indemnizaciones_edad_sexo"
                             ,col_names=TRUE,guess_max = 24000)
indemnizaciones_edad_sexo <- as.data.table( indemnizaciones_edad_sexo )
setnames( indemnizaciones_edad_sexo , col_nom )

#----------Monto de Indemnizaciones por sexo del fondo de RT
message( '\tLeyendo indemizaciones por edad y sexo  del fondo de RT' )
col_nom <- c( 'personas', 'grupo', 'sexo', 'anio')
col_tip <- c( 'numeric','numeric', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

monto_indemnizaciones_sexo <-read_excel(file,sheet="monto_indemnizaciones_sexo"
                                       ,col_names=TRUE,guess_max = 24000)
monto_indemnizaciones_sexo <- as.data.table( monto_indemnizaciones_sexo )
setnames( monto_indemnizaciones_sexo , col_nom )

#----------Pensiones porivisionales por incapacidad temporal del fondo de RT
message( '\tLeyendo pensiones provisionales por incapacidad temporal del fondo de RT' )
col_nom <- c( 'anio', 'numero', 'creci_num', 'monto', 'creci_monto')
col_tip <- c( 'numeric','numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

pensiones_provisionales_inc_tem <-read_excel(file,sheet="pensiones_provisionales_inc_tem"
                             ,col_names=TRUE,guess_max = 24000)
pensiones_provisionales_inc_tem <- as.data.table( pensiones_provisionales_inc_tem)
setnames( pensiones_provisionales_inc_tem, col_nom )

#----------Incapacidad permanente parcial del fondo de RT
message( '\tLeyendo incapacidad permanente parcial del fondo de RT' )
col_nom <- c( 'anio', 'numero', 'creci_num', 'monto', 'creci_monto')
col_tip <- c( 'numeric','numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

incapacidad_parcial <-read_excel(file,sheet="incapacidad_parcial"
                                             ,col_names=TRUE,guess_max = 24000)
incapacidad_parcial<- as.data.table(incapacidad_parcial )
setnames( incapacidad_parcial, col_nom )

#----------Incapacidad permanente parcial por edad y sexo  del fondo de RT
message( '\tLeyendo incapacidad permanente parcial por edad y sexo del fondo de RT' )
col_nom <- c( 'personas', 'edad', 'sexo', 'anio')
col_tip <- c( 'numeric','character', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

incapacidad_parcial_edad_sexo <-read_excel(file,sheet="incapacidad_parcial_edad_sexo"
                                        ,col_names=TRUE,guess_max = 24000)
incapacidad_parcial_edad_sexo <- as.data.table( incapacidad_parcial_edad_sexo )
setnames( incapacidad_parcial_edad_sexo , col_nom )

#----------Monto Incapacidad permanente parcial por edad y sexo  del fondo de RT
message( '\tLeyendo monto incapacidad permanente parcial por sexo del fondo de RT' )
col_nom <- c( 'personas', 'grupo', 'sexo', 'anio')
col_tip <- c( 'numeric','numeric', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

monto_incapacidad_parcial_sexo <-read_excel(file,sheet="monto_incapacidad_parcial_sexo"
                                        ,col_names=TRUE,guess_max = 24000)
monto_incapacidad_parcial_sexo <- as.data.table( monto_incapacidad_parcial_sexo )
setnames( monto_incapacidad_parcial_sexo , col_nom )

#----------Incapacidad permanente total del fondo de RT
message( '\tLeyendo incapacidad permanente total del fondo de RT' )
col_nom <- c( 'anio', 'numero', 'creci_num', 'monto', 'creci_monto')
col_tip <- c( 'numeric','numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

incapacidad_total <-read_excel(file,sheet="incapacidad_total"
                                 ,col_names=TRUE,guess_max = 24000)
incapacidad_total <- as.data.table( incapacidad_total )
setnames( incapacidad_total, col_nom )

#----------Incapacidad permanente total por edad y sexo  del fondo de RT
message( '\tLeyendo incapacidad permanente total por edad y sexo del fondo de RT' )
col_nom <- c( 'personas', 'edad', 'sexo', 'anio')
col_tip <- c( 'numeric','character', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

incapacidad_total_edad_sexo <-read_excel(file,sheet="incapacidad_total_edad_sexo"
                                           ,col_names=TRUE,guess_max = 24000)
incapacidad_total_edad_sexo <- as.data.table( incapacidad_total_edad_sexo )
setnames( incapacidad_total_edad_sexo , col_nom )

#----------Monto Incapacidad permanente total por edad y sexo  del fondo de RT
message( '\tLeyendo monto incapacidad permanente total por sexo del fondo de RT' )
col_nom <- c( 'personas', 'grupo', 'sexo', 'anio')
col_tip <- c( 'numeric','numeric', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

monto_incapacidad_total_sexo <-read_excel(file,sheet="monto_incapacidad_total_sexo"
                                            ,col_names=TRUE,guess_max = 24000)
monto_incapacidad_total_sexo <- as.data.table( monto_incapacidad_total_sexo )
setnames( monto_incapacidad_total_sexo , col_nom )

#----------Incapacidad permanente absoluta del fondo de RT
message( '\tLeyendo incapacidad permanente absoluta del fondo de RT' )
col_nom <- c( 'anio', 'numero', 'creci_num', 'monto', 'creci_monto')
col_tip <- c( 'numeric','numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

incapacidad_absoluta <-read_excel(file,sheet="incapacidad_absoluta"
                               ,col_names=TRUE,guess_max = 24000)
incapacidad_absoluta <- as.data.table( incapacidad_absoluta )
setnames( incapacidad_absoluta, col_nom )

#----------Incapacidad permanente absoluta por edad y sexo  del fondo de RT
message( '\tLeyendo incapacidad permanente absoluta por edad y sexo del fondo de RT' )
col_nom <- c( 'personas', 'edad', 'sexo', 'anio')
col_tip <- c( 'numeric','character', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

incapacidad_absoluta_edad_sexo <-read_excel(file,sheet="incapacidad_absoluta_edad_sexo"
                                         ,col_names=TRUE,guess_max = 24000)
incapacidad_absoluta_edad_sexo <- as.data.table( incapacidad_absoluta_edad_sexo )
setnames( incapacidad_absoluta_edad_sexo , col_nom )

#----------Monto Incapacidad permanente absoluta por edad y sexo  del fondo de RT
message( '\tLeyendo monto incapacidad permanente absoluta por sexo del fondo de RT' )
col_nom <- c( 'personas', 'grupo', 'sexo', 'anio')
col_tip <- c( 'numeric','numeric', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

monto_incapacidad_absoluta_sexo <-read_excel(file,sheet="monto_incapacidad_absoluta_sexo"
                                          ,col_names=TRUE,guess_max = 24000)
monto_incapacidad_absoluta_sexo <- as.data.table( monto_incapacidad_absoluta_sexo )
setnames( monto_incapacidad_absoluta_sexo , col_nom )

#----------Pensionistas de orfandad del fondo de RT
message( '\tLeyendo pensionistas de orfandad del fondo de RT' )
col_nom <- c( 'anio', 'numero', 'creci_num', 'monto', 'creci_monto')
col_tip <- c( 'numeric','numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

pensionitas_orfandad <-read_excel(file,sheet="pensionitas_orfandad"
                                  ,col_names=TRUE,guess_max = 24000)
pensionitas_orfandad <- as.data.table( pensionitas_orfandad )
setnames( pensionitas_orfandad, col_nom )

#----------Pensionistas por orfandad por edad y sexo  del fondo de RT
message( '\tLeyendo pensionistas por orfandad por edad y sexo del fondo de RT' )
col_nom <- c( 'personas', 'edad', 'sexo', 'anio')
col_tip <- c( 'numeric','character', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

pensionistas_orfandad_edad_sexo <-read_excel(file,sheet="pensionistas_orfandad_edad_sexo"
                                            ,col_names=TRUE,guess_max = 24000)
pensionistas_orfandad_edad_sexo <- as.data.table( pensionistas_orfandad_edad_sexo )
setnames( pensionistas_orfandad_edad_sexo , col_nom )

#----------Monto Incapacidad permanente absoluta por edad y sexo  del fondo de RT
message( '\tLeyendo monto pensionistas por orfandad por sexo del fondo de RT' )
col_nom <- c( 'personas', 'grupo', 'sexo', 'anio')
col_tip <- c( 'numeric','numeric', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

monto_pensionistas_orfandad_sex <-read_excel(file,sheet="monto_pensionistas_orfandad_sex"
                                             ,col_names=TRUE,guess_max = 24000)
monto_pensionistas_orfandad_sex <- as.data.table( monto_pensionistas_orfandad_sex )
setnames( monto_pensionistas_orfandad_sex , col_nom )

#----------Pensionistas de viudedad del fondo de RT
message( '\tLeyendo pensionistas de viudedad del fondo de RT' )
col_nom <- c( 'anio', 'numero', 'creci_num', 'monto', 'creci_monto')
col_tip <- c( 'numeric','numeric', 'numeric', 'numeric', 'numeric' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

pensionistas_viudedad <-read_excel(file,sheet="pensionistas_viudedad"
                                  ,col_names=TRUE,guess_max = 24000)
pensionistas_viudedad <- as.data.table( pensionistas_viudedad )
setnames( pensionistas_viudedad, col_nom )

#----------Pensionistas por viudedad por edad y sexo  del fondo de RT
message( '\tLeyendo pensionistas por viudead por edad y sexo del fondo de RT' )
col_nom <- c( 'personas', 'edad', 'sexo', 'anio')
col_tip <- c( 'numeric','character', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

pensionistas_viudedad_edad_sexo <-read_excel(file,sheet="pensionistas_viudedad_edad_sexo"
                                             ,col_names=TRUE,guess_max = 24000)
pensionistas_viudedad_edad_sexo <- as.data.table( pensionistas_viudedad_edad_sexo )
setnames( pensionistas_viudedad_edad_sexo , col_nom )

#----------Monto de pensionistas por viudedad por sexo  del fondo de RT
message( '\tLeyendo monto pensionistas por viudead por sexo del fondo de RT' )
col_nom <- c( 'personas', 'grupo', 'sexo', 'anio')
col_tip <- c( 'numeric','numeric', 'character', 'character' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_analisis_demografico.xlsx' )

monto_pensionistas_viudedad_sex <-read_excel(file,sheet="monto_pensionistas_viudedad_sex"
                                             ,col_names=TRUE,guess_max = 24000)
monto_pensionistas_viudedad_sex <- as.data.table( monto_pensionistas_viudedad_sex )
setnames( monto_pensionistas_viudedad_sex , col_nom )

lista <- c('pob_afi_ini', 'masa_salarial_ini', 'pob_afi_edad_sexo_ini', 'masa_sal_edad_monto_ini'
           , 'afi_tiempo_aportacion', 'subsidios', 'subsidios_edad_sexo', 'montos_subsidios_sexo'
           , 'indemnizaciones', 'indemnizaciones_edad_sexo', 'monto_indemnizaciones_sexo'
           , 'pensiones_provisionales_inc_tem', 'incapacidad_parcial', 'incapacidad_parcial_edad_sexo'
           , 'monto_incapacidad_parcial_sexo', 'incapacidad_total', 'incapacidad_total_edad_sexo'
           , 'monto_incapacidad_total_sexo', 'incapacidad_absoluta', 'incapacidad_absoluta_edad_sexo'
           , 'monto_incapacidad_absoluta_sexo', 'pensionitas_orfandad', 'pensionistas_orfandad_edad_sexo'
           , 'monto_pensionistas_orfandad_sex', 'pensionistas_viudedad', 'pensionistas_viudedad_edad_sexo'
           , 'monto_pensionistas_viudedad_sex')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_demografico.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()