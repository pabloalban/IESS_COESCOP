message( paste( rep('-', 100 ), collapse = '' ) )

#Lectura: Tiempo de aportacion -----------------------------------------------------------------
message( '\tLeyendo tiempo de aportación de afiliados al SGO incluido TNRH del IESS' )

file<-paste0(parametros$Data_seg, 'IESS_RTR_afiliados_tiempo_aportacion_incluye_TNRH.csv' )

tabla<-read.table(file, dec = ".",header = FALSE,sep = ";",na.strings = "NA")

message( '\tGuardando tiempo de aportación de afilidos' )
save( tabla , file = paste0( parametros$RData_seg, 'IESS_afi_tiempo_aportacion_incluye_TNRH.RData' ) )


