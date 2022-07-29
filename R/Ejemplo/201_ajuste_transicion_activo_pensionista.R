message( paste( rep('-', 100 ), collapse = '' ) )

#Cargando datos-------------------------------------------------------------------------------------
message( '\tCargando la probabilidad observada de jubilación PA y PT, dado que se es un afiliado' )
load( paste0( parametros$RData_seg, 'IESS_RTR_transicion_afiliados_pensionistas.RData' ) )

#Ajuste de la tasa de transición de afiliado a pensionista rtr, por edad----------------------------
message( '\tAjustando la tasa de jubilación PA y PT de rtr' )

aux<-tasa_jubilacion_rtr_edad %>%
     filter(!(edad %in% c('97')),!is.na(tasa_jubilacion))  

mod<-smooth.spline(aux$edad,
                   log(aux$tasa_jubilacion),df=4) 

pred<-data.frame(edad=c(15:105),
                 log_tasa_jubilacion_int=predict(mod, 
                                             c(15:105), deriv = 0)[["y"]])
tasa_jubilacion_rtr_int<-left_join(pred,tasa_jubilacion_rtr_edad,
                                        by='edad') %>%
                          mutate(log_tasa_jubilacion_int=if_else(log_tasa_jubilacion_int>0,0,
                                                                 log_tasa_jubilacion_int),
                                 log_ui_rtr = log(tasa_jubilacion)) %>% 
                          select(x:=edad,
                                 ui_rtr:=tasa_jubilacion,
                                 log_ui_rtr,
                                 log_ui_rtr_int:=log_tasa_jubilacion_int)%>%as.data.table()

#Gráfica del ajuste de la mortalidad----------------------------------------------------------------

# plot(tasa_jubilacion_rtr_int$x,
#      tasa_jubilacion_rtr_int$log_ui_rtr,
#      col="grey",xlab="edad",ylab="log tasa jubilación")
# lines(tasa_jubilacion_rtr_int$x,
#       tasa_jubilacion_rtr_int$log_ui_rtr_int)

#Guarda la tasa de uso interpolada en un Rdata------------------------------------------
message( '\tGuardando tasa de mortalidad interpolada de los pensionistas de RTR' )

save( tasa_jubilacion_rtr_int,
      file = paste0( parametros$RData_seg, 'IESS_RTR_transicion_activo_jubilado_int.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()