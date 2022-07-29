message( paste( rep('-', 100 ), collapse = '' ) )

#Cargando datos-------------------------------------------------------------------------------------
message( '\tCargando datos mortalidad de jubilados PA, PT y PP de rtr' )
load( paste0( parametros$RData_seg, 'IESS_RTR_mortalidad_pensionistas.RData' ) )

#Interpolación de la tasa de mortalidad de pensionistas rtr-----------------------------------------
message( '\tInterpolando la tasa de mortalidad de jubilados PA, PT y PP de rtr' )

mortalidad_jubilados_rtr_int<-mortalidad_jubilados_edad_rtr

aux<-mortalidad_jubilados_edad_rtr %>%
     filter(!(edad %in% c('91','97','26')),!is.na(tasa_mortalidad))  

mod<-smooth.spline(aux$edad,
                   log(aux$tasa_mortalidad),df=6) 

pred<-data.frame(edad=c(15:105),
                 log_tasa_mortalidad_int=predict(mod, 
                                      c(15:105), deriv = 0)[["y"]])
mortalidad_jubilados_rtr_int<-left_join(pred,mortalidad_jubilados_edad_rtr,
                                        by='edad') %>%
                              mutate(log_tasa_mortalidad_int=if_else(log_tasa_mortalidad_int>0,0,
                                                                 log_tasa_mortalidad_int),
                                     log_ud_rtr = log(tasa_mortalidad)) %>% 
                              select(x:=edad,
                                     ud_rtr:=tasa_mortalidad,
                                     log_ud_rtr,
                                     log_ud_rtr_int:=log_tasa_mortalidad_int)%>%as.data.table()

#Gráfica del ajuste de la mortalidad----------------------------------------------------------------

# plot(mortalidad_jubilados_rtr_int$x,
#      mortalidad_jubilados_rtr_int$log_ud_rtr,
#      col="grey",xlab="edad",ylab="tasa mortalidad")
# lines(mortalidad_jubilados_rtr_int$x,
#       mortalidad_jubilados_rtr_int$log_ud_rtr_int)

#Guarda la tasa de mortalidad ajustada en un Rdata--------------------------------------------------
message( '\tGuardando tasa de mortalidad interpolada de los pensionistas PA, PT y PP de RTR' )

save( mortalidad_jubilados_rtr_int,
      file = paste0( parametros$RData_seg, 'IESS_RTR_tasa_mortalidad_pensionistas_int.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()