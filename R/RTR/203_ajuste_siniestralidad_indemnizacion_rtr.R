message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCargando indemnizaciones de RTR' )
load( paste0( parametros$RData_seg, 'IESS_RTR_indemnizaciones_prestaciones.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_cotizantes_historicos.RData' ) )

#Calculando la tasa de siniestralidad de las indemnizaciones por edad y sexo, desde el año 2012-----
message( '\tCalculando la siniestralidad de las indemnizaciones por edad y sexo' )
set_anios <- c('2012','2013','2014','2015','2016','2017','2018')

#Calculando número de subsidios por edad y sexo, exposición desde 2012------------------------------
message( '\tCalculando la tasa de siniestralidad de indeminizaciones de rtr' )

#Número de siniestros en 2018
siniestros_2018 <-NROW(indemnizaciones_rtr %>%
                  filter(tipo_prestacion=='ID') %>%
                  filter(anio=='2018') %>%
                  distinct(cedula,anio, .keep_all = TRUE)) 

siniestros_indeminizaciones <-indemnizaciones_rtr %>%
                              filter(tipo_prestacion=='ID') %>%
                              filter(anio %in% set_anios) %>%
                              distinct(cedula,anio, .keep_all = TRUE) %>%
                              group_by(sexo,edad_siniestro) %>%
                              mutate(siniestros=n()) %>%
                              ungroup() %>%
                              distinct(sexo,edad_siniestro, .keep_all = TRUE) %>%
                              select(sexo,edad_siniestro,siniestros) %>%
                              arrange(sexo,edad_siniestro)

cotizantes_sgo_18 <- cotizantes_sgo %>%
                     filter(anio=='2018') %>%
                     group_by(edad,genero) %>%
                     mutate(cotizantes_sgo_18=sum(cotizantes_sgo,na.rm = TRUE)) %>%
                     distinct(edad,genero, .keep_all = TRUE) %>%
                     select(-anio,-cotizantes_sgo) %>%
                     arrange( genero, edad)

cotizantes_sgo <- cotizantes_sgo %>%
                  filter(anio %in% set_anios) %>%
                  group_by(edad,genero) %>%
                  mutate(cotizantes_sgo=sum(cotizantes_sgo,na.rm = TRUE)) %>%
                  distinct(edad,genero, .keep_all = TRUE) %>%
                  select(-anio) %>%
                  arrange( genero, edad)

cotizantes_sgo <- left_join(cotizantes_sgo,cotizantes_sgo_18,by=c('genero','edad'))


siniestros_indeminizaciones <-left_join(siniestros_indeminizaciones,
                                        cotizantes_sgo, 
                                        by=c('edad_siniestro'='edad','sexo'='genero')) %>%
                              mutate(tasa_siniestro=siniestros/cotizantes_sgo)

#Corrección de la tasa de siniestralidad para concidir con 2018-------------------------------------
l_8<-siniestros_indeminizaciones$tasa_siniestro %*% siniestros_indeminizaciones$cotizantes_sgo_18
l_8<-as.numeric(l_8)
siniestros_indeminizaciones <-siniestros_indeminizaciones %>%
                              mutate(tasa_siniestro=0.9996378*tasa_siniestro*siniestros_2018/l_8)
#Interpolación de la probailidad de siniestralidad de indeminizaciones de rtr-----------------------

#Tasa de siniestralidad en Hombres------------------------------------------------------------------
siniestros_indeminizaciones_int_m<-siniestros_indeminizaciones %>%
                                   filter(sexo=='M')


aux<-siniestros_indeminizaciones %>%
     filter(sexo=='M',!(edad_siniestro %in% c('88','78','74','80')))                                

mod<-smooth.spline(aux$edad_siniestro,
                   aux$tasa_siniestro,df=6) 

pred<-data.frame(edad_siniestro=c(15:115),
                 tasa_siniestro_int=predict(mod, 
                                            c(15:115), deriv = 0)[["y"]])
siniestros_indeminizaciones_int_m<-left_join(pred,
                                             siniestros_indeminizaciones_int_m,
                                             by='edad_siniestro') %>%
                                    mutate(sexo:='M',
                                           tasa_siniestro_int= if_else(tasa_siniestro_int<0,
                                                                       0,tasa_siniestro_int)) %>%
                                    select(x:=edad_siniestro,
                                           sexo,
                                           tasa_sin_indem:=tasa_siniestro,
                                           tasa_sin_indem_int:=tasa_siniestro_int)

#Gráfico del ajuste para hombres------------------------------------------------------------------
plot(siniestros_indeminizaciones_int_m$x,
     siniestros_indeminizaciones_int_m$tasa_sin_indem,
     col="grey",xlab="edad",ylab="tasa")
lines(siniestros_indeminizaciones_int_m$x,
      siniestros_indeminizaciones_int_m$tasa_sin_indem_int)

#Tasa de siniestralidad en Mujeres------------------------------------------------------------------
siniestros_indeminizaciones_int_f<- siniestros_indeminizaciones %>%
                                    filter(sexo=='F')


aux<-siniestros_indeminizaciones %>%
     filter(sexo=='F',!(edad_siniestro %in% c('70','71','72','73')))                                

mod<-smooth.spline(aux$edad_siniestro,
                   aux$tasa_siniestro,df=6) 

pred<-data.frame(edad_siniestro=c(15:115),
                 tasa_siniestro_int=predict(mod, 
                                            c(15:115), deriv = 0)[["y"]])
siniestros_indeminizaciones_int_f<-left_join(pred,
                                             siniestros_indeminizaciones_int_f,
                                             by='edad_siniestro') %>%
                                    mutate(sexo:='F',
                                           tasa_siniestro_int= if_else(tasa_siniestro_int<0,
                                                                       0,tasa_siniestro_int)) %>%
                                    select(x:=edad_siniestro,
                                           sexo,
                                           tasa_sin_indem:=tasa_siniestro,
                                           tasa_sin_indem_int:=tasa_siniestro_int)

#Gráfico del ajuste para mujeres--------------------------------------------------------------------
plot(siniestros_indeminizaciones_int_f$x,
     siniestros_indeminizaciones_int_f$tasa_sin_indem,
     col="grey",xlab="edad",ylab="tasa")
lines(siniestros_indeminizaciones_int_f$x,
      siniestros_indeminizaciones_int_f$tasa_sin_indem_int)

#Guarda la tasa de uso interpolada en un Rdata------------------------------------------------------
message( '\tGuardando tasa de siniestralidad interpolada de los indemnizaciones de RTR' )

siniestralidad_indeminizaciones_edad_sexo_int<-rbind(siniestros_indeminizaciones_int_f,
                                                     siniestros_indeminizaciones_int_m)
siniestralidad_indeminizaciones_edad_sexo_int <- as.data.table(siniestralidad_indeminizaciones_edad_sexo_int)
save( siniestralidad_indeminizaciones_edad_sexo_int,
      file = paste0( parametros$RData_seg,
                     'IESS_RTR_siniestralidad_indemnizaciones_edad_sexo_int.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()