message( paste( rep('-', 100 ), collapse = '' ) )

#Cargando datos-------------------------------------------------------------------------------------
message( '\tCargando datos siniestralidad de subsidios de rtr' )
load( paste0( parametros$RData_seg, 'IESS_RTR_duracion_subsidios_sexo_edad_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_cotizantes_historicos.RData' ) )

#Calculando número de subsidios por edad y sexo, exposición desde 2012------------------------------
message( '\tCalculando la tasa de siniestralidad de subsidios de rtr' )
set_anios <- c('2012','2013','2014','2015','2016','2017','2018')

siniestros_subsidios<-subsidios_rtr %>%
                      filter(sr_anio %in% set_anios) %>%
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

siniestros_subsidios <- left_join(siniestros_subsidios,
                                  cotizantes_sgo, 
                                  by=c('edad_siniestro'='edad','sexo'='genero')) %>%
                        mutate(tasa_siniestro=siniestros/cotizantes_sgo)
#Corrección de los subsidios por pesos al año 2018--------------------------------------------------
subsidios_2018 <- NROW(subsidios_rtr %>% filter(sr_anio=='2018')) 

l_11<-siniestros_subsidios$tasa_siniestro %*% siniestros_subsidios$cotizantes_sgo_18
l_11<-as.numeric(l_11)
siniestros_subsidios <-siniestros_subsidios %>%
                       mutate(tasa_siniestro=1.011583*tasa_siniestro*subsidios_2018/l_11)


#Interpolación de la tasa de siniestralidad de subsidios otorgados en rtr---------------------------
message( '\tInterpolando la tasa de siniestralidad de subsidios de rtr' )
#Tasa de siniestralidad en Hombres------------------------------------------------------------------
siniestros_subsidios_edad_sexo_int_m<-siniestros_subsidios %>%
                                      filter(sexo=='M')


aux<-siniestros_subsidios %>%
     filter(sexo=='M',!(edad_siniestro %in% c('15','85','86','87')))                                

mod<-smooth.spline(aux$edad_siniestro,
                   aux$tasa_siniestro,df=10) 

pred<-data.frame(edad_siniestro=c(15:115),
                 tasa_siniestro_int=predict(mod, 
                                                  c(15:115), deriv = 0)[["y"]])
siniestros_subsidios_edad_sexo_int_m<-left_join(pred,
                                                siniestros_subsidios_edad_sexo_int_m,
                                                by='edad_siniestro') %>%
                                      mutate(sexo:='M',
                                             tasa_siniestro_int= if_else(tasa_siniestro_int<0,
                                                                         0,tasa_siniestro_int)) %>%
                                      select(x:=edad_siniestro,
                                             sexo,
                                             tasa_sin_sub:=tasa_siniestro,
                                             tasa_sin_sub_int:=tasa_siniestro_int)

#Gráfico del ajuste para hombres------------------------------------------------------------------
plot(siniestros_subsidios_edad_sexo_int_m$x,
     siniestros_subsidios_edad_sexo_int_m$tasa_sin_sub,
     col="grey",xlab="edad",ylab="tasa")
lines(siniestros_subsidios_edad_sexo_int_m$x,
      siniestros_subsidios_edad_sexo_int_m$tasa_sin_sub_int)


#Tasa de siniestralidad en Mujeres------------------------------------------------------------------
siniestros_subsidios_edad_sexo_int_f<-siniestros_subsidios %>%
                                      filter(sexo=='F')


aux<-siniestros_subsidios %>%
     filter(sexo=='F',!(edad_siniestro %in% c('21','23')))                                

mod<-smooth.spline(aux$edad_siniestro,
                   aux$tasa_siniestro,df=6) 

pred<-data.frame(edad_siniestro=c(15:115),
                 tasa_siniestro_int=predict(mod, 
                                            c(15:115), deriv = 0)[["y"]])
siniestros_subsidios_edad_sexo_int_f<-left_join(pred,
                                                siniestros_subsidios_edad_sexo_int_f,
                                                by='edad_siniestro') %>%
                                      mutate(sexo:='F',
                                             tasa_siniestro_int= if_else(tasa_siniestro_int<0,
                                             0,tasa_siniestro_int)) %>%
                                      select(x:=edad_siniestro,
                                             sexo,
                                             tasa_sin_sub:=tasa_siniestro,
                                             tasa_sin_sub_int:=tasa_siniestro_int)

#Gráfico del ajuste para hombres------------------------------------------------------------------
plot(siniestros_subsidios_edad_sexo_int_f$x,
     siniestros_subsidios_edad_sexo_int_f$tasa_sin_sub,
     col="grey",xlab="edad",ylab="tasa")
lines(siniestros_subsidios_edad_sexo_int_f$x,
      siniestros_subsidios_edad_sexo_int_f$tasa_sin_sub_int)

#Guarda la tasa de uso interpolada en un Rdata------------------------------------------
message( '\tGuardando tasa de mortalidad interpolada de los pensionistas de RTR' )

siniestralidad_subsidios_edad_sexo_int<-rbind(siniestros_subsidios_edad_sexo_int_f,
                                          siniestros_subsidios_edad_sexo_int_m)
siniestralidad_subsidios_edad_sexo_int <- as.data.table(siniestralidad_subsidios_edad_sexo_int)
save( siniestralidad_subsidios_edad_sexo_int,
      file = paste0( parametros$RData_seg, 'IESS_RTR_siniestralidad_subsidios_edad_sexo_int.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()