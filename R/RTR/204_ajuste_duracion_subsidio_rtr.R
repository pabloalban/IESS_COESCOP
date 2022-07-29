message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCargando subsidios de RTR' )
load( paste0( parametros$RData_seg, 'IESS_RTR_subsidios_prestaciones.RData' ) )
load( paste0( parametros$RData, 'IESS_salarios_pensiones_iniciales_v3.RData' ) )

#Calculando la el número de semanas promedio de subsidios por edad y sexo, desde el año 2012--------
message( '\tCalculando número de semanas promedio de subsidios por edad y sexo' )
set_anios <- c('2012','2013','2014','2015','2016','2017','2018')
#Generando pagos únicos por año---------------------------------------------------------------------
subsidios_anulidades <- subsidios_rtr %>%
                        lazy_dt() %>%
                        group_by(sr_cedula) %>%
                        mutate(sr_porcentaje_cobro=mean(sr_porcentaje_cobro,na.rm = TRUE),
                               edad_siniestro=round(edad_siniestro,0))%>%
                        ungroup() %>%
                        group_by(sr_cedula,sr_anio)%>%
                        mutate(sr_valor=sum(sr_valor,na.rm = TRUE),
                                cp_dias=sum(cp_dias,na.rm = TRUE)) %>%
                        ungroup() %>%
                        distinct(sr_cedula,sr_anio, .keep_all = TRUE) %>%
                        arrange(sr_cedula,sr_anio)%>%
                        as_tibble()

subsidios_2018 <- subsidios_anulidades %>% filter(sr_anio=='2018') %>%
                  mutate(sal_sub=sr_valor*30/(sr_porcentaje_cobro*cp_dias/100)) %>%
   group_by(sexo,edad_siniestro) %>%             
     mutate(sal_sub=mean(sal_sub)) %>%
   ungroup() %>%
   distinct(sexo,edad_siniestro, .keep_all = TRUE) %>%
                  select(sexo,edad_siniestro,sal_sub)
sal_afi <- as_tibble(sal_afi) %>% select(x,sexo,sal)

relacion<- left_join(sal_afi,subsidios_2018, by=c('x'='edad_siniestro','sexo'))%>%
            mutate(relacion=sal_sub/sal)
           

#Calculando la duración promedio del subsidio, en días, por sexo y edad al siniestro----------------
duracion_subsidios_sexo_edad<-subsidios_anulidades %>%
                              lazy_dt() %>%
                              filter(sr_anio %in% set_anios,cp_dias<360)%>%
                              mutate(duracion_semanas=cp_dias/7,
                                     edad_siniestro=round(edad_siniestro,0)) %>%
                              group_by(sexo,edad_siniestro) %>%
                              mutate(pro_duracion_semanas=mean(duracion_semanas,na.rm=TRUE)) %>%
                              ungroup() %>%
                              select(sexo,edad_siniestro,pro_duracion_semanas) %>%
                              distinct(sexo,edad_siniestro, .keep_all = TRUE) %>%
                              arrange(sexo,edad_siniestro)%>%
                              mutate(pro_duracion_dias=pro_duracion_semanas*7) %>%
                              as_tibble()

plot(duracion_subsidios_sexo_edad$edad_siniestro,duracion_subsidios_sexo_edad$pro_duracion_dias)

#Interpolación de la duración de los subsidios------------------------------------------------------
message( '\tInterpolando número de semanas promedio de subsidios por edad y sexo' )
#Hombres--------------------------------------------------------------------------------------------
duracion_subsidios_sexo_edad_int_m<-duracion_subsidios_sexo_edad %>%
                                    filter(sexo=='M')

aux<-duracion_subsidios_sexo_edad %>% filter(sexo=='M',!(edad_siniestro %in% c('17')))                                

mod<-smooth.spline(aux$edad_siniestro,
                   log(aux$pro_duracion_dias),df=6) 

pred<-data.frame(edad_siniestro=c(15:115),
                 log_pro_duracion_dias_int=predict(mod, 
                                      c(15:115), deriv = 0)[["y"]])
duracion_subsidios_sexo_edad_int_m<-left_join(pred,
                                              duracion_subsidios_sexo_edad_int_m,
                                             by='edad_siniestro') %>%
                                    mutate(sexo:='M',
                                           pro_duracion_dias_int=exp(log_pro_duracion_dias_int)) %>%
                                    select(edad:=edad_siniestro,
                                           sexo,
                                           pro_duracion_dias,
                                           log_pro_duracion_dias_int,
                                           pro_duracion_dias_int)
                                    

#Gráfica del ajuste---------------------------------------------------------------------------------
plot(duracion_subsidios_sexo_edad_int_m$edad,
     log(duracion_subsidios_sexo_edad_int_m$pro_duracion_dias),
     col="grey",xlab="edad",ylab="semanas")
lines(duracion_subsidios_sexo_edad_int_m$edad,
      duracion_subsidios_sexo_edad_int_m$log_pro_duracion_dias_int)

#Mujeres--------------------------------------------------------------------------------------------
duracion_subsidios_sexo_edad_int_f<-duracion_subsidios_sexo_edad %>%
                                    filter(sexo=='F')

aux<-duracion_subsidios_sexo_edad %>% filter(sexo=='F',!(edad_siniestro %in% c('78')))                                

mod<-smooth.spline(aux$edad_siniestro,
                   log(aux$pro_duracion_dias),df=5) 

pred<-data.frame(edad_siniestro=c(15:115),
                 log_pro_duracion_dias_int=predict(mod, 
                                                   c(15:115), deriv = 0)[["y"]])
duracion_subsidios_sexo_edad_int_f<-left_join(pred,
                                              duracion_subsidios_sexo_edad_int_f,
                                              by='edad_siniestro') %>%
                                    mutate(sexo:='F',
                                           pro_duracion_dias_int=exp(log_pro_duracion_dias_int)) %>%
                                    select(edad:=edad_siniestro,
                                           sexo,
                                           pro_duracion_dias,
                                           log_pro_duracion_dias_int,
                                           pro_duracion_dias_int)


#Gráfica del ajuste---------------------------------------------------------------------------------
plot(duracion_subsidios_sexo_edad_int_f$edad,
     log(duracion_subsidios_sexo_edad_int_f$pro_duracion_dias),
     col="grey",xlab="edad",ylab="semanas")
lines(duracion_subsidios_sexo_edad_int_f$edad,
      duracion_subsidios_sexo_edad_int_f$log_pro_duracion_dias_int)

#Calculo del porcentaje promedio de incapacidad en subsidios por edad y sexo------------------------
message( '\tCalculando el porcentaje promedio de incapacidad por subsidio otorgado por edad' )
porc_subsidios_edad_sexo <- subsidios_anulidades %>%
                            lazy_dt() %>%
                            filter(!is.na(sr_fecha_generacion))%>%
                            mutate(sr_porcentaje_cobro=if_else(is.na(sr_porcentaje_cobro),75,
                                                               sr_porcentaje_cobro),
                                   edad_siniestro=round(edad_siniestro)) %>%
                            mutate(sr_porcentaje_cobro= (sr_porcentaje_cobro)/100) %>%
                            group_by(edad_siniestro,sexo)%>%
                            mutate(porc_subsidios_prom=mean(sr_porcentaje_cobro,na.rm = TRUE))%>%
                            ungroup() %>%
                            distinct(edad_siniestro,sexo, .keep_all = TRUE) %>%
                            arrange(edad_siniestro,sexo) %>%
                            select(edad_siniestro,sexo,
                                   porc_subsidios_prom)%>%
                            as_tibble()

#Interpolación del porcentaje de incapacidad en subsidios por edad----------------------------------
message( '\tInterpolando el porcentaje promedio de incapacidad en subsidios por edad' )

#Hombres
porc_subsidios_edad_sexo_int_m <- porc_subsidios_edad_sexo %>%
                                  filter( sexo == 'M')
aux<-porc_subsidios_edad_sexo_int_m %>% filter(!(edad_siniestro %in% c('77')))                                

mod<-smooth.spline(aux$edad_siniestro,
                   aux$porc_subsidios_prom,df=6) 

pred<-data.frame(edad_siniestro=c(15:115),
                 porc_subsidios_prom_int=predict(mod, 
                                                  c(15:115), deriv = 0)[["y"]])
porc_subsidios_edad_sexo_int_m<-left_join(pred,
                                          porc_subsidios_edad_sexo_int_m,
                                              by='edad_siniestro') %>%
                                    mutate(porc_subsidios_prom_int=if_else(porc_subsidios_prom_int>0.75,
                                                                           0.75,porc_subsidios_prom_int),
                                           sexo := 'M') %>%
                                    select(edad:=edad_siniestro, sexo,
                                           porc_subsidios_prom:= porc_subsidios_prom,
                                           porc_subsidios_prom_int:=porc_subsidios_prom_int)

#Gráfica del ajuste---------------------------------------------------------------------------------
plot(porc_subsidios_edad_sexo_int_m$edad,
     porc_subsidios_edad_sexo_int_m$porc_subsidios_prom,
     col="grey",xlab="Edad",ylab="% incapacidad")
lines(porc_subsidios_edad_sexo_int_m$edad,
      porc_subsidios_edad_sexo_int_m$porc_subsidios_prom_int)

#Mujeres
porc_subsidios_edad_sexo_int_f <- porc_subsidios_edad_sexo %>%
                                  filter( sexo == 'F')
aux<-porc_subsidios_edad_sexo_int_f %>% filter(!(edad_siniestro %in% c('18','76', '77')))                                

mod<-smooth.spline(aux$edad_siniestro,
                   aux$porc_subsidios_prom,df=6) 

pred<-data.frame(edad_siniestro=c(15:115),
                 porc_subsidios_prom_int=predict(mod, 
                                                 c(15:115), deriv = 0)[["y"]])
porc_subsidios_edad_sexo_int_f<-left_join(pred,
                                          porc_subsidios_edad_sexo_int_f,
                                          by='edad_siniestro') %>%
                                mutate(porc_subsidios_prom_int=if_else(porc_subsidios_prom_int>0.75,
                                                                       0.75,porc_subsidios_prom_int),
                                       porc_subsidios_prom_int=if_else(porc_subsidios_prom_int<0.66,
                                                                       0.66,porc_subsidios_prom_int),
                                       sexo := 'F') %>%
                                select(edad:=edad_siniestro, sexo,
                                       porc_subsidios_prom:= porc_subsidios_prom,
                                       porc_subsidios_prom_int:=porc_subsidios_prom_int)

#Gráfica del ajuste---------------------------------------------------------------------------------
plot(porc_subsidios_edad_sexo_int_f$edad,
     porc_subsidios_edad_sexo_int_f$porc_subsidios_prom,
     col="grey",xlab="Edad",ylab="% incapacidad")
lines(porc_subsidios_edad_sexo_int_f$edad,
      porc_subsidios_edad_sexo_int_f$porc_subsidios_prom_int)
#Creación de tabla para estadísticas----------------------------------------------------------------
subsidios_rtr <- subsidios_anulidades %>%
                 mutate(edad_siniestro=round(edad_siniestro))
porc_incap_subsidios_edad_sexo_int <- rbind(porc_subsidios_edad_sexo_int_f,
                                      porc_subsidios_edad_sexo_int_m)

#Guardar la duración promedio por edad y sexo en un Rdata-------------------------------------------
message( '\tGuardando la duración interpolada del subsidio en semanas' )

duracion_subsidios_sexo_edad_int<-rbind(duracion_subsidios_sexo_edad_int_f,
                                        duracion_subsidios_sexo_edad_int_m)
duracion_subsidios_sexo_edad_int <- as.data.table(duracion_subsidios_sexo_edad_int)
porc_incap_subsidios_edad_sexo_int <- as.data.table(porc_incap_subsidios_edad_sexo_int)
save( subsidios_rtr,
      duracion_subsidios_sexo_edad_int,
      porc_incap_subsidios_edad_sexo_int,
      file = paste0( parametros$RData_seg, 'IESS_RTR_duracion_subsidios_sexo_edad_int.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
