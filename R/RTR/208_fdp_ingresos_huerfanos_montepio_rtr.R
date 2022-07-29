message( paste( rep('-', 100 ), collapse = '' ) )

#Carga ingresos de nuevos beneficiarios montepío (huerfanos) de RTR---------------------------------
message( '\tCargando ingresos de nuevos beneficiarios montepío (huerfanos) de RTR' )
load( paste0( parametros$RData_seg, 'IESS_beneficiarios_causante_montepio_rtr.RData' ) )

#Calculando nuevos ingresos de beneficiarios (huerfanos) de montepío entre 2012 y 2018, por edad----

ingresos_huerfanos <- ingresos_huerfanos %>%
                      as_tibble() %>%
                      filter(fecha_derecho > as.Date("31/12/2011","%d/%m/%Y") &
                             fecha_derecho <= as.Date("31/12/2018","%d/%m/%Y")) %>%
                      mutate(fecha_nacimiento=if_else(is.na(fecha_nacimiento),
                                                     as.Date("24/10/2004","%d/%m/%Y"), #fechas de nac perdidas se remplazan por media
                                                     fecha_nacimiento),
                             anio=year(fecha_derecho),
                             sexo=ifelse(is.na(sexo),'M',sexo)) %>%
                      filter(fecha_derecho > fecha_nacimiento) %>%
                      mutate(edad=round(age_calc(fecha_nacimiento, 
                                                 enddate = fecha_derecho, 
                                                 units = "years", 
                                                 precise = TRUE))) %>%
                      filter(edad<18) %>% #se filtran a los custodios que se guardaron en vez de los huerfanos
                      #group_by(sexo) %>%
                      mutate(n=n()) %>%
                      #ungroup() %>%
                      group_by(edad,sexo) %>%
                      mutate(huerfanos=n()) %>%
                      ungroup()


fdp_ingresos_huerfanos <- ingresos_huerfanos %>%
                          group_by(edad,sexo) %>%
                          mutate(fdp=n()/n,
                                 log_fdp=log(fdp)) %>%
                          ungroup() %>%
                          distinct(edad,sexo, .keep_all = TRUE) %>%
                          select(edad,sexo,fdp,log_fdp) %>%
                          arrange(edad,sexo)

Total <- NROW(ingresos_huerfanos %>% distinct(cedula, .keep_all = TRUE))

Mujeres <- NROW(ingresos_huerfanos %>% distinct(cedula, .keep_all = TRUE) %>% filter(sexo=='F'))

Hombres <- Total - Mujeres
# aux<-fdp_ingresos_huerfanos %>% filter(sexo=='M')
# plot(aux$edad,aux$huerfanos)

#Suavizamiento de la fdp de los ingresos de huerfanos por edad--------------------------------------
message( '\tInterpolando la fdp de los ingresos de huerfanos de rtr por edad' )
#Hombres--------------------------------------------------------------------------------------------
fdp_ingresos_huerfanos_m <- fdp_ingresos_huerfanos %>% filter(sexo=='M')
aux<-fdp_ingresos_huerfanos_m %>%
     filter(!(edad %in% c('17','1','2')))

mod<-smooth.spline(aux$edad,
                   aux$log_fdp,df=7) 

pred<-data.frame(edad=c(0:17),
                 log_fdp_int=predict(mod,c(0:17), deriv = 0)[["y"]])
#pred[1,2]<-  (-5.66737)
fdp_ingresos_huerfanos_m<-left_join(pred,fdp_ingresos_huerfanos_m,
                                    by='edad') %>%
                          mutate(fdp_int=exp(log_fdp_int)) %>%
                          mutate(fdp_int= fdp_int*(Hombres/Total)/sum(fdp_int))
# #Gráfica del ajuste ------------------------------------------------------------------------------
plot(fdp_ingresos_huerfanos_m$edad,
     fdp_ingresos_huerfanos_m$log_fdp,
     col="grey",xlab="edad",ylab="fdp")
lines(fdp_ingresos_huerfanos_m$edad,
      fdp_ingresos_huerfanos_m$log_fdp_int)

#Mujeres--------------------------------------------------------------------------------------------
fdp_ingresos_huerfanos_f <- fdp_ingresos_huerfanos %>% filter(sexo=='F')
aux<-fdp_ingresos_huerfanos_f %>%
     filter(!(edad %in% c('9','8','7')))

mod<-smooth.spline(aux$edad,
                   aux$log_fdp,df=6) 

pred<-data.frame(edad=c(0:17),
                 log_fdp_int=predict(mod,c(0:17), deriv = 0)[["y"]])
fdp_ingresos_huerfanos_f<-left_join(pred,fdp_ingresos_huerfanos_f,
                                    by='edad') %>%
                          mutate(fdp_int=exp(log_fdp_int)) %>%
                          mutate(fdp_int= fdp_int*(Mujeres/Total)/sum(fdp_int))
# #Gráfica del ajuste ------------------------------------------------------------------------------
plot(fdp_ingresos_huerfanos_f$edad,
     fdp_ingresos_huerfanos_f$log_fdp,
     col="grey",xlab="edad",ylab="fdp")
lines(fdp_ingresos_huerfanos_f$edad,
      fdp_ingresos_huerfanos_f$log_fdp_int)


#Unir data.frames-----------------------------------------------------------------------------------
fdp_ingresos_huerfanos_edad_sexo <- rbind(fdp_ingresos_huerfanos_m,fdp_ingresos_huerfanos_f) %>%
                                    select(x:=edad,
                                           sexo,
                                           log_fdp,
                                           log_fdp_int,
                                           fdp,
                                           fdp_int)

#Guardar la fdp de los ingresos de huerfanos en un Rdata--------------------------------------------
message( '\tGuardar la fdp de los ingresos de huerfanos por edad y sexo en un Rdata' )
fdp_ingresos_huerfanos_edad_sexo <- as.data.table(fdp_ingresos_huerfanos_edad_sexo)
save( fdp_ingresos_huerfanos_edad_sexo,
      file = paste0( parametros$RData_seg, 'IESS_RTR_fdp_ingresos_huerfanos_montepio.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
