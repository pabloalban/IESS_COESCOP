message( paste( rep('-', 100 ), collapse = '' ) )

#Carga ingresos de nuevos beneficiarios montepío (viudas) de RTR---------------------------------
message( '\tCargando ingresos de nuevos beneficiarios montepío (viudas) de RTR' )
load( paste0( parametros$RData_seg, 'IESS_beneficiarios_causante_montepio_rtr.RData' ) )

#Calculando nuevos ingresos de beneficiarios (viudas) de montepío entre 2012 y 2018, por edad----

ingresos_viudas <-  ingresos_viudas %>%
                    as_tibble() %>%
                    filter(fecha_derecho > as.Date("31/12/2011","%d/%m/%Y") &
                             fecha_derecho <= as.Date("31/12/2018","%d/%m/%Y")) %>%
                    mutate(fecha_nacimiento=if_else(is.na(fecha_nacimiento),
                                                    as.Date("24/10/2004","%d/%m/%Y"), #fechas de nac perdidas se remplazan por media
                                                    fecha_nacimiento),
                           anio=year(fecha_derecho),
                           sexo=ifelse(is.na(sexo),'F',sexo)) %>%
                    filter(fecha_derecho > fecha_nacimiento) %>%
                    mutate(edad=round(age_calc(fecha_nacimiento, 
                                               enddate = fecha_derecho, 
                                               units = "years", 
                                               precise = TRUE))) %>%
                    filter(edad>=15) %>% 
                    #group_by(sexo) %>%
                    mutate(n=n()) %>%
                    #ungroup() %>%
                    group_by(edad,sexo) %>%
                    mutate(viudas=n()) %>%
                    ungroup()


fdp_ingresos_viudas_edad_sexo <-  ingresos_viudas %>%
                                  group_by(edad,sexo) %>%
                                  mutate(fdp=n()/n,
                                         log_fdp=log(fdp)) %>%
                                  ungroup() %>%
                                  distinct(edad,sexo, .keep_all = TRUE) %>%
                                  select(edad,sexo,fdp,log_fdp) %>%
                                  arrange(edad,sexo)

Total <- NROW(ingresos_viudas %>% distinct(cedula, .keep_all = TRUE))

Mujeres <- NROW(ingresos_viudas %>% distinct(cedula, .keep_all = TRUE) %>% filter(sexo=='F'))

Hombres <- Total - Mujeres

fdp_ingresos_viudas_edad  <-  ingresos_viudas %>%
                              group_by(edad) %>%
                              mutate(fdp=n()/n,
                                     log_fdp=log(fdp)) %>%
                              ungroup() %>%
                              distinct(edad, .keep_all = TRUE) %>%
                              select(edad,fdp,log_fdp) %>%
                              arrange(edad)
# aux<-fdp_ingresos_viudas_edad
# plot(aux$edad,aux$fdp)

#Suavizamiento de la fdp de los ingresos de viudas por edad-----------------------------------------
message( '\tInterpolando la fdp de los ingresos de viudas de rtr por edad' )

fdp_ingresos_viudas_edad_int <- fdp_ingresos_viudas_edad 
aux<-fdp_ingresos_viudas_edad %>%  filter(!(edad %in% c('29','17','16','18')))

mod<-smooth.spline(aux$edad,
                   log(aux$fdp),df=6) 

pred<-data.frame(edad=c(15:105),
                 log_fdp_int=predict(mod,c(15:105), deriv = 0)[["y"]])
fdp_ingresos_viudas_edad_int<-left_join(pred,fdp_ingresos_viudas_edad_int,
                                    by='edad') %>%
                              mutate(fdp_int=exp(log_fdp_int))%>%
                              mutate(fdp_int= fdp_int/sum(fdp_int)) %>%
                              select(x:=edad,fdp,   log_fdp,     fdp_int,log_fdp_int)
#Gráfica del ajuste --------------------------------------------------------------------------------
# plot(fdp_ingresos_viudas_edad_int$x,
#      fdp_ingresos_viudas_edad_int$log_fdp,
#      col="grey",xlab="edad",ylab="fdp")
# lines(fdp_ingresos_viudas_edad_int$x,
#       fdp_ingresos_viudas_edad_int$log_fdp_int)
#Hombres--------------------------------------------------------------------------------------------
fdp_ingresos_viudas_edad_m<-fdp_ingresos_viudas_edad_int %>%
                            mutate(fdp_int= fdp_int*(Hombres/Total),
                                   sexo:='M') %>%
                            select(x,sexo,fdp_int)

#Mujeres--------------------------------------------------------------------------------------------
fdp_ingresos_viudas_edad_f<-fdp_ingresos_viudas_edad_int %>%
                            mutate(fdp_int= fdp_int*(Mujeres/Total),
                                   sexo:='F') %>%
                            select(x,sexo,fdp_int)
#Unir en un datatable-------------------------------------------------------------------------------
fdp_ingresos_viudas_edad_sexo<-rbind(fdp_ingresos_viudas_edad_f,fdp_ingresos_viudas_edad_m) %>%
                               as.data.table()

#Guardar la fdp de los ingresos de viudas en un Rdata--------------------------------------------
message( '\tGuardar la fdp de los ingresos de viudas por edad y sexo en un Rdata' )
fdp_ingresos_viudas_edad_int <- as.data.table(fdp_ingresos_viudas_edad_int)
save( fdp_ingresos_viudas_edad_int,fdp_ingresos_viudas_edad_sexo,
      file = paste0( parametros$RData_seg, 'IESS_RTR_fdp_ingresos_viudas_montepio.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()