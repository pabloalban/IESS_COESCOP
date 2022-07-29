message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de pagos de indemnizaciones de RTR' )


#Cargando información ------------------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_RTR_indemnizaciones_prestaciones.xlsx' )


#Carga de indemnizaciones pagadas de rtr mensualmente-----------------------------------------------
indemnizaciones_rtr <- read_excel(file,
                            sheet = 1,
                            col_names = TRUE,
                            col_types = NULL,
                            na = "",
                            skip = 0) %>% clean_names()


indemnizaciones_rtr<-indemnizaciones_rtr%>%
                      select(-cod_sexo,
                             -nombres,
                             -fecha_derecho,
                             -fecha_nacimiento,
                             -fecha_acuerdo) %>%
                      mutate(fecha_nac=as.Date(as.character(indemnizaciones_rtr$fecha_nacimiento),
                                               "%Y-%m-%d"),
                             fecha_acu=as.Date(as.character(indemnizaciones_rtr$fecha_acuerdo), 
                                               "%Y-%m-%d")) %>%
                      mutate(anio=year(fecha_acu)) 
                      #%>%
                      # group_by( anio) %>%
                      # mutate(suma=sum(valor, na.rm = TRUE)) %>%
                      # distinct(anio, .keep_all = TRUE)


#Cargar Calculo de prestaciones de rtr--------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'calculo_rentas_rtr.Rdata' )
load( file )

#Cruzar para determinar el porcentaje de discapasitación--------------------------------------------
#Agrupar porcentaje de discapacidad por rangos y filtrar bases--------------------------------------
rangos_discapacidad<-c(-0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)
labels<-c("[0-10%]","(10-20%]","(20-30%]","(30-40%]","(40-50%]","(50-60%]","(60-70%]","(70-80%]")

indemnizaciones_rtr<- left_join(as_tibble(indemnizaciones_rtr),
                               calculo_rentas_rtr,
                               by=c('cedula'='asegurado')) %>%
                      mutate(porcentaje_discapacidad=coeficiente_real) %>%
                      mutate(fecha_nac=if_else(is.na(fecha_nac),
                                               as.Date("1976-03-05", "%Y-%m-%d"),
                                               fecha_nac)) %>%
                      mutate(edad_siniestro=round(age_calc(fecha_nac, 
                                                           enddate = fecha_acu, 
                                                           units = "years", precise = TRUE))) %>%
                      mutate(rangos_porc_disc=cut(porcentaje_discapacidad, 
                                                  breaks = rangos_discapacidad,
                                                  labels = labels,
                                                  #include.lowest = TRUE,
                                                  right = TRUE)) %>%
                      mutate(sexo=if_else(sexo=='FEMENINO','F','M')) %>%
                      filter(anio>=2011)

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando data.frame' )

save( indemnizaciones_rtr,
      file = paste0( parametros$RData_seg, 'IESS_RTR_indemnizaciones_prestaciones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()