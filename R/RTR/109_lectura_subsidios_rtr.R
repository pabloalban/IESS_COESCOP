message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de pagos de subsidios de RTR' )


#Archivo pagos de subsidios-------------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_RTR_subsidios_prestaciones.xlsx' )


#Cargando pagos de subsidios------------------------------------------------------------------------
subsidios_rtr <- read_excel(file,
                           sheet = 1,
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           skip = 0) %>% clean_names()

#Filtros de pagos entre 2011 y 2018-----------------------------------------------------------------
subsidios_rtr <-subsidios_rtr %>% 
                select(sr_cedula,
                        genero,
                        fec_nacimiento,
                        sr_anio,
                        sr_mesper,
                        sr_fecha_generacion,
                        sr_valor,
                        sr_porcentaje_cobro,
                        cp_dias) %>%
                mutate(sexo=if_else(genero=='FEMENINO','F','M'),
                       sr_fecha_generacion=as.Date(as.character(
                                            substr(subsidios_rtr$sr_fecha_generacion, 
                                                   start = 1, stop = 10)), "%Y-%m-%d"),
                       fec_nacimiento=as.Date(as.character(
                         substr(subsidios_rtr$fec_nacimiento, 
                                start = 1, stop = 10)), "%Y-%m-%d"),
                       sr_anio=year(sr_fecha_generacion),
                       sr_mesper=month(sr_fecha_generacion),
                       edad_siniestro=(age_calc(fec_nacimiento, 
                                                     enddate = sr_fecha_generacion, 
                                                     units = "years", 
                                                     precise = TRUE))) %>%
                select(-genero) %>%
                filter(sr_anio>=2011,sr_anio<2019)

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando data.frame de subsidios' )

save( subsidios_rtr,
      file = paste0( parametros$RData_seg, 'IESS_RTR_subsidios_prestaciones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()