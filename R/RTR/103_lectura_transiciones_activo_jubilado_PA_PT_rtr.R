message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo probabilidades de transiciones de afiliados a pensionistas de abso y total RTR' )
#Carga de cotizantes al SGO (exposición)------------------------------------------------------------
file<- paste0( parametros$Data_seg, 'IESS_SGO_cotizantes.txt' )
sgo_cotizantes<-read.table(file, dec = ",",
                           header = TRUE,
                           sep = "\t",na.strings = "NA",
                           stringsAsFactors = FALSE) %>% filter(anio>=2012) %>%
                group_by(edad) %>%
                mutate(cotizantes_sgo=sum(cotizantes_sgo,na.rm = TRUE)) %>%
                ungroup() %>%
                distinct(edad, .keep_all = TRUE) %>%
                arrange(edad) %>%
                select(-genero,-anio)%>%
                filter(edad>=18,edad<=105)
#Carga de beneficiarios de RTR----------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_RTR_rentas.RData' ) )
load( paste0( parametros$Data_seg, 'IESS_beneficiarios_rentas_rtr.Rdata' ) )


#Se estrae la fecha de derecho----------------------------------------------------------------------
calculo_rentas_rtr<- calculo_rentas_rtr %>%
                     filter(tipo_seguro=='RT',
                            tipo_prestacion %in% c('PA','PT')) %>%
                     select(cedula:=asegurado,fecha_derecho,tipo_seguro,tipo_prestacion) %>% 
                     distinct(cedula,tipo_seguro,tipo_prestacion, .keep_all = TRUE)                        


#Nuevos jubilados (por año) de incapacidad permanente adsoluta y total, entre 2012 y 2018-----------
nuevos_ingresos_jubilados_rtr<- base %>%
                                distinct(cedula,tipo_seguro,tipo_prestacion,.keep_all = TRUE) %>%
                                filter(tipo_prestacion %in% c('PA','PT')) %>%
                                left_join(.,calculo_rentas_rtr,by=c("cedula",
                                                                    "tipo_seguro",
                                                                    "tipo_prestacion")) %>%
                                filter(fecha_derecho>as.Date('31/12/2011',"%d/%m/%Y"),
                                       fecha_derecho<as.Date('01/01/2019',"%d/%m/%Y")) %>%
                                mutate(edad=round(age_calc(fecha_nacimiento, 
                                                      enddate = fecha_derecho, 
                                                      units = "years", 
                                                      precise = TRUE))) %>%
                                group_by(edad) %>%
                                mutate(nuevos_pensionistas=n()) %>%
                                ungroup() %>%
                                distinct(edad, .keep_all = TRUE) %>%
                                select(edad,nuevos_pensionistas) %>%
                                arrange(edad)

#Probailidad de jubilarse por incapacidad permanente adsoluta y total, dado que se está activo------
tasa_jubilacion_rtr_edad<- left_join(sgo_cotizantes,nuevos_ingresos_jubilados_rtr,
                                                   by="edad") %>%
                                  mutate(tasa_jubilacion=nuevos_pensionistas/cotizantes_sgo) %>%
                                  filter(edad>=18)
# Guardando mortalidad -----------------------------------------------------------------------------
message( '\tGuardando transiciones RT' )
save( tasa_jubilacion_rtr_edad,
      file = paste0( parametros$RData_seg, 'IESS_RTR_transicion_afiliados_pensionistas.RData' ) )

#Borrando Rdatas------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
