message( paste( rep('-', 100 ), collapse = '' ) )

#Carga ingresos de nuevos beneficiarios montepío (huerfanos) de RTR---------------------------------
message( '\tCargando ingresos de nuevos beneficiarios montepío (huerfanos) de RTR' )
load( paste0( parametros$RData_seg, 'IESS_beneficiarios_causante_montepio_rtr.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_cotizantes_historicos.RData' ) )

#Calculando accidentes laborales entre 2012 y 2018--------------------------------------------------
fecha<-as.Date("31/12/2013","%d/%m/%Y")
fecha_corte<-as.Date("31/12/2018","%d/%m/%Y")
anio_corte<-year(fecha)

Total<-NROW(subset(accidentes_laborales,!duplicated(numero_expediente) &
                     fecha_derecho > fecha &
                     fecha_derecho <= as.Date("31/12/2018","%d/%m/%Y")))

Mujeres<-NROW(subset(accidentes_laborales,!duplicated(numero_expediente) &
                     sexo=='F' &
                     fecha_derecho > fecha &
                     fecha_derecho <= as.Date("31/12/2018","%d/%m/%Y")))
Hombres<-Total-Mujeres

#Accidentes laborales fatales por año---------------------------------------------------------------
FA_anio<-accidentes_laborales %>%
          as_tibble() %>%
          filter(fecha_derecho > fecha &
                   fecha_derecho <= fecha_corte) %>%
          mutate(fecha_nacimiento=if_else(is.na(fecha_nacimiento),
                                          as.Date("19/01/1994","%d/%m/%Y"), #fechas de nac perdidas se remplazan por media
                                          fecha_nacimiento),
                 anio=year(fecha_derecho),
                 sexo=ifelse(is.na(sexo),'F',sexo)) %>%
          distinct(numero_expediente, .keep_all = TRUE) %>%
          
          filter(fecha_derecho > fecha_nacimiento,
                 #nchar(causante)=='10',
                 !is.na(fecha_defuncion)) %>%
          mutate(n=n()) %>%
          group_by(anio) %>%
          mutate(FA=n(),
                 fdp_FA=FA/n,
                 FA_corregida=Total*fdp_FA) %>%
          ungroup() %>%
          distinct(anio, .keep_all = TRUE) %>%
          select(anio,FA,fdp_FA,FA_corregida) %>%
          arrange(anio)

#Incidencia de accidentes laborales fatales por edad y sexo-----------------------------------------
FA_edad_sexo <- accidentes_laborales %>%
                as_tibble() %>%
                filter(fecha_derecho >fecha &
                         fecha_derecho <= fecha_corte ) %>%
                mutate(fecha_nacimiento=if_else(is.na(fecha_nacimiento),
                                                as.Date("19/01/1994","%d/%m/%Y"), #fechas de nac perdidas se remplazan por media
                                                fecha_nacimiento),
                       anio=year(fecha_derecho),
                       sexo=ifelse(is.na(sexo),'F',sexo)) %>%
                distinct(causante, .keep_all = TRUE) %>%
                
                filter(fecha_derecho > fecha_nacimiento,
                       nchar(causante)=='10',
                       !is.na(fecha_defuncion)) %>%
                group_by(sexo) %>%
                mutate(n=n()) %>%
                ungroup() %>%
                mutate(edad=round(age_calc(fecha_nacimiento, 
                                           enddate = fecha_defuncion, 
                                           units = "years", 
                                           precise = TRUE))) %>%
                
                group_by(edad,sexo) %>%
                mutate(FA=n(),
                       fdp_FA=FA/n,
                       FA_corregida=if_else(sexo=='F',Mujeres*fdp_FA,Hombres*fdp_FA)) %>%
                ungroup() %>%
                distinct(edad,sexo ,.keep_all = TRUE) %>%
                select(edad,sexo,FA,fdp_FA,FA_corregida) %>%
                arrange(edad)

#Incidencia de accidentes laborales fatales por edad------------------------------------------------
FA_edad <-accidentes_laborales %>%
          as_tibble() %>%
          filter(fecha_derecho > fecha &
                   fecha_derecho <= fecha_corte ) %>%
          mutate(fecha_nacimiento=if_else(is.na(fecha_nacimiento),
                                          as.Date("19/01/1994","%d/%m/%Y"), #fechas de nac perdidas se remplazan por media
                                          fecha_nacimiento),
                 anio=year(fecha_derecho),
                 sexo=ifelse(is.na(sexo),'F',sexo)) %>%
          distinct(causante, .keep_all = TRUE) %>%
          
          filter(fecha_derecho > fecha_nacimiento,
                 nchar(causante)=='10',
                 !is.na(fecha_defuncion)) %>%
          mutate(n=n()) %>%
          mutate(edad=round(age_calc(fecha_nacimiento, 
                                     enddate = fecha_defuncion, 
                                     units = "years", 
                                     precise = TRUE))) %>%
          
          group_by(edad) %>%
          mutate(FA=n(),
                 fdp_FA=FA/n,
                 FA_corregida=Total*fdp_FA) %>%
          ungroup() %>%
          distinct(edad, .keep_all = TRUE) %>%
          select(edad,FA,fdp_FA,FA_corregida) %>%
          arrange(edad)

cotizantes_sgo <- cotizantes_sgo %>% 
                  filter(anio>=anio_corte) %>%
                  group_by(edad) %>%
                  mutate(cotizantes_sgo=sum(cotizantes_sgo,na.rm = TRUE)) %>%
                  ungroup() %>%
                  distinct(edad, .keep_all = TRUE) %>%
                  select(edad,cotizantes_sgo)
  

incidencia_FA <- FA_edad %>% left_join(.,cotizantes_sgo,by='edad') %>%
                 mutate(tasa_inc_FA=FA_corregida/cotizantes_sgo,
                        log_tasa_inc_FA=log(FA_corregida/cotizantes_sgo)) %>%
                 filter(edad>18)

#Suavizamiento de la incidencia accidentes fatales -------------------------------------------------
message( '\tInterpolando la incidencia accidentes fatales' )
aux<-incidencia_FA %>% filter(!(edad %in% c('77','78','80','64','70','71','73','58')))

mod<-smooth.spline(aux$edad,
                   log(aux$tasa_inc_FA),df= 4) 

pred<-data.frame(edad=c(15:105),
                 log_tasa_inc_FA_int=predict(mod,c(15:105), deriv = 0)[["y"]])
incidencia_FA<-left_join(pred,incidencia_FA,
                               by='edad')
# #Gráfica del ajuste ------------------------------------------------------------------------------
plot(incidencia_FA$edad,
     incidencia_FA$log_tasa_inc_FA,
     col="grey",xlab="edad")
lines(incidencia_FA$edad,
      incidencia_FA$log_tasa_inc_FA_int)

#Guardar la fdp de los ingresos de viudas en un Rdata-----------------------------------------------
message( '\tGuardar la tasa de incidencia de accidentes laborales fatales en un Rdata' )
incidencia_FA<-as.data.table(incidencia_FA)
save( incidencia_FA, fecha, anio_corte,
      file = paste0( parametros$RData_seg, 'IESS_RTR_siniestralidad_accidentes_laborales_fatales.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()