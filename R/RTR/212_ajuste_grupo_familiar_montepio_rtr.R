message( paste( rep('-', 100 ), collapse = '' ) )

#Carga ingresos de nuevos beneficiarios montepío (viudas) de RTR------------------------------------
message( '\tCargando ingresos de nuevos beneficiarios montepío (viudas) de RTR' )
load( paste0( parametros$RData_seg, 'IESS_beneficiarios_causante_montepio_rtr.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_siniestralidad_accidentes_laborales_fatales.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_rentas.RData' ) )

#Calculando la probabilidad de que los fallecidos de accidentes laborales posean conyuges-----------
Total_viudas <- accidentes_laborales %>%
                       filter(tipo_beneficiario=='VIUDEDAD') %>%
                       filter(fecha_derecho > fecha &
                                fecha_derecho <= as.Date("31/12/2018","%d/%m/%Y")) %>%
                       distinct(beneficiario, .keep_all = TRUE)

GF_viudez<- Total_viudas %>%
            filter(nchar(causante)=='10') %>%
            mutate(fecha_nacimiento=if_else(is.na(fecha_nacimiento),
                                            as.Date("29/12/1974","%d/%m/%Y"), #fechas de nac perdidas se remplazan por media
                                            fecha_nacimiento),
                                        sexo=ifelse(is.na(sexo),'M',sexo)) %>%
            mutate(edad=round(age_calc(fecha_nacimiento, 
                                       enddate = fecha_derecho, 
                                       units = "years", 
                                       precise = TRUE))) %>%
            mutate(n=n()) %>%
            group_by(edad) %>%
            mutate(GF_viudas=n()) %>%
            mutate(GF_viudas_corregida = GF_viudas * NROW(Total_viudas)/n) %>%
            ungroup() %>%  
            distinct(edad, .keep_all = TRUE) %>%
            select(edad,GF_viudas,GF_viudas_corregida) %>%
            arrange(edad)

GF_viudez <-  left_join(as_tibble(incidencia_FA),GF_viudez,by='edad') %>%
              mutate(pGF_viudez=GF_viudas_corregida/FA_corregida) %>%
              select(edad,pGF_viudez) %>%
              mutate(pGF_viudez=ifelse(is.na(pGF_viudez),0,pGF_viudez),
                     pGF_viudez=ifelse(pGF_viudez>1,1,pGF_viudez))
              
#plot(GF_viudez$edad,GF_viudez$pGF_viudez)
#Suavizamiento de la incidencia accidentes fatales -------------------------------------------------
message( '\tInterpolando la probabilidad de que los fallecidos de accidentes laborales posean conyuges' )
aux<-GF_viudez %>% filter(!(edad %in% c('55','57','58','60','62','63','65','66','67','68')))

mod<-smooth.spline(aux$edad,
                   aux$pGF_viudez,df= 7) 

pred<-data.frame(edad=c(15:105),
                 pGF_viudez_int=predict(mod,c(15:105), deriv = 0)[["y"]])
GF_viudez<- left_join(pred,GF_viudez,
                         by='edad') %>%
            mutate(pGF_viudez_int=ifelse(pGF_viudez_int<0,0,pGF_viudez_int)) %>%
            as.data.table()
# #Gráfica del ajuste ------------------------------------------------------------------------------
plot(GF_viudez$edad,
     GF_viudez$pGF_viudez,
     col="grey",xlab="edad")
lines(GF_viudez$edad,
      GF_viudez$pGF_viudez_int)


#Calculando la prob de que los fallecidos de accidentes laborales posean al menos un hijo < 18------

#Huerfanos hasta el 31/12/2012
Huerfanos2012<- prestaciones_orfandad %>% 
                filter(anio=='2012') %>%
                distinct(cedula, .keep_all = TRUE) 
#Ingresos de huerfanos desde 2013 a 2018
Total_huerfanos <-  prestaciones_orfandad %>%
                    filter(anio>'2012') %>%
                    distinct(cedula, .keep_all = TRUE) %>%
                    anti_join(.,Huerfanos2012,by='cedula') 


causantes_con_hijos <-  accidentes_laborales %>%
                        filter(tipo_beneficiario=='ORFANDAD') %>%
                        filter(fecha_derecho > fecha &
                                 fecha_derecho <= as.Date("31/12/2018","%d/%m/%Y")) %>%
                        distinct(numero_expediente, .keep_all = TRUE)
rho<- NROW(causantes_con_hijos)/NROW(Total_huerfanos)

GF_orfandad<- accidentes_laborales %>%
              filter(tipo_beneficiario=='ORFANDAD') %>%
              filter(fecha_derecho > fecha &
                       fecha_derecho <= as.Date("31/12/2018","%d/%m/%Y")) %>%
              distinct(beneficiario, .keep_all = TRUE) %>%
              filter(nchar(causante)=='10') %>%
              mutate(fecha_nacimiento=if_else(is.na(fecha_nacimiento),
                                              as.Date("29/12/1974","%d/%m/%Y"), #fechas de nac perdidas se remplazan por media
                                              fecha_nacimiento),
                     sexo=ifelse(is.na(sexo),'M',sexo)) %>%
              mutate(edad=round(age_calc(fecha_nacimiento, 
                                         enddate = fecha_derecho, 
                                         units = "years", 
                                         precise = TRUE))) %>%
              mutate(n=n()) %>%
              group_by(edad) %>%
              mutate(GF_orfandad=n()) %>%
              mutate(GF_orfandad_corregida = GF_orfandad * NROW(Total_huerfanos)/n) %>%
              ungroup() %>%  
              distinct(edad, .keep_all = TRUE) %>%
              select(edad,GF_orfandad,GF_orfandad_corregida) %>%
              arrange(edad)

GF_orfandad <-  left_join(as_tibble(incidencia_FA),GF_orfandad,by='edad') %>%
                mutate(pGF_orfandad=GF_orfandad_corregida/FA_corregida) %>%
                select(edad,pGF_orfandad) %>%
                mutate(pGF_orfandad=ifelse(is.na(pGF_orfandad),0,pGF_orfandad))

#plot(GF_orfandad$edad,GF_orfandad$pGF_orfandad)
#Suavizamiento de la incidencia accidentes fatales -------------------------------------------------
message( '\tInterpolando la probabilidad de que los fallecidos de accidentes laborales posean conyuges' )
aux<-GF_orfandad %>% filter(!(edad %in% c('55','57','58','60','62','63','65','66','67','68')))

mod<-smooth.spline(aux$edad,
                   aux$pGF_orfandad,df= 12) 

pred<-data.frame(edad=c(15:105),
                 pGF_orfandad_int=predict(mod,c(15:105), deriv = 0)[["y"]])
GF_orfandad<- left_join(pred,GF_orfandad,
                        by='edad') %>%
              mutate(pGF_orfandad_int=ifelse(edad>65,0,pGF_orfandad_int),
                     pGF_orfandad_int=ifelse(pGF_orfandad_int<0,0,pGF_orfandad_int))

# #Gráfica del ajuste ------------------------------------------------------------------------------
plot(GF_orfandad$edad,
     GF_orfandad$pGF_orfandad,
     col="grey",xlab="edad")
lines(GF_orfandad$edad,
      GF_orfandad$pGF_orfandad_int)
mean(GF_orfandad[which(GF_orfandad$edad<60),]$pGF_orfandad_int)
#Guardar la fdp de los ingresos de viudas en un Rdata-----------------------------------------------
message( '\tGuardar la  probabilidad de que los fallecidos de accidentes laborales posean conyuges en un Rdata' )
GF_orfandad<-as.data.table(GF_orfandad)
GF_viudez <- as.data.table(GF_viudez)
save( GF_viudez, fecha, anio_corte,GF_orfandad, rho,
      file = paste0( parametros$RData_seg, 'IESS_RTR_grupo_familiar.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
