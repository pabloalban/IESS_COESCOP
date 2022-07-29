message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCargando indemnizaciones de RTR' )
load( paste0( parametros$RData_seg, 'IESS_RTR_indemnizaciones_prestaciones.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_indemnizaciones_prestaciones.RData' ) )
load( paste0( parametros$RData, 'IESS_salarios_pensiones_iniciales_v3.RData' ) )
#Calculando la el porcentaje de incapacidad promedio por edad y sexo, desde el año 2012-------------
message( '\tCalculando la porcentaje de incapacidad por edad y sexo' )

porc_incap_indemn_edad_sexo<- indemnizaciones_rtr %>%
                              filter(anio<='2018',tipo_prestacion=='ID') %>%
                              group_by(edad_siniestro,sexo) %>%
                              mutate(porc_incap_indemn=mean(porcentaje_discapacidad,na.rm = TRUE)) %>%
                              distinct(edad_siniestro,sexo, .keep_all = TRUE) %>%
                              ungroup() %>%
                              select(edad_siniestro,sexo,porc_incap_indemn)%>%
                              arrange(edad_siniestro,sexo)

#Suavizamiento  del porcentaje deincapacidad por sexo-----------------------------------------------
#Suavizamiento del porcentaje de incapacadidad para Hombres-----------------------------------------
porc_incap_indemn_edad_sexo_int_m<-porc_incap_indemn_edad_sexo %>%
                                   filter(sexo=='M')

aux<-porc_incap_indemn_edad_sexo %>%
     filter(sexo=='M',!(edad_siniestro %in% c('88','79','18')))                                

mod<-smooth.spline(aux$edad_siniestro,
                   log(aux$porc_incap_indemn),df=4) 

pred<-data.frame(edad_siniestro=c(15:115),
                 log_porc_incap_indemn_int=predict(mod, 
                                      c(15:115), deriv = 0)[["y"]])
porc_incap_indemn_edad_sexo_int_m<-left_join(pred,
                                             porc_incap_indemn_edad_sexo_int_m,
                                             by='edad_siniestro') %>%
                                    mutate(sexo:='M',
                                           porc_incap_indemn_int=exp(log_porc_incap_indemn_int))

#Gráfica del ajuste para hombres--------------------------------------------------------------------
plot(porc_incap_indemn_edad_sexo_int_m$edad_siniestro,
     log(porc_incap_indemn_edad_sexo_int_m$porc_incap_indemn),
     col="grey",xlab="Age",ylab="Wages")
lines(predict(mod,newdata = list(edad_siniestro=c(15:105)))$x,
predict(mod,newdata = list(edad_siniestro=c(15:105)))$y)

#Suavizamiento del porcentaje de incapacadidad para Mujeres-----------------------------------------
porc_incap_indemn_edad_sexo_int_f<-porc_incap_indemn_edad_sexo %>%
                                   filter(sexo=='F')

aux<-porc_incap_indemn_edad_sexo %>%
    filter(sexo=='F',!(edad_siniestro %in% c('70','71','73')))


mod<-smooth.spline(aux$edad_siniestro,
                   log(aux$porc_incap_indemn),df=5) 

pred<-data.frame(edad_siniestro=c(15:115),
                 log_porc_incap_indemn_int=predict(mod, 
                                      c(15:115), deriv = 0)[["y"]])
porc_incap_indemn_edad_sexo_int_f<-left_join(pred,
                                             porc_incap_indemn_edad_sexo_int_f,
                                             by='edad_siniestro') %>%
                                   mutate(sexo:='F',
                                          porc_incap_indemn_int=exp(log_porc_incap_indemn_int))

#Gráfica del ajuste para mujeres--------------------------------------------------------------------
plot(porc_incap_indemn_edad_sexo_int_f$edad_siniestro,
     log(porc_incap_indemn_edad_sexo_int_f$porc_incap_indemn),
     col="grey",xlab="Age",ylab="Wages")
lines(predict(mod,newdata = list(edad_siniestro=c(15:105)))$x,
      predict(mod,newdata = list(edad_siniestro=c(15:105)))$y)

#Relación entre el salario de cotizantes vs salario de beneficiarios de Indemnizaciones-------------
indemn_2018 <-  indemnizaciones_rtr %>%
                filter(anio=='2018',
                       tipo_prestacion=='ID') %>%
                distinct(cedula,anio,.keep_all = TRUE) %>%
                group_by(sexo,edad_siniestro) %>%             
                mutate(sal_indm=mean(promedio_sueldo_real),
                       ben=n(),
                       coef=mean(coeficiente_real,na.rm = TRUE)) %>%
                ungroup() %>%
                distinct(sexo,edad_siniestro, .keep_all = TRUE) %>%
                select(sexo,edad_siniestro,sal_indm,ben,coef)

sal_afi <- as_tibble(sal_afi) %>% select(x,sexo,sal)

relacion<-  left_join(sal_afi,indemn_2018, by=c('x'='edad_siniestro','sexo'))%>%
            mutate(relacion=sal_indm/sal) %>%
            mutate(masa_afi=60*ben*sal*coef,
                   masa_indm=60*ben*sal_indm*coef)

(r<-sum(relacion$masa_indm,na.rm = TRUE)/sum(relacion$masa_afi,na.rm = TRUE))

#Guardar el porcentaje de incapacidad promedio suavizado por edad y sexo en un Rdata----------------
message( '\tGuardando porcentaje de incapacidad parcial de RTR' )
porc_incap_indemn_edad_sexo_int<-rbind(porc_incap_indemn_edad_sexo_int_f,
                                       porc_incap_indemn_edad_sexo_int_m)

save( porc_incap_indemn_edad_sexo_int,r,
      file = paste0( parametros$RData_seg, 'IESS_RTR_porc_incap_indemn_edad_sexo_int.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()