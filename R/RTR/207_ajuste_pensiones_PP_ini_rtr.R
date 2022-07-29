message( paste( rep('-', 100 ), collapse = '' ) )

#Carga del pago de pensiones mensuales de las rentas de RTR-----------------------------------------
message( '\tCargando rentas mensuales de RTR' )
load( paste0( parametros$RData_seg, 'IESS_RTR_rentas.RData' ) )

#Calculo de las pensiones promedio en el 2018, por edad, de PP--------------------------------------
message( '\tCalculando las rentas de PT promedio, sin decimos' )
pensiones_PP_rtr<-base %>% lazy_dt() %>%
                  filter(tipo_prestacion %in% c('PP'), 
                         anio=='2018',
                         tipo_seguro=='RT') %>%
                  mutate(edad=round(age_calc(fecha_nacimiento, 
                                             enddate = as.Date(paste0("31/12/",anio),"%d/%m/%Y"), 
                                             units = "years", 
                                             precise = FALSE))) %>%
                  mutate(tot_ingr=12*(tot_ingr-394/12)/(13)) %>%
                  group_by(edad) %>%
                  mutate(pension_pp_ini=mean(tot_ingr,na.rm = TRUE)) %>%
                  ungroup() %>%
                  distinct(edad, .keep_all = TRUE) %>%
                  select(edad,pension_pp_ini) %>%
                  arrange(edad) %>%
                  as_tibble()


#Ajuste de las pensiones promedio en el 2018, por edad, de PP---------------------------------------
message( '\tInterpolando las pensiones promedio iniciales de PP de rtr' )
aux<-pensiones_PP_rtr %>%
  filter(!(edad %in% c('98','94','95')),!is.na(pension_pp_ini))  

mod<-smooth.spline(aux$edad,
                   aux$pension_pp_ini,df=6) 

pred<-data.frame(edad=c(18:105),
                 pension_pp_ini_int=predict(mod, 
                                          c(18:105), deriv = 0)[["y"]])
pensiones_PP_ini_int_rtr<-left_join(pred,pensiones_PP_rtr,
                                   by='edad')
pensiones_PP_ini_int_rtr<-as.data.table(pensiones_PP_ini_int_rtr)
#Gráfica del ajuste de la mortalidad----------------------------------------------------------------

plot(pensiones_PP_ini_int_rtr$edad,
     pensiones_PP_ini_int_rtr$pension_pp_ini,
     col="grey",xlab="edad",ylab="pensión promedio")
lines(predict(mod,newdata = list(edad=c(18:105)))$x,
      predict(mod,newdata = list(edad=c(18:105)))$y)

#Guarda la tasa de mortalidad ajustada en un Rdata--------------------------------------------------
message( '\tGuardando pensiones promedio PP de RTR' )

save( pensiones_PP_ini_int_rtr,
      file = paste0( parametros$RData_seg, 'IESS_RTR_pensiones_PP_ajustadas_ini.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

