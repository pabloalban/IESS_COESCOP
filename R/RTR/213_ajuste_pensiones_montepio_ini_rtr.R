message( paste( rep('-', 100 ), collapse = '' ) )

#Carga del pago de pensiones mensuales de las rentas de RTR-----------------------------------------
message( '\tCargando rentas mensuales de RTR' )
load( paste0( parametros$RData_seg, 'IESS_RTR_rentas.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_grupo_familiar.RData' ) )


#Calculo de las pensiones promedio en el 2018, por edad, de viudedad--------------------------------
message( '\tCalculando las rentas de viudez promedio, sin decimos' )
pensiones_MV_rtr<-as.data.table(base) %>%
                  lazy_dt() %>%
                  filter(tipo_prestacion %in% c('VO'),
                         beneficiario == 'VIUDEDAD',
                         anio=='2018',
                         tipo_seguro=='RT') %>%
                  mutate(edad=round(age_calc(fecha_nacimiento, 
                                             enddate = as.Date(paste0("31/12/",anio),"%d/%m/%Y"), 
                                             units = "years", 
                                             precise = TRUE))) %>%
                  mutate(tot_ingr=12*(tot_ingr-0.6*394/12)/(13)) %>%
                  group_by(edad) %>%
                  mutate(pension_mv_ini=mean(tot_ingr,na.rm = TRUE)) %>%
                  ungroup() %>%
                  distinct(edad, .keep_all = TRUE) %>%
                  select(edad,pension_mv_ini) %>%
                  arrange(edad) %>%
                  as_tibble()

#plot(pensiones_MV_rtr$edad,log(pensiones_MV_rtr$pension_mv_ini))
#Ajuste de las pensiones promedio en el 2018, por edad, de viudedad---------------------------------
message( '\tInterpolando las pensiones promedio iniciales de viudedad de rtr' )
aux<-pensiones_MV_rtr #%>% filter(!(edad %in% c('98','94','95')),!is.na(pension_pp_ini))  

mod<-smooth.spline(aux$edad,
                   aux$pension_mv_ini,df=6) 

pred<-data.frame(edad=c(18:105),
                 pension_mv_ini_int=predict(mod, 
                                            c(18:105), deriv = 0)[["y"]])
pensiones_MV_rtr_int<-left_join(pred,pensiones_MV_rtr,
                                    by='edad')
pensiones_MV_rtr_int<-as.data.table(pensiones_MV_rtr_int %>% select(x:=edad,
                                                                    pension_mv_ini,
                                                                    pension_mv_ini_int))
#Gráfica del ajuste de la mortalidad----------------------------------------------------------------
# 
# plot(pensiones_MV_rtr_int$edad,
#      pensiones_MV_rtr_int$pension_mv_ini,
#      col="grey",xlab="edad",ylab="pensión promedio")
# lines(pensiones_MV_rtr_int$edad,
#       pensiones_MV_rtr_int$pension_mv_ini_int)



#Calculo de las pensiones promedio en el 2018, por edad, de orfandad--------------------------------
message( '\tCalculando las rentas de orfandad promedio, sin decimos' )
pensiones_MO_rtr<-as.data.table(base) %>%
                  lazy_dt() %>%
                  filter(tipo_prestacion %in% c('VO'),
                         beneficiario == 'ORFANDAD',
                         anio=='2018',
                         mes=='12',
                         tipo_seguro=='RT') %>%
                  mutate(edad=round(age_calc(fecha_nacimiento, 
                                             enddate = as.Date(paste0("31/12/",anio),"%d/%m/%Y"), 
                                             units = "years", 
                                             precise = TRUE))) %>%
                  filter(edad<=17) %>%
                  mutate(tot_ingr=12*(tot_ingr-0.4*rho*394/12)/(13)) %>%
                  group_by(edad) %>%
                  mutate(pension_mo_ini=mean(tot_ingr,na.rm = TRUE)) %>%
                  ungroup() %>%
                  distinct(edad, .keep_all = TRUE) %>%
                  select(edad,pension_mo_ini) %>%
                  arrange(edad) %>%
                  as_tibble()

#plot(pensiones_MO_rtr$edad,(pensiones_MO_rtr$pension_mo_ini))
#Ajuste de las pensiones promedio en el 2018, por edad, de orfandad---------------------------------
message( '\tInterpolando las pensiones promedio iniciales de orfandad de rtr' )
pensiones_MO_rtr <- pensiones_MO_rtr 

aux<-pensiones_MO_rtr #%>% filter(!(edad %in% c('0','1','5')))  

mod<-smooth.spline(aux$edad,
                   aux$pension_mo_ini,df=3) 

pred<-data.frame(edad=c(0:17),
                 pension_mo_ini_int=predict(mod, 
                                            c(0:17), deriv = 0)[["y"]])
pensiones_MO_rtr_int<-left_join(pred,pensiones_MO_rtr,
                                by='edad')
pensiones_MO_rtr_int<-as.data.table(pensiones_MO_rtr_int %>% select(x:=edad,
                                                                    pension_mo_ini,
                                                                    pension_mo_ini_int))
#Gráfica del ajuste de la mortalidad----------------------------------------------------------------
# 
plot(pensiones_MO_rtr_int$x,
     pensiones_MO_rtr_int$pension_mo_ini,
     col="grey",xlab="edad",ylab="pensión promedio")
lines(pensiones_MO_rtr_int$x,
      pensiones_MO_rtr_int$pension_mo_ini_int)
#Guarda la tasa de mortalidad ajustada en un Rdata--------------------------------------------------
message( '\tGuardando pensiones promedio VO de RTR' )

save( pensiones_MV_rtr_int,pensiones_MO_rtr_int,
      file = paste0( parametros$RData_seg, 'IESS_RTR_pensiones_VO_ajustadas_ini.RData' ) )

#Borrar elementos restantes-------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
