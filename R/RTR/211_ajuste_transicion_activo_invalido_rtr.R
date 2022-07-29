message( paste( rep('-', 100 ), collapse = '' ) )

#Cargando inventario de pensionistas----------------------------------------------------------------
message( '\tCargando inventario de pensionistas' )
load( paste0( parametros$RData_seg, 'IESS_inventario_pensionistas.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_cotizantes_historicos_sgo.RData' ) )

#Exposicion entre 2012 a 2018: cotizantes activos del SGO-------------------------------------------

cotizantes_sgo <- cotizantes_sgo %>%
                  filter( anio >= 2012 & anio <= 2018) %>%
                  group_by(genero,edad) %>%
                  mutate(cotizantes_sgo = sum(cotizantes_sgo, na.rm = TRUE)) %>%
                  ungroup() %>%
                  distinct(genero,edad, .keep_all = TRUE) %>%
                  select(x:=edad, sexo:=genero, ER:=cotizantes_sgo, -anio)

#Filtrando transiciones cotizantes activos a pensionistas de invalidez------------------------------

trans_activo_invalidez <- inventario_jubilados %>%
                          filter(tipo_seguro == 'SG',
                                tipo_prestacion == 'IN') %>%
                          mutate(anio=year(fecha_derecho)) %>%
                          mutate(fecha_derecho=as.Date(fecha_derecho,"%d/%m/%Y")) %>%
                          filter(fecha_derecho >= as.Date("01/01/2012","%d/%m/%Y") &
                                 fecha_derecho <= as.Date("31/12/2018","%d/%m/%Y")  ) %>%
                          distinct(asegurado, .keep_all = TRUE) %>%
                          mutate(fecha_nacimiento=if_else(is.na(fecha_nacimiento),
                                                          as.Date("1959-10-01","%Y-%m-%d"),
                                                         fecha_nacimiento)) %>%
                          mutate(fecha_nacimiento=as.Date(fecha_nacimiento,"%Y-%m-%d")) %>%
                          mutate(edad=round(age_calc(fecha_nacimiento, 
                                                     enddate = fecha_derecho, 
                                                     units = "years", 
                                                     precise = TRUE)))

trans_activo_invalidez <- trans_activo_invalidez %>%
                          group_by(sexo,edad) %>%
                          mutate(inv := n()) %>%
                          ungroup() %>%
                          distinct(sexo,edad, .keep_all = TRUE) %>%
                          select(x:=edad, sexo, inv) %>%
                          arrange(x,sexo) %>%
                          left_join(cotizantes_sgo,., by=c('x','sexo')) %>%
                          mutate(uin := 0.79*inv/ER,
                                 loguin=log(uin))

#Ajuste de transiciones de activo a invalidez-------------------------------------------------------
message( '\tInterpolando la transiciones de activo a invalidez del SGO' )

#Hombres--------------------------------------------------------------------------------------------
trans_activo_invalidez_m <- trans_activo_invalidez %>%
                            filter(sexo=='M') %>% filter(!is.na(loguin))
          
aux<-trans_activo_invalidez_m %>%
     filter(!(x %in% c('101')))  

mod<-smooth.spline(aux$x,
                   aux$loguin,df=6) 

pred<-data.frame(x=c(15:105),
                 loguin_int=predict(mod, 
                                            c(15:105), deriv = 0)[["y"]])
trans_activo_invalidez_m<-left_join(pred,trans_activo_invalidez_m,
                                    by='x') %>%
                          mutate(sexo='M')
trans_activo_invalidez_m<-as.data.table(trans_activo_invalidez_m)
#Gráfica de transiciones de activo a invalidez------------------------------------------------------

# plot(trans_activo_invalidez_m$x,
#      trans_activo_invalidez_m$loguin,
#      col="grey",xlab="edad",ylab="loguin")
# lines(trans_activo_invalidez_m$x,
#       trans_activo_invalidez_m$loguin_int)

#Mujeres--------------------------------------------------------------------------------------------
trans_activo_invalidez_f <- trans_activo_invalidez %>%
                            filter(sexo=='F') %>% filter(!is.na(loguin))

aux<-trans_activo_invalidez_f 
#%>% filter(!(x %in% c('101')))  

mod<-smooth.spline(aux$x,
                   aux$loguin,df=6) 

pred<-data.frame(x=c(15:105),
                 loguin_int=predict(mod, 
                                    c(15:105), deriv = 0)[["y"]])
trans_activo_invalidez_f<-left_join(pred,trans_activo_invalidez_f,
                                    by='x') %>%
                          mutate(sexo='F')
trans_activo_invalidez_f<-as.data.table(trans_activo_invalidez_f)
#Gráfica de transiciones de activo a invalidez------------------------------------------------------

# plot(trans_activo_invalidez_f$x,
#      trans_activo_invalidez_f$loguin,
#      col="grey",xlab="edad",ylab="loguin")
# lines(trans_activo_invalidez_f$x,
#       trans_activo_invalidez_f$loguin_int)

#Unión data.frame-----------------------------------------------------------------------------------
trans_activo_invalidez<-rbind(trans_activo_invalidez_m,
                              trans_activo_invalidez_f)

#Guarda la transiciones de activo a invalidez en Rdata----------------------------------------------
message( '\tGuardando la tasa de decremento de activo a invalidez del SGO' )

save( trans_activo_invalidez,
      file = paste0( parametros$RData_seg, 'IESS_SGO_tasa_decremento_act_invalidez_int.RData' ) )

#Limpiando memoria RAM------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

