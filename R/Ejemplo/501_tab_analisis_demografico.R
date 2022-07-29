message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCreación de las tablas del análisis demográfico' )

#--------------------------------Carga de datos
load( file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_demografico.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_RTR_pensiones_min_max.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_afi_tiempo_aportacion_incluye_TNRH.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_rentas_2020.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_subsidios_prestaciones_2020.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_indemnizaciones_prestaciones_2020.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_indemnizaciones_prestaciones_2020.RData' ) )
# Tabla afiliados activos 2005-2020 a diciembre-----------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_IVM_analisis_demografico.RData' ) ) 

aux <- copy( pob_afi_ini )
aux[ , Tasa:=100*Tasa]
aux[, anio := as.character( anio ) ]
#aux[ anio=="2020", anio:="2020-04"]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pob_afi_ini', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla masa salarial 2005-2020 --------------------------------------------------------------------
message( '\tTabla masa salarial inicial' )
#load( file = paste0( parametros$RData_seg, 'IESS_masa_salarial_inicial.RData' ) ) 

aux <- copy( masa_salarial_ini )
aux[ , Tasa:=100*Tasa]
aux[, anio := as.character( anio ) ]
#aux[ anio=="2020", anio:="2020-04"]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2,2 ,2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_masa_salarial_ini', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

load( file = paste0( parametros$RData_seg, 'IESS_poblacion_afiliada_inicial.RData' ) ) 
#Tiempo de aportación de afiliados
message( '\tTabla de Tiempo de aportación de afiliados' )
aux <- copy( afi_tiempo_aportacion )
aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

#Subsidios------------------------------------------------------------------------------------------
message( '\tTabla de Subsidios' )
aux <- subsidios_rtr %>%
        mutate(anio=year(sr_fecha_generacion)) %>%
        arrange(anio) %>%
        group_by(anio) %>%
        mutate(monto=sum(sr_valor,na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(sr_cedula,anio,.keep_all = TRUE) %>%
        group_by(anio) %>%
        mutate(numero=n()) %>%
        ungroup() %>%
        distinct(anio,.keep_all = TRUE) %>%
        select(anio,numero,monto)

aux[nrow(aux), 2:3 ] <- 3 * aux[nrow(aux), 2:3 ]

aux <- aux %>%
        mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
        mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
        mutate(anio=as.character(anio)) %>%
        select(anio,numero,creci_num,monto,creci_monto)


aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2 , 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_subsidios_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
#Montos de subsidios por rango de sexo en 2020------------------------------------------------------
message( '\tTabla de Montos de subsidios por rango de edad y sexo, en 2020' )

cortes_monto<-c(0,80,seq(200,1600,200),1900,2400,3000,4000,5000,7000,28000)
etiquetas_monto<-c(paste0("(\\$",formatC( c(19,80,seq(200,1600,200),1900,2400,3000,4000,5000,7000), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-\\$",formatC( c(80,seq(200,1600,200),1900,2400,3000,4000,5000,7000,28000), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))
aux  <- subsidios_rtr %>%
        mutate(anio=year(sr_fecha_generacion),
               edad=round(edad_siniestro,0)) %>%
        filter(anio=='2020') %>%
        group_by(sr_cedula) %>%
        mutate(monto=sum(sr_valor,na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(sr_cedula,.keep_all = TRUE) %>%
        mutate(rango_monto=cut(monto, breaks = cortes_monto,
                               labels = etiquetas_monto,
                               #include.lowest = TRUE,
                               right = TRUE)) %>%
        group_by(sexo,rango_monto) %>%
        mutate(beneficiarios=n()) %>%
        ungroup() %>%
        mutate(dist=beneficiarios/n()) %>%
        distinct(sexo,rango_monto,.keep_all = TRUE) %>%
        select(sexo,beneficiarios,rango_monto,dist) %>%
        arrange(rango_monto,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
        select(rango_monto,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
        select(rango_monto,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_monto') %>%
        select(rango_monto,M_ben,M_dist,F_ben,F_dist) %>%
        mutate(M_dist=100*M_dist,
               F_dist=100*F_dist,
               rango_monto=as.character(rango_monto))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_subsidios_monto_dist_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Indemnizaciones------------------------------------------------------------------------------------
message( '\tTabla de Indemnizaciones' )
aux <- indemnizaciones_rtr %>%
        mutate( anio = ano ) %>%
        filter(anio<='2020',
               tipo_prestacion=='ID') %>%
        group_by(anio) %>%
        mutate(monto=sum(valor_de_la_prestacion,na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(cedula,anio,.keep_all = TRUE) %>%
        group_by(anio) %>%
        mutate(numero=n()) %>%
        ungroup() %>%
        distinct(anio,.keep_all = TRUE) %>%
        select(anio,numero,monto) 

aux[nrow(aux), 2:3 ] <- 3 * aux[nrow(aux), 2:3 ]

aux <- aux %>%
        arrange(anio) %>%
        mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
        mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
        mutate(anio=as.character(anio)) %>%
        select(anio,numero,creci_num,monto,creci_monto) 

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2 , 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_indemnizaciones_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Montos de indemnizaciones por rango de sexo en 2020------------------------------------------------
message( '\tTabla de Montos de indemnizaciones por rango de edad y sexo, en 2020' )

cortes_monto<-c(148,seq(2000,20000,2000),25000,30000,35000,499396)
etiquetas_monto<-c(paste0("(\\$",formatC( c(148,seq(2000,20000,2000),25000,30000,35000), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-\\$",formatC( c(seq(2000,20000,2000),25000,30000,35000,410436), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))


aux  <- indemnizaciones_rtr %>%
        mutate( anio = ano ) %>%
        filter(anio<='2020',
               tipo_prestacion=='ID') %>%
        mutate( edad_siniestro=(age_calc(fecha_de_nacimiento, 
                                         enddate = as.Date("30/04/2020","%d/%m/%Y"), 
                                         units = "years", 
                                         precise = TRUE))) %>%
        mutate(edad=round(edad_siniestro,0)) %>%
        group_by(cedula) %>%
        mutate(monto=sum(valor_de_la_prestacion,na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(rango_monto=cut(monto, breaks = cortes_monto,
                               labels = etiquetas_monto,
                               #include.lowest = TRUE,
                               right = TRUE)) %>%
        group_by(sexo,rango_monto) %>%
        mutate(beneficiarios=n()) %>%
        ungroup() %>%
        mutate(dist=beneficiarios/n()) %>%
        distinct(sexo,rango_monto,.keep_all = TRUE) %>%
        select(sexo,beneficiarios,rango_monto,dist) %>%
        arrange(rango_monto,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
        select(rango_monto,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
        select(rango_monto,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_monto') %>%
        select(rango_monto,M_ben,M_dist,F_ben,F_dist) %>%
        mutate(M_dist=100*M_dist,
               F_dist=100*F_dist,
               rango_monto=as.character(rango_monto))

aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_indemnizaciones_monto_dist_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


# #Pensiones provisionales por incapacidad temporal---------------------------------------------------
# message( '\tTabla de pensiones provisionales por incapacidad temporal' )
# aux <- copy( pensiones_provisionales_inc_tem )
# aux[ , creci_num := creci_num*100]
# aux[ , creci_monto := creci_monto*100]
# aux[, anio:=as.character(anio)]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2 , 2) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_pensiones_provisionales_inc_tem_rtr', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )

#Incapacidad permanente parcial---------------------------------------------------------------------
message( '\tTabla de pensionistas por incapacidad permanente parcial' )
aux<- base %>%
        filter(tipo_prestacion=='PP') %>%
        group_by(anio) %>%
        mutate(monto=sum(tot_ingr,na.rm = TRUE)) %>%
        #filter(mes=='12') %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(numero=n()) %>%
        distinct(anio,.keep_all = TRUE) %>%
        ungroup() %>%
        select(anio,numero,monto) %>%
        mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
        mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
        mutate(anio=as.character(anio)) %>%
        select(anio,numero,creci_num,monto,creci_monto)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2 , 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_incapacidad_parcial_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Montos de incp permanente parcial por rango de sexo en 2020----------------------------------------
message( '\tTabla de Montos de incapacidad permanente parcial por rango de edad y sexo, en 2020' )

cortes_monto<-c(0,seq(200,1600,200),1720)
etiquetas_monto<-c(paste0("(\\$",formatC( c(61,seq(200,1600,200)), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-\\$",formatC( c(seq(200,1600,200),1720), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

aux  <- base %>%
        filter(tipo_prestacion=='PP') %>%
        filter(anio=='2020') %>%
        mutate(edad=round(edad_siniestro,0)) %>%
        group_by(cedula) %>%
        mutate(monto=sum(tot_ingr,na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(rango_monto=cut(monto, breaks = cortes_monto,
                               labels = etiquetas_monto,
                               #include.lowest = TRUE,
                               right = TRUE)) %>%
        group_by(sexo,rango_monto) %>%
        mutate(beneficiarios=n()) %>%
        ungroup() %>%
        mutate(dist=beneficiarios/n()) %>%
        distinct(sexo,rango_monto,.keep_all = TRUE) %>%
        select(sexo,beneficiarios,rango_monto,dist) %>%
        arrange(rango_monto,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
        select(rango_monto,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
        select(rango_monto,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_monto') %>%
        select(rango_monto,M_ben,M_dist,F_ben,F_dist) %>%
        mutate(M_dist=100*M_dist,
               F_dist=100*F_dist,
               rango_monto=as.character(rango_monto))

aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pp_monto_dist_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


#Incapacidad permanente total-----------------------------------------------------------------------
message( '\tTabla de pensionistas por incapacidad permanente total' )
aux<- prestaciones_pt %>%
        group_by(anio) %>%
        mutate(monto=sum(tot_ingr,na.rm = TRUE)) %>%
        #filter(mes=='12') %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(numero=n()) %>%
        distinct(anio,.keep_all = TRUE) %>%
        ungroup() %>%
        select(anio,numero,monto) %>%
        mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
        mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
        mutate(anio=as.character(anio)) %>%
        select(anio,numero,creci_num,monto,creci_monto)
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2 , 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_incapacidad_total_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Montos de incp permanente total por rango de sexo en 2020----------------------------------------
message( '\tTabla de Montos de incapacidad permanente total por rango de edad y sexo, en 2020' )

cortes_monto<-c(192,seq(400,1600,200),1778)
etiquetas_monto<-c(paste0("(\\$",formatC( c(193,seq(400,1600,200)), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-\\$",formatC( c(seq(400,1600,200),1778), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

aux  <- base %>%
        filter(tipo_prestacion=='PT') %>%
        filter( anio=='2020') %>%
        mutate(edad=round(edad_siniestro,0)) %>%
        group_by(cedula) %>%
        mutate(monto=sum(tot_pension,na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(rango_monto=cut(monto, breaks = cortes_monto,
                               labels = etiquetas_monto,
                               #include.lowest = TRUE,
                               right = TRUE)) %>%
        group_by(sexo,rango_monto) %>%
        mutate(beneficiarios=n()) %>%
        ungroup() %>%
        mutate(dist=beneficiarios/n()) %>%
        distinct(sexo,rango_monto,.keep_all = TRUE) %>%
        select(sexo,beneficiarios,rango_monto,dist) %>%
        arrange(rango_monto,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
        select(rango_monto,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
        select(rango_monto,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_monto') %>%
        select(rango_monto,M_ben,M_dist,F_ben,F_dist) %>%
        mutate(M_dist=100*M_dist,
               F_dist=100*F_dist,
               rango_monto=as.character(rango_monto))
aux[is.na(aux)] = 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pt_monto_dist_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


#Incapacidad permanente absoluta--------------------------------------------------------------------
message( '\tTabla de pensionistas por incapacidad permanente absoluta' )
aux<- prestaciones_pa %>%
        group_by(anio) %>%
        mutate(monto=sum(tot_pension,na.rm = TRUE)) %>%
        #filter(mes=='12') %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(numero=n()) %>%
        distinct(anio,.keep_all = TRUE) %>%
        ungroup() %>%
        select(anio,numero,monto) %>%
        mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
        mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
        mutate(anio=as.character(anio)) %>%
        select(anio,numero,creci_num,monto,creci_monto)
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2 , 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_incapacidad_absoluta_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Montos de incp permanente absoluta por rango de sexo en 2020----------------------------------------
message( '\tTabla de Montos de incapacidad permanente absoluta por rango de edad y sexo, en 2020' )

cortes_monto<-c(192,seq(400,1600,200),1700)
etiquetas_monto<-c(paste0("(\\$",formatC( c(193,seq(400,1600,200)), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-\\$",formatC( c(seq(400,1600,200),1700), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

aux  <- base %>%
        filter(tipo_prestacion=='PA') %>%
        filter( anio=='2020') %>%
        mutate(edad=round(edad_siniestro,0)) %>%
        group_by(cedula) %>%
        mutate(monto=sum(tot_pension,na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(rango_monto=cut(monto, breaks = cortes_monto,
                               labels = etiquetas_monto,
                               #include.lowest = TRUE,
                               right = TRUE)) %>%
        group_by(sexo,rango_monto) %>%
        mutate(beneficiarios=n()) %>%
        ungroup() %>%
        mutate(dist=beneficiarios/n()) %>%
        distinct(sexo,rango_monto,.keep_all = TRUE) %>%
        select(sexo,beneficiarios,rango_monto,dist) %>%
        arrange(rango_monto,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
        select(rango_monto,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
        select(rango_monto,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_monto') %>%
        select(rango_monto,M_ben,M_dist,F_ben,F_dist) %>%
        mutate(M_dist=100*M_dist,
               F_dist=100*F_dist,
               rango_monto=as.character(rango_monto))
aux[is.na(aux)] = 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pa_monto_dist_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


#Pensionistas de orfandad---------------------------------------------------------------------------
message( '\tTabla de pensionistas por orfandad del fondo de RT' )
aux<- prestaciones_orfandad %>%
        group_by(anio) %>%
        mutate(monto=sum(tot_ingr,na.rm = TRUE)) %>%
        #filter(mes=='12') %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(numero=n()) %>%
        distinct(anio,.keep_all = TRUE) %>%
        ungroup() %>%
        select(anio,numero,monto) %>%
        mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
        mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
        mutate(anio=as.character(anio)) %>%
        select(anio,numero,creci_num,monto,creci_monto)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2 , 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pensionistas_orfadad_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Montos de orfandad por rango de sexo en 2020-------------------------------------------------------
message( '\tTabla de Montos de orfandad por rango de edad y sexo, en 2020' )

cortes_monto<-c(0,seq(100,500,100),805)
etiquetas_monto<-c(paste0("(\\$",formatC( c(0,seq(100,500,100)), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-\\$",formatC( c(seq(100,500,100),805), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

aux  <- prestaciones_orfandad %>% 
        filter( anio=='2020') %>%
        mutate(edad=round(edad_siniestro,0)) %>%
        group_by(cedula) %>%
        mutate(monto=sum(renta_mensual,na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(rango_monto=cut(monto, breaks = cortes_monto,
                               labels = etiquetas_monto,
                               #include.lowest = TRUE,
                               right = TRUE)) %>%
        group_by(sexo,rango_monto) %>%
        mutate(beneficiarios=n()) %>%
        ungroup() %>%
        mutate(dist=beneficiarios/n()) %>%
        distinct(sexo,rango_monto,.keep_all = TRUE) %>%
        select(sexo,beneficiarios,rango_monto,dist) %>%
        arrange(rango_monto,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
        select(rango_monto,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
        select(rango_monto,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_monto') %>%
        select(rango_monto,M_ben,M_dist,F_ben,F_dist) %>%
        mutate(M_dist=100*M_dist,
               F_dist=100*F_dist,
               rango_monto=as.character(rango_monto))
aux[is.na(aux)] = 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_mo_monto_dist_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


#Pensionistas de viudedad---------------------------------------------------------------------------
message( '\tTabla de pensionistas por viudedad del fondo de RT' )

aux<- prestaciones_viudez %>%
        group_by(anio) %>%
        mutate(monto=sum(tot_ingr,na.rm = TRUE)) %>%
        #filter(mes=='12') %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(numero=n()) %>%
        distinct(anio,.keep_all = TRUE) %>%
        ungroup() %>%
        select(anio,numero,monto) %>%
        mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
        mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
        mutate(anio=as.character(anio)) %>%
        select(anio,numero,creci_num,monto,creci_monto)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2 , 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pensionistas_viudedad_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Montos de viudedad por rango de sexo en 2020-------------------------------------------------------
message( '\tTabla de Montos de viudedad por rango de edad y sexo, en 2020' )

cortes_monto<-c(0,seq(100,800,100),1341)
etiquetas_monto<-c(paste0("(\\$",formatC( c(0,seq(100,800,100)), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-\\$",formatC( c(seq(100,800,100),1341), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

aux  <- prestaciones_viudez %>% 
        filter(anio=='2020') %>%
        mutate(edad=round(edad_siniestro,0)) %>%
        group_by(cedula) %>%
        mutate(monto=sum(renta_mensual,na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(rango_monto=cut(monto, breaks = cortes_monto,
                               labels = etiquetas_monto,
                               #include.lowest = TRUE,
                               right = TRUE)) %>%
        group_by(sexo,rango_monto) %>%
        mutate(beneficiarios=n()) %>%
        ungroup() %>%
        mutate(dist=beneficiarios/n()) %>%
        distinct(sexo,rango_monto,.keep_all = TRUE) %>%
        select(sexo,beneficiarios,rango_monto,dist) %>%
        arrange(rango_monto,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
        select(rango_monto,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
        select(rango_monto,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_monto') %>%
        select(rango_monto,M_ben,M_dist,F_ben,F_dist) %>%
        mutate(M_dist=100*M_dist,
               F_dist=100*F_dist,
               rango_monto=as.character(rango_monto))
aux[is.na(aux)] = 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_mv_monto_dist_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


# Tabla pensiones mínimas RT 2012-2020 -------------------------------------------------------------
aux <- copy( pen_min_rtr )
aux[ , sbu:=100*sbu]
aux[ , rango:=as.character(rango)]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_min_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


message( '\tPensiones máximas' )

# Tabla pensiones máximas RT 2012-2020 -------------------------------------------------------------
aux <- copy( pen_max_rt )
aux[ , sbu:=100*sbu]
aux[ , tipo:=as.character(tipo)]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_max_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )



# Tabla salarios por aportaciones de afiliados (inyendo TNRH)---------------------------------------
message( '\tTabla salarios por aportaciones de afiliados (inyendo TNRH)' )
aux <- tabla
aux_xtable <- xtable( aux, digits = c( rep(0,14) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(34),
       sanitize.text.function = identity )

#Tabla de la inflación de rtr-----------------------------------------------------------------------
aux <- as.data.table( inflacion_rtr )
aux$anio <- as.character( aux$anio )
aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ipc_hist', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


#Limpiando memoria RAM------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()