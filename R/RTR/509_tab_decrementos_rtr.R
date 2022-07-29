message( paste( rep('-', 100 ), collapse = '' ) )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_tablas_biometricas_mortalidad_pensionistas_rtr.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_tabla_decrementos.RData') )
load( paste0( parametros$RData_seg, 'IESS_RTR_siniestralidad_indemnizaciones_edad_sexo_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_porc_incap_indemn_edad_sexo_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_siniestralidad_subsidios_edad_sexo_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_duracion_subsidios_sexo_edad_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_siniestralidad_accidentes_laborales_fatales.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_grupo_familiar.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_fdp_ingresos_huerfanos_montepio.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_fdp_ingresos_viudas_montepio.RData' ) )
#1. Generando tabla de decrementos -----------------------------------------------------------------
message( '\tGenerando tablas de decrementos' )
aux_f <- tab_dec[ sexo == 'F' & x <= 105, list( x, lx_f = lx, dv_f = dv, di_f = di, 
                                                din_f = din, dinpp_f = lx*0 , dd_f = dd ) ]
aux_m <- tab_dec[ sexo == 'M' & x <= 105, list( x, x_m = x, lx_m = lx, dv_m = dv, di_m = di,
                                                din_m = din, dinpp_m = lx*0 , dd_m = dd ) ]
aux <- merge( aux_f, aux_m, by = c( 'x' ) )
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 4, 4, 4, 0, 4, 0, 2, 4, 4, 4, 0, 4 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_dec_rtr.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

#2. Generando tabla de mortalidad pensionistas de RTR-----------------------------------------------
message( '\tGenerando tabla de mortalidad pensionistas de incapacidad permanete absoluta, total y parcial' )
aux <- merge( rtr_mor[ , list( x, lx, qx, px, ex) ],
              rtr_mor[ , list( x, x_m = x, lx, qx, px, ex) ], 
              by = c( 'x' ) )
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 6, 6, 2, 0, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_pen_incap_mort_rtr.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

#3. Tabla de siniestralidad de Indemnizaciones------------------------------------------------------
message( '\tTabla de siniestralidad de Indemnizaciones' )
aux <-  as.data.table(siniestralidad_indeminizaciones_edad_sexo_int)
aux <- aux[,list(x,sexo,tasa_sin_indem_int)]
aux_sin_indm <- merge( aux[ sexo=='F', list( x,  tasa_f=tasa_sin_indem_int) ],
              aux[ sexo=='M', list( x,  tasa_m=tasa_sin_indem_int) ], 
              by = c( 'x' ) )
# xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 6, 6, 2, 0, 2, 6, 6, 2 ) )
# print( xtb_aux,
#        file = paste0( parametros$resultado_tablas, 'iess_indem_sinies_rtr.tex' ),
#        type = 'latex', 
#        include.colnames = FALSE, include.rownames = FALSE, 
#        format.args = list( decimal.mark = ',', big.mark = '.' ), 
#        only.contents = TRUE, 
#        hline.after = NULL, sanitize.text.function = identity )

#3. 1. Porcentaje de incapacidad permanente parcial-------------------------------------------------
message( '\tTabla de porcentaje de incapacidad permanente parcial en indemnizaciones' )
aux <-  as.data.table(porc_incap_indemn_edad_sexo_int)
aux <- aux[,list(x=edad_siniestro,sexo,porc_incap_indemn_int)]
aux_porc_inc_indm <- merge( aux[ sexo=='F', list( x, porc_f=porc_incap_indemn_int) ],
              aux[ sexo=='M', list( x,  porc_m=porc_incap_indemn_int) ], 
              by = c( 'x' ) )
#unir las dos tablas de indemnizaciones
aux<- merge( aux_sin_indm,
             aux_porc_inc_indm, 
             by = c( 'x' ) )
aux<-aux[,x2:=x]


aux <- aux[,list(x,tasa_f,porc_f,x2,tasa_m,tasa_f)]

xtb_aux <- xtable( aux, digits = c( 0, 0, 8, 6, 0, 8, 6 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_indem_sinies_rtr.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

#4. Tabla siniestralidad de subsidios---------------------------------------------------------------
message( '\tTabla de siniestralidad de los subsidios otorgados por incapacidad temporal' )
aux <-  as.data.table(siniestralidad_subsidios_edad_sexo_int)
aux <- aux[,list(x,sexo,tasa_sin_sub_int)]
aux_sin_subs <- merge( aux[ sexo=='F', list( x, tasa_f=tasa_sin_sub_int) ],
                            aux[ sexo=='M', list( x,  tasa_m=tasa_sin_sub_int) ], 
                            by = c( 'x' ) )

#4. 1. Porcentaje de incapacidad temporal-----------------------------------------------------------
message( '\tTabla de porcentaje de incapacidad temporal en subsidios' )
aux <-  as.data.table(porc_incap_subsidios_edad_sexo_int)
aux <- aux[,list(x=edad,sexo,porc_subsidios_prom_int)]
aux_porc_inc_subs <- merge( aux[ sexo=='F', list( x, porc_f=porc_subsidios_prom_int) ],
                            aux[ sexo=='M', list( x,  porc_m=porc_subsidios_prom_int) ], 
                            by = c( 'x' ) )
#4. 2. Porcentaje de incapacidad temporal-----------------------------------------------------------
message( '\tTabla de duración (en días) de los subsidios por incapacidad temporal' )
aux <-  as.data.table(duracion_subsidios_sexo_edad_int)
aux <- aux[,list(x=edad,sexo,pro_duracion_dias_int)]
aux_dura_inc_subs <- merge( aux[ sexo=='F', list( x, dura_f=pro_duracion_dias_int) ],
                            aux[ sexo=='M', list( x, dura_m=pro_duracion_dias_int) ], 
                            by = c( 'x' ) )
#unir las dos tablas de indemnizaciones
aux<- merge( aux_sin_subs,
             aux_porc_inc_subs, 
             by = c( 'x' ) )
aux <- merge( aux,
              aux_dura_inc_subs, 
              by = c( 'x' ) )

aux<-aux[,x2:=x]

aux <- aux[,list(x,tasa_f,porc_f,dura_f,x2,tasa_m,porc_m,dura_m)]

xtb_aux <- xtable( aux, digits = c( 0, 0, 8, 6, 6, 0, 8, 6, 6 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_subs_sinies_rtr.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

#5. Factores de riesgo por montepío-----------------------------------------------------------------
message( '\tTabla de factores de riesgo de montepío' )
incidencia_FA <- as_tibble(incidencia_FA) %>%
                 mutate(tasa_FA:=exp(log_tasa_inc_FA_int)) %>%
                 select(x:=edad,tasa_FA)

fdp_huerfanos <- as_tibble(fdp_ingresos_huerfanos_edad_sexo) %>%
                 select(x, sexo,fdp_mo=fdp_int)

fdp_viudas <- as_tibble(fdp_ingresos_viudas_edad_sexo) %>%
              select(x, sexo,fdp_mv=fdp_int)

GF_orfandad <-  as_tibble(GF_orfandad) %>%
                select(x:=edad,
                       pGF_mo:=pGF_orfandad_int)

GF_viudez <-    as_tibble(GF_viudez) %>%
                select(x:=edad,
                       pGF_mv:=pGF_viudez_int)

aux <-  rbind(data.frame(x=c(0:105),sexo=c('M')),
              data.frame(x=c(0:105),sexo=c('F')))%>%
        left_join(.,fdp_viudas,by=c('x','sexo')) %>%
        left_join(.,fdp_huerfanos,by=c('x','sexo')) %>%
        left_join(.,incidencia_FA,by='x') %>%
        left_join(.,GF_orfandad,by='x') %>%
        left_join(.,GF_viudez,by='x') %>%
        select(x,sexo,tasa_FA,fdp_mo,fdp_mv,pGF_mo,pGF_mv)%>%
        arrange(x,sexo)

aux[is.na(aux)]<-0

aux <- cbind(aux[which(aux$sexo=='F'),],aux[which(aux$sexo=='M'),])[,-c(2,9)]

xtb_aux <- xtable( aux, digits = c( 0, 0, 6, 6, 6, 6, 6,0, 6, 6, 6, 6, 6 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_factor_montepio_rtr.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )
# Limpieza de RAM-----------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
