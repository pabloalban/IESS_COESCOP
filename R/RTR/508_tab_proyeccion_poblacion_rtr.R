message( paste( rep( '-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_RTR_proy_beneficarios_prestacion.RData' ) )

message( '\tGenerando tablas de proyección de la población de beneficiarios de RTR' )

# Generando de estados de IVM-----------------------------------------------------------------------
y_max <- parametros$anio_ini + parametros$horizonte

aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, 
                                              l1_f = l1, l2_f = l2, l3_f = l3, l4_f = l4, 
                                              l5_f = l5 ) ]
aux_f <- aux_f[ t <= y_max ]
aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, 
                                              l1_m = l1, l2_m = l2, l3_m = l3, l4_m = l4, 
                                              l5_m = l5 ) ]
aux_m <- aux_m[ t <= y_max ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )

aux[, t := as.character( t ) ]
aux <- aux[ t > 2018 ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_ivm.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla de estados de RTR- ---------------------------------------------------------------
y_max <- parametros$anio_ini + parametros$horizonte

aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, 
                                              l9_f = l9, l15_f = l15, l13_f = l13, l14_f = l14, 
                                              l10_f = l10, l_11_f = l11 , l12 ) ]
aux_f <- aux_f[ t <= y_max ]
aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, 
                                              l9_f = l9, l15_f = l15, l13_f = l13, l14_f = l14, 
                                              l10_f = l10, l_11_f = l11 , l12 ) ]
aux_m <- aux_m[ t <= y_max ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[1, ] <- round(aux[1, ],0)

aux[, t := as.character( t ) ]
aux <- aux[ t > 2018 ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_rtr.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla de transiciones de estados de IVM-------------------------------------------------
aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, 
                                              l1_2_f = l1_2, l1_5_f = l1_5, 
                                              l2_3_f = l2_3, l2_4_f = l2_4, l2_5_f = l2_5, 
                                              l3_5_f = l3_5, l4_5_f = l4_5 ) ]
aux_f <- aux_f[ t <= y_max ]

aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini,
                                              l1_2_f = l1_2, l1_5_f = l1_5, 
                                              l2_3_f = l2_3, l2_4_f = l2_4, l2_5_f = l2_5, 
                                              l3_5_f = l3_5, l4_5_f = l4_5 ) ]
aux_m <- aux_m[ t <= y_max ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux <- aux[ t > 2018 ]
aux[, t := as.character( t ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_tran_ivm.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla de transiciones de estados de RTR-------------------------------------------------
aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, 
                                              l25_f = 0, l55_f = 0, l56_f= 0,
                                              l76_f = 0, l77_f = 0 ) ]
aux_f <- aux_f[ t <= y_max ]

aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, 
                                              l25_m = 0, l55_m = 0, l56_m= 0,
                                              l76_m = 0, l77_m = 0 ) ]
aux_m <- aux_m[ t <= y_max ]

aux_t <- pob_proy_tot[ , list( t = t + parametros$anio_ini, 
                                              l25_t= 0, l55_t= 0, l56_t= 0,
                                              l76_t= 0, l77_t= 0) ]

aux_t <- aux_t[ t <= y_max ]
aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux <- merge( aux, aux_t, by = c( 't' ) )
aux <- aux[ t > 2018 ]
aux[, t := as.character( t ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_tran_rtr.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla: iess_tab_pob_proy_cot------------------------------------------------------------
aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, 
                                              l2_f = l2, l2_cot_f = l2_cot, l2_ces_f = l2_ces ) ]
aux_f <- aux_f[ t <= y_max ]

aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, 
                                              l2_m = l2, l2_cot_m = l2_cot, l2_ces_m = l2_ces ) ]
aux_m <- aux_m[ t <= y_max ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[ , l2 := l2_f + l2_m ]
aux[ , l2_cot := l2_cot_f + l2_cot_m ]
aux[ , l2_ces := l2_ces_f + l2_ces_m ]
aux <- aux[ t > 2018 ]
aux[, t := as.character( t ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_cot.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

#Limpiando memoria RAM------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
