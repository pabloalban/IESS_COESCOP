message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()

# Escenario 1 --------------------------------------------------------------------------------------
# escenario <- 'escenario_1'
# load( paste0( parametros$RData_seg, 'IESS_RTR_configuracion_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_RTR_primas_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_RTR_balances_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$duracion_esc_1 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini -1
# 
# REP$cap_ini <- format( esc$V0, 
#                        digits = 2, nsmall = 2, big.mark = '.', 
#                        decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 4, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$apo_est_esc_1 <- format( 100 * esc$hip_esc$apo_est[2],
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$tasa_act_esc_1 <- format( 100 * esc$hip_esc$i_a[2],
#                               digits = 2, nsmall = 2, big.mark = '.', 
#                               decimal.mark = ',', format = 'f' )
# 
# REP$tasa_apo_jub_esc_1 <- format( 100 * esc$hip_esc$apo_jub[2],
#                                   digits = 2, nsmall = 2, big.mark = '.', 
#                                   decimal.mark = ',', format = 'f' )
# 
# REP$tasa_por_gas_esc_1 <- format( 100 * esc$hip_esc$por_gas[2],
#                                   digits = 2, nsmall = 2, big.mark = '.', 
#                                   decimal.mark = ',', format = 'f' )
# 
# REP$tasa_aporte_salud_esc_1 <- format( 100 * esc$hip_esc$apo_sal[2],
#                                        digits = 2, nsmall = 2, big.mark = '.', 
#                                        decimal.mark = ',', format = 'f' )
# 
# # a <- data.table(a=2018:2058, b=100 * esc$hip_esc$apo_cot)
# REP$tasa_aporte_esc_1_2020 <- format( 100 * ( esc$hip_esc$apo_cot[3] + esc$hip_esc$apo_sal[3] ),
#                                       digits = 2, nsmall = 2, big.mark = '.', 
#                                       decimal.mark = ',', format = 'f' )
# 
# REP$tasa_aporte_esc_1_2021 <- format( 100 * ( esc$hip_esc$apo_cot[4] + esc$hip_esc$apo_sal[4] ),
#                                       digits = 2, nsmall = 2, big.mark = '.', 
#                                       decimal.mark = ',', format = 'f' )
# 
# REP$bal_sum_act_1 <- format( balance_anual[ t == parametros$horizonte ]$Act_vap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_sum_pas_1 <- format( balance_anual[ t == parametros$horizonte ]$Pas_vap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$ing_jubilados_1 <- format( balance_anual[ t == parametros$horizonte ]$A_pen_vap,
#                                digits = 2, nsmall = 2, big.mark = '.', 
#                                decimal.mark = ',', format = 'f' )
# 
# REP$ing_apo_est_1 <- format( balance_anual[ t == parametros$horizonte ]$A_est_vap,
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )