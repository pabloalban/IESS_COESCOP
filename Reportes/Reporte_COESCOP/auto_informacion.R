message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()

# Escenario único-----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_balance.RData"))
load( file = paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
load( file = paste0( parametros$RData, 'COESCOP_esquema_pensional.RData' ) )

balance_anual <- as.data.table(balance_anual)

REP$bal_act_esc_1 <- format( balance_anual[  anio == parametros$anio_fin  ]$V,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$bal_cap_esc_1 <- format( balance_anual[ anio == parametros$horizonte ]$V_cap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )


REP$aporte_coescop_vap <- format( balance_anual[  anio == parametros$anio_fin  ]$aporte_coescop_vap,
                              digits = 2, nsmall = 2, big.mark = '.', 
                              decimal.mark = ',', format = 'f' )
 
REP$reserva_a_pagar_vap <- format( balance_anual[  anio == parametros$anio_fin  ]$reserva_a_pagar_vap,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )

REP$reserva_matematica_vap  <- format( balance_anual[  anio == parametros$anio_fin  ]$reserva_matematica_vap ,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )


REP$montepio_vap <- format( balance_anual[  anio == parametros$anio_fin  ]$montepio_vap,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )


REP$gastos_administrativos_vap <- format( balance_anual[  anio == parametros$anio_fin  ]$gastos_administrativos_vap,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )


REP$egreso_total_vap  <- format( balance_anual[  anio == parametros$anio_fin  ]$egreso_total_vap,
                                          digits = 2, nsmall = 2, big.mark = '.', 
                                          decimal.mark = ',', format = 'f' )

#Balance corriente----------------------------------------------------------------------------------

#Impacto--------------------------------------------------------------------------------------------
impacto <- as.data.table( impacto )

REP$impacto  <- format( impacto[  anio == parametros$anio_fin  ]$total_vap,
                                 digits = 2, nsmall = 2, big.mark = '.', 
                                 decimal.mark = ',', format = 'f' )

REP$aporte_ivm_vap <- format( impacto[  anio == parametros$anio_fin  ]$aporte_ivm_vap  ,
                        digits = 2, nsmall = 2, big.mark = '.', 
                        decimal.mark = ',', format = 'f' )


REP$aporte_salud_vap <- format( impacto[  anio == parametros$anio_fin  ]$aporte_salud_vap  ,
                                 digits = 2, nsmall = 2, big.mark = '.', 
                                 decimal.mark = ',', format = 'f' )
