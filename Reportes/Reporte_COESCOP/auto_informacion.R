message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tEstableciendo información para la configuración del reporte' )
# Carga de información------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_balance.RData"))
load( file = paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
load( file = paste0( parametros$RData, 'COESCOP_esquema_pensional.RData' ) )
load( file = paste0( parametros$RData, 'IESS_tablas_contingencia_2.RData' ) )
load( paste0( parametros$RData, 'COESCOP_transito.RData' ) ) #trásito
load( paste0( parametros$RData, 'COESCOP_control.RData' ) ) #control

REP <- new.env()
# Demografía de los agentes de Control--------------------------------------------------------------

ciudades <- c( 'Ambato',
               'Duran',
               'Esmeraldas',
               'Guayaquil',
               'Latacunga',
               'Machala',
               'Portoviejo',
               'Quito',
               'Santo Domingo' )

nombre <- c( 'ambato',
             'duran',
             'esmeraldas',
             'guayaquil',
             'latacunga',
             'machala',
             'p_viejo',
             'quito',
             's_domingo')

for ( i in 1:length( ciudades ) ) {
  
  ciudad <- ciudades[ i ]
  
  message( '\tGenerando auto información agentes de control de ', ciudad )
  
  expr <- expression( {
    
  REP$c_nombre_dem_1 <- format( fun_auto_dem( control, {{ciudad}} )$dem_1,
                            digits = 2, nsmall = 0, big.mark = '.',
                            decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_1_m <- format( fun_auto_dem( control, {{ciudad}} )$dem_1_m,
                              digits = 2, nsmall = 0, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_1_f <- format( fun_auto_dem( control, {{ciudad}} )$dem_1_f,
                              digits = 2, nsmall = 0, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_2_m <- format( fun_auto_dem( control, {{ciudad}} )$dem_2_m,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_2_f <- format( fun_auto_dem( control, {{ciudad}} )$dem_2_f,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_3 <- format( fun_auto_dem( control, {{ciudad}} )$dem_3,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_4_1 <- format( fun_auto_dem( control, {{ciudad}} )$dem_4[ 1 ],
                              digits = 2, nsmall = 0, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_4_2 <- format( fun_auto_dem( control, {{ciudad}} )$dem_4[ 2 ],
                              digits = 2, nsmall = 0, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_4_3 <- format( fun_auto_dem( control, {{ciudad}} )$dem_4[ 3 ],
                              digits = 2, nsmall = 0, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_5_1 <- format( fun_auto_dem( control, {{ciudad}} )$dem_5[ 1 ],
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_5_2 <- format( fun_auto_dem( control, {{ciudad}} )$dem_5[ 2 ],
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  
  REP$c_nombre_dem_5_3 <- format( fun_auto_dem( control, {{ciudad}} )$dem_5[ 3 ],
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
  })

  expr <- gsub( 'nombre', nombre[ i ], deparse( expr ) )
  eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
}

# Demografía de los agentes de tránsito-------------------------------------------------------------

ciudades <- c( 'Cuenca',
               'Duran',
               'Esmeraldas',
               'Guayaquil',
               'Ibarra',
               'Latacunga',
               'Machala',
               'Quito',
               'Santo Domingo',
               'Ambato',
               'Riobamba',
               'Loja' )

nombre <- c( 'cuenca',
             'duran',
             'esmeraldas',
             'guayaquil',
             'ibarra',
             'latacunga',
             'machala',
             'quito',
             's_domingo',
             'ambato',
             'riobamba',
             'loja' )

for ( i in 1:length( ciudades ) ) {
  
  ciudad <- ciudades[ i ]
  
  message( '\tGenerando auto información agentes de tránsito de ', ciudad )
  
  expr <- expression( {
    
    REP$t_nombre_dem_1 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_1,
                                  digits = 2, nsmall = 0, big.mark = '.',
                                  decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_1_m <- format( fun_auto_dem( transito, {{ciudad}} )$dem_1_m,
                                    digits = 2, nsmall = 0, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_1_f <- format( fun_auto_dem( transito, {{ciudad}} )$dem_1_f,
                                    digits = 2, nsmall = 0, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_2_m <- format( fun_auto_dem( transito, {{ciudad}} )$dem_2_m,
                                    digits = 2, nsmall = 2, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_2_f <- format( fun_auto_dem( transito, {{ciudad}} )$dem_2_f,
                                    digits = 2, nsmall = 2, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_3 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_3,
                                  digits = 2, nsmall = 2, big.mark = '.',
                                  decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_4_1 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_4[ 1 ],
                                    digits = 2, nsmall = 0, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_4_2 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_4[ 2 ],
                                    digits = 2, nsmall = 0, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_4_3 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_4[ 3 ],
                                    digits = 2, nsmall = 0, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_5_1 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_5[ 1 ],
                                    digits = 2, nsmall = 2, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_5_2 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_5[ 2 ],
                                    digits = 2, nsmall = 2, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
    
    REP$t_nombre_dem_5_3 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_5[ 3 ],
                                    digits = 2, nsmall = 2, big.mark = '.',
                                    decimal.mark = ',', format = 'f' )
  })
  
  expr <- gsub( 'nombre', nombre[ i ], deparse( expr ) )
  eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
}

# Escenario único-----------------------------------------------------------------------------------
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

REP$aporte_ivm_vap_millones <- format( round( impacto[  anio == parametros$anio_fin  ]$aporte_ivm_vap / 1000000, 2 ),
                              digits = 2, nsmall = 2, big.mark = '.', 
                              decimal.mark = ',', format = 'f' )

REP$aporte_salud_vap <- format( impacto[  anio == parametros$anio_fin  ]$aporte_salud_vap  ,
                                 digits = 2, nsmall = 2, big.mark = '.', 
                                 decimal.mark = ',', format = 'f' )

