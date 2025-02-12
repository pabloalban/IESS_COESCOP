message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos")

# Carga de bases------------------------------------------------------------------------------------
load( paste0( parametros$RData, "IESS_reserva_matematica.RData" ) ) 
load( paste0( parametros$RData, "IESS_proy_coescop.RData" ) )

message( '\tGenerando Balance corriente y actuarial' )

# 0. Parámetros-------------------------------------------------------------------------------------
i_a <- 0.0625

# 1. Aportes desde 2023 por el COESCOP--------------------------------------------------------------

aportes <- malla_ivm %>%
  filter( anio >= 2023 & anio <= 2042 ) %>%
  group_by( anio ) %>%
  mutate( aporte_ivm = sum( aporte_ivm, na.rm = TRUE ),
          aporte_salud = sum( aporte_salud, na.rm = TRUE ),
          aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 aporte_ivm,
                 aporte_salud,
                 aporte_coescop )

aportes_ins <- malla_ivm %>%
  filter( anio >= 2023 & anio <= 2042 ) %>%
  group_by( anio, tipo ) %>%
  mutate( aporte_ivm = sum( aporte_ivm, na.rm = TRUE ),
          aporte_salud = sum( aporte_salud, na.rm = TRUE ),
          aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., tipo, anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 tipo,
                 aporte_ivm,
                 aporte_salud,
                 aporte_coescop )

aportes_transito <- malla_ivm %>%
  filter( anio >= 2023 & anio <= 2042, tipo == 'transito' ) %>%
  group_by( anio, ciudad ) %>%
  mutate( aporte_ivm = sum( aporte_ivm, na.rm = TRUE ),
          aporte_salud = sum( aporte_salud, na.rm = TRUE ),
          aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., ciudad, anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 ciudad,
                 aporte_ivm,
                 aporte_salud,
                 aporte_coescop )

aportes_control <- malla_ivm %>%
  filter( anio >= 2023 & anio <= 2042, tipo == 'control' ) %>%
  group_by( anio, ciudad ) %>%
  mutate( aporte_ivm = sum( aporte_ivm, na.rm = TRUE ),
          aporte_salud = sum( aporte_salud, na.rm = TRUE ),
          aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., ciudad, anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 ciudad,
                 aporte_ivm,
                 aporte_salud,
                 aporte_coescop )

aportes_bomberos <- malla_ivm %>%
  filter( anio >= 2023 & anio <= 2042, tipo == 'bombero' ) %>%
  group_by( anio, ciudad ) %>%
  mutate( aporte_ivm = sum( aporte_ivm, na.rm = TRUE ),
          aporte_salud = sum( aporte_salud, na.rm = TRUE ),
          aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., ciudad, anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 ciudad,
                 aporte_ivm,
                 aporte_salud,
                 aporte_coescop )


jubilados_ins <- malla_coescop %>%
  filter( anio >= 2023 & anio <= 2042 ) %>%
  group_by( anio, tipo ) %>%
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., tipo, anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, tipo,  n_old ) %>%
  arrange( tipo, anio )


jubilados <- malla_coescop %>%
  filter( anio >= 2023 & anio <= 2042 ) %>%
  group_by( anio ) %>%
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, n_old ) %>%
  arrange( anio )

jubilados_transito <- malla_coescop %>%
  filter( anio >= 2023 & anio <= 2042, tipo == 'transito' ) %>%
  group_by( anio, ciudad ) %>%
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., ciudad, anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, ciudad,  n_old ) %>%
  arrange( ciudad, anio )

reserva_tipo_anio <- reserva_matematica %>% 
  group_by( anio, tipo ) %>% 
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos = sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, tipo, .keep_all = TRUE ) %>% 
  dplyr::select( anio, tipo, reserva_matematica, montepio, gastos_administrativos, egreso_total ) %>% 
  arrange( tipo, anio )

reserva_transito <- reserva_matematica %>% 
  filter( tipo == 'transito' ) %>% 
  group_by( anio, ciudad ) %>% 
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos = sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>% 
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  distinct( anio, ciudad, .keep_all = TRUE ) %>% 
  dplyr::select( anio, ciudad, n_old, reserva_matematica, montepio, gastos_administrativos, egreso_total ) %>% 
  arrange( ciudad, anio )

reserva_control <- reserva_matematica %>% 
  filter( tipo == 'control' ) %>% 
  group_by( anio, ciudad ) %>% 
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos = sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>% 
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  distinct( anio, ciudad, .keep_all = TRUE ) %>% 
  dplyr::select( anio, ciudad, n_old, reserva_matematica, montepio, gastos_administrativos, egreso_total ) %>% 
  arrange( ciudad, anio )

reserva_bombero <- reserva_matematica %>% 
  filter( tipo == 'bombero' ) %>% 
  group_by( anio, ciudad ) %>% 
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos = sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>% 
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  distinct( anio, ciudad, .keep_all = TRUE ) %>% 
  dplyr::select( anio, ciudad, n_old, reserva_matematica, montepio, gastos_administrativos, egreso_total ) %>% 
  arrange( ciudad, anio )

reserva_matematica_ins <- reserva_matematica %>%
  group_by( anio, tipo ) %>%
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos =  sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>%
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, tipo, .keep_all = TRUE ) %>%
  dplyr::select( anio,
                 tipo,
                 n_old,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total )

# 2. Impacto----------------------------------------------------------------------------------------

impacto <- aportes %>%
  dplyr::select( -aporte_coescop ) %>%
  mutate( total_cor = aporte_ivm + aporte_salud ) %>%
  mutate( r = 1 + i_a ) %>%
  mutate( r = if_else( anio == '2023',
                       1, 
                       r ) ) %>%
  mutate( r = cumprod( r ) ) %>%
  mutate( v = 1 / r ) %>%
  mutate( aporte_ivm_vap = cumsum( v * aporte_ivm ),
          aporte_salud_vap = cumsum( v * aporte_salud ),
          total_vap = cumsum( v * total_cor ) )


## 2.1. Impacto por entidad-------------------------------------------------------------------------

aux <- malla_ivm %>%
  filter( anio >= 2023 & anio <= 2042 ) %>%
  group_by( anio, tipo ) %>%
  mutate( aporte_ivm = sum( aporte_ivm, na.rm = TRUE ),
          aporte_salud = sum( aporte_salud, na.rm = TRUE ),
          aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., anio, tipo,  .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 tipo,
                 aporte_ivm,
                 aporte_salud,
                 aporte_coescop ) %>%
  dplyr::select( -aporte_coescop ) %>%
  mutate( total_cor = aporte_ivm + aporte_salud ) %>%
  group_by( tipo ) %>%
  mutate( r = 1 + i_a ) %>%
  mutate( r = if_else( anio == '2023',
                       1, 
                       r ) ) %>%
  mutate( r = cumprod( r ) ) %>%
  mutate( v = 1 / r ) %>%
  mutate( aporte_ivm_vap = cumsum( v * aporte_ivm ),
          aporte_salud_vap = cumsum( v * aporte_salud ),
          total_vap = cumsum( v * total_cor ) ) %>%
  ungroup( )

impacto_entidades <- aux %>%
  filter( anio == 2042)

sum(impacto_entidades$total_vap)

sum(impacto_entidades$aporte_ivm)

#1. Balance-----------------------------------------------------------------------------------------
reserva_mat_anual <- reserva_matematica %>%
  group_by( anio ) %>%
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos =  sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total )


balance_anual <- data.frame( anio = c( 2023:2042 ) ) %>%
  left_join( ., aportes, by = 'anio' ) %>%
  left_join( ., jubilados, by = 'anio' ) %>%
  left_join( .,reserva_mat_anual , by = 'anio' ) %>%
  mutate( reserva_a_pagar = egreso_total - aporte_coescop ) %>%
  mutate( ingresos_total =  aporte_coescop + reserva_a_pagar ) %>%
  mutate( V_cor = ingresos_total - egreso_total ) %>%
  dplyr::select( anio,
                 n_old,
                 aporte_coescop,
                 reserva_a_pagar,
                 ingresos_total,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total,
                 V_cor ) %>%
  mutate( r = 1 + i_a ) %>%
  mutate( r = if_else( anio == '2023',
                       1, 
                       r ) ) %>%
  mutate( r = cumprod( r ) ) %>%
  mutate( v = 1 / r ) %>%
  mutate( aporte_coescop_vap = cumsum( v * aporte_coescop  ),
          reserva_a_pagar_vap = cumsum( v * reserva_a_pagar  ),
          ingresos_total_vap = cumsum( v * ingresos_total  ),
          reserva_matematica_vap = cumsum( v * reserva_matematica   ),
          montepio_vap = cumsum( v * montepio   ),
          gastos_administrativos_vap = cumsum( v * gastos_administrativos   ),
          egreso_total_vap = cumsum( v * egreso_total  ),
          V_cap = r * cumsum( v * V_cor ),
          V = v *  V_cap )

# 2. Aportes por instituciones----------------------------------------------------------------------

aportes_ins <- malla_ivm %>%
  filter( anio >= 2023 & anio <= 2042 ) %>%
  group_by( anio, tipo ) %>%
  mutate( aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., anio, tipo, .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 tipo,
                 aporte_coescop )


reserva_matematica_ins <- reserva_matematica %>%
  group_by( anio, tipo ) %>%
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos =  sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>%
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, tipo, .keep_all = TRUE ) %>%
  dplyr::select( anio,
                 tipo,
                 n_old,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total )


aux <- aportes_ins %>%
  left_join( ., reserva_matematica_ins, by = c('anio', 'tipo') )

# 3. Función Balance anual por institución----------------------------------------------------------

fun_balance_anual <- function(.data, sector) {
  
  a <- data.frame( anio = c(2023:2042) ) %>% 
    left_join(., aux, by = 'anio' ) %>% 
    filter( tipo == {{sector}} ) %>% 
    #filter( tipo == 'aduanero' ) %>% 
    mutate_if( is.numeric , replace_na, replace = 0) %>%
    mutate( reserva_a_pagar = egreso_total - aporte_coescop ) %>%
    mutate( ingresos_total =  aporte_coescop + reserva_a_pagar ) %>%
    mutate( V_cor = ingresos_total - egreso_total ) %>%
    dplyr::select( anio,
                   n_old,
                   aporte_coescop,
                   reserva_a_pagar,
                   ingresos_total,
                   reserva_matematica,
                   montepio,
                   gastos_administrativos,
                   egreso_total,
                   V_cor ) %>%
    mutate( r = 1 + i_a ) %>%
    mutate( r = if_else( anio == '2023',
                         1, 
                         r ) ) %>%
    mutate( r = cumprod( r ) ) %>%
    mutate( v = 1 / r ) %>%
    mutate( aporte_coescop_vap = cumsum( v * aporte_coescop  ),
            reserva_a_pagar_vap = cumsum( v * reserva_a_pagar  ),
            ingresos_total_vap = cumsum( v * ingresos_total  ),
            reserva_matematica_vap = cumsum( v * reserva_matematica   ),
            montepio_vap = cumsum( v * montepio   ),
            gastos_administrativos_vap = cumsum( v * gastos_administrativos   ),
            egreso_total_vap = cumsum( v * egreso_total  ),
            V_cap = r * cumsum( v * V_cor ),
            V = v *  V_cap )

    return( a )
}


b_adu <- fun_balance_anual(aux, 'aduanero') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )
b_bom <- fun_balance_anual(aux, 'bombero') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )
b_cte <- fun_balance_anual(aux, 'cte') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )
b_control <- fun_balance_anual(aux, 'control') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )
b_transito <- fun_balance_anual(aux, 'transito') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )
b_snmlcf <- fun_balance_anual(aux, 'snmlcf') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )

valor_a_pagar_ins <- data.frame( tipo = c('Cuerpo de Vigilancia Aduanera',
                                          'Cuerpos de Bomberos',
                                          'Comisión de Tránsito del Ecuador',
                                          'Cuerpo de Agentes de Control Municipal o Metropolitano',
                                          'Cuerpos de Agentes Civiles de Tránsito',
                                          'Servicio Nacional de Medicina Legal y Ciencias Forenses'),
                                 reserva_a_pagar_vap = rbind( b_adu,
                                                              b_bom,
                                                              b_cte,
                                                              b_control,
                                                              b_transito,
                                                              b_snmlcf ) ) %>%
  rbind( ., c( "Total", as.character(sum(.[,2],  na.rm =TRUE ) ) ) ) %>%
  mutate_at( c( 2 ), as.numeric )


balance_adu <- fun_balance_anual(aux, 'aduanero') %>%  dplyr::select( anio, reserva_a_pagar_vap  )
balance_bom <- fun_balance_anual(aux, 'bombero') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )
balance_cte <- fun_balance_anual(aux, 'cte') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )
balance_control <- fun_balance_anual(aux, 'control') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )
balance_transito <- fun_balance_anual(aux, 'transito')
balance_snmlcf <- fun_balance_anual(aux, 'snmlcf') %>% filter( anio == 2042) %>% dplyr::select( reserva_a_pagar_vap  )

#4. Valor a pagar por ciudad en Cuerpos de Agentes Civiles de Transito---------------------------------

aux <- aportes_transito %>%
  left_join( ., reserva_transito, by = c('anio', 'ciudad') )

fun_balance_anual <- function(.data, ciudad ) {
  
  a <- data.frame( anio = c(2023:2042) ) %>% 
    left_join(., aux, by = 'anio' ) %>%
    #filter( tipo == 'transito' ) %>% 
    filter( ciudad == {{ciudad}} ) %>% 
    mutate_if( is.numeric , replace_na, replace = 0) %>%
    mutate( reserva_a_pagar = egreso_total - aporte_coescop ) %>%
    mutate( ingresos_total =  aporte_coescop + reserva_a_pagar ) %>%
    mutate( V_cor = ingresos_total - egreso_total ) %>%
    dplyr::select( anio,
                   ciudad,
                   n_old,
                   aporte_coescop,
                   reserva_a_pagar,
                   ingresos_total,
                   reserva_matematica,
                   montepio,
                   gastos_administrativos,
                   egreso_total,
                   V_cor ) %>%
    mutate( r = 1 + i_a ) %>%
    mutate( r = if_else( anio == '2023',
                         1, 
                         r ) ) %>%
    mutate( r = cumprod( r ) ) %>%
    mutate( v = 1 / r ) %>%
    mutate( aporte_coescop_vap = cumsum( v * aporte_coescop  ),
            reserva_a_pagar_vap = cumsum( v * reserva_a_pagar  ),
            ingresos_total_vap = cumsum( v * ingresos_total  ),
            reserva_matematica_vap = cumsum( v * reserva_matematica   ),
            montepio_vap = cumsum( v * montepio   ),
            gastos_administrativos_vap = cumsum( v * gastos_administrativos   ),
            egreso_total_vap = cumsum( v * egreso_total  ),
            V_cap = r * cumsum( v * V_cor ),
            V = v *  V_cap )
  
  return( a )
}


balance_cuenca <- fun_balance_anual( aux, c( 'Cuenca' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_duran <- fun_balance_anual( aux, c( 'Duran' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_esmeraldas <- fun_balance_anual( aux, c( 'Esmeraldas' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_guayaquil <- fun_balance_anual( aux, c( 'Guayaquil' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_ibarra <- fun_balance_anual( aux, c( 'Ibarra' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_latacunga <- fun_balance_anual( aux, c( 'Latacunga' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_machala <- fun_balance_anual( aux, c( 'Machala' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_quito <- fun_balance_anual( aux, c( 'Quito' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_s_domingo <- fun_balance_anual( aux, c( 'Santo Domingo' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_ambato <- fun_balance_anual( aux, c( 'Ambato' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_riobamba <- fun_balance_anual( aux, c( 'Riobamba' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_loja <- fun_balance_anual( aux, c( 'Loja' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )

balance_transito_corriente <- rbind( balance_cuenca,
                           balance_duran,
                           balance_esmeraldas,
                           balance_guayaquil,
                           balance_ibarra,
                           balance_latacunga,
                           balance_machala,
                           balance_quito,
                           balance_s_domingo,
                           balance_ambato,
                           balance_riobamba,
                           balance_loja ) %>% 
  dplyr::select( anio, ciudad, reserva_a_pagar ) %>% 
  mutate( reserva_a_pagar = if_else( reserva_a_pagar < 0,
                                     0,
                                     reserva_a_pagar ) ) %>% 
  pivot_wider( names_from = ciudad, values_from = reserva_a_pagar, names_prefix = "" )

#Resumen Valor a pagar por ciudad en Cuerpos de Agentes Civiles de Transito

valor_transferir_transito_vap <- rbind( balance_cuenca,
                                        balance_duran,
                                        balance_esmeraldas,
                                        balance_guayaquil,
                                        balance_ibarra,
                                        balance_latacunga,
                                        balance_machala,
                                        balance_quito,
                                        balance_s_domingo,
                                        balance_ambato,
                                        balance_riobamba,
                                        balance_loja ) %>% 
  filter( anio == '2042' ) %>% 
  dplyr::select( ciudad, reserva_a_pagar_vap )  %>%
  rbind( ., c( "Total", as.character( sum( .[,2], na.rm =TRUE ) ) ) ) %>% 
  mutate_at( c( 2:ncol( . ) ), as.numeric ) 

#5. Valor a pagar por ciudad en Cuerpos de Control--------------------------------------------------

aux <- aportes_control %>%
  left_join( ., reserva_control, by = c('anio', 'ciudad') )

#balance_cuenca <- fun_balance_anual( aux, c( 'Cuenca' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_ambato <- fun_balance_anual( aux, c( 'Ambato' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_duran <- fun_balance_anual( aux, c( 'Duran' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_esmeraldas <- fun_balance_anual( aux, c( 'Esmeraldas' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_guayaquil <- fun_balance_anual( aux, c( 'Guayaquil' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
#balance_ibarra <- fun_balance_anual( aux, c( 'Ibarra' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_latacunga <- fun_balance_anual( aux, c( 'Latacunga' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_machala <- fun_balance_anual( aux, c( 'Machala' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_quito <- fun_balance_anual( aux, c( 'Quito' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_s_domingo <- fun_balance_anual( aux, c( 'Santo Domingo' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
#balance_riobamba <- fun_balance_anual( aux, c( 'Riobamba' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
#balance_loja <- fun_balance_anual( aux, c( 'Loja' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )

balance_control_corriente <- rbind( balance_ambato,
                                    balance_duran,
                                     balance_esmeraldas,
                                     balance_guayaquil,
                                     #balance_ibarra,
                                     balance_latacunga,
                                     balance_machala,
                                     balance_quito,
                                     balance_s_domingo ) %>% 
  dplyr::select( anio, ciudad, reserva_a_pagar ) %>% 
  mutate( reserva_a_pagar = if_else( reserva_a_pagar < 0,
                                     0,
                                     reserva_a_pagar ) ) %>% 
  pivot_wider( names_from = ciudad, values_from = reserva_a_pagar, names_prefix = "" )


valor_transferir_control_vap <-  rbind( balance_ambato,
                                        balance_duran,
                                        balance_esmeraldas,
                                        balance_guayaquil,
                                        #balance_ibarra,
                                        balance_latacunga,
                                        balance_machala,
                                        balance_quito,
                                        balance_s_domingo ) %>% 
  filter( anio == '2042' ) %>% 
  dplyr::select( ciudad, reserva_a_pagar_vap )  %>%
  rbind( ., c( "Total", as.character( sum( .[,2], na.rm =TRUE ) ) ) ) %>% 
  mutate_at( c( 2:ncol( . ) ), as.numeric )


#6. Valor a pagar por ciudad en Cuerpos de Bomberos-------------------------------------------------

aux <- aportes_bomberos %>%
  left_join( ., reserva_bombero, by = c('anio', 'ciudad') )


balance_ambato <- fun_balance_anual( aux, c( 'Ambato' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_cuenca <- fun_balance_anual( aux, c( 'Cuenca' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
#balance_duran <- fun_balance_anual( aux, c( 'Duran' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
#balance_esmeraldas <- fun_balance_anual( aux, c( 'Esmeraldas' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_guayaquil <- fun_balance_anual( aux, c( 'Guayaquil' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_ibarra <- fun_balance_anual( aux, c( 'Ibarra' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_loja <- fun_balance_anual( aux, c( 'Loja' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
#balance_latacunga <- fun_balance_anual( aux, c( 'Latacunga' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_machala <- fun_balance_anual( aux, c( 'Machala' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_manta <- fun_balance_anual( aux, c( 'Manta' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_milagro <- fun_balance_anual( aux, c( 'Milagro' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_p_viejo <- fun_balance_anual( aux, c( 'Portoviejo' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_quito <- fun_balance_anual( aux, c( 'Quito' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_riobamba <- fun_balance_anual( aux, c( 'Riobamba' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )
balance_s_domingo <- fun_balance_anual( aux, c( 'Santo Domingo' ) ) %>%  dplyr::select( anio, ciudad, reserva_a_pagar, reserva_a_pagar_vap )

balance_bombero_corriente <- rbind( balance_ambato,
                                    balance_cuenca,
                                    balance_guayaquil,
                                    balance_ibarra,
                                    balance_loja,
                                    balance_machala,
                                    balance_manta,
                                    balance_milagro,
                                    balance_p_viejo,
                                    balance_quito,
                                    balance_riobamba,
                                    balance_s_domingo ) %>% 
  dplyr::select( anio, ciudad, reserva_a_pagar ) %>% 
  mutate( reserva_a_pagar = if_else( reserva_a_pagar < 0,
                                     0,
                                     reserva_a_pagar ) ) %>% 
  pivot_wider( names_from = ciudad, values_from = reserva_a_pagar, names_prefix = "" )


valor_transferir_bombero_vap <-  rbind( balance_ambato,
                                        balance_cuenca,
                                        balance_guayaquil,
                                        balance_ibarra,
                                        balance_loja,
                                        balance_machala,
                                        balance_manta,
                                        balance_milagro,
                                        balance_p_viejo,
                                        balance_quito,
                                        balance_riobamba,
                                        balance_s_domingo ) %>% 
  filter( anio == '2042' ) %>% 
  dplyr::select( ciudad, reserva_a_pagar_vap )  %>%
  rbind( ., c( "Total", as.character( sum( .[,2], na.rm =TRUE ) ) ) ) %>% 
  mutate_at( c( 2:ncol( . ) ), as.numeric )
# Balance por institución---------------------------------------------------------------------------

balance_anual_ins <- expand.grid( anio = c( 2023:2042 ),
                                 tipo = c( 'aduanero',
                                           'bombero',
                                           'control',
                                           'cte',
                                           'snmlcf',
                                           'transito' ) ) %>%
  left_join( ., aportes_ins, by = c( 'anio', 'tipo' ) ) %>%
  left_join( ., jubilados_ins, by = c( 'anio', 'tipo' ) ) %>%
  left_join( ., reserva_tipo_anio , by = c( 'anio', 'tipo' ) ) %>%
  group_by( anio, tipo ) %>%
  replace( is.na( . ), 0 ) %>% 
  mutate( reserva_a_pagar = egreso_total - aporte_coescop ) %>%
  mutate( ingresos_total =  aporte_coescop + reserva_a_pagar ) %>%
  mutate( V_cor = ingresos_total - egreso_total ) %>%
  ungroup( ) %>% 
  dplyr::select( anio,
                 tipo,
                 n_old,
                 aporte_coescop,
                 reserva_a_pagar,
                 ingresos_total,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total,
                 V_cor ) %>%
  arrange( tipo, anio ) %>% 
  group_by( tipo ) %>% 
  mutate( r = 1 + i_a ) %>%
  mutate( r = if_else( anio == '2023',
                       1, 
                       r ) ) %>%
  mutate( r = cumprod( r ) ) %>%
  mutate( v = 1 / r ) %>%
  mutate( aporte_coescop_vap = cumsum( v * aporte_coescop  ),
          reserva_a_pagar_vap = cumsum( v * reserva_a_pagar  ),
          ingresos_total_vap = cumsum( v * ingresos_total  ),
          reserva_matematica_vap = cumsum( v * reserva_matematica   ),
          montepio_vap = cumsum( v * montepio   ),
          gastos_administrativos_vap = cumsum( v * gastos_administrativos   ),
          egreso_total_vap = cumsum( v * egreso_total  ),
          V_cap = r * cumsum( v * V_cor ),
          V = v *  V_cap ) %>% 
  ungroup( )

#Guardar en Rdata-----------------------------------------------------------------------------------
message( '\tGuardando balance' )

save( impacto,
      balance_anual,
      valor_a_pagar_ins,
      balance_transito_corriente,
      valor_transferir_transito_vap,
      balance_control_corriente,
      valor_transferir_control_vap,
      balance_control_corriente,
      valor_transferir_control_vap,
      balance_bombero_corriente,
      valor_transferir_bombero_vap,
      balance_anual_ins,
      file = paste0( parametros$RData, 'IESS_balance.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()