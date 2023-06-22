message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para la reserva matmática")
load(paste0(parametros$RData, "IESS_proy_coescop.RData"))
load(paste0( parametros$RData, 'IESS_tabla_mortalidad.RData'))
load(paste0( parametros$RData, 'ISSPOL_coeficientes.RData'))

message("\tCalculando reserva matemática")

#Pensionistas con derecho a IVM al corte------------------------------------------------------------

derecho_ivm <- malla_coescop %>%
  filter( a_d_ivm == 0 ) %>%
  left_join(., coeficientes, by = c('anios_imp'='anio') ) %>%
  mutate( coef = coef_isspol - coef_iess ) %>%
  mutate( n = edad + a_d_ivm + 1,
          k = n - edad ) %>%
  mutate( ric = coef_isspol * salario ) %>%
  left_join(., tabla_mortalidad, by=c('edad','sexo'='g')) %>%
  mutate( reserva_matematica = a_x * (coef) * ric  * 13 ) %>%
  dplyr::select( cedula,
                 sueldo,
                 salario,
                 coef,
                 coef_isspol,
                 coef_iess,
                 n,
                 k,
                 ric,
                 a_x,
                 N_x,
                 D_x,
                 reserva_matematica  )


#Pensionistas sin derecho a IVM al corte------------------------------------------------------------
sin_derecho_ivm <- nomina %>%
  left_join(., aux, by =c('cedula')) %>%
  filter( edad_derecho_ivm > edad ) %>%
  group_by( cedula ) %>%
  filter( periodo == max(periodo)) %>%
  distinct( cedula, .keep_all = TRUE ) %>%
  ungroup() %>%
  mutate( coef = ric_ivm / ric ) %>%
  mutate( coef = ifelse( coef > 1, 1, coef ) ) %>%
  dplyr::select( id,
                 cedula,
                 sexo,
                 ric,
                 fecha_derecho_ivm,
                 edad_derecho_ivm,
                 coef,
                 edad,
                 pension_aumentos,
                 n, 
                 k) %>%
  mutate( x_mas_k = edad + k + 1  ) %>%
  left_join(., tabla_mortalidad, by=c('edad','sexo'='g'))

aux_1 <- tabla_mortalidad %>%
  dplyr::select( edad, g, N_n:=N_x )

aux_2 <- tabla_mortalidad %>%
  dplyr::select( edad, g, N_x_mas_k:=N_x, D_x_mas_k := D_x )

sin_derecho_ivm <- sin_derecho_ivm %>%
  left_join(., aux_1, by = c('sexo'='g', 'n'='edad')) %>%
  left_join(., aux_2, by = c('sexo'='g', 'x_mas_k'='edad')) %>%
  mutate( renta_ivm = coef * pension_aumentos,
          renta_ce = ( 1-coef ) * pension_aumentos )

#Renta anticipada y temporal------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( a_x_n = ( N_x - N_n )/ D_x )

#Renta anticipada, diferida y vitalicia-------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( a_n_w = N_x_mas_k / D_x_mas_k )

#Reserva matemática---------------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( res_mat_temporal =  a_x_n * ( 13 * pension_aumentos + 425 ) )  %>%
  mutate( res_mat_diferida = a_n_w * ( (1 - coef) * 13 * pension_aumentos ) ) %>%
  mutate( reserva_matematica = res_mat_temporal + res_mat_diferida )

#Pensionistas fallecidos----------------------------------------------------------------------------
reserva_fallecidos <- fallecidos %>%
          dplyr::select( id,
                        cedula,
                        edad,
                        sexo,
                        f1_renta,
                        fecha_derecho_ivm,
                        edad_derecho_ivm,
                        ric_ivm) %>%
  left_join(., actualizacion_pensiones %>%
              filter( anio == 2022) %>%
              dplyr::select( cedula, coef), by = 'cedula' ) %>%
  mutate( renta_concedida = ric_ivm / coef ) %>%
  mutate( renta_ce  = (1-coef) * renta_concedida,
          renta_ivm = coef * renta_concedida ) %>%
  left_join( ., tabla_mortalidad, by = c('sexo'='g', 'edad') ) %>%
  mutate( reserva_matematica = a_x * renta_ce * 13 * 0.1398 )

#Concatenar en un RData-----------------------------------------------------------------------------
reserva_matematica <- rbind( derecho_ivm %>%
                               mutate( N_n = NA,
                                       N_x_mas_k = NA,
                                       k = NA,
                                       n = NA,
                                       res_mat_temporal = NA,
                                       res_mat_diferida = NA,
                                       a_x_n = NA,
                                       a_n_w = NA
                                       ) %>%
                               dplyr::select( id,
                                              cedula,
                                              edad,
                                              sexo,
                                              #f1_renta,
                                              fecha_derecho_ivm,
                                              edad_derecho_ivm,
                                              renta_ivm,
                                              renta_ce,
                                              pension_aumentos,
                                              k,
                                              a_x_n,
                                              n,
                                              res_mat_temporal,
                                              a_n_w,
                                              res_mat_diferida,
                                              a_x,
                                              reserva_matematica ),
                             sin_derecho_ivm %>%
                               mutate( a_x = NA ) %>%
                               dplyr::select( id,
                                              cedula,
                                              edad,
                                              sexo,
                                              #f1_renta,
                                              fecha_derecho_ivm,
                                              edad_derecho_ivm,
                                              renta_ivm,
                                              renta_ce,
                                              pension_aumentos,
                                              k,
                                              a_x_n,
                                              n,
                                              res_mat_temporal,
                                              a_n_w,
                                              res_mat_diferida,
                                              a_x,
                                              reserva_matematica ) )

#Beneficio de montepío------------------------------------------------------------------------------


aux1 <- reserva_matematica %>%
  filter( id < 47 ) %>%
  mutate( montepio = 0.1398 * reserva_matematica ) 

aux2 <- reserva_fallecidos %>%
  filter( id %in% c(48,47,49) ) %>%
  dplyr::select( cedula,  montepio:= reserva_matematica)

aux3 <- reserva_matematica %>%
  filter( id %in% c(48,47,49) ) %>%
  left_join(., aux2, by = 'cedula')



reserva_matematica <- rbind( aux1, aux3 )
#Guardar en Rdata-----------------------------------------------------------------------------------
message( '\tGuardando reservas matemáticas' )

save( reserva_matematica,
      file = paste0( parametros$RData, 'IESS_reserva_matematica.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()