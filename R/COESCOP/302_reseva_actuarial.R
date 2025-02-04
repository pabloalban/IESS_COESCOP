message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para la reserva matmática")
load( paste0( parametros$RData, "IESS_proy_coescop.RData" ) )
load( paste0( parametros$RData, 'IESS_tabla_mortalidad.RData' ) )
load( paste0( parametros$RData, 'ISSPOL_coeficientes.RData' ) )
#load( paste0( parametros$RData, 'IESS_consolidado_coescop.RData' ) )
message("\tCalculando reserva matemática")

#Pensionistas con derecho a IVM al corte------------------------------------------------------------

derecho_ivm <- malla_coescop %>%
  filter( a_d_ivm == 0 ) %>%
  left_join(., coeficientes, by = c('anios_imp'='anio') ) %>%
  mutate( coef = coef_isspol - coef_iess ) %>%
  mutate( coef = if_else( coef_iess > coef_isspol,
                          0,
                          coef ) ) %>%
  mutate( n = edad + a_d_ivm + 1,
          k = n - edad ) %>%
  mutate( ric = coef_isspol * salario ) %>%
  left_join(., tabla_mortalidad, by=c('edad','sexo'='sexo')) %>%
  mutate( reserva_matematica = factor * i_p_acu * a_x * coef * salario  * 13 ) %>%
  dplyr::select( anio,
                 tipo,
                 ciudad,
                 cedula,
                 edad,
                 sexo,
                 sueldo,
                 salario,
                 anios_imp,
                 i_p_acu,
                 coef,
                 coef_isspol,
                 coef_iess,
                 n,
                 k,
                 ric,
                 a_x,
                 N_x,
                 D_x,
                 reserva_matematica  ) %>%
  mutate( N_n = NA,
          N_x_mas_k = NA,
          D_x_mas_k = NA,
          a_x_n = NA,
          a_n_w = NA,
          res_mat_temporal = NA,
          res_mat_diferida = NA ) %>%
  mutate( reserva_ivm = factor * coef_iess * i_p_acu * a_x * salario  * 13 )
  
sum(derecho_ivm$reserva_matematica, na.rm = TRUE )
sum(derecho_ivm$reserva_ivm, na.rm = TRUE )

#Pensionistas sin derecho a IVM al corte------------------------------------------------------------

sin_derecho_ivm <- malla_coescop %>%
  filter( anio_derecho_coescop > 2023 ) %>%
  mutate( edad = edad_i ) %>%
  mutate( imp_tot_coescop =  a_d_coescop + anios_imp ) %>%
  left_join(., coeficientes, by = c('imp_tot_coescop'='anio') ) %>%
  mutate( coef = coef_isspol ) %>%
  dplyr::select( -coef_isspol, - coef_iess ) %>%
  left_join(., coeficientes, by = c('anios_imp'='anio') ) %>%
  mutate( coef_isspol = coef ) %>%
  mutate( coef = coef_isspol -  coef_iess ) %>%
  mutate( coef = if_else( coef_iess > coef_isspol,
                          0,
                          coef ) ) %>%
  mutate( n = edad + a_d_ivm + 1,
          k = n - edad ) %>%
  mutate( ric = coef_isspol * salario ) %>%
  left_join(., tabla_mortalidad, by=c('edad','sexo'='sexo')) %>%
  mutate( reserva_matematica = 0 ) %>%
  mutate( x_mas_k = edad + k + 1  )

aux_1 <- tabla_mortalidad %>%
  dplyr::select( edad, sexo, N_n:=N_x )

aux_2 <- tabla_mortalidad %>%
  dplyr::select( edad, sexo, N_x_mas_k:=N_x, D_x_mas_k := D_x )

sin_derecho_ivm <- sin_derecho_ivm %>%
  left_join(., aux_1, by = c('sexo'='sexo', 'n'='edad')) %>%
  left_join(., aux_2, by = c('sexo'='sexo', 'x_mas_k'='edad')) %>%
  dplyr::select( anio,
                 tipo,
               ciudad,
               cedula,
               edad,
               sexo,
               sueldo,
               salario,
               anios_imp,
               i_p_acu,
               coef,
               coef_isspol,
               coef_iess,
               n,
               k,
               ric,
               a_x,
               N_x,
               D_x,
               reserva_matematica,
               N_n,
               N_x_mas_k,
               D_x_mas_k )
#Renta anticipada y temporal------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( a_x_n = ( N_x - N_n )/ D_x )

#Renta anticipada, diferida y vitalicia-------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( a_n_w = N_x_mas_k / D_x_mas_k )

#Reserva matemática---------------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( res_mat_temporal =  factor * i_p_acu * a_x_n * 13 * coef_isspol * ( salario + 425 ) )   %>%
  mutate( res_mat_diferida = factor * i_p_acu * a_n_w * (  coef * 13 * salario ) ) %>%
  mutate( reserva_matematica = res_mat_temporal + res_mat_diferida ) %>%
  mutate( reserva_ivm = factor * coef_iess * i_p_acu * a_x * salario  * 13 )

sum( sin_derecho_ivm$reserva_matematica )
sum( sin_derecho_ivm$reserva_ivm )

#Concatenar en un RData-----------------------------------------------------------------------------
reserva_matematica <- rbind( derecho_ivm,
                             sin_derecho_ivm )
#Beneficio de montepío------------------------------------------------------------------------------

reserva_matematica <- reserva_matematica %>%
  mutate( montepio = 0.1398 * reserva_matematica ) %>%
  mutate( gastos_administrativos = 0.03 * ( reserva_matematica + montepio ) ) %>%
  mutate( reserva_total = reserva_matematica + montepio + gastos_administrativos )

#Guardar en Rdata-----------------------------------------------------------------------------------
message( '\tGuardando reservas matemáticas' )

save( reserva_matematica,
      file = paste0( parametros$RData, 'IESS_reserva_matematica.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()

