message(paste(rep("-", 100), collapse = ""))

message("\tCargando servidores COESCOP")

# Carga de bases------------------------------------------------------------------------------------
load( paste0( parametros$RData, "IESS_consolidado_coescop.RData" ) ) 
load( paste0( parametros$RData, "ILO_pension_inputs.RData" ) )
load(paste0( parametros$RData, 'IESS_tabla_mortalidad.RData'))

##Probabilidades de transición----------------------------------------------------------------------
#q^{s}_{g,x,t} : Probability of death in the interval from t to t+1 for an individual of the group g of sex s and age x at time t.
#ir^{s}_{g,x,t} : Probability of incapacitating disability arriving in the interval from t to t+1 for an individual of the group g of sex s and age x at time t (input by the user).
#er^{s}_{g,x,t} : Probability of leaving the active contributing population for any reason other than death or disability in the interval from t to t+1, for an individual of group g of sex s and age x at time t
#qi^{s}_{g,x,t} :  Probability of death in the interval from t to t+1 for an inactive contributor, an old-age pensioner or a disability pensioner of sex s and age x at time t.


#Probabilidad de salida por muerte, incapacidad y inactividad---------------------------------------

i_p <- q %>%
  left_join( ., ir, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., er, by = c( 't', 'sexo', 'x' ) ) %>%
  mutate( i_p = (1-q/10 )* (1-ir) ) %>%
  dplyr::select(-q, -ir, -er) %>%
  mutate( sexo = if_else( sexo == 'M',
                          'F',
                          'M' ) )

#Fecha de derecho-----------------------------------------------------------------------------------

coescop <- coescop %>%
  mutate( anios_imp = floor(numimp/12) ) %>%
  mutate( a_d_coescop = if_else( anios_imp > 20,
          0,
          20 - anios_imp ) ) %>%
  mutate( anio_derecho_coescop = 2023 + a_d_coescop ) %>%
  mutate( a_d_ivm = NA ) %>%
  mutate( a_d_ivm = ifelse(  anios_imp >= 30  & edad >= 60,
                             0,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = if_else( anios_imp>=15 & anios_imp<=29 & edad >= 65,
                             0,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = if_else( anios_imp>=10 & anios_imp<=14 & edad >= 70,
                             0,
                             a_d_ivm ) ) %>%
  mutate( a = 60-edad,
          b = 30-anios_imp ) %>%
  mutate( a = if_else( a<0,
                       0,
                       a),
          b = if_else( b<0,
                       0,
                       b ) ) %>%
  rowwise() %>%
  mutate( a_d_ivm = if_else( is.na( a_d_ivm ),
                             max(a,b),
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = if_else( edad + a_d_ivm >= 65 & (a_d_ivm + anios_imp) >= 15,
                             65 - edad,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = if_else( edad + a_d_ivm >= 65 & (a_d_ivm + anios_imp) < 10,
                             70 - edad,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = if_else( a_d_ivm < 0,
                             0,
                             a_d_ivm ) ) %>%
  mutate( anio_derecho_ivm = 2023 + a_d_ivm ) %>%
  mutate(across('sueldo', str_replace, ',', '')) %>%
  mutate( sueldo = as.numeric( sueldo )) %>%
  filter( sueldo > 400 )

#Generar malla para IVM-----------------------------------------------------------------------------

malla_ivm <- coescop %>%
  mutate( anio_f1 = 2023 ) %>%
  mutate( i = a_d_ivm + 1) %>%
  dplyr::slice(rep( 1 : n(), i ) ) %>%
  group_by(cedula) %>%
  mutate( contador = 1:n() ) %>%
  mutate( anio = contador + anio_f1 - 1 ) %>%
  ungroup() %>%
  mutate( edad_i = edad + contador - 1) %>%
  left_join(., i_p, by = c( 'anio'='t', 'edad_i'='x', 'sexo'='sexo' ) ) %>%
  group_by( cedula ) %>%
  mutate( i_p_acu = cumprod( i_p ) ) %>%
  ungroup( ) %>%
  mutate( salario = sueldo * (1 + 0.0253)^contador ) %>%
  mutate( aporte_ivm = 0.1096 * salario,
          aporte_salud = 0.0516 * salario ) %>%
  mutate( aporte_ivm = if_else( anio > anio_derecho_coescop,
                                aporte_ivm * i_p_acu,
                                0 ) ) %>%
  mutate( aporte_salud = if_else( anio > anio_derecho_coescop,
                                aporte_salud * i_p_acu,
                                0 ) ) %>%
  mutate( aporte_coescop = if_else( anio > anio_derecho_coescop,
                                    0.1290 * salario * i_p_acu,
                                    0 ) )
  

#Generación de la malla para COESCOP----------------------------------------------------------------

malla_coescop <- coescop %>%
  mutate( anio_f1 = 2023 ) %>%
  mutate( i = a_d_coescop + 1) %>%
  dplyr::slice(rep( 1 : n(), i ) ) %>%
  group_by(cedula) %>%
  mutate( contador = 1:n() ) %>%
  mutate( anio = contador + anio_f1 - 1 ) %>%
  ungroup() %>%
  mutate( edad_i = edad + contador - 1) %>%
  left_join(., i_p, by = c( 'anio'='t', 'edad_i'='x', 'sexo'='sexo' ) ) %>%
  group_by( cedula ) %>%
  mutate( i_p_acu = cumprod( i_p ) ) %>%
  ungroup( ) %>%
  mutate( salario = sueldo * (1 + 0.0253)^contador ) %>%
  filter( anio == anio_derecho_coescop )

#Guardar en Rdata-----------------------------------------------------------------------------------
save( coescop,
      factor,
      malla_coescop,
      malla_ivm,
      file = paste0( parametros$RData, 'IESS_proy_coescop.RData' ) )

# #-------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
