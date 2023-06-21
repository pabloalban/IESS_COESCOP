message(paste(rep("-", 100), collapse = ""))

message("\tCargando servidores COESCOP")

# Carga de bases------------------------------------------------------------------------------------
load( paste0( parametros$RData, "IESS_consolidado_coescop.RData" ) ) 
load( paste0( parametros$RData, "ILO_pension_inputs.RData" ) )



##Probabilidades de transición----------------------------------------------------------------------
#q^{s}_{g,x,t} : Probability of death in the interval from t to t+1 for an individual of the group g of sex s and age x at time t.
#ir^{s}_{g,x,t} : Probability of incapacitating disability arriving in the interval from t to t+1 for an individual of the group g of sex s and age x at time t (input by the user).
#er^{s}_{g,x,t} : Probability of leaving the active contributing population for any reason other than death or disability in the interval from t to t+1, for an individual of group g of sex s and age x at time t
#qi^{s}_{g,x,t} :  Probability of death in the interval from t to t+1 for an inactive contributor, an old-age pensioner or a disability pensioner of sex s and age x at time t.


#Probabilidad de salida por muerte, incapacidad y inactividad---------------------------------------

i_p <- q %>%
  left_join( ., ir, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., er, by = c( 't', 'sexo', 'x' ) ) %>%
  mutate( i_p = (1 -q) * (1-ir) * (1-er) )

#Fecha de derecho-----------------------------------------------------------------------------------

aux <- coescop %>%
  mutate( anios_imp = floor(numimp/12) ) %>%
  mutate( a_d_coescop = if_else( anios_imp > 20,
          0,
          20 - anios_imp ) ) %>%
  mutate( anio_derecho_coescop = 2023 + a_d_coescop ) %>%
  mutate( a_d_ivm = 60 - edad ) %>%
  mutate( a_d_ivm = ifelse(  a_d_ivm < 0,
                             60 - edad ,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = ifelse(  anios_imp >= 30  & edad < 60,
                             60 - edad ,
                             a_d_ivm ) ) %>%
  
  mutate( a_d_ivm = ifelse(  anios_imp >= 30  & edad < 60,
                             60 - edad ,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = if_else( anios_imp>=10 & anios_imp<=14 & edad < 70 & edad > 65,
                             70 - edad ,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = if_else( anios_imp>=15 & anios_imp<=29 & edad > 60 & edad < 64,
                             65 - edad,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = ifelse(  anios_imp >= 30  & edad >= 60,
                             0,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = if_else( anios_imp>=15 & anios_imp<=29 & edad >= 65,
                             0,
                             a_d_ivm ) ) %>%
  mutate( a_d_ivm = if_else( anios_imp>=10 & anios_imp<=14 & edad >= 70,
                             0,
                             a_d_ivm ) ) %>%
  mutate( anio_derecho_ivm = 2023 + a_d_ivm )

#Consolidación en una sola tabla--------------------------------------------------------------------

coescop <- rbind( ad,
                  bo,
                  me,
                  snai,
                  sn,
                  cte) %>%
  filter(cargo_coescop!="Administrativo" ) %>%
  left_join( ., imposiciones, by = 'cedula') %>%
  filter( !is.na(cedula) & fecha_nacimiento > as.Date('1950-01-01') & !is.na( numimp ) & fecha_nacimiento < as.Date('2004-01-01') ) %>%
  mutate( edad = round(age_calc(fecha_nacimiento,
                                enddate = as.Date("31/03/2022","%d/%m/%Y"),
                                units = "years",
                                precise = TRUE ) ) ) %>%
  filter( numimp > 5 )


#Guardar en Rdata-----------------------------------------------------------------------------------
save( coescop,
      factor,
      file = paste0( parametros$RData, 'IESS_consolidado_coescop.RData' ) )

# #-------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
