message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tCreación de las tablas del análisis demográfico' )

#Carga de función tildes a latex--------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R',
       encoding = 'UTF-8',
       echo = FALSE )

#Carga de datos-------------------------------------------------------------------------------------
load( paste0( parametros$RData, "IESS_tablas_contingencia.RData" ) )

#Tabla SNAI rangos de edad y sexo-------------------------------------------------------------------
message( '\tTabla de servidores publicos del SNAI por rango de edad y sexo, en marzo 2022' )

cortes_edad <- c( 17, seq( 20, 70, 10 ) )
etiquetas_edad <- c( paste0( 
  "( ",
  formatC( 
    c( 18, seq( 20, 60, 10 ) ),
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
   ),
  " - ",
  formatC( 
    c( seq( 20, 70, 10 ) ),
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
   ),
  "]"
 ) )
aux  <- tabla_snai_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux )
print( 
  aux_xtable,
  file = paste0( parametros$resultado_tablas, 'iess_snai_edad_sexo', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla SNAI de sexo y cargos------------------------------------------------------------------------
message( '\tTabla de servidores publicos del SNAI por cargo y sexo, en marzo 2022' )

aux  <- tabla_snai_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( parametros$resultado_tablas, 'iess_snai_cargo_sexo', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )


#Tabla SNAI de cargos y salarios--------------------------------------------------------------------
message( '\tTabla de servidores publicos del SNAI de cargos y salarios, en marzo 2022' )

aux  <- tabla_snai_salario[order( tabla_snai_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_snai_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )


#Tabla SNMLCF rangos de edad y sexo-----------------------------------------------------------------
message( '\tTabla de servidores publicos del SNMLCF por rango de edad y sexo, en marzo 2022' )

aux  <- tabla_snmlcf_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( parametros$resultado_tablas, 'iess_snmlcf_edad_sexo', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla SNMLCF de sexo y cargos----------------------------------------------------------------------
message( '\tTabla de servidores publicos del SNMLCF por cargo y sexo, en marzo 2022' )

aux  <- tabla_snmlcf_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_snmlcf_cargo_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla SNMLF de cargos y salarios-------------------------------------------------------------------
message( '\tTabla de servidores publicos del SNMLCF de cargos y salarios, en marzo 2022' )

aux  <- tabla_snmlcf_salario[order( tabla_snmlcf_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_snmlcf_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

#Tabla METROPOLITANOS rangos de edad y sexo---------------------------------------------------------
message( 
  '\tTabla de servidores publicos del Cuerpo de Agentes de Control Municipal o Metropolitano por rango de edad y sexo, en marzo 2022'
 )

aux  <- tabla_metropolitanos_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

## Tablas metropolitanos rangos de edad y sexo por ciudad-------------------------------------------

### Quito-------------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_quito_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_quito_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )


### Ambato------------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_ambato_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_ambato_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )


### Cuenca------------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_cuenca_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_cuenca_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Guayaquil---------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_gye_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_gye_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Loja--------------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_loja_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_loja_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Machala-----------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_machala_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_machala_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Portoviejo--------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_prtvj_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_prtvj_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )


#Tabla METROPOLITANOS de sexo y cargos--------------------------------------------------------------
message( 
  '\tTabla de servidores publicos del Cuerpo de Agentes de Control Municipal o Metropolitano por cargo y sexo, en marzo 2022'
 )

aux  <- tabla_metropolitanos_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_cargo_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )


##Tabla METROPOLITANOS de sexo y cargos por ciudad--------------------------------------------------

### Quito-------------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_quito_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_quito_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Ambato------------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_ambato_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_ambato_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Cuenca------------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_cuenca_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_cuenca_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Guayaquil---------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_gye_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_gye_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Loja--------------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_loja_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_loja_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Machala-----------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_machala_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_machala_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Portoviejo--------------------------------------------------------------------------------------

aux  <- tabla_metropolitanos_prtvj_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_prtvj_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla METROPOLITANOS de sexo y ciudades------------------------------------------------------------
message( 
  '\tTabla de servidores publicos del Cuerpo de Agentes Metropolitanos y Municipales por ciudad y sexo, en marzo 2022'
 )

aux  <- tabla_metropolitanos_ciudad %>%
  group_by( ciudad, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( ciudad, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, ciudad, dist ) %>%
  arrange( ciudad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( ciudad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( ciudad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'ciudad' ) %>%
  select( ciudad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    ciudad = as.character( ciudad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

aux_xtable <- tildes_a_latex( aux_xtable )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_ciudad',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla METROPOLITANOS de cargos y salarios----------------------------------------------------------
message( '\tTabla de servidores publicos de METROPOLITANOS de cargos y salarios, en marzo 2022' )

aux  <-
  tabla_metropolitanos_salario[order( tabla_metropolitanos_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

#Tabla METROPOLITANOS de cargos y salarios por ciudad-----------------------------------------------

##Quito---------------------------------------------------------------------------------------------

aux  <-
  tabla_metropolitanos_quito_salario[order( tabla_metropolitanos_quito_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_quito_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

##Ambato--------------------------------------------------------------------------------------------

aux  <-
  tabla_metropolitanos_ambato_salario[order( tabla_metropolitanos_ambato_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_ambato_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

##Cuenca--------------------------------------------------------------------------------------------

aux  <-
  tabla_metropolitanos_cuenca_salario[order( tabla_metropolitanos_cuenca_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_cuenca_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

##Guayaquil-----------------------------------------------------------------------------------------

aux  <-
  tabla_metropolitanos_gye_salario[order( tabla_metropolitanos_gye_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_gye_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

##Loja----------------------------------------------------------------------------------------------

aux  <-
  tabla_metropolitanos_loja_salario[order( tabla_metropolitanos_loja_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_loja_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

##Machala-------------------------------------------------------------------------------------------

aux  <-
  tabla_metropolitanos_machala_salario[order( tabla_metropolitanos_machala_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_machala_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

##Portoviejo----------------------------------------------------------------------------------------

aux  <-
  tabla_metropolitanos_prtvj_salario[order( tabla_metropolitanos_prtvj_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_metropolitanos_prtvj_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )


#Tabla CTE rangos de edad y sexo--------------------------------------------------------------------
message( '\tTabla de servidores publicos del CTE por rango de edad y sexo, en marzo 2022' )

aux  <- tabla_cte_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( parametros$resultado_tablas, 'iess_cte_edad_sexo', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla CTE de sexo y cargos-------------------------------------------------------------------------
message( '\tTabla de servidores publicos del CTE por cargo y sexo, en marzo 2022' )

aux  <- tabla_cte_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

aux_xtable <- tildes_a_latex( aux_xtable )

print( 
  aux_xtable,
  file = paste0( parametros$resultado_tablas, 'iess_cte_cargo_sexo', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla CTE de cargos y salarios---------------------------------------------------------------------
message( '\tTabla de servidores publicos del CTE de cargos y salarios, en marzo 2022' )

aux  <- tabla_cte_salario[order( tabla_cte_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )

aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_cte_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )


#Tabla BOMBEROS rangos de edad y sexo---------------------------------------------------------------
message( 
  '\tTabla de servidores publicos del Cuerpo de Bomberos por rango de edad y sexo, en marzo 2022'
 )

aux  <- tabla_bomberos_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )
## Tabla BOMBEROS rangos de edad y sexo por ciudad--------------------------------------------------

### Quito-------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_quito_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_quito_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Ambato------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_ambato_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_ambato_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Guayaquil---------------------------------------------------------------------------------------

aux  <- tabla_bomberos_gye_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_gye_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Ibarra------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_ibarra_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_ibarra_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Machala-----------------------------------------------------------------------------------------

aux  <- tabla_bomberos_machala_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_machala_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Manta-------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_manta_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_manta_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Milagro-----------------------------------------------------------------------------------------

aux  <- tabla_bomberos_milagro_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_milagro_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Portoviejo--------------------------------------------------------------------------------------

aux  <- tabla_bomberos_prtvj_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_prtvj_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Riobamba----------------------------------------------------------------------------------------

aux  <- tabla_bomberos_riobamba_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_riobamba_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Santo Domingo-----------------------------------------------------------------------------------

aux  <- tabla_bomberos_sto_dom_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_sto_dom_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )


### Loja--------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_loja_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_loja_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )


### Cuenca------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_cuenca_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_cuenca_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )


#Tabla BOMBEROS de sexo y cargos--------------------------------------------------------------------
message( '\tTabla de servidores publicos del Cuerpo de Bomberos por cargo y sexo, en marzo 2022' )

aux  <- tabla_bomberos_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_cargo_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

## Tabla BOMBEROS de sexo y cargos por ciudad-------------------------------------------------------

### Quito-------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_quito_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_quito_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Ambato------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_ambato_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_ambato_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Guayaquil---------------------------------------------------------------------------------------

aux  <- tabla_bomberos_gye_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_gye_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Ibarra------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_ibarra_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_ibarra_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Machala-----------------------------------------------------------------------------------------

aux  <- tabla_bomberos_machala_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_machala_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Manta-------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_manta_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_manta_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Milagro-----------------------------------------------------------------------------------------

aux  <- tabla_bomberos_milagro_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_milagro_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Portoviejo--------------------------------------------------------------------------------------

aux  <- tabla_bomberos_prtvj_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_prtvj_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Riobamba----------------------------------------------------------------------------------------

aux  <- tabla_bomberos_riobamba_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_riobamba_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Santo Domingo-----------------------------------------------------------------------------------

aux  <- tabla_bomberos_sto_dom_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_sto_dom_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Loja--------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_loja_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_loja_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

### Cuenca------------------------------------------------------------------------------------------

aux  <- tabla_bomberos_cuenca_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_cuenca_cargo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )


#Tabla BOMBEROS de sexo y ciudades------------------------------------------------------------------
message( '\tTabla de servidores publicos del Cuerpo de Bomberos por ciudad y sexo, en marzo 2022' )

aux  <- tabla_bomberos_ciudad %>%
  group_by( canton, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( canton, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, canton, dist ) %>%
  arrange( canton, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( canton, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( canton, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'canton' ) %>%
  select( canton, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    canton = as.character( canton )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )

aux_xtable <- tildes_a_latex( aux_xtable )
aux_xtable <- tildes_a_latex( aux_xtable )

print( 
  aux_xtable,
  file = paste0( parametros$resultado_tablas, 'iess_bomberos_ciudad', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla BOMBEROS de cargos y salarios----------------------------------------------------------------
message( '\tTabla de servidores publicos de BOMBEROS de cargos y salarios, en marzo 2022' )

aux  <- tabla_bomberos_salario[order( tabla_bomberos_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

##Tabla BOMBEROS   de cargos y salarios por ciudad--------------------------------------------------

###Quito--------------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_quito_salario[order( tabla_bomberos_quito_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_quito_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Ambato-------------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_ambato_salario[order( tabla_bomberos_ambato_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_ambato_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Guayaquil----------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_gye_salario[order( tabla_bomberos_gye_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_gye_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Ibarra-------------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_ibarra_salario[order( tabla_bomberos_ibarra_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_ibarra_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Machala------------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_machala_salario[order( tabla_bomberos_machala_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_machala_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Manta--------------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_manta_salario[order( tabla_bomberos_manta_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_manta_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Milagro------------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_milagro_salario[order( tabla_bomberos_milagro_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_milagro_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Portoviejo---------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_prtvj_salario[order( tabla_bomberos_prtvj_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_prtvj_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Riobamba-----------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_riobamba_salario[order( tabla_bomberos_riobamba_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_riobamba_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Santo Domingo------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_sto_dom_salario[order( tabla_bomberos_sto_dom_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_sto_dom_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Loja---------------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_loja_salario[order( tabla_bomberos_loja_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_loja_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

###Cuenca-------------------------------------------------------------------------------------------

aux  <-
  tabla_bomberos_cuenca_salario[order( tabla_bomberos_cuenca_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bomberos_cuenca_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )


#Tabla ADUANEROS rangos de edad y sexo--------------------------------------------------------------
message( 
  '\tTabla de servidores publicos del Cuerpo de Vigilancia Aduanera por rango de edad y sexo, en marzo 2022'
 )

aux  <- tabla_aduaneros_edad_sexo %>%
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
   ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, rango_edad, dist ) %>%
  arrange( rango_edad, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( rango_edad, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( rango_edad, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
  select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    rango_edad = as.character( rango_edad )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_aduaneros_edad_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla ADUANEROS de sexo y cargos-------------------------------------------------------------------
message( 
  '\tTabla de servidores publicos del Cuerpo de Vigilancia Aduanera por cargo y sexo, en marzo 2022'
 )

aux  <- tabla_aduaneros_cargo %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( beneficiarios = sum( frecuencia, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, sexo , .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  select( sexo, beneficiarios, cargo_coescop, dist ) %>%
  arrange( cargo_coescop, sexo )


auxa <- spread( select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
  select( cargo_coescop, M_ben := M, F_ben := F )
auxb <- spread( select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
  select( cargo_coescop, M_dist := M, F_dist := F )

aux <- left_join( auxa, auxb, by = 'cargo_coescop' ) %>%
  select( cargo_coescop, M_ben, M_dist, F_ben, F_dist ) %>%
  mutate( 
    M_dist = 100 * M_dist,
    F_dist = 100 * F_dist,
    cargo_coescop = as.character( cargo_coescop )
   )
aux[is.na( aux )] <- 0
aux <-
  rbind( ( aux ), c( "Total", as.character( colSums( aux[, 2:ncol( aux )] ) ) ) )
aux[2:ncol( aux )] <-
  lapply( aux[2:ncol( aux )], function( x )
    as.numeric( x ) )
aux <- aux %>% mutate( T_ben = M_ben + F_ben,
                      T_dist = M_dist + F_dist )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_aduaneros_cargo_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtable ) - 1 ),
                    command = c( paste( "\\hline \n" ) ) )
 )

#Tabla ADUANEROS de cargos y salarios---------------------------------------------------------------
message( '\tTabla de servidores publicos de ADUANEROS de cargos y salarios, en marzo 2022' )

aux  <-
  tabla_aduaneros_salario[order( tabla_aduaneros_salario$media ), ]
aux_xtable <- xtable( aux, digits = c( 2, 0, 2 ) )
aux_xtable <- tildes_a_latex( aux_xtable )
print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_aduaneros_cargo_salario',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )


#Limpiando memoria RAM------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% c( 'parametros' ) )] )
gc(  )
