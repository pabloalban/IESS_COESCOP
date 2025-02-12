message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los población COESCOP' )

#Cargando población de beneficiarios----------------------------------------------------------------

# load( paste0( parametros$RData, "COESCOP_snai_cargos.RData" ) ) #snai
# load( paste0( parametros$RData, "COESCOP_snmlcf_cargos.RData" ) ) #snmlcf
# load( paste0( parametros$RData, "COESCOP_metropolitanos_cargos.RData" ) ) #metropolitanos
# load( paste0( parametros$RData, "COESCOP_cte_cargos.RData" ) ) #cte
# load( paste0( parametros$RData, "COESCOP_bomberos_cargos.RData" ) ) #bomberos
# load( paste0( parametros$RData, "COESCOP_aduaneros_cargos.RData" ) ) #aduaneros
load( paste0( parametros$RData, 'COESCOP_transito.RData' ) ) #trásito
load( paste0( parametros$RData, 'COESCOP_control.RData' ) ) #control
load( paste0( parametros$RData, 'IESS_tablas_contingencia.RData' ) )

#Tablas de contingencia por edad y sexo-------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tResumiendo información en tablas de contingencia' )

control <- control %>% 
  mutate( sexo = if_else( ciudad == 'Machala' & cedula == '0705041671',
                          'F',
                          sexo ) )

# Tránsito------------------------------------------------------------------------------------------
## Funciones----------------------------------------------------------------------------------------

#Función pirámide poblacional
fun_piramide <-  function( .data, ciudad ) {
  a <- .data %>%   
    filter( ciudad == {{ciudad}} ) %>% 
    distinct( cedula, .keep_all = TRUE ) %>%
    mutate( fecha_nacimiento = if_else( is.na( fecha_nacimiento ),
                                        mean( fecha_nacimiento, na.rm =TRUE ),
                                        fecha_nacimiento ) ) %>%
    mutate( edad = round( age_calc( fecha_nacimiento,
                                    enddate = as.Date( "31/12/2022","%d/%m/%Y" ),
                                    units = "years",
                                    precise = FALSE ) ) ) %>%
    group_by( sexo, edad ) %>%
    mutate( freq = n(  ) ) %>%
    ungroup(  ) %>%
    distinct( sexo, edad, .keep_all = TRUE ) %>%
    mutate( fdp = freq / sum( freq ) ) %>% 
    dplyr::select( sexo,
                   edad,
                   freq, 
                   fdp ) %>%
    arrange( sexo, edad ) %>% 
    dplyr::select( sexo, edad, n:= fdp, freq ) %>%
    mutate(  n = if_else(  sexo == 'M',
                           -n,
                           n )  )
  
  return( a )
}

#Función distribución de edad por rangos

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

fun_edad_rangos <-  function( .data, ciudad ) {
  
  aux  <- fun_piramide( .data, {{ciudad}} ) %>%
    mutate( rango_edad = cut( 
      edad,
      breaks = cortes_edad,
      labels = etiquetas_edad,
      #include.lowest = TRUE,
      right = TRUE
    ) ) %>%
    group_by( sexo, rango_edad ) %>%
    mutate( beneficiarios = sum( freq, na.rm = TRUE ) ) %>%
    ungroup(  ) %>%
    distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
    mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
    mutate( dist = beneficiarios / N ) %>%
    dplyr::select( sexo, beneficiarios, rango_edad, dist ) %>%
    arrange( rango_edad, sexo )
  
  auxa <- spread( dplyr::select( aux, -dist ), sexo, value = c( beneficiarios ) ) %>%
    dplyr::select( rango_edad, M_ben := M, F_ben := F )
  auxb <- spread( dplyr::select( aux, -beneficiarios ), sexo, value = c( dist ) ) %>%
    dplyr::select( rango_edad, M_dist := M, F_dist := F )
  
  aux <- left_join( auxa, auxb, by = 'rango_edad' ) %>%
    dplyr::select( rango_edad, M_ben, M_dist, F_ben, F_dist ) %>%
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
  return( aux )
}

#Función distribución por cargos
fun_num_puestos <-  function( .data, ciudad ) {
  aux <-  .data %>%  
    filter( ciudad == {{ciudad}} ) %>% 
    distinct( cedula, .keep_all = TRUE ) %>%
    group_by( puesto ) %>% 
    mutate( ben = n( ) ) %>% 
    ungroup( ) %>% 
    distinct( . , puesto, .keep_all = TRUE ) %>% 
    dplyr::select( puesto, ben )
  
  return( aux )
}

#Función distribución por cargo y sexo

fun_num_puestos_sexo <-  function( .data, ciudad ) {
  aux_1 <-  .data %>%   
    filter( ciudad == {{ciudad}} ) %>% 
    distinct( cedula, .keep_all = TRUE ) %>%
    arrange( desc( sexo ) ) %>% 
    group_by( puesto, sexo ) %>% 
    mutate( ben = n( ) ) %>% 
    ungroup( ) %>% 
    distinct( . , puesto, sexo, .keep_all = TRUE ) %>% 
    dplyr::select( puesto, sexo, ben ) %>% 
    pivot_wider( names_from = sexo, values_from = ben, names_prefix = "" ) %>% 
    mutate( total = rowSums( across( where( is.numeric ) ), na.rm = TRUE ) ) %>% 
    replace( is.na( . ), 0 ) %>% 
    rbind( ., c( "Total", as.character( colSums( .[, 2:ncol( . )] ) ) ) ) %>% 
    mutate_at( c( 2:ncol( . ) ), as.numeric ) 
  
  aux_2 <-  .data %>%   
    filter( ciudad == {{ciudad}} ) %>% 
    distinct( cedula, .keep_all = TRUE ) %>%
    arrange( desc( sexo ) ) %>% 
    mutate( N = n( ) ) %>% 
    group_by( puesto, sexo ) %>% 
    mutate( ben = 100 * n( ) / N ) %>% 
    ungroup( ) %>% 
    distinct( . , puesto, sexo, .keep_all = TRUE ) %>% 
    dplyr::select( puesto, sexo, ben ) %>% 
    pivot_wider( names_from = sexo, values_from = ben, names_prefix = "porc_" ) %>% 
    mutate( porc_total = rowSums( across( where( is.numeric ) ), na.rm = TRUE ) ) %>% 
    replace( is.na( . ), 0 ) %>% 
    rbind( ., c( "Total", as.character( colSums( .[, 2:ncol( . )] ) ) ) ) %>% 
    mutate_at( c( 2:ncol( . ) ), as.numeric ) 
  
  aux<- aux_1 %>% 
    left_join( ., aux_2, by = 'puesto' ) %>% 
    dplyr::select( puesto, M, porc_M, F, porc_F, total, porc_total )
  
  return( aux )
}

#Función de salario promedio
fun_sal_prom <-  function( .data, ciudad ) {
  aux <-  .data %>%   
    filter( ciudad == {{ciudad}} ) %>% 
    group_by( puesto ) %>% 
    mutate( sal_prom = mean( remuneracion, na.rm = TRUE ) ) %>% 
    ungroup( ) %>% 
    distinct( . , puesto, .keep_all = TRUE ) %>% 
    dplyr::select( puesto, sal_prom )
  
  return( aux )
}

#Función para ejecutar todas la funciones
fun_ciudades <- function( .data, ciudad ) {
  aux <- .data %>%   
    filter( ciudad == {{ciudad}} )
  
  t1 <- fun_piramide( .data, {{ciudad}} )
  t2 <- fun_edad_rangos( .data, {{ciudad}} )
  t3 <- fun_num_puestos( .data, {{ciudad}} )
  t4 <- fun_num_puestos_sexo( .data, {{ciudad}} )
  t5 <- fun_sal_prom( .data, {{ciudad}} )
  
  lista <- list( 'piramide' = t1,
                 'edad_rangos' = t2,
                 'num_puestos' = t3,
                 'num_puestos_sexo' = t4,
                 'sal_prom' = t5 )
  
  return( lista )
}

fun_auto_dem <- function( .data, ciudad ) {
  
aux <- fun_ciudades( .data, {{ciudad}} )

dem_1 <- aux$edad_rangos$T_ben[ nrow( aux$edad_rangos ) ] 
dem_1_m <- aux$edad_rangos$M_ben[ nrow( aux$edad_rangos ) ] 
dem_1_f <- aux$edad_rangos$F_ben[ nrow( aux$edad_rangos ) ] 
  
aux_2 <- .data %>%
  filter( ciudad == {{ciudad}} ) %>% 
  distinct( cedula, .keep_all = TRUE ) %>%
  mutate( fecha_nacimiento = if_else( is.na( fecha_nacimiento ),
                                      mean( fecha_nacimiento, na.rm =TRUE ),
                                      fecha_nacimiento ) ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date( "31/12/2022","%d/%m/%Y" ),
                                  units = "years",
                                  precise = FALSE ) ) ) %>%
  group_by( sexo ) %>%
  mutate( edad_prom = mean( edad, na.rm = TRUE  ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, edad_prom )

dem_2_m <- as.double( aux_2[ aux_2$sexo == 'M', 2 ] )
dem_2_f <- as.double( aux_2[ aux_2$sexo == 'F', 2 ] )

dem_3 <- max( aux$edad_rangos$T_dist[ -length( aux$edad_rangos$T_dist ) ]  )

dem_4 <-  as.vector( matrix( sort( aux$num_puestos_sexo$total[ -length( aux$num_puestos_sexo$total ) ], decreasing = TRUE ), 1, 3 ) )

dem_5 <- as.vector( matrix( sort( aux$sal_prom$sal_prom, , decreasing = FALSE ), 1, 3 ) )   
lista <- list( 'dem_1' = dem_1,
               'dem_1_m' = dem_1_m,
               'dem_1_f' = dem_1_f,
               'dem_2_m' = dem_2_m,
               'dem_2_f' = dem_2_f,
               'dem_3' = dem_3,
               'dem_4' = dem_4,
               'dem_5' = dem_5 )

return( lista )
}

## Ciudades-----------------------------------------------------------------------------------------
#Control Municipal----------------------------------------------------------------------------------
tab_control_ambato <- fun_ciudades( control, 'Ambato')
tab_control_duran <- fun_ciudades( control, 'Duran')
tab_control_esmeraldas <- fun_ciudades( control, 'Esmeraldas')
tab_control_guayaquil <- fun_ciudades( control, 'Guayaquil')
tab_control_latacunga <- fun_ciudades( control, 'Latacunga')
tab_control_machala <- fun_ciudades( control, 'Machala')
tab_control_p_viejo <- fun_ciudades( control, 'Portoviejo')
tab_control_quito <- fun_ciudades( control, 'Quito')
tab_control_s_domingo <- fun_ciudades( control, 'Santo Domingo')

#Agentes civiles de Tránsito------------------------------------------------------------------------
tab_transito_cuenca <- fun_ciudades( transito, 'Cuenca')
tab_transito_duran <- fun_ciudades( transito, 'Duran')
tab_transito_esmeraldas <- fun_ciudades( transito, 'Esmeraldas')
tab_transito_guayaquil <- fun_ciudades( transito, 'Guayaquil')
tab_transito_ibarra <- fun_ciudades( transito, 'Ibarra')
tab_transito_latacunga <- fun_ciudades( transito, 'Latacunga')
tab_transito_machala <- fun_ciudades( transito, 'Machala')
tab_transito_quito <- fun_ciudades( transito, 'Quito')
tab_transito_s_domingo <- fun_ciudades( transito, 'Santo Domingo')
tab_transito_ambato <- fun_ciudades( transito, 'Ambato')
tab_transito_riobamba <- fun_ciudades( transito, 'Riobamba')
tab_transito_loja <- fun_ciudades( transito, 'Loja')

#Guardando en un Rdata todas las tablas-------------------------------------------------------------
message( '\tGuardando en data.frame' )

tablas <- c( 'tab_control_ambato',
             'tab_control_duran',
             'tab_control_esmeraldas',
             'tab_control_guayaquil', 
             'tab_control_latacunga',
             'tab_control_machala',
             'tab_control_p_viejo',
             'tab_control_quito',
             'tab_control_s_domingo',
             
             'tab_transito_cuenca',
             'tab_transito_duran',
             'tab_transito_esmeraldas',
             'tab_transito_guayaquil',
             'tab_transito_ibarra',
             'tab_transito_latacunga',
             'tab_transito_machala',
             'tab_transito_quito',
             'tab_transito_s_domingo',
             'tab_transito_ambato',
             'tab_transito_riobamba',
             'tab_transito_loja' )

save( list = tablas,
      fun_auto_dem,
      fun_ciudades,
      fun_sal_prom,
      fun_num_puestos_sexo,
      fun_num_puestos,
      fun_edad_rangos,
      fun_piramide,
      cortes_edad,
      etiquetas_edad,
      file = paste0( parametros$RData, 'IESS_tablas_contingencia_2.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ]  )
gc( )
