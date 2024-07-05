

message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura de los policías de la Concordia' )

#Cargando población de beneficiarios----------------------------------------------------------------

load( paste0( parametros$RData, "COESCOP_concordia.RData" ) ) #concordia

#Tablas de contingencia por edad y sexo-------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tResumiendo información en tablas de contingencia' )

#Policías de la concordia----------------------------------------------------------------------------

tabla_concordia_edad_sexo <- concordia %>%
  filter( fecha_nacimiento < as.Date( "31/03/2022", "%d/%m/%Y" ) ) %>%
  mutate( edad = round( 
    age_calc( 
      fecha_nacimiento,
      enddate = as.Date( "31/03/2022", "%d/%m/%Y" ),
      units = "years",
      precise = TRUE
     )
   ) ) %>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n(  ) ) %>%
  ungroup(  ) %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select( sexo,
                edad,
                frecuencia ) %>%
  arrange( sexo, edad ) %>%
  filter( edad > 17, edad < 70 )



tabla_concordia_cargo <- concordia %>%
  filter( fecha_nacimiento < as.Date( "31/03/2022", "%d/%m/%Y" ) ) %>%
  mutate( edad = round( 
    age_calc( 
      fecha_nacimiento,
      enddate = as.Date( "31/03/2022", "%d/%m/%Y" ),
      units = "years",
      precise = TRUE
     )
   ) ) %>%
  filter( edad > 17, edad < 70 ) %>%
  mutate( cargo_coescop = puesto_institucional ) %>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n(  ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select( cargo_coescop,
                sexo,
                frecuencia )

tabla_concordia_salario <- concordia %>%
  filter( fecha_nacimiento < as.Date( "31/03/2022", "%d/%m/%Y" ) ) %>%
  mutate( sueldo = as.numeric( remuneracion_mensual_unificada ) ) %>%
  filter( !is.na( sueldo ), sueldo > 0 ) %>%
  mutate( edad = round( 
    age_calc( 
      fecha_nacimiento,
      enddate = as.Date( "31/03/2022", "%d/%m/%Y" ),
      units = "years",
      precise = TRUE
     )
   ) ) %>%
  filter( edad > 17, edad < 70 ) %>%
  mutate( cargo_coescop = puesto_institucional ) %>%
  group_by( cargo_coescop, sexo ) %>%
  mutate( media = mean( sueldo, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, media, sexo, .keep_all = TRUE ) %>%
  dplyr::select( cargo_coescop, sexo, media ) %>% 
  rbind( ., c( 'Policía Municipal',
               'Total',
               mean( concordia$remuneracion_mensual_unificada, na.rm = TRUE ) ) ) %>% 
  mutate_at( c( ncol( . ) ), as.numeric ) %>% 
  mutate( sexo = c( 'Hombres',
                    'Mujeres',
                    'Total' ) )

tabla_concordia_salario_edad_sexo <- concordia %>%
  filter( fecha_nacimiento < as.Date( "31/03/2022", "%d/%m/%Y" ) ) %>%
  mutate( sueldo = as.numeric( remuneracion_mensual_unificada ) ) %>%
  filter( !is.na( sueldo ), sueldo > 0 ) %>%
  mutate( edad = round( 
    age_calc( 
      fecha_nacimiento,
      enddate = as.Date( "31/03/2022", "%d/%m/%Y" ),
      units = "years",
      precise = TRUE
     )
   ) ) %>%
  filter( edad > 17, edad < 70 ) %>%
  mutate( cargo_coescop = puesto_institucional ) %>%
  group_by( cargo_coescop, edad, sexo ) %>%
  mutate( media = mean( sueldo, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( cargo_coescop, media, sexo, edad, .keep_all = TRUE ) %>%
  dplyr::select( cargo_coescop, sexo, edad, media )

#Estadísticas

aux <- concordia %>%
  filter( fecha_nacimiento < as.Date( "31/03/2022", "%d/%m/%Y" ) ) %>%
  mutate( edad = round( 
    age_calc( 
      fecha_nacimiento,
      enddate = as.Date( "31/03/2022", "%d/%m/%Y" ),
      units = "years",
      precise = TRUE
    )
  ) ) %>% 
  group_by( sexo ) %>% 
  mutate( edad_prom = mean( edad, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, edad_prom )



#Guardando en un Rdata todas las tablas-------------------------------------------------------------
message( '\tGuardando en data.frame' )

tablas <- c( 
  'tabla_concordia_edad_sexo',
  'tabla_concordia_cargo',
  'tabla_concordia_salario',
  'tabla_concordia_salario_edad_sexo'
 )


save( 
  list = tablas ,
  file = paste0( 
    parametros$RData,
    'IESS_tablas_contingencia_concordia.RData'
   )
 )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )
