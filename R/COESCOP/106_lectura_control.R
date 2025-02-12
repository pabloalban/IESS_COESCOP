message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura del Cuerpo de Agentes de control' )

# 0. Cargando RC------------------------------------------------------------------------------------
load( paste0( parametros$RData, "IESS_Reg_Civil.RData" ) )


## 0.1 Cargando función de texto--------------------------------------------------------------------
source("~/IESS_COESCOP/R/503_tildes_a_latex.R")

rc <- rc %>% 
  filter( fecha_nacimiento < as.Date( '2003-01-01' ),
          fecha_nacimiento > as.Date( '1952-01-01' ),
          is.na( fecha_defuncion ) )

rc <- cod_utf_win( rc ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo,
                 fecha_nacimiento )

# 1.Ambato------------------------------------------------------------------------------------------
file_puesto <- paste0( parametros$Data, 'control/ambato_puestos.xlsx' )
file_sueldo <- paste0( parametros$Data, 'control/ambato_sueldos.xlsx' )

ambato_puestos <- read_excel( file_puesto,
                              sheet = 1,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0 ) %>%
  clean_names( ) %>%
  mutate( nombre =  toupper( nombre ) ) %>% 
  cod_utf_win( . )


ambato_sueldos <- read_excel( file_sueldo,
                              sheet = 1,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0 ) %>%
  clean_names( ) %>%
  mutate( nombre = firstup ( toupper( puesto ) ) ) %>% 
  distinct( puesto, .keep_all = TRUE ) %>% 
  dplyr::select( puesto, remuneracion )


ambato <- ambato_puestos %>% 
  left_join( .,  ambato_sueldos, by = 'puesto' ) %>% 
  left_join( ., rc, by = 'nombre' ) %>% 
  distinct( nombre, .keep_all = TRUE ) %>% 
  filter( !is.na( cedula ), !is.na( remuneracion ) ) %>% 
  dplyr::select( cedula, nombre, sexo, fecha_nacimiento, puesto, remuneracion ) %>% 
  mutate( ciudad = 'Ambato' ) %>% 
  mutate( puesto = firstup( puesto ) ) %>% 
  filter( grepl( c( 'Agente de control' ), puesto ) |
            grepl( c( 'nspector tecnico de control' ), puesto ) |
            grepl( c( 'efe de control urbano' ), puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Inspector tecnico de control urbano' , puesto ),
                            'Inspector de control',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente de control municipal' , puesto ),
                            'Agente de control 1',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Jefe de control urbano' , puesto ),
                            'Jefe de control',
                            puesto ) )

# # 2.Duran-------------------------------------------------------------------------------------------
file_puesto <- paste0( parametros$Data, 'control/duran_puestos.xlsx' )
file_sueldo <- paste0( parametros$Data, 'control/duran_sueldos.xlsx' )

duran_puestos <- read_excel( file_puesto,
                              sheet = 1,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0 ) %>%
  clean_names( ) %>%
  mutate( nombre =  toupper( nombre ) ) %>% 
  mutate( puesto = firstup( toupper( puesto ) ) ) %>% 
  cod_utf_win( . )


duran_sueldos <- read_excel( file_sueldo,
                              sheet = 1,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0 ) %>%
  clean_names( ) %>%
  mutate( nombre = firstup ( toupper( puesto ) ) ) %>% 
  distinct( puesto, .keep_all = TRUE ) %>% 
  dplyr::select( puesto, remuneracion ) %>% 
  mutate( puesto = firstup( toupper( puesto ) ) )


duran <- duran_puestos %>% 
  left_join( .,  duran_sueldos, by = 'puesto' ) %>% 
  left_join( ., rc, by = 'nombre' ) %>% 
  distinct( nombre, .keep_all = TRUE ) %>% 
  filter( !is.na( cedula ), !is.na( remuneracion ) ) %>% 
  dplyr::select( cedula, nombre, sexo, fecha_nacimiento, puesto, remuneracion ) %>% 
  mutate( ciudad = 'Duran' ) %>% 
  filter( grepl( c( 'de control', 'Jefe', 'Inspector' ), puesto ) ) %>% 
  mutate( puesto = if_else( puesto == 'Jefe de control de asentamientos irregulares',
                            'Jefe de Control',
                            puesto ) ) %>% 
  mutate( puesto = if_else( puesto == 'Agente de control municipal',
                            'Agente de control 1',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Jefe' , puesto ),
                            'Jefe de control',
                            puesto ) )

# 3.Esmeraldas--------------------------------------------------------------------------------------

file<-paste0( parametros$Data, '/control/esmeraldas_abril_2023.xlsx' )

esmeraldas <- read_excel( file,
                          sheet = 1,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

esmeraldas <- cod_utf_win( esmeraldas ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Esmeraldas') %>% 
  mutate( puesto = firstup( toupper( puesto ) ) ) %>% 
  filter( puesto %in% c( 'Agente de control municipal', 
                    'Jefe de control de uso y ocupacion de suelo', 
                    'Supervisor centro de operaciones y control' ) ) %>% 
  mutate( puesto = if_else( puesto == 'Jefe de control de uso y ocupacion de suelo',
                            'Jefe de Control',
                            puesto ) ) %>% 
  mutate( puesto = if_else( puesto == 'Agente de control municipal',
                            'Agente de control 1',
                            puesto ) ) %>% 
  mutate( puesto = if_else( puesto == 'Jefe de control de uso y ocupacion de suelo',
                            'Jefe de control',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Supervisor' , puesto ),
                            'Subinspector de Control',
                            puesto ) )

# 4. Guayaquil--------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/control/guayaquil_marzo_2022.xlsx' )

guayaquil <- read_excel( file,
                         sheet = 1,
                         col_names = TRUE,
                         col_types = NULL,
                         na = "",
                         skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

guayaquil <- cod_utf_win( guayaquil ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Guayaquil') %>% 
  mutate( puesto = firstup( toupper( puesto ) ) ) %>% 
  filter( grepl( "Jefe de control" , puesto ) |
          grepl( "Agente de control" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Jefe' , puesto ),
                            'Jefe de control',
                            puesto ) ) %>% 
  mutate( puesto = if_else( puesto == 'Agente de control metropolitana 3',
                            'Agente de control metropolitano 3',
                            puesto ) )

# 5. ibarra-----------------------------------------------------------------------------------------
# file<-paste0( parametros$Data, '/control/ibarra_marzo_2022.xlsx' )
# 
# ibarra <- read_excel( file,
#                       sheet = 1,
#                       col_names = TRUE,
#                       col_types = NULL,
#                       na = "",
#                       skip = 0 ) %>% 
#   clean_names( ) %>%
#   mutate( nombre = toupper( nombre ) )
# 
# ibarra <- cod_utf_win( ibarra ) %>% 
#   right_join( rc, ., by = 'nombre' ) %>% 
#   dplyr::select( cedula,
#                  nombre,
#                  sexo, 
#                  fecha_nacimiento,
#                  puesto,
#                  remuneracion ) %>% 
#   mutate( ciudad = 'ibarra')


# 6. latacunga--------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/control/latacunga_marzo_2022.xlsx' )

latacunga <- read_excel( file,
                         sheet = 1,
                         col_names = TRUE,
                         col_types = NULL,
                         na = "",
                         skip = 0 ) %>%
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

latacunga <- cod_utf_win( latacunga ) %>%
  right_join( rc, ., by = 'nombre' ) %>%
  dplyr::select( cedula,
                 nombre,
                 sexo,
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>%
  mutate( ciudad = 'Latacunga') %>% 
  mutate( puesto = firstup( toupper( puesto ) ) ) %>% 
  filter( grepl( "Jefe de control" , puesto ) |
            grepl( "Agente de control" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente de control' , puesto ),
                            'Agente de control 1',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Jefe' , puesto ),
                            'Jefe de control',
                            puesto ) )

# 7. Machala----------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/control/machala_septiembre_2022.xlsx' )

machala <- read_excel( file,
                       sheet = 1,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

machala <- cod_utf_win( machala ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Machala') %>% 
  mutate( puesto = firstup( toupper( puesto ) ) ) %>% 
  filter( grepl( "Jefe de control" , puesto ) |
            grepl( "Agente de control" , puesto ) |
            grepl( "Inspector de control" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Jefe' , puesto ),
                            'Jefe de control',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente de control' , puesto ),
                            'Agente de control 1',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Inspector de control' , puesto ),
                            'Inspector de control',
                            puesto ) ) %>% 
  mutate( sexo = if_else( cedula == '0705041671',
                          'F',
                          sexo ) )

# 9. Portoviejo-------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/control/portoviejo_marzo_2022.xlsx' )

portoviejo <- read_excel( file,
                          sheet = 1,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) ) %>% 
  mutate( remuneracion = as.numeric( remuneracion ) )

portoviejo <- cod_utf_win( portoviejo ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Portoviejo') %>% 
  mutate( puesto = firstup( toupper( puesto ) ) ) %>% 
  filter( grepl( "Jefe tecnico - administrativo de la direccion de control" , puesto ) |
            grepl( "Agente de control" , puesto ) |
            grepl( "Inspector de control" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Jefe tecnico - administrativo de la direccion de control' , puesto ),
                            'Jefe de control',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente de control' , puesto ),
                            'Agente de control 1',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Inspector de control' , puesto ),
                            'Inspector de control',
                            puesto ) )

# 10. Quito-------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/control/quito_septiembre_2023.xlsx' )

quito <- read_excel( file,
                     sheet = 1,
                     col_names = TRUE,
                     col_types = NULL,
                     na = "",
                     skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) ) %>% 
  mutate( remuneracion = as.numeric( remuneracion ) )

quito <- cod_utf_win( quito ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Quito') %>% 
  mutate( puesto = firstup( toupper( puesto ) ) ) %>% 
  distinct( nombre, .keep_all = TRUE ) %>% 
  filter( grepl( "de control metropolitano" , puesto ) |
            grepl( "Director/a general cuerpo de agentes de control" , puesto ) |
            grepl( "Inspector de control" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'de control metropolitano' , puesto ),
                            'Agente de control 1',
                            puesto ) ) %>% 
  mutate( puesto = if_else( cedula == '1710537687',
                            'Jefe de control',
                            puesto ) ) %>% 
  filter( !is.na( puesto ) )


# 11. Santo Domingo---------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/control/santo_domingo_julio_2023.xlsx' )

s_domingo <- read_excel( file,
                         sheet = 1,
                         col_names = TRUE,
                         col_types = NULL,
                         na = "",
                         skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

s_domingo <- cod_utf_win( s_domingo ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Santo Domingo') %>% 
  mutate( puesto = firstup( toupper( puesto ) ) ) %>% 
  distinct( nombre, .keep_all = TRUE )  %>% 
  filter( grepl( "Agente de control municipal" , puesto ) |
            grepl( "Director del cuerpo de agentes de control" , puesto ) |
            grepl( "Subdirector del cuerpo de agentes de control" , puesto ) |
            grepl( "Director de control", puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente de control municipal' , puesto ),
                            'Agente de control 1',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Director' , puesto ),
                            'Jefe de control',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Subdirector' , puesto ),
                            'Subjefe de control',
                            puesto ) ) %>% 
  filter( !is.na( puesto ) )


# 12. Concordia---------------------------------------------------------------------------------
# file<-paste0( parametros$Data, '/control/concordia_06_2022.xlsx' )
# 
# concordia <- read_excel( file,
#                          sheet = 1,
#                          col_names = TRUE,
#                          col_types = NULL,
#                          na = "",
#                          skip = 0 ) %>% 
#   clean_names( ) %>%
#   mutate( nombre = toupper( nombre ) )
# 
# concordia <- cod_utf_win( concordia ) %>% 
#   right_join( rc, ., by = 'nombre' ) %>% 
#   dplyr::select( cedula,
#                  nombre,
#                  sexo, 
#                  fecha_nacimiento,
#                  puesto,
#                  remuneracion ) %>% 
#   mutate( ciudad = 'concordia') %>% 
#   filter( puesto == 'POLICIA MUNICIPAL' ) %>% 
#   mutate( sexo = if_else( is.na( sexo ),
#                           'M',
#                           sexo ) ) %>% 
#   mutate( puesto = if_else( puesto == 'POLICIA MUNICIPAL',
#                                           'Policía Municipal',
#                             puesto ) )

# 20. Consolidación---------------------------------------------------------------------------------

control <- rbind( ambato,
                  duran,
                  esmeraldas,
                  guayaquil,
                  latacunga,
                  machala,
                  portoviejo,
                  quito,
                  #concordia,
                  s_domingo ) %>% 
  mutate( remuneracion = as.double( remuneracion ) ) %>% 
  mutate( sexo = if_else( is.na( sexo ),
                          'M',
                          sexo ),
          fecha_nacimiento = if_else( is.na( fecha_nacimiento ),
                                      mean( fecha_nacimiento, na.rm = TRUE ),
                                      fecha_nacimiento ) ) %>% 
  filter( remuneracion > 0 )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( #cuenca,
      ambato,
      duran,
      esmeraldas,
      guayaquil,
      #ibarra,
      latacunga,
      machala,
      quito,
      #concordia,
      s_domingo,
      control,
      file = paste0( parametros$RData, 'COESCOP_control.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ]  )
gc( )
