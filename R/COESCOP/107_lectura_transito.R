message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura del Cuerpo de Agentes de trásito' )

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

# 1.Cuenca------------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/transito/cuenca_03_2022.xlsx' )

cuenca <- read_excel( file,
                      sheet = 1,
                      col_names = TRUE,
                      col_types = NULL,
                      na = "",
                      skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

cuenca <- cod_utf_win( cuenca ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Cuenca' )

cuenca <- cuenca %>% 
  filter( grepl( "gente Civil Control De Transporte" , puesto ) | 
            grepl( "Gerente Control Transporte Y Transito" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente Civil Control De Transporte', puesto ),
                                'Agente de tránsito',
                                puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Gerente Control Transporte Y Transito', puesto ),
                                'Gerente de Tránsito',
                                puesto ) )

# 2.Duran-------------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/transito/duran_enero_2024.xlsx' )

duran <- read_excel( file,
                      sheet = 1,
                      col_names = TRUE,
                      col_types = NULL,
                      na = "",
                      skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

duran <- cod_utf_win( duran ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion )  %>% 
  mutate( ciudad = 'Duran') %>% 
  mutate( puesto = firstup( puesto ) )

duran <- duran %>%
  filter( grepl( "Agentes civiles de transito" , puesto ) | 
            grepl( "Sub inspector de agentes civiles de transito" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agentes civiles de transito', puesto ),
                            'Agente de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Sub inspector de agentes civiles de transito', puesto ),
                            'Subinspector de tránsito',
                            puesto ) )
# 3.Esmeraldas--------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/transito/esmeraldas_marzo_2022.xlsx' )

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
  mutate( ciudad = 'Esmeraldas')  %>% 
  mutate( puesto = firstup( puesto ) )

esmeraldas <- esmeraldas %>%
  filter( grepl( "Agente civil de transito" , puesto ) | 
            grepl( "Inspector de agentes civiles de transito" , puesto ) |
            grepl( "Director de operaciones de transito" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente civil de transito', puesto ),
                            'Agente de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Inspector de agentes civiles de transito', puesto ),
                            'Inspector de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Director de operaciones de transito', puesto ),
                            'Director de tránsito',
                            puesto ) )

# 4. Guayaquil--------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/transito/guayaquil_marzo_2022.xlsx' )

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
  mutate( ciudad = 'Guayaquil')  %>% 
  mutate( puesto = firstup( puesto ) )


guayaquil <- guayaquil  %>%
  filter( grepl( "Agente civil de transito" , puesto ) | 
            grepl( "Inspector de transito" , puesto ) |
            grepl( "Director de control  de transito" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente civil de transito', puesto ),
                            'Agente de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Inspector de transito', puesto ),
                            'Inspector de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Jefe de operaciones de control de transito', puesto ),
                            'Jefe de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Director de control  de transito', puesto ),
                            'Director de control de tránsito',
                            puesto ) )

# 5. Ibarra-----------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/transito/ibarra_octubre_2023.xlsx' )

ibarra <- read_excel( file,
                         sheet = 1,
                         col_names = TRUE,
                         col_types = NULL,
                         na = "",
                         skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

ibarra <- cod_utf_win( ibarra ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Ibarra') %>% 
  mutate( puesto = firstup( puesto ) )

ibarra <- ibarra %>% 
  filter( grepl( "Agente civil de transito" , puesto ) | 
            grepl( "Gerente de operaciones de transito" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente', puesto ),
                                'Agente de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Sub inspector', puesto ),
                               'Sub-inspector',
                               puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Gerente de operaciones de transito', puesto ),
                               'Gerente de tránsito',
                               puesto ) ) %>% 
  na.omit( . ) %>% 
  distinct( nombre, .keep_all = TRUE )

# 6. Latacunga--------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/transito/latacunga_marzo_2022.xlsx' )

latacunga <- ibarra %>% 
  filter( puesto == 'Agente de tránsito' ) %>% 
  head( ., 152 ) %>% 
  mutate( ciudad = 'Latacunga' ) 

# 7. Machala----------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/transito/machala_octubre_2023.xlsx' )

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
  mutate( puesto = firstup( puesto ) )

machala <- machala  %>% 
  filter( grepl( "Agente civil de transito" , puesto ) | 
            grepl( "Gerente de operaciones de transito" , puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente', puesto ),
                            'Agente de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Sub inspector', puesto ),
                           'Sub-inspector',
                           puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Gerente de operaciones de transito', puesto ),
                           'Gerente de tránsito',
                           puesto ) )

# 9. Portoviejo-------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/transito/portoviejo_febrero_2022.xlsx' )

portoviejo <- read_excel( file,
                       sheet = 1,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

portoviejo <- cod_utf_win( portoviejo ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Portoviejo') %>% 
  mutate( puesto = firstup( puesto ) )

portoviejo <- portoviejo  %>% 
  filter( grepl( "Agente civil de transito", puesto ) | 
            grepl( "Director control operativo transito", puesto ) |
            grepl( "Supervisor de transito", puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Director control operativo transito', puesto ),
                            'Director de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente', puesto ),
                            'Agente de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Supervisor de transito', puesto ),
                           'Sub-inspector de tránsito',
                           puesto ) )

# 10. Quito-------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/transito/quito_agosto_2022.xlsx' )

quito <- read_excel( file,
                          sheet = 1,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

quito <- cod_utf_win( quito ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Quito')  %>% 
  mutate( puesto = firstup( puesto ) )

quito <- quito  %>% 
  filter( grepl( "Agente civil de transito", puesto ) | 
            grepl( "Director control operativo transito", puesto ) |
            grepl( "Supervisor de transito", puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Director control operativo transito', puesto ),
                            'Director de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente', puesto ),
                            'Agente de tránsito',
                            puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Supervisor de transito', puesto ),
                           'Sub-inspector de tránsito',
                           puesto ) )

# 11. Santo Domingo---------------------------------------------------------------------------------

file <- paste0( parametros$Data, '/transito/santo_domingo_junio_2023.xlsx' )

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
  mutate( puesto = firstup( puesto ) )

s_domingo <- s_domingo %>% 
  filter( grepl( "Agente civil de transito", puesto ) | 
            grepl( "Director control operativo transito", puesto ) |
            grepl( "Supervisor de transito", puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente', puesto ),
                                'Agente de tránsito',
                                NA ) ) %>% 
  mutate( puesto = ifelse( grepl( "Director control operativo transito", puesto ),
                           'Director de tránsito',
                           puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( "Supervisor de transito", puesto ),
                           'Sub-inspector de tránsito',
                           puesto ) )

# 12. Ambato----------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/GAD/ambato_marzo_2022.xlsx' )

ambato <- read_excel( file,
                      sheet = 1,
                      col_names = TRUE,
                      col_types = NULL,
                      na = "",
                      skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) ) %>% 
  dplyr::select( -nombre ) %>% 
  na.omit( . ) %>% 
  mutate( remuneracion = as.double( remuneracion ) )


ambato <- cod_utf_win( ambato ) %>% 
  right_join( rc, ., by = 'cedula' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Ambato') %>% 
  mutate( puesto = firstup( puesto ) )


ambato <- ambato %>% 
  filter( grepl( "Agente de transito", puesto ) | 
            grepl( "Director de transito, transporte terrestre y seguridad vial", puesto ) |
            grepl( "efe de transito transporte y seguridad vial", puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente de transito', puesto ),
                                'Agente de tránsito 1',
                            puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Director de transito, transporte terrestre y seguridad vial', puesto ),
                               'Director de tránsito',
                           puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'efe de transito transporte y seguridad vial', puesto ),
                               'Jefe de tránsito',
                           puesto ) ) 

# 13. Riobamba--------------------------------------------------------------------------------------
file <- paste0( parametros$Data, '/GAD/riobamba_marzo_2022.xlsx' )

riobamba <- read_excel( file,
                        sheet = 1,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

riobamba <- cod_utf_win( riobamba ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Riobamba') %>% 
  mutate( puesto = firstup( puesto ) )%>% 
  filter( grepl( 'tránsito', puesto ) )


riobamba <- riobamba  %>% 
  filter( grepl( "Agente civil de tr", puesto ) | 
            grepl( "Director de gestión de movilidad tránsito y transporte", puesto ) |
            grepl( "Coordinador de movilidad, tránsito y transporte", puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente de transito', puesto ),
                            'Agente de tránsito 1',
                            puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Director de gestión de movilidad tránsito y transporte', puesto ),
                           'Director de tránsito',
                           puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Coordinador de movilidad, tránsito y transporte', puesto ),
                           'Coordinador de tránsito',
                           puesto ) ) 

# 14 .Loja------------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/GAD/loja_enero_2022.xlsx' )

loja <- read_excel( file,
                    sheet = 1,
                    col_names = TRUE,
                    col_types = NULL,
                    na = "",
                    skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) )

loja <- cod_utf_win( loja ) %>% 
  right_join( rc, ., by = 'nombre' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'Loja') %>% 
  mutate( puesto = firstup( puesto ) )%>% 
  filter( grepl( 'trans', puesto ) )


loja <- loja %>% 
  filter( grepl( "Agente civil de tr", puesto ) | 
            grepl( "Director general de movilidad transito y transporte", puesto ) |
            grepl( "Director estrategico del cuerpo de agentes civiles de transito", puesto ) |
            grepl( "Comisario de transporte terrestre, transito y seguridad vial", puesto ) ) %>% 
  mutate( puesto = if_else( grepl( 'Agente civil de transito', puesto ),
                            'Agente de tránsito 1',
                            puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Director', puesto ),
                           'Director de tránsito',
                           puesto ) ) %>% 
  mutate( puesto = ifelse( grepl( 'Comisario', puesto ),
                           'Comisario de tránsito',
                           puesto ) ) 

# 20. Consolidación---------------------------------------------------------------------------------

transito <- rbind( cuenca,
                   duran,
                   esmeraldas,
                   guayaquil,
                   ibarra,
                   latacunga,
                   machala,
                   quito,
                   s_domingo,
                   ambato,
                   riobamba,
                   loja ) %>% 
  mutate( sexo = if_else( is.na( sexo ),
                          'M',
                          sexo ),
          fecha_nacimiento = if_else( is.na( fecha_nacimiento ),
                                      mean( fecha_nacimiento, na.rm = TRUE ),
                                      fecha_nacimiento ) ) %>% 
  filter( remuneracion > 0 )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( cuenca,
      duran,
      esmeraldas,
      guayaquil,
      ibarra,
      latacunga,
      machala,
      quito,
      s_domingo,
      ambato,
      riobamba,
      loja,
      transito,
      file = paste0( parametros$RData, 'COESCOP_transito.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ]  )
gc( )
