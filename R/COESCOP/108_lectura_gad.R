message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura de servidores por GAD' )

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
file<-paste0( parametros$Data, '/GAD/ambato_marzo_2022.xlsx' )

ambato <- read_excel( file,
                      sheet = 1,
                      col_names = TRUE,
                      col_types = NULL,
                      na = "",
                      skip = 0 ) %>% 
  clean_names( ) %>%
  mutate( nombre = toupper( nombre ) ) %>% 
  dplyr::select( -nombre ) %>% 
  mutate( remuneracion = as.double( remuneracion ) )
  

ambato <- cod_utf_win( ambato ) %>% 
  right_join( rc, ., by = 'cedula' ) %>% 
  dplyr::select( cedula,
                 nombre,
                 sexo, 
                 fecha_nacimiento,
                 puesto,
                 remuneracion ) %>% 
  mutate( ciudad = 'ambato') %>% 
  mutate( puesto = firstup( puesto ) )%>% 
  filter( grepl( 'trans', puesto ) )


ambato <- ambato %>% 
  mutate( puesto_cod = if_else( grepl( 'Agente', puesto ),
                                'Agente de tránsito 1',
                                NA ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Analista', puesto ),
                               'Analista de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'efe', puesto ),
                               'Jefe de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Especialista', puesto ),
                               'Analista de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Director', puesto ),
                               'Director de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Tecnico', puesto ),
                               'Técnico de tránsito',
                               puesto_cod ) ) 

# 2.Riobamba-----------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/GAD/riobamba_marzo_2022.xlsx' )

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
  mutate( ciudad = 'riobamba') %>% 
  mutate( puesto = firstup( puesto ) )%>% 
  filter( grepl( 'tránsito', puesto ) )


riobamba <- riobamba %>% 
  mutate( puesto_cod = if_else( grepl( 'Agente', puesto ),
                                'Agente de tránsito',
                                NA ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Analista', puesto ),
                               'Analista de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'efe', puesto ),
                               'Jefe de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Especialista', puesto ),
                               'Analista de tránsito',
                               puesto_cod ) ) %>%
  mutate( puesto_cod = ifelse( grepl( 'Coordinador', puesto ),
                               'Coordinador de tránsito',
                               puesto_cod ) ) %>%
  mutate( puesto_cod = ifelse( grepl( 'Director', puesto ),
                               'Director de tránsito',
                               puesto_cod ) ) %>%
  mutate( puesto_cod = ifelse( grepl( 'Tecnico', puesto ),
                               'Técnico de tránsito',
                               puesto_cod ) ) 

# 3.Loja-----------------------------------------------------------------------------------------
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
  mutate( ciudad = 'loja') %>% 
  mutate( puesto = firstup( puesto ) )%>% 
  filter( grepl( 'trans', puesto ) )


loja <- loja %>% 
  mutate( puesto_cod = if_else( grepl( 'Agente', puesto ),
                                'Agente de tránsito',
                                NA ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Analista', puesto ),
                               'Analista de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'efe', puesto ),
                               'Jefe de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Especialista', puesto ),
                               'Analista de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Comisario', puesto ),
                               'Comisario de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Coordinador', puesto ),
                               'Coordinador de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Director', puesto ),
                               'Director de tránsito',
                               puesto_cod ) ) %>% 
  mutate( puesto_cod = ifelse( grepl( 'Tecnico', puesto ),
                               'Técnico de tránsito',
                               puesto_cod ) ) 

# 20. Consolidación---------------------------------------------------------------------------------

gad <- rbind( ambato,
              riobamba,
              loja )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( ambato,
      riobamba,
      loja,
      file = paste0( parametros$RData, 'COESCOP_gad.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ]  )
gc( )
