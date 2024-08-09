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
file<-paste0( parametros$Data, '/transito/cuenca_03_2022.xlsx' )

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
  mutate( ciudad = 'cuenca')


# 2.Duran-------------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/transito/duran_enero_2024.xlsx' )

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
  mutate( ciudad = 'duran')

# 3.Esmeraldas--------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/transito/esmeraldas_marzo_2022.xlsx' )

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
  mutate( ciudad = 'esmeraldas')


# 4. Guayaquil--------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/transito/guayaquil_marzo_2022.xlsx' )

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
  mutate( ciudad = 'guayaquil')



# 5. ibarra-----------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/transito/ibarra_octubre_2023.xlsx' )

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
  mutate( ciudad = 'ibarra')


# 6. latacunga--------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/transito/latacunga_marzo_2022.xlsx' )

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
  mutate( ciudad = 'latacunga')

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
  mutate( ciudad = 'machala')


# 9. Portoviejo-------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/transito/portoviejo_febrero_2022.xlsx' )

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
  mutate( ciudad = 'portoviejo')


# 10. Quito-------------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/transito/quito_agosto_2022.xlsx' )

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
  mutate( ciudad = 'quito')

# 11. Santo Domingo---------------------------------------------------------------------------------
file<-paste0( parametros$Data, '/transito/santo_domingo_junio_2023.xlsx' )

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
  mutate( ciudad = 's_domingo')


#Carga de metropolitanos por ciudad-----------------------------------------------------------------------
metropolitanos <- read_excel( file,
                              sheet = 1,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0 ) %>% clean_names( )

metropolitanos$nombre <- toupper( metropolitanos$nombre ) #transformar a mayusculas

#Cargando base del RC-------------------------------------------------------------------------------

load( paste0( parametros$RData, "IESS_Reg_Civil.RData" ) )

#Cruce con base del Registro civil------------------------------------------------------------------

metropolitanos <- left_join( metropolitanos , rc, by = c( "nombre"="nombre" ) )


#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( metropolitanos,
      file = paste0( parametros$RData, 'COESCOP_metropolitanos.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ]  )
gc( )
