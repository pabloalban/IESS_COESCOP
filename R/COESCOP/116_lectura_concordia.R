message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de policias de la Concordia' )

#Cargando información financiera--------------------------------------------------------------------
file <- paste0( parametros$Data, 'COESCOP_concordia.xlsx' )


#Carga de datos-------------------------------------------------------------------------------------
concordia <- read_excel( file,
                         sheet = 1,
                         col_names = TRUE,
                         col_types = NULL,
                         na = "N/A",
                         skip = 9 ) %>% clean_names()

#Cargando base del RC-------------------------------------------------------------------------------

load( paste0( parametros$RData, "IESS_Reg_Civil.RData" ) )

#Función quitar tildes------------------------------------------------------------------------------
quitar_tildes <- function(xtb_aux) {
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Á", "A", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("É", "E", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Í", "I", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ó", "O", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ú", "U", x, fixed = TRUE) else x }))
  #xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ñ", "N", x, fixed = TRUE) else x }))
  #xtb_aux <- xtable(xtb_aux)
  return(xtb_aux)
}

#Cruce con base del Registro civil------------------------------------------------------------------

concordia <- quitar_tildes( concordia ) %>% 
  filter( puesto_institucional == 'POLICIA MUNICIPAL' ) %>% 
  left_join( .,rc, by = c( "nombre" = "nombre" ) ) %>% 
  mutate( fecha_nacimiento = if_else( is.na( fecha_nacimiento ),
                                      mean( fecha_nacimiento, na.rm = TRUE ),
                                      fecha_nacimiento ) ) %>% 
  mutate( sexo = if_else( is.na( sexo ),
                          'M',
                          sexo ) ) %>% 
  mutate( puesto_institucional = if_else( puesto_institucional == 'POLICIA MUNICIPAL',
                                          'Policía Municipal',
                                          puesto_institucional ) )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( concordia,
      file = paste0( parametros$RData, 'COESCOP_concordia.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()