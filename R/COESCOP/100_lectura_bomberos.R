message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura del cuerpo de bomberos por ciudades' )

#Cargando información financiera------------------------------------------------
file<-paste0(parametros$Data, 'COESCOP_bomberos.xlsx' )


#Carga de bomberos por ciudad---------------------------------------------------
bomberos <- read_excel(file,
                               sheet = 1,
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0) %>% clean_names()

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( bomberos,
      file = paste0( parametros$RData, 'COESCOP_bomberos.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()