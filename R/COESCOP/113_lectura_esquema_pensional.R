message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura del esquema pensional del COESCOP' )

#Cargando información financiera--------------------------------------------------------------------
file_tasa <- paste0(parametros$Data, 'COESCOP_tasa_aportacion.xlsx' )
file_coeficiente <- paste0(parametros$Data, 'COESCOP_tabla_coeficiente.xlsx' )

#Carga de la tasa de aportación---------------------------------------------------------------------
tab_tasa <- read_excel( file_tasa,
                        sheet = 1,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0) %>% clean_names()


#Carga de los coeficientes--------------------------------------------------------------------------
tab_coeficientes <- read_excel( file_coeficiente,
                        sheet = 1,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0) %>% clean_names()

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( tab_tasa,
      tab_coeficientes,
      file = paste0( parametros$RData, 'COESCOP_esquema_pensional.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()