message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los coeficientes del ISSPOL' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data, 'COESCOP_coeficientes.xlsx' )


#Caraga de recursos administrados por el BIESS------------------------------------------------------
coeficientes <- read_excel(file,
                               sheet = 1,
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0) %>% clean_names()



#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando coeficientes en un solo data.frame' )

save( coeficientes, 
      file = paste0( parametros$RData, 'ISSPOL_coeficientes.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()