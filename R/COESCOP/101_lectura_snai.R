message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los agentes del snai' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data, 'COESCOP_snai.xlsx' )


#Carga de snai por ciudad-----------------------------------------------------------------------
snai <- read_excel(file,
                       sheet = 1,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()


#Cargando base del RC-------------------------------------------------------------------------------

load(paste0(parametros$RData, "IESS_Reg_Civil.RData"))

#Cruce con base del Registro civil------------------------------------------------------------------

snai <- left_join( snai , rc, by = c("nombre"="nombre") )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( snai,
      file = paste0( parametros$RData, 'COESCOP_snai.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
