message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de las Entidades del Sistema Especializado Integral de Investigación, Medicina Legal y Ciencias Forenses' )

#Cargando información financiera------------------------------------------------
file<-paste0(parametros$Data, 'COESCOP_snmlcf.xlsx' )


#Carga de bomberos por ciudad---------------------------------------------------
snmlcf <- read_excel(file,
                       sheet = 1,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()

#Cargando base del RC-------------------------------------------------------------------------------

load(paste0(parametros$RData, "IESS_Reg_Civil.RData"))

#Cruce con base del Registro civil------------------------------------------------------------------

snmlcf <- left_join( snmlcf , rc, by = c("nombre"="nombre") )



#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( snmlcf,
      file = paste0( parametros$RData, 'COESCOP_snmlcf.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
