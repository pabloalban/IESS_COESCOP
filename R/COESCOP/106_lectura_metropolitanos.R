message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura del Cuerpo de Agentes de Control Municipal o Metropolitano' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data, 'COESCOP_metropolitanos.xlsx' )


#Carga de metropolitanos por ciudad-----------------------------------------------------------------------
metropolitanos <- read_excel(file,
                   sheet = 1,
                   col_names = TRUE,
                   col_types = NULL,
                   na = "",
                   skip = 0) %>% clean_names()

metropolitanos$nombre <- toupper(metropolitanos$nombre) #transformar a mayusculas

#Cargando base del RC-------------------------------------------------------------------------------

load(paste0(parametros$RData, "IESS_Reg_Civil.RData"))

#Cruce con base del Registro civil------------------------------------------------------------------

metropolitanos <- left_join( metropolitanos , rc, by = c("nombre"="nombre") )


#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( metropolitanos,
      file = paste0( parametros$RData, 'COESCOP_metropolitanos.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
