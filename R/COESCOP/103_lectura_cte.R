message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura del cuerpo de vigilancia de la comisión de tránsito' )

#Cargando información financiera------------------------------------------------
file<-paste0(parametros$Data, 'COESCOP_cte.xlsx' )


#Carga del cte------------------------------------------------------------------
transito <- read_excel(file,
                       sheet = 1,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()

#Cargando base del RC-------------------------------------------------------------------------------

load(paste0(parametros$RData, "IESS_Reg_Civil.RData"))

#Cruce con base del Registro civil------------------------------------------------------------------

transito <- left_join( transito , rc, by = c("nombre"="nombre") )

#Guardando en un Rdata----------------------------------------------------------
message( '\tGuardando en data.frame' )

save( transito,
      file = paste0( parametros$RData, 'COESCOP_cte.RData' ) )

#Borrando data.frames-----------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
