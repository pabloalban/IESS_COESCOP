message(paste(rep("-", 100), collapse = ""))

message("\tCargando servidores COESCOP")

# Carga de bases------------------------------------------------------------------------------------
load( paste0( parametros$RData, "COESCOP_bomberos_cargos.RData" )) #bomberos
load( paste0( parametros$RData, "COESCOP_cte_cargos.RData") ) #cte
load( paste0( parametros$RData, "COESCOP_snai_cargos.RData") ) #snai
load( paste0( parametros$RData, "COESCOP_snmlcf_cargos.RData") ) #snmlcf
load( paste0( parametros$RData, "COESCOP_metropolitanos_cargos.RData") ) #metropolitanos
load( paste0( parametros$RData, "COESCOP_aduaneros_cargos.RData") ) #aduaneros
load( paste0( parametros$RData, "IESS_imposiciones_2022_12.RData") ) 


#Proyección de salarios-----------------------------------------------------------------------------



#Proyección de pensiones----------------------------------------------------------------------------


#Guardar en Rdata-----------------------------------------------------------------------------------
save( coescop,
      factor,
      file = paste0( parametros$RData, 'IESS_consolidado_coescop.RData' ) )

# #-------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()