message( paste( rep('-', 100 ), collapse = '' ) )
#Carga de Rdata-------------------------------------------------------------------------------------
#load( paste0( parametros$RData, 'IESS_tabla_mortalidad_dinamica.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_tasa_mortalidad_pensionistas_int.RData' ) )

#Tabla de mortalidad de pensionistas de incapacidad permanete total, absoluta y parcial-------------
message( '\t Tabala de mortalidad de pensionistas de incapacidad permanete total, absoluta y parcial' )
rtr_mor <- mortalidad_jubilados_rtr_int[ x >= 0, list( x, ud_rtr= ud_rtr_est ) ]
rtr_mor[ , px := exp( -ud_rtr )]
rtr_mor[ , qx := 1 - px ]
rtr_mor[ , lx := shift( px, fill = 1, type = 'lag' ) ]
rtr_mor[ , lx := 1e5 * cumprod( lx ) ]
rtr_mor[ , dx := lx * qx ]
rtr_mor[ , ex := rev( cumsum( rev( lx ) ) ) ]
rtr_mor[ , ex := ex / lx - 0.5 ]

#Guardar tabla mortalidad en Rdata------------------------------------------------------------------
save( rtr_mor, 
      file = paste0( parametros$RData_seg, 'IESS_tablas_biometricas_mortalidad_pensionistas_rtr.RData' ) )

#Limpiar memoria------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
