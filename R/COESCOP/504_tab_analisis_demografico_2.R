message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tCreación de las tablas del análisis demográfico' )

#Carga de función tildes a latex--------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R',
        encoding = 'UTF-8',
        echo = FALSE )

#Carga de datos-------------------------------------------------------------------------------------
load( paste0( parametros$RData, "IESS_tablas_contingencia_2.RData" ) )


#Tablas de agentes de control-----------------------------------------------------------------------
message( '\tCreación de las tablas de agentes de control' )

##Tabla por rangos de edades------------------------------------------------------------------------
##Ambato
aux <- tab_control_ambato$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_ambato_sexo', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ),
                   nrow( aux ) - 1 ),
  sanitize.text.function = identity
)

##Duran
aux <- tab_control_duran$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_duran_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Esmeralda
aux <- tab_control_esmeraldas$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_esmeraldas_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Guayaquil
aux <- tab_control_guayaquil$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_guayaquil_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Guayaquil
aux <- tab_control_latacunga$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_latacunga_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Machala
aux <- tab_control_machala$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_machala_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Portoviejo
aux <- tab_control_p_viejo$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_p_viejo_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Quito
aux <- tab_control_quito$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_quito_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Santo Domingo
aux <- tab_control_s_domingo$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_s_domingo_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)


##Tabla de cargos por sexo--------------------------------------------------------------------------
##Ambato
aux <- tab_control_ambato$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_ambato_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Duran
aux <- tab_control_duran$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_duran_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Esmeralda
aux <- tab_control_esmeraldas$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_esmeraldas_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Guayaquil
aux <- tab_control_guayaquil$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_guayaquil_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Latacunga
aux <- tab_control_latacunga$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_latacunga_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Machala
aux <- tab_control_machala$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_machala_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Portoviejo
aux <- tab_control_p_viejo$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_p_viejo_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)


##Quito
aux <- tab_control_quito$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_quito_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Santo Domingo
aux <- tab_control_s_domingo$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_s_domingo_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Tabla de salarios promedios-----------------------------------------------------------------------
##Ambato
aux <- tab_control_ambato$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_ambato_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Duran
aux <- tab_control_duran$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_duran_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Esmeralda
aux <- tab_control_esmeraldas$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_esmeraldas_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Guayaquil
aux <- tab_control_guayaquil$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_guayaquil_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Latacunga
aux <- tab_control_latacunga$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_latacunga_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Machala
aux <- tab_control_machala$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_machala_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Portoviejo
aux <- tab_control_p_viejo$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_p_viejo_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Quito
aux <- tab_control_quito$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_quito_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Santo Domingo
aux <- tab_control_s_domingo$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_control_s_domingo_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)


#Tablas de agentes de trásito-----------------------------------------------------------------------
message( '\tCreación de las tablas de agentes de trásito' )

##Tabla por rangos de edades------------------------------------------------------------------------

##Cuenca
aux <- tab_transito_cuenca$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_cuenca_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Duran
aux <- tab_transito_duran$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_duran_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Esmeraldas
aux <- tab_transito_esmeraldas$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_esmeraldas_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Guayaquil
aux <- tab_transito_guayaquil$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_guayaquil_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Ibarra
aux <- tab_transito_ibarra$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_ibarra_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Latacunga
aux <- tab_transito_latacunga$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_latacunga_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Machala
aux <- tab_transito_machala$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_machala_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Quito
aux <- tab_transito_quito$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_quito_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Santo Domingo
aux <- tab_transito_s_domingo$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_s_domingo_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Ambato
aux <- tab_transito_ambato$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_ambato_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Riobamba
aux <- tab_transito_riobamba$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_riobamba_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Loja
aux <- tab_transito_loja$edad_rangos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_loja_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Tabla de cargos por sexo--------------------------------------------------------------------------

##Cuenca
aux <- tab_transito_cuenca$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_cuenca_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Duran
aux <- tab_transito_duran$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_duran_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Esmeraldas
aux <- tab_transito_esmeraldas$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_esmeraldas_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Guayaquil
aux <- tab_transito_guayaquil$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_guayaquil_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Ibarra
aux <- tab_transito_ibarra$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_ibarra_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Latacunga
aux <- tab_transito_latacunga$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_latacunga_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Machala
aux <- tab_transito_machala$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_machala_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Quito
aux <- tab_transito_quito$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_quito_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Santo Domingo
aux <- tab_transito_s_domingo$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_s_domingo_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Ambato
aux <- tab_transito_ambato$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_ambato_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Riobamba
aux <- tab_transito_riobamba$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_riobamba_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Loja
aux <- tab_transito_loja$num_puestos_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_loja_puestos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)

##Tabla de salarios promedios-----------------------------------------------------------------------
##Cuenca
aux <- tab_transito_cuenca$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_cuenca_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Duran
aux <- tab_transito_duran$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_duran_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Esmeraldas
aux <- tab_transito_esmeraldas$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_esmeraldas_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Guayaquil
aux <- tab_transito_guayaquil$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_guayaquil_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Ibarra
aux <- tab_transito_ibarra$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_ibarra_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Latacunga
aux <- tab_transito_latacunga$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_latacunga_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Machala
aux <- tab_transito_machala$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_machala_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Quito
aux <- tab_transito_quito$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_quito_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Santo Domingo
aux <- tab_transito_s_domingo$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_s_domingo_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Ambato
aux <- tab_transito_ambato$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_ambato_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Riobamba
aux <- tab_transito_riobamba$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_riobamba_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

##Loja
aux <- tab_transito_loja$sal_prom %>%
  xtable( ., digits = c( 0, 0, rep( 2, ncol( . ) - 1 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_transito_loja_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) ),
       sanitize.text.function = identity
)

#Limpiando memoria RAM------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% c( 'parametros' ) )] )
gc(  )


