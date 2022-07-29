message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura del portafolio de inversiones' )

# Carga de datos -----------------------------------------------------------------------------------
file_inversiones <- paste0( parametros$RData_seg, 'IESS_RTR_inversiones.RData' )
load( file = file_inversiones )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Tabla de la evolución del portafolio -------------------------------------------------------------
aux <- recurs_adm_biess
aux$ano <- as.character(aux$ano)
aux$rendimiento_neto <- aux$rendimiento_neto*100
aux$rendimiento_ponderado <- aux$rendimiento_ponderado*100
aux$rendimiento_neto_real <- aux$rendimiento_neto_real*100

aux_xtab <- xtable( aux, digits = c(0,0,rep(2,6),0) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_total_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla Resumen situacion actual de las inversiones--------------------------------------------------
aux <- inver_corte
aux$rendimiento_promedio <- aux$rendimiento_promedio*100
aux$rendimiento_promedio_real <- aux$rendimiento_promedio_real*100 
aux <- as.data.table( aux )
aux_xtab <- tildes_a_latex( aux_xtab )
aux <- aux[ , print_names := c( "Bonos",
                                "CETES",
                                "Certificados CFN",
                                "Obligaciones",
                                "Titularizaciones",
                                "Papel Comercial",
                                "Pr\\\'{e}stamos Quirografarios",
                                "Total inversiones")]

aux <- aux[ ,list(print_names,
                  valor_nominal,
                  rendimiento_promedio,
                  rendimiento_promedio_real,
                  plazo_promedio_dias
                  )]
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_corte', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))



# Tabla Rendimientos con ingresos y gastos -----------------------------------------------------
aux <- rendimientos_netos
aux$rendimiento_bruto <- aux$rendimiento_bruto*100
aux$rendimiento_neto <- aux$rendimiento_neto*100
aux$corte_a <- format(aux$corte_a, "%b/%Y")
aux$corte_a <- as.character(aux$corte_a)
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rend_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Tabla del detalle de los ingresos que producieron las inversiones --------------------------------
aux <- ingresos[,c(1,4:10)]

# aux[2,1]<-"De deuda renta fija sector p\\\'{u}blico"
# aux[3,1]<-"De inversiones privativas pr\\\'{e}stamos quirografarios"
# aux[4,1]<-"En valuaci\\\'{o}n inversiones"
aux_xtab <- xtable( aux, digits = c(0,rep(2,8)) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingre_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Tabla del detalle de los gastos que producieron las inversiones ----------------------------------
aux <- gastos_opera[,c(1,4:10)]
# aux[1,1]<-"Comisi\\\'{o}n bolsa de valores"
# aux[3,1]<-"En valuaci\\\'{o}n de inversiones"
# aux[5,1]<-"Provisiones antic\\\'{i}clicas y gen\\\'{e}ricas"
# aux[6,1]<-"Gasto liquidaci\\\'{o}n fideicomisos"
# aux[10,1]<-"Gastos provisi\\\'{o}n mora patronal pr\\\'{e}stamos"
# aux[11,1]<-"Provisi\\\'{o}n para valuaci\\\'{o}n inversiones de capital"
aux_xtab <- xtable( aux, digits = c(0,rep(2,8)) )
aux_xtab <- tildes_a_latex(aux_xtab)
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       compress = FALSE,
       fileEncoding="UTF-8",
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Tabla evolución Inversiones en Créditos --------------------------------------
aux <- inv_instrumento %>%
       filter(instrumento=='Créditos Quirografarios') %>%
       na.omit() %>%
       mutate(rdto_prom_pond=rdto_prom_pond*100,
              rend_promedio_real=rend_promedio_real*100,
              ano=as.character(ano)) %>%
       select(-inflacion,-instrumento) %>%
  arrange(ano)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_creditos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


# Tabla evolución Inversiones en Bonos del Estado Ecuatoriano --------------------------------------
aux <- inv_instrumento %>%
        filter(instrumento=='Bonos del Estado') %>%
        na.omit() %>%
        mutate(rdto_prom_pond=rdto_prom_pond*100,
               rend_promedio_real=rend_promedio_real*100,
               ano=as.character(ano)) %>%
        select(-inflacion,-instrumento) %>%
  arrange(ano)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


# Tabla detalle Inversiones en Bonos del Estado Ecuatoriano al corte--------------------------------
aux <- detalle_bonos
#aux <- na.omit(aux)
#aux <- select(aux,-Detalle)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,0,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla Recepción de Bonos del Estado por el 40% del pago de las pensiones---------------------------
aux <- detalle_bonos_40 %>%
       mutate(tasa=tasa*100,
              fecha_colocacion=as.character(fecha_colocacion),
              vencimiento=as.character(vencimiento),
              pago_del_periodo=as.character(format(detalle_bonos_40$pago_del_periodo,"%Y-%B")))
aux_xtab <- xtable( aux, digits = c(0,0,0,0,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_repbonos40_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla Evolución Inversiones en Obligaciones--------------------------------------------------------
aux <- inv_instrumento %>%
      filter(instrumento=='Obligaciones') %>%
      na.omit() %>%
      mutate(rdto_prom_pond=rdto_prom_pond*100,
             rend_promedio_real=rend_promedio_real*100,
             ano=as.character(ano)) %>%
      select(-inflacion,-instrumento) %>%
  arrange(ano)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_obligaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla detalle de las inversiones en obligaciones---------------------------------------------------
aux <- detalle_obligaciones
aux[5,1]<-"EL ORDE\'NO S.A."
aux[14,1]<-"PETROLEOS DE LOS R\\\'{I}OS PETROLRIOS C.A."
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_oblig_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla Evolución Inversiones en Titularizaciones----------------------------------------------------
aux <- inv_instrumento %>%
        filter(instrumento=='Titularizaciones') %>%
        na.omit() %>%
        mutate(rdto_prom_pond=rdto_prom_pond*100,
               rend_promedio_real=rend_promedio_real*100,
               ano=as.character(ano)) %>%
        select(-inflacion,-instrumento) %>%
  arrange(ano)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_titularizaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla detalle de las inversiones en Titularizaciones-----------------------------------------------
aux <- detalle_titularizaciones
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_titul_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
gc()
