message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero_portafolio_inversiones.RData' ) )
# Activo Fondo--------------------------------------------------------------------------------------
message( '\tGraficando activo del fondo' )
unidad<-1e6
aux <- as.data.table(activo_del_fondo)
aux[,Activo:=Activo/unidad]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1400)
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_activo_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Activo, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_activo_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_activo_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Cuentas por cobrar del Fondo de RT ----------------------------------------------------------------
message( '\tGraficando Cuentas por cobrar del Fondo de RT ' )
unidad<-1e6
aux <- as.data.table( cuentas_cobrar_fondo )
aux <- aux[,`Cuentas por Cobrar`:=`Cuentas por Cobrar`/unidad]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 90)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_cobrar_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = `Cuentas por Cobrar`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_cuentas_cobrar_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_cobrar_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pasivo del Fondo de RT-----------------------------------------------------------------------------
message( '\tGraficando Pasivo del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( pasivos_fondo )
aux <- aux[,Pasivo:=Pasivo/unidad]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pasivo_fondo <-ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Pasivo, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5 ) )

ggsave( plot = iess_pasivo_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_pasivo_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Cuentas por pagar del  Fondo de RT-----------------------------------------------------------------
message( '\tGraficando cuentas por pagar del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( cuentas_pagar_fondo )
aux <- aux[,`Cuentas por Pagar`:=`Cuentas por Pagar`/unidad]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_pagar_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = `Cuentas por Pagar`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5 ) )

ggsave( plot = iess_cuentas_pagar_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_pagar_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Patrimonio del  Fondo de RT------------------------------------------------------------------------
message( '\tGraficando patrimonio del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( patrimonio_fondo )
aux <- aux[,Patrimonio:=Patrimonio/unidad]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1500)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6  )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_patrimonio_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Patrimonio, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5 ) )

ggsave( plot = iess_patrimonio_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_patrimonio_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Ingresos del  Fondo de RT--------------------------------------------------------------------------
message( '\tGraficando ingrensos del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( ingresos_fondo )
aux <- aux[, Ingresos:=Ingresos/unidad]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 250)
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ingresos_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Ingresos, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5 ) )

ggsave( plot = iess_ingresos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_ingresos_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Ingresos por aporte del  Fondo de RT---------------------------------------------------------------
message( '\tGraficando ingrensos por aporte del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( ingresos_aportes )
aux <- aux[, `Aportes Afiliados ($)`:=`Aportes Afiliados ($)`/unidad]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 200)
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ingresos_aportes_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = `Aportes Afiliados ($)`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5 ) )

ggsave( plot = iess_ingresos_aportes_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_ingresos_aportes_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Contribución del estado al  Fondo de RT-----------------------------------------------------------
message( '\tGraficando contribución del estado al Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( otros_ingresos )
aux <- aux[, `Contribucion del Estado`:=`Contribucion del Estado`/unidad]
aux[ is.na(`Contribucion del Estado`),`Contribucion del Estado`:=0]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 24)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6  )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cont_estado_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = `Contribucion del Estado`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5 ) )

ggsave( plot = iess_cont_estado_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cont_estado_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución del gasto del   Fondo de RT--------------------------------------------------------------
message( '\tGraficando evolución del gasto del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( gastos )
aux <- aux[, `Gastos`:=`Gastos`/unidad]

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 100)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_gastos_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Gastos, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5 ) )

ggsave( plot = iess_gastos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_gastos_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# #Egresos pagados por prestaciones del Fondo de RT---------------------------------------------------
message( '\tGraficando egresos pagados por prestaciones del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( gastos_prestacionales )
aux <- aux[, `Prestaciones por Pensiones`:=`Prestaciones por Pensiones y subsidios`/unidad]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 80)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_gastos_prestacionales_fondo <- ggplot( data = aux ) +
  geom_line( aes( x = AÑO,
                  y =`Prestaciones por Pensiones`,
                  color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ),
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl,
                      limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl,
                      limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5 ) )

ggsave( plot = iess_gastos_prestacionales_fondo,
        filename = paste0( parametros$resultado_graficos, 'iess_gastos_prestacionales_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Otros gastos del Fondo de RT.----------------------------------------------------------------------
message( '\tGraficando otros gastos del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( gast_adm )
x_lim <- c( 2013, 2020 )
x_brk <- 2013:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1500000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_otros_gatos_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = AÑO, 
                  y =`Gastos Administrativos`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'USD') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5 ) )

ggsave( plot = iess_otros_gatos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_otros_gatos_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Portafolio de inversiones -------------------------------------------------------------------------
message( '\tGraficando evolución histórica de las inversiones en valor nominal' )

aux <- copy( evo_recur_adm)
aux <- aux[ano>2011,]
unidad <- 1e6
aux[,inversiones:=inversiones/unidad]
aux[,anio:=as.character(ano)]

aux1 <- data.frame(f=seq( 1, 7, 1 ), anio=aux$anio, 
                   inver=aux$inversiones,
                   rendi=aux$rendimiento_neto*100)

x_lim <- c( 1, 8 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- c(aux1[,2])

y_lim <- c( 0, 1000 )
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim_dual <- c( 0, 12 )
y_brk_dual <- seq(0,11,2)
y_lbl_dual <- paste0(formatC( y_brk_dual, 
                              digits = 0, 
                              format = 'f', 
                              big.mark = '.', 
                              decimal.mark = ',' ),"%")

iess_evol_hist_inver_valor_nominal <- ggplot(data=aux1) + 
  geom_bar(mapping = aes(x = f, y = inver, 
                         fill=parametros$iess_green),
           stat="identity",
           colour='black' ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(mapping = aes(x = f, y = rendi*900/10, group = 1, 
                          linetype = 'Rendimiento neto'
  )
  )+
  scale_linetype_manual(NULL, values = 1)+
  scale_y_continuous( name = 'Saldo (Millones USD)', breaks = y_brk, 
                      labels = y_lbl, limits = y_lim ,
                      sec.axis = sec_axis(~ . *10/900, 
                                          name="Rendimiento neto",
                                          labels = y_lbl_dual,
                                          breaks = y_brk_dual)
  ) +
  scale_x_discrete( limits = x_lbl )+
  #scale_fill_manual(values = c('#007435', '#003F8A'))+
  scale_fill_manual(values = c(parametros$iess_green),
                    labels = c("Inversiones (USD)"))+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,
                             label.position = "right", 
                             label.hjust = 0))+
  theme(legend.position="bottom")+
  labs( x = '', y = '' )+
  #theme(legend.box.spacing=unit(0.75, "cm"))+
  theme(legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent',
                                             colour = 'NA'),
        legend.key = element_rect(fill = 'transparent'),
        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_evol_hist_inver_valor_nominal, 
        filename = paste0( parametros$resultado_graficos, 'iess_evol_hist_inver_valor_nominal_rtr', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución histórica de las inversiones en préstamos quirografarios---------------------------------
message( '\tGraficando evolución histórica de las inversiones en préstamos quirografarios ' )
aux <- copy( prestamos_quirografarios)
unidad <- 1e6
aux[,credito:=credito/unidad]
aux[,anio:=as.character(anio)]

aux1 <- data.frame(f=seq( 1, 8, 1 ), anio=aux$anio, credito=aux$credito, rendi=aux$rendimiento*100)

x_lim <- c( 1, 8 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- c(aux1[,2])

y_lim <- c( 0, 500 )
y_brk <- seq( y_lim[1], y_lim[2], 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim_dual <- c( 0, 15 )
y_brk_dual <- seq(0,15,3)
y_lbl_dual <- paste0(formatC( y_brk_dual, 
                              digits = 0, 
                              format = 'f', 
                              big.mark = '.', 
                              decimal.mark = ',' ),"%")

iess_prestamos_quirografarios <- ggplot(data=aux1) + 
  geom_bar(mapping = aes(x = f, y = credito, fill=parametros$iess_green),
           stat="identity",
           colour='black' ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(mapping = aes(x = f, y = rendi*500/15, group = 1,
                          linetype = 'Rendimiento Promedio Ponderado'
  )
  )+
  scale_linetype_manual(NULL, values = 1)+
  scale_y_continuous( name = 'Saldo (Millones USD)', breaks = y_brk, labels = y_lbl, 
                      limits = y_lim ,
                      sec.axis = sec_axis(~ . *15/500, name="Rendimiento",
                                          labels = y_lbl_dual,
                                          breaks = y_brk_dual)
  ) +
  scale_x_discrete( limits = x_lbl )+
  #scale_fill_manual(values = c('#007435', '#003F8A'))+
  scale_fill_manual(values = c(parametros$iess_green),
                    labels = c("Créditos Quirografario"))+
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0))+
  theme(legend.position="bottom")+
  labs( x = '', y = '' )+
  theme(legend.box.spacing=unit(-0.75, "cm"))+
  theme(legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent',
                                             colour = 'NA'),
        legend.key = element_rect(fill = 'transparent'),
        legend.spacing = unit(0.75, 'lines'))

ggsave( plot = iess_prestamos_quirografarios, 
        filename = paste0( parametros$resultado_graficos, 'iess_prestamos_quirografarios_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución histórica de las inversiones entitularizaciones------------------------------------------
message( '\tGraficando evolución histórica de las inversiones en titularizaciones' )
aux <- copy( inversiones_titulos_financieros)
unidad <- 1e6
aux[,titularizacion:=titularizacion/unidad]
aux[,anio:=as.character(anio)]

aux1 <- data.frame(f=seq( 1, 8, 1 ), anio=aux$anio, titularizacion=aux$titularizacion, 
                   rendi=aux$rendimiento*100)

x_lim <- c( 1, 8 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- c(aux1[,2])

y_lim <- c( 0, 60 )
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim_dual <- c( 0, 12 )
y_brk_dual <- seq(0, 12, 3)
y_lbl_dual <- paste0(formatC( y_brk_dual, 
                              digits = 0, 
                              format = 'f', 
                              big.mark = '.', 
                              decimal.mark = ',' ),"%")

iess_inversiones_titulos_financieros <- ggplot(data=aux1) + 
  geom_bar(mapping = aes(x = f, y = titularizacion, fill=parametros$iess_green),
           stat="identity",
           colour='black' ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(mapping = aes(x = f, y = rendi*60/12, group = 1, 
                          linetype = 'Rendimiento Promedio Ponderado'))+
  scale_linetype_manual(NULL, values = 1)+
  scale_y_continuous( name = 'Saldo (Millones USD)', breaks = y_brk, labels = y_lbl, 
                      limits = y_lim ,
                      sec.axis = sec_axis(~ . *12/60, name="Rendimiento",
                                          labels = y_lbl_dual,
                                          breaks = y_brk_dual)) +
  scale_x_discrete( limits = x_lbl )+
  #scale_fill_manual(values = c('#007435', '#003F8A'))+
  scale_fill_manual(values = c(parametros$iess_green),
                    labels = c("Titularizaciones"))+
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0))+
  theme(legend.position="bottom")+
  labs( x = '', y = '' )+
  theme(legend.box.spacing=unit(-0.75, "cm"))+
  theme(legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent', colour = 'NA'),
        legend.key = element_rect(fill = 'transparent'),
        legend.spacing = unit(0.75, 'lines'))

ggsave( plot = iess_inversiones_titulos_financieros, 
        filename = paste0( parametros$resultado_graficos, 
                           'iess_inversiones_titulos_financieros_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, 
        dpi = graf_dpi )

#Evolución histórica de las inversiones en obligaciones---------------------------------------------
message( '\tGraficando evolución histórica de las inversiones en obligaciones' )
aux <- copy( inversiones_obligaciones)
aux[,anio:=as.character(anio)]

aux1 <- data.frame(f=seq( 1, 8, 1 ), anio=aux$anio,obligacion=aux$obligacion, rendi=aux$rendimiento)

scl = 10000000 # escala en miles de millones
hmts= 3

x_lim <- c( 1, 8 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- c(aux1[,2])

y_lim <- c( 0, 40000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/unidad, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

#y_lim_dual <- c( 0, 9 )
y_brk_dual <- c( 0.074, 0.077, 0.08, 0.083, 0.086 )
y_lbl_dual <- paste0(formatC( y_brk_dual*100, 
                              digits = 1, 
                              format = 'f', 
                              big.mark = '.', 
                              decimal.mark = ',' ),"%")

iess_inversiones_obligaciones <- ggplot(data=aux1) + 
  geom_bar(mapping = aes(x = f, y = obligacion, 
                         fill=parametros$iess_green),
           stat="identity",
           colour='black' ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(mapping = aes(x = f, 
                          y = rendi*scl*hmts*100-231300000+10000000,
                          group = 1,
                          linetype = 'Rendimiento Promedio Ponderado' ) )+
  scale_linetype_manual(NULL, values = 1)+
  scale_y_continuous( name = 'Saldo (Millones USD)',
                      breaks = y_brk, labels = y_lbl,
                      limits = y_lim ,
                      sec.axis = sec_axis(~ ./(scl*hmts*100)+ 0.07376667, name="Rendimiento",
                                          labels = y_lbl_dual,
                                          breaks = y_brk_dual)) +
  scale_x_discrete( limits = x_lbl )+
  #scale_fill_manual(values = c('#007435', '#003F8A'))+
  scale_fill_manual(values = c(parametros$iess_green),
                    labels = c("Obligaciones"))+
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,label.position = "right",
                             label.hjust = 0, reverse = TRUE))+
  theme(legend.position="bottom")+
  labs( x = '', y = '' )+
  theme(legend.box.spacing=unit(-0.75, "cm"))+
  theme(legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent', 
                                             colour = 'NA'),
        legend.key = element_rect(fill = 'transparent'),
        legend.spacing = unit(0.75, 'lines'))

ggsave( plot = iess_inversiones_obligaciones, 
        filename = paste0( parametros$resultado_graficos, 'iess_inversiones_obligaciones_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
#Evolución histórica de las inversiones en bonos del estado-----------------------------------------
message( '\tGraficando evolución histórica de las inversiones en bonos del estado' )
unidad <- 1e6
aux <- copy( inversiones_bonos)
aux[,anio:=as.character(anio)]

aux1 <- data.frame(f=seq( 1, 8, 1 ), anio=aux$anio,bonos=aux$bonos, rendi=aux$rendimiento)

scl = 100000000 # escala en miles de millones
hmts= 3

x_lim <- c( 1, 8 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- c(aux1[,2])

y_lim <- c( 0, 500000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/unidad, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

#y_lim_dual <- c( 0, 9 )
y_brk_dual <- c( 0.06, 0.064, 0.068, 0.072, 0.076 )
y_lbl_dual <- paste0(formatC( y_brk_dual*100, 
                              digits = 1, 
                              format = 'f', 
                              big.mark = '.', 
                              decimal.mark = ',' ),"%")

iess_inversiones_bonos <- ggplot(data=aux1) + 
  geom_bar(mapping = aes(x = f, y = bonos, 
                         fill=parametros$iess_green),
           stat="identity",
           colour='black' ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(mapping = aes(x = f, 
                          y = rendi*scl*hmts*100-1956000000 + 160000000,
                          group = 1,
                          linetype = 'Rendimiento Promedio Ponderado'))+
  scale_linetype_manual(NULL, values = 1)+
  scale_y_continuous( name = 'Saldo (Millones USD)', 
                      breaks = y_brk, 
                      labels = y_lbl, 
                      limits = y_lim ,
                      sec.axis = sec_axis(~ ./(scl*hmts*100)+ 0.05986667, 
                                          name="Rendimiento",
                                          labels = y_lbl_dual,
                                          breaks = y_brk_dual ) ) +
  scale_x_discrete( limits = x_lbl )+
  #scale_fill_manual(values = c('#007435', '#003F8A'))+
  scale_fill_manual(values = c(parametros$iess_green),
                    labels = c("Bonos del Estado"))+
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, 
                             reverse = TRUE))+
  theme(legend.position="bottom")+
  labs( x = '', y = '' )+
  theme(legend.box.spacing=unit(-0.75, "cm"))+
  theme(legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent', 
                                             colour = 'NA'),
        legend.key = element_rect(fill = 'transparent'),
        legend.spacing = unit(0.75, 'lines'))

ggsave( plot = iess_inversiones_bonos, 
        filename = paste0( parametros$resultado_graficos, 'iess_inversiones_bonos_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Relación patrimonio gasto y beneficio de pensiones------------------------------------------------
message( '\tGraficando patrimonio gasto y beneficio de pensiones' )
aux <- as.data.table( relacion_patrimonio_beneficio )
aux <- aux[1:9]
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 4, 16)
y_brk <- seq( y_lim[1], y_lim[2], 2 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_relacion_patrimonio_beneficio <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = relacion, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Patrimonio / Obligaciones' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, 
                      limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl,
                      limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_relacion_patrimonio_beneficio, 
        filename = paste0( parametros$resultado_graficos, 'iess_relacion_patrimonio_beneficio_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Casos de lesiones profesionales no fatales en america latina---------------------------------------
aux <- copy( lesiones_profesionales )
aux[, pais:=as.character(pais)]
aux <- data.frame(
  pais = factor(c(aux$pais), levels=c(aux$pais)),
  casos = c(aux$casos), constante=rep("pais_1", 7)
)

y_lim <- c( 0, 10000)
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
h <- 4240.3

iess_lesiones_profesionales <- ggplot(data=aux, aes(x=pais, y=casos, fill=constante)) + 
  geom_bar(stat="identity") +  
  geom_text(aes(label =  formatC(casos,digits = 2, format = 'f', big.mark = '.', decimal.mark = ',')
                , y = casos), size = 3,
            position = "identity", vjust = -1, family = "Times New Roman" )+
  geom_hline( aes(yintercept = h,  linetype = "Valor promedio"), 
              colour = parametros$iess_green ) +
  geom_text(aes(0, h, label = formatC(h, digits=2, format='f', big.mark = '.', decimal.mark = ','))
            , vjust=-0.75, hjust=-9, position = "identity", size=3
            , family = "Times New Roman", colour=parametros$iess_green ) + 
  scale_linetype_manual(name = NULL, values = c(2), 
                        guide = guide_legend(override.aes = list(color = c(parametros$iess_green)))) +
  labs( x = NULL, y = 'Casos' ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("País", "´Promedio")) +
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL, label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom") +
  theme(legend.box.spacing=unit(0.2, "cm"))

ggsave( plot = iess_lesiones_profesionales, 
        filename = paste0( parametros$resultado_graficos, 'iess_lesiones_profesionales', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()