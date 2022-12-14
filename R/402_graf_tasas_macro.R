message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )

graf_point_size = 1
#Gráfico del PIB------------------------------------------------------------------------------------
message( '\tGraficando pib' )

aux<- tasas_macro_pred %>%
  mutate( pib_nominal = pib_nominal / 1e3)

aux_his <- aux %>% filter( anio <= 2020 )

aux_pred <- aux %>% filter( anio >= 2020 )


lim_y <- c(0,300000)
salto_y <- 50000
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- formatC( brks_y, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pib_pred <-ggplot() + geom_line( data = aux_his, aes( x = anio, y = pib_nominal,group=1 ),size = graf_line_size ,colour=parametros$iess_green) + 
               geom_line( data = aux_pred, aes( x = anio, y = pib_nominal,group=1), 
               size = graf_line_size, colour= parametros$iess_green,linetype = "dashed") + 
               xlab(TeX("Años"))+
               ylab(TeX("PIB Nominal (millones USD)")) +
               scale_y_continuous( breaks = brks_y, labels = lbls_y, limits = lim_y) +
               scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) + 
               theme_bw() + plt_theme


ggsave( plot = iess_pib_pred, 
        filename = paste0( parametros$resultado_graficos, 'iess_pib_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



#Gráfico del salario promedio anual-----------------------------------------------------------------
message( '\tGraficando salario promedio anual' )

aux<- tasas_macro_pred 

aux_his <- aux %>% filter( anio <= 2020 )

aux_pred <- aux %>% filter( anio >= 2020 )


lim_y<- c(0,25000)
salto_y = 5000
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- formatC( brks_y, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_sal_pred <-  
  ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = sal_anual,group=1 ),
             size = graf_line_size,colour= parametros$iess_green) + 
  geom_line( data = aux_pred, aes( x = anio, y = sal_anual, group=1 ), 
             size = graf_line_size,colour = parametros$iess_green,linetype = "dashed") + 
  xlab("Años")+
  ylab("Salario promedio anual (USD)") +
  scale_y_continuous( breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) + 
  theme_bw() +
  plt_theme

ggsave( plot = iess_sal_pred, 
        filename = paste0( parametros$resultado_graficos, 'iess_sal_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )




#Gráfico del salario básico unificado---------------------------------------------------------------
message( '\tGraficando salario básico unificado' )

aux<- tasas_macro_pred 

aux_his <- aux %>% filter( anio <= 2020 )

aux_pred <- aux %>% filter( anio >= 2020 )


lim_y<- c(0,1100)
salto_y <- 100
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- formatC( brks_y, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_sbu_pred <-  
  ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = sbu, group=1),
             size = graf_line_size, colour = parametros$iess_green ) + 
  geom_line( data = aux_pred, aes( x = anio, y = sbu, group=1), 
             size = graf_line_size,  colour = parametros$iess_green,linetype = "dashed") + 
  xlab("Años")+
  ylab("SBU (USD)") +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) + 
  theme_bw() +
  plt_theme

ggsave( plot = iess_sbu_pred, 
        filename = paste0( parametros$resultado_graficos, 'iess_sbu_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



#Gráfico tasa activa--------------------------------------------------------------------------------
message( '\tGraficando tasa activa' )

aux <- tasas_macro_pred 

aux_his <- aux %>% filter( anio <= 2020 )

aux_pred <- aux %>% filter( anio >= 2020 )


lim_y<- c(0.07,0.11)
salto_y = 0.005
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), abs(lim_y[2]), salto_y)*100) ), "%" )


iess_ta_pred <-  
  ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = tasa_activa, group=1 ),
             size = graf_line_size,colour = parametros$iess_green ) + 
  geom_line( data = aux_pred, aes( x = anio, y = tasa_activa, group=1), 
             size = graf_line_size, colour = parametros$iess_green, linetype = "dashed") + 
  xlab("Años")+
  ylab("Tasa activa referencial") +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) + 
  theme_bw() +
  plt_theme

ggsave( plot = iess_ta_pred, 
        filename = paste0( parametros$resultado_graficos, 'iess_ta_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



#Gráfico tasa pasiva--------------------------------------------------------------------------------
message( '\tGraficando tasa pasiva' )

aux <- tasas_macro_pred 

aux_his <- aux %>% filter( anio <= 2020 )

aux_pred <- aux %>% filter( anio >= 2020 )


lim_y<- c(0.03,0.07)
salto_y = 0.01
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), abs(lim_y[2]), salto_y)*100) ), "%" )


iess_tp_pred <-  
  ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = tasa_pasiva, group=1 ),
             colour = parametros$iess_green,size = graf_line_size ) + 
  geom_line( data = aux_pred, aes( x = anio, y = tasa_pasiva, group=1 ), 
             size = graf_line_size,colour = parametros$iess_green,size = graf_line_size, linetype = "dashed") + 
  xlab("Años")+
  ylab("Tasa pasiva referencial") +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) + 
  theme_bw() +
  plt_theme

ggsave( plot = iess_tp_pred, 
        filename = paste0( parametros$resultado_graficos, 'iess_tp_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Gráfico inflación--------------------------------------------------------------------------------
message( '\tGraficando inflación' )

aux <- tasas_macro_pred 

aux_his <- aux %>% filter( anio <= 2020 )

aux_pred <- aux %>% filter( anio >= 2020 )


lim_y<- c(-0.01,0.09)
salto_y = 0.01
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- paste0(as.character(seq(lim_y[1], lim_y[2], salto_y)*100), "%")


iess_inf_pred <-  
  ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = inflación_prom, group=1),colour = parametros$iess_green,
             size = graf_line_size ) + 
  geom_line( data = aux_pred, aes( x = anio, y = inflación_prom, group=1 ), 
             size = graf_line_size, colour = parametros$iess_green,linetype = "dashed") + 
  xlab("Años")+
  ylab("Tasa inflación promedio") +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) + 
  theme_bw() +
  plt_theme

ggsave( plot = iess_inf_pred, 
        filename = paste0( parametros$resultado_graficos, 'iess_inf_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IVM/', 'IESS_IVM_variables_macroeconomicas.RData') )

#IPC---------------------------------------------------------------------------------

# aux <- IPCgraf_ivm
# #scl <- 5
# #aux[ , inf := inflacion * scl + 1 ]
# #coeff <- 1
# aux[ , inf:=106/91*inflacion]
# aux[ , anio:=year(Anio)]
# 
# x_lim <- c( 2000, 2020 )
# x_brk <- 2000:2020
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( -10, 120)
# y_brk <- seq( y_lim[1], y_lim[2],by = 10 )
# y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# iess_ipc_hist  <- ggplot( data = aux ) +
#   geom_line( aes( x = anio, y = IPC, colour = "IPC" ), size = graf_line_size ) +
#   geom_line( aes( x = anio, y = inf, colour = "Inflación" ), size = graf_line_size, linetype = "dashed"  ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim,
#                       sec.axis = sec_axis( ~.*(91/106),
#                                            name = "Inflación",
#                                            labels = function( b ) { paste0( round( b * 1, 0 ), "%" )} )
#                     ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   scale_colour_manual( "",
#                        breaks = c( "IPC", "Inflación" ),
#                        values = c( "IPC" = parametros$iess_green ,
#                                    "Inflación" = parametros$iess_blue ) ) +
#   theme_bw( ) +
#   plt_theme +
#   labs( x = 'Año', y = 'IPC') +
#   theme( legend.position = "bottom",
#          axis.text.x = element_text( angle = 90, hjust = 1, vjust=0.5 ) ) 

# Gráfico evolución histórica del índice de precios------------------------------------------------------------
aux <- IPCgraf_ivm
aux1 <- aux[, list( anio = year( Anio ), Anio, valor = IPC, tipo = 'IPC' ) ]
aux2 <- aux[, list( anio = year( Anio ), Anio, valor = 106/91*inflacion, tipo = 'Inflación' ) ]
x_lim <- c( 2000, 2020 )
x_brk <- seq(2000,2020,2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( -10, 120)
y_brk <- seq( y_lim[1], y_lim[2],by = 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ipc_hist  <- ggplot( ) +
  scale_x_continuous( breaks = x_brk, limits = x_lim) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim,
                      sec.axis = sec_axis( ~.*(91/106),
                                           name = "Inflación",
                                           labels = function( b ) { paste0( round( b * 1, 0 ), "%" )} ) ) +
  geom_line( aes( x=aux1$anio, y = aux1$valor, colour= 'IPC' ),linetype = 2,size = 0.7 ) +
  geom_line( aes( x=aux2$anio, y = aux2$valor, colour='Inflacion'), size = 0.7 ) +
  scale_colour_manual( values = c(  parametros$iess_green , parametros$iess_blue ) ) +
  theme_bw( ) +
  plt_theme +
  labs( x = 'Año', y = 'IPC') +
  theme( legend.position = "bottom",
         axis.text.x = element_text( angle = 0, hjust = 0.5 ) )
ggsave( plot = iess_ipc_hist,
        filename = paste0( parametros$resultado_graficos, 'iess_ipc_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#SBU---------------------------------------------------------------------------------

aux <- copy(SBU_ivm[, .(Anio, SBU)] )

x_lim <- c( 2002, 2020)
x_brk <- seq( x_lim[1], x_lim[2], 2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 100, 410 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sbu <- ggplot( data = aux ) + 
  geom_line( aes(x = Anio,
                 y = SBU,
                 color = parametros$iess_green),
             size = graf_line_size ) + 
  labs( x = 'Año', y = 'USD' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 0.5, vjust=0.5 ) )

ggsave( plot = iess_sbu,
        filename = paste0( parametros$resultado_graficos, 'iess_sbu', parametros$graf_ext ),
        width = graf_width , height = graf_height, units = graf_units, dpi = graf_dpi )

# SALARIO PROMEDIO -----------------------
aux <- copy( Salprom_ivm [, .(Anio, Salario_declarado_promedio)] )

x_lim <- c( 2010, 2020 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 500, 750 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_spd_hist <- ggplot( data = aux, 
                         aes( x = Anio, y = Salario_declarado_promedio ) ) + 
  geom_line( color = parametros$iess_green, size = graf_line_size ) + 
  labs( x = 'Año', y = 'USD' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 0.5, vjust=0.5 ) )

ggsave( plot = iess_spd_hist,
        filename = paste0( parametros$resultado_graficos, 'iess_spd_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#TASA DEL PIB -------------------------------------------------------

aux <- copy( crec_PIB_ivm )
aux[ , Anio:=as.numeric( Anio )]

x_lim <- c( 1960, 2020 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( -10, 15 )
y_brk <- seq( y_lim[1], y_lim[2], 5)
y_lbl <- paste0( formatC( y_brk, digits = 1, format = 'f', big.mark = '.', decimal.mark = ',' ), '%' )

iess_pib_hist <- ggplot( data = aux, aes( x = Anio, y = CrecimientoPIB) ) + 
  geom_line( color = parametros$iess_green, size = graf_line_size ) + 
  labs( x = 'Año', y = 'Porcentaje' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust=0.5 ) )


ggsave( plot = iess_pib_hist,
        filename = paste0( parametros$resultado_graficos, 'iess_pib_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# EVOLUCIÓN Y COMPORTAMIENTO DEL PIB---------------------------------------------------------------------------------
# aux <- PIBvsIPC_ivm
# #scl <- 1
# #aux[ , inf := inflacion * scl + 1 ]
# aux[ , inf := inflacion ]
# x_lim <- c( 2000, 2020 )
# x_brk <- 2000:2020
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( -10, 100)
# y_brk <- seq( y_lim[1], y_lim[2],by=10 )
# y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# 
# iess_ipcpib_hist  <- ggplot( data = aux ) +
#   geom_line( aes( x = Anio, y = CrecimientoPIB, colour = "Crecimiento real del PIB" ), size = graf_line_size ) +
#   geom_line( aes( x = Anio, y = inf, colour = "Inflación Acumulada Anual" ), size = graf_line_size ) +
#   scale_colour_manual( "",
#                        breaks = c( "Crecimiento real del PIB", "Inflación Acumulada Anual" ),
#                        values = c( "Crecimiento real del PIB" = parametros$iess_green ,
#                                    "Inflación Acumulada Anual" = parametros$iess_blue ) ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   theme_bw( ) +
#   plt_theme +
#   labs( x = 'Año', y = "Porcentaje") +
#   theme( legend.position = "bottom",
#          axis.text.x = element_text( angle = 90, hjust = 1, vjust=0.5 ) )

aux <- PIBvsIPC_ivm
aux1 <- aux[, list( Anio, valor = CrecimientoPIB, tipo = 'Crecimiento real del PIB' ) ]
aux2 <- aux[, list( Anio, valor = inflacion, tipo = 'Inflación acumulada anual' ) ]
aux <- rbind( aux1, aux2 )
# scl <- 5
# aux[ tipo == 'Inflación', valor := valor * scl + 1 ]
# coeff <- 1
x_lim <- c( 2000, 2020 )
x_brk <- seq( 2000, 2020, by = 2 ) #2000:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( -10, 120)
y_brk <- seq( y_lim[1], y_lim[2],by = 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ipcpib_hist  <- ggplot( data = aux, aes( x = Anio, y= valor, group = tipo ) ) +
  geom_line( aes( linetype = tipo, color = tipo ) ) +
  scale_x_continuous( breaks = x_brk, limits = x_lim, labels = x_lbl ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim,
                      sec.axis = sec_axis( ~.*(91/106),
                                           name = "Inflación",
                                           labels = function( b ) { paste0( round( b * 1, 0 ), "%" )} ) ) +
  scale_colour_manual( values = c(  parametros$iess_green , parametros$iess_blue ) ) +
  theme_bw( ) +
  plt_theme +
  labs( x = 'Año', y = 'PIB') +
  theme( legend.position = "bottom",
         axis.text.x = element_text( angle = 0, hjust = 1 ) )

ggsave( plot = iess_ipcpib_hist,
        filename = paste0( parametros$resultado_graficos, 'iess_ipcpib_hist', parametros$graf_ext ),
        width = graf_width+2, height = graf_height, units = graf_units, dpi = graf_dpi )

plt_pob_var_macro <- marrangeGrob( list( iess_pib_pred, iess_sal_pred, iess_sbu_pred,
                                   iess_ta_pred, iess_tp_pred, iess_inf_pred ),
                             nrow = 2, ncol = 3, top = '' )

ggsave( plot = plt_pob_var_macro, 
        filename = paste0( parametros$resultado_graficos, 'plt_pob_var_macro', parametros$graf_ext ),
        width = 24.5, height = 12, units = graf_units, dpi = graf_dpi )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

