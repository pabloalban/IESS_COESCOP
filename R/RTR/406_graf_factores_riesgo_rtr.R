message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_RTR_siniestralidad_indemnizaciones_edad_sexo_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_porc_incap_indemn_edad_sexo_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_siniestralidad_subsidios_edad_sexo_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_duracion_subsidios_sexo_edad_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_siniestralidad_accidentes_laborales_fatales.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_grupo_familiar.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_fdp_ingresos_huerfanos_montepio.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_fdp_ingresos_viudas_montepio.RData' ) )
#1. Gráficos de Indemnizaciones---------------------------------------------------------------------
#1. 1. Gráfico del alisado de tasa de uso de las Indemnizaciones------------------------------------
#Hombres
message( '\tGraficando alisado de tasa de uso de las Indemnizaciones en hombres' )
aux <- as.data.frame(siniestralidad_indeminizaciones_edad_sexo_int) %>%
  filter(sexo=='M')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.0005 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


siniestra_indem_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = tasa_sin_indem, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = tasa_sin_indem_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p^{2,8}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = siniestra_indem_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_siniestra_indem_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres
message( '\tGraficando alisado de tasa de uso de las Indemnizaciones en mujeres' )
aux <- as.data.frame(siniestralidad_indeminizaciones_edad_sexo_int) %>%
  filter(sexo=='F')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.0002 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 3, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


siniestra_indem_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = tasa_sin_indem, color = parametros$iess_green ), 
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = tasa_sin_indem_int, color = parametros$iess_blue ), 
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p^{2,8}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = siniestra_indem_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_siniestra_indem_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#1. 2. Gráfico del alisado porcentaje de incapacidad PP utilizado en indemnizaciones----------------
#Hombres
message( '\tGraficando alisado del porcentaje de incapacidad PP utilizado en indemnizaciones hombres' )
aux <- as.data.frame(porc_incap_indemn_edad_sexo_int) %>%
  filter(sexo=='M')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


porc_incap_pp_indem_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad_siniestro, y = porc_incap_indemn,
                   color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = edad_siniestro, y = porc_incap_indemn_int, 
                  color = parametros$iess_blue ), 
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\beta^{Indem}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = porc_incap_pp_indem_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_incap_pp_indem_m',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Mujeres
message( '\tGraficando alisado del porcentaje de incapacidad PP utilizado en indemnizaciones mujeres' )
aux <- as.data.frame(porc_incap_indemn_edad_sexo_int) %>%
  filter(sexo=='F')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


porc_incap_pp_indem_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad_siniestro, y = porc_incap_indemn, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad_siniestro, y = porc_incap_indemn_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\beta^{Indem}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = porc_incap_pp_indem_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_incap_pp_indem_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#2.Gráficos de subsidios----------------------------------------------------------------------------
#2. 1. Gráficos del alisado de la tasa de uso de los subsidios--------------------------------------
#Hombres
message( '\tGraficando alisado de la tasa de uso de los subsidios en hombres' )
aux <- as.data.frame(siniestralidad_subsidios_edad_sexo_int) %>%
  filter(sexo=='M')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.01 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


siniestra_subs_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = tasa_sin_sub, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = tasa_sin_sub_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p^{2,11}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = siniestra_subs_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_siniestra_subs_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Mujeres
message( '\tGraficando alisadode la tasa de uso de los subsidios en mujeres' )
aux <- as.data.frame(siniestralidad_subsidios_edad_sexo_int) %>%
  filter(sexo=='F')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.0025 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


siniestra_subs_f <- ggplot( data = aux ) + 
  geom_point( aes( x = x, y = tasa_sin_sub, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = tasa_sin_sub_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p^{2,11}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = siniestra_subs_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_siniestra_subs_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#2. 2. Gráficos del alisado del porcentaje de incapacidad temporal----------------------------------
#Hombres
message( '\tGraficando alisado del porcentaje de incapacidad temporal en hombres' )
aux <- as.data.frame(porc_incap_subsidios_edad_sexo_int) %>%
  filter(sexo=='M')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0.72, 0.76 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


porc_incap_subs_m <-ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = porc_subsidios_prom, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = porc_subsidios_prom_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\beta^{Subs}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1,decimal.mark = "," ),
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = porc_incap_subs_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_incap_subs_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres
message( '\tGraficando alisado del porcentaje de incapacidad temporal en mujeres' )
aux <- as.data.frame(porc_incap_subsidios_edad_sexo_int) %>%
  filter(sexo=='F')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0.72, 0.76 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


porc_incap_subs_f <-ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = porc_subsidios_prom, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = porc_subsidios_prom_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\beta^{Subs}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1,decimal.mark = "," ),
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = porc_incap_subs_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_incap_subs_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#2. 3. Gráficos del alisado de la duración de subsidios por incapacidad temporal--------------------
#Hombres
message( '\tGraficando alisado de la duración de subsidios por incapacidad temporal en hombres' )
aux <- as.data.frame(duracion_subsidios_sexo_edad_int) %>%
  filter(sexo=='M')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 60 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 7 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


dura_incap_subs_m <-ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = (pro_duracion_dias), color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = (pro_duracion_dias_int), color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\,d^{Subs}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = dura_incap_subs_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_dura_incap_subs_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres
message( '\tGraficando alisado de la duración de subsidios por incapacidad temporal en mujeres' )
aux <- as.data.frame(duracion_subsidios_sexo_edad_int) %>%
  filter(sexo=='F')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 10, 100 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


dura_incap_subs_f <-ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = (pro_duracion_dias), color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = pro_duracion_dias_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\,d^{Subs}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = dura_incap_subs_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_dura_incap_subs_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#3 .Gráficos de factores de riesgo en montepío------------------------------------------------------
#3. 1. Gráficos del alisado de accidentes laborales fatales-----------------------------------------
message( '\tGraficando alisado de accidentes laborales fatales' )
aux <-  as_tibble(incidencia_FA) %>%
  mutate(x:=edad,tasa_inc_FA_int:=exp(log_tasa_inc_FA_int))

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -13,-8 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


siniestralidad_fa <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_tasa_inc_FA, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_tasa_inc_FA_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$\\log{\\,p^{FA}_{x}}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl, breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = siniestralidad_fa, 
        filename = paste0( parametros$resultado_graficos, 'iess_siniestralidad_fa', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#3. 2. Gráficos del alisado del grupo familiar (huerfanos)------------------------------------------
message( '\tGraficando alisado del grupo familiar (huerfanos)' )
aux <-  as_tibble(GF_orfandad) %>% mutate(x:=edad)

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0,6 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 7 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


gf_huerfanos <- ggplot( data = aux ) + 
  geom_point( aes( x = x, y = pGF_orfandad, color = parametros$iess_green ), 
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = pGF_orfandad_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$GF^{\\,orf}_{x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl, breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = gf_huerfanos, 
        filename = paste0( parametros$resultado_graficos, 'iess_gf_huerfanos', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
#3. 3. Gráficos del alisado del grupo familiar (viudas)------------------------------------------
message( '\tGraficando aalisado del grupo familiar (viudas)' )
aux <-  as_tibble(GF_viudez) %>% mutate(x:=edad)

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0,1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


gf_viudas <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = pGF_viudez, color = parametros$iess_green ), 
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = pGF_viudez_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$GF^{\\upsilon i \\mu}_{x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl, breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = gf_viudas, 
        filename = paste0( parametros$resultado_graficos, 'iess_gf_viudas', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#3. 4. Gráficos del alisado de la fdp de edad de ingrsos de huerfanos-------------------------------
#Hombres
message( '\tGraficando alisado de la fdp de edad de ingrsos de huerfanos' )
aux <- as.data.frame(fdp_ingresos_huerfanos_edad_sexo) %>%
  filter(sexo=='M')

x_lim <- c( 0, 18 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- x_brk

y_lim <- c( -7, -2 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


fdp_orf_m  <- ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_fdp, color = parametros$iess_green ), 
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_fdp_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres\\,\\,f^{\\,orf}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = fdp_orf_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_fdp_orf_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres
aux <- as.data.frame(fdp_ingresos_huerfanos_edad_sexo) %>%
  filter(sexo=='F')

x_lim <- c( 0, 18 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- x_brk

y_lim <- c( -6, -2 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


fdp_orf_f  <- ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_fdp, color = parametros$iess_green ), 
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_fdp_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, f^{\\,orf}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = fdp_orf_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_fdp_orf_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Agrupar en solo gráfico----------------------------------------------------------------------------
g1<-ggarrange(siniestra_indem_f,porc_incap_pp_indem_f,
              siniestra_indem_m,porc_incap_pp_indem_m,
              ncol = 2, nrow = 2)

g2<-ggarrange(
  siniestra_subs_f,porc_incap_subs_f,dura_incap_subs_f,
  siniestra_subs_m,porc_incap_subs_m,dura_incap_subs_m,
  ncol = 3, nrow = 2)

g31<-ggarrange(siniestralidad_fa,
               gf_viudas,
               gf_huerfanos,
               ncol = 3, nrow = 1)

g32<-ggarrange(
  fdp_orf_f,
  fdp_orf_m, ncol = 2)

g3<- ggarrange(g31,g32, nrow = 2)


ggsave( plot = g1, 
        filename = paste0( parametros$resultado_graficos, 'iess_factores_riesgos_indm', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )

ggsave( plot = g2, 
        filename = paste0( parametros$resultado_graficos, 'iess_factores_riesgos_subs', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )

ggsave( plot = g3, 
        filename = paste0( parametros$resultado_graficos, 'iess_factores_riesgos_montepio', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )
#Limpiar memoria RAM--------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()