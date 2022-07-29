message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# Carga de datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_RTR_proy_beneficarios_prestacion.RData' ) )
pob_proy <- pob_proy[ t <= parametros$horizonte ]

# Población PEA no afiliados -----------------------------------------------------------------------
message( '\tGraficando proyección PEA no afiliados' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l1 ) ]
plt_l1_f <- ggplot() +
            geom_line( data = aux_f, aes( x = x, y = l1, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{1}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l1_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l1_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l1 ) ]
plt_l1_m <- ggplot() +
            geom_line( data = aux_m, aes( x = x, y = l1, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$hombres \\, \\, l_{t,2,x}^{1}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l1_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l1_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población afiliados activos-----------------------------------------------------------------------
message( '\tGraficando proyección afiliados' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l2 = l2_cot ) ]
plt_l2_f <- ggplot() +
            geom_line( data = aux_f, aes( x = x, y = l2, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{2,cot}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l2_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l2_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l2 = l2_cot ) ]
plt_l2_m <- ggplot() +
            geom_line( data = aux_m, aes( x = x, y = l2, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$hombres \\, \\, l_{t,2,x}^{2,cot}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l2_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l2_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas vejez (jubilados) ---------------------------------------------------------
message( '\tGraficando proyección pensionistas por vejez' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 50, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l3 ) ]
plt_l3_f <- ggplot() +
            geom_line( data = aux_f, aes( x = x, y = l3, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{3}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l3_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l3_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l3 ) ]
plt_l3_m <- ggplot() +
            geom_line( data = aux_m, aes( x = x, y = l3, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$hombres \\, \\, l_{t,2,x}^{3}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l3_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l3_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas invalidez (jubilados por invalidez)----------------------------------------
message( '\tGraficando proyección pensionistas por invalidez' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e4 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l4 ) ]
plt_l4_f <- ggplot() +
            geom_line( data = aux_f, aes( x = x, y = l4, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{4}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l4_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l4 ) ]
plt_l4_m <- ggplot() +
            geom_line( data = aux_m, aes( x = x, y = l4, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$hombres \\, \\, l_{t,2,x}^{4}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l4_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas incapacidad permanente total y absoluta------------------------------------
message( '\tGraficando proyección pensionistas incapacidad permanente total y absoluta' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l9 ) ]
plt_l5_f <- ggplot() +
            geom_line( data = aux_f, aes( x = x, y = l9, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{9}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l5_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l9_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

y_lim <- c( 0, 250 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l9 ) ]
plt_l5_m <- ggplot() +
            geom_line( data = aux_m, aes( x = x, y = l9, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$hombres \\, \\, l_{t,2,x}^{9}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l5_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l9_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas incapacidad permanente parcial---------------------------------------------
message( '\tGraficando proyección pensionistas incapacidad permanente parcial' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 20 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l15 ) ]
plt_l7_f <- ggplot() +
            geom_line( data = aux_f, aes( x = x, y = l15, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{15}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l7_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l15_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

y_lim <- c( 0, 150 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l15 ) ]
plt_l7_m <- ggplot() +
            geom_line( data = aux_m, aes( x = x, y = l15, color = t ), size = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$hombres \\, \\, l_{t,2,x}^{15}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l7_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l15_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Población pensionistas por orfandad---------------------------------------------------------------
message( '\tGraficando proyección pensionistas por orfandad' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 1, 17 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 500 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l13 ) ]
plt_l9_f <- ggplot() +
        geom_line( data = aux_f, aes( x = x, y = l13, color = t ), size = graf_line_size ) +
        scale_color_manual( values = cols_graf ) +
        scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
        scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
        xlab(TeX("edad $x$"))+
        ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{13}$")) +
        theme_bw() +
        plt_theme

ggsave( plot = plt_l9_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l13_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l13 ) ]
plt_l9_m <- ggplot() +
        geom_line( data = aux_m, aes( x = x, y = l13, color = t ), size = graf_line_size ) +
        scale_color_manual( values = cols_graf ) +
        scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
        scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
        xlab(TeX("edad $x$"))+
        ylab(TeX("$hombres \\, \\, l_{t,2,x}^{13}$")) +
        theme_bw() +
        plt_theme

ggsave( plot = plt_l9_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l13_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas por viudedad---------------------------------------------------------------
message( '\tGraficando proyección pensionistas por viudedad' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 15, 106 )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 8 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 150 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l14 ) ]
plt_l10_f <- ggplot() +
        geom_line( data = aux_f, aes( x = x, y = l14, color = t ), size = graf_line_size ) +
        scale_color_manual( values = cols_graf ) +
        scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
        scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
        xlab(TeX("edad $x$"))+
        ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{14}$")) +
        theme_bw() +
        plt_theme

ggsave( plot = plt_l9_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

x_lim <- c( 15, 106 )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 8 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 10 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l14 ) ]
plt_l10_m <- ggplot() +
                geom_line( data = aux_m, aes( x = x, y = l14, color = t ), size = graf_line_size ) +
                scale_color_manual( values = cols_graf ) +
                scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
                scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                xlab(TeX("edad $x$"))+
                ylab(TeX("$hombres \\, \\, l_{t,2,x}^{14}$")) +
                theme_bw() +
                plt_theme

ggsave( plot = plt_l9_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Agrupar en un solo gráfico-------------------------------------------------------------------------
plt_pob_1 <- marrangeGrob( list( plt_l2_f, plt_l2_m, plt_l3_f, plt_l3_m, plt_l4_f, plt_l4_m ),
                         nrow = 2, ncol = 3, top = '' )

ggsave( plot = plt_pob_1, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_1', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )


plt_pob_2 <- marrangeGrob( list( plt_l5_f, plt_l5_m, plt_l7_f, plt_l7_m ),
                             nrow = 2, ncol = 2, top = '' )

ggsave( plot = plt_pob_2, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_2', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

plt_pob_3 <- marrangeGrob( list( plt_l9_f, plt_l9_m, plt_l10_f, plt_l10_m ),
                           nrow = 2, ncol = 2, top = '' )

ggsave( plot = plt_pob_3, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_3', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()