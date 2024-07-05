
message(  paste(  rep( '-', 100  ), collapse = ''  )  )

message(  '\tGraficando demografía COESCOP'  )

# Plantilla gráfica --------------------------------------------------------------------------------
source(  'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE  )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load(  file = paste0(  parametros$RData, 'IESS_tablas_contingencia_concordia.RData'  )  )



#Gráfico pirámide poblacional Concordia-------------------------------------------------------------

message(  '\tGraficando pirámide poblacional concordia'  )

aux<-(  tabla_concordia_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )

# Parámetros del gráfico

max_edad<-100
min_edad<-15

salto_y<-10
salto_x<-0.02
brks_y <- seq( -0.14,0.1,salto_x )
lbls_y <- paste0( as.character( c( seq( 0.14, 0, -salto_x )*100, seq( salto_x, 0.1, salto_x )*100 ) ), "%" )
brks_x <- seq( 15,100,salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_concordia <- ggplot(  aux, aes(  x = edad, y = n, fill = sexo  )  ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous(  breaks = brks_y, labels = lbls_y  ) +
  scale_x_continuous(  breaks = brks_x, labels = lbls_x  ) +
  coord_flip(   ) +
  #theme_tufte(  )+
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_concordia, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_concordia', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )


#Gráfico pirámide de salarios promedios Concordia---------------------------------------------------

message(  '\tGraficando pirámide poblacional concordia'  )

aux<-(  tabla_concordia_salario_edad_sexo %>% 
         select(  sexo, edad, n:= media )  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )

# Parámetros del gráfico

salto_y <- 10
brks_y <- round(  c(  seq(  min( aux$n ), 0, length.out = 6 ), seq(  0, max( aux$n ), length.out = 4  )[-1]  )  )
lbls_y <- paste0(  '$', formatC(  abs(  brks_y  ), digits = 0, format = 'f', big.mark = '.', decimal.mark = ','  )  )
brks_x <- seq(  15, 100, salto_y  )
lbls_x <- paste0(  as.character(  brks_x  )  )


iess_pir_salarios_concordia <- ggplot(  aux, aes(  x = edad, y = n, fill = sexo  )  ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous(  breaks = brks_y, labels = lbls_y  ) +
  scale_x_continuous(  breaks = brks_x, labels = lbls_x  ) +
  coord_flip(   ) +
  #theme_tufte(  )+
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_salarios_concordia, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_salarios_concordia', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

#Gráfico de barras cargo Concordia------------------------------------------------------------------

message(  '\tGráfico de barras por cargo de la concordia'  )

aux <- (  tabla_concordia_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_concordia <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
  geom_bar( stat = "identity", fill = parametros$iess_green )+
  theme_bw(  ) +
  plt_theme +
  geom_text( aes( label=N ), 
            vjust=-0.2, 
            color="black", 
            hjust=0.5,
            position = position_dodge( 0.9 ),  
            angle=0, 
            size=3.0 ) + 
  labs( x = "Cargo Institucional", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_concordia, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_concordia', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Limpiar Memoria RAM--------------------------------------------------------------------------------
message(  paste(  rep( '-', 100  ), collapse = ''  )  )
rm(  list = ls(  )[ !(  ls(  ) %in% c(  'parametros'  )  ) ]  )
gc(  )

