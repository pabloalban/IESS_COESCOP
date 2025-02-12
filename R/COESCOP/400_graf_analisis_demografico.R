
message(  paste(  rep( '-', 100  ), collapse = ''  )  )

message(  '\tGraficando demografía COESCOP'  )

# Plantilla gráfica --------------------------------------------------------------------------------
source(  'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE  )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load(  file = paste0(  parametros$RData, 'IESS_tablas_contingencia.RData'  )  )


#Gráfico pirámide poblacional SNAI------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional SNAI'  )
aux<-(  tabla_snai_edad_sexo %>% 
       select(  sexo, edad, n:= frecuencia )  ) %>%
       #group_by(  sexo  ) %>%
       mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
       mutate(  n = n/N ) %>%
       ungroup(  ) %>%
       mutate(  n = ifelse(  sexo == 'M', -n, n )  )


max_edad<-100
min_edad<-15

salto_y<-10
salto_x<-0.02
brks_y <- seq( -0.1,0.1,salto_x )
lbls_y <- paste0( as.character( c( seq( 0.1, 0, -salto_x )*100, seq( salto_x, 0.1, salto_x )*100 ) ), "%" )
brks_x <- seq( 15,100,salto_y )
lbls_x <- paste0( as.character( brks_x ) )


iess_pir_snai<-ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
               xlab(  'Edad'  ) +
               ylab(  ''  ) +
               geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
               geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
               scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
               scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
               coord_flip(  ) +
               theme_bw(  ) +
               plt_theme +
               guides( fill = guide_legend( title = NULL,label.position = "right",
                                         label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
               theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
               scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                                 labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_snai, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_snai', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

#Gráfico de barras cargo SNAI------------------------------------------------------------------

message(  '\tGráfico de barras por cargo SNAI'  )

aux <- (  tabla_snai_cargo %>% 
         select(  cargo_coescop, n:= frecuencia )  ) %>%
         group_by(  cargo_coescop  ) %>%
         mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
         ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_snai <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
                 labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
                 theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )
  
ggsave(  plot = iess_bar_snai, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_snai', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

#Gráfico pirámide poblacional SNMLCF----------------------------------------------------------------

message(  '\tGraficando pirámide poblacional SNMLCF'  )
aux<-(  tabla_snmlcf_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
         #group_by(  sexo  ) %>%
         mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
         mutate(  n = n/N ) %>%
         ungroup(  ) %>%
         mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_snmlcf <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
                   xlab(  'Edad'  ) +
                   ylab(  ''  ) +
                   geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
                   geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
                   scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
                   scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
                   coord_flip(  ) +
                   theme_bw(  ) +
                   plt_theme +
                   guides( fill = guide_legend( title = NULL,label.position = "right",
                                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
                   theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
                   scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                                     labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_snmlcf, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_snmlcf', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

#Gráfico de barras cargo SNAI------------------------------------------------------------------

message(  '\tGráfico de barras por cargo SNMLCF'  )

aux <- (  tabla_snmlcf_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
           group_by(  cargo_coescop  ) %>%
           mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
           ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_snmlcf <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
                   labs( x = "Cargo COESCOP", y = "Número de Servidores" )


ggsave(  plot = iess_bar_snmlcf, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_snmlcf', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )



#Gráfico pirámide poblacional METROPOLITANOS-----------------------------------------------------------

message(  '\tGraficando pirámide poblacional metropolitanos'  )
aux<-(  tabla_metropolitanos_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
         #group_by(  sexo  ) %>%
         mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
         mutate(  n = n/N ) %>%
         ungroup(  ) %>%
         mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_metropolitanos <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
                           xlab(  'Edad'  ) +
                           ylab(  ''  ) +
                           geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
                           geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
                           scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
                           scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
                           coord_flip(  ) +
                           theme_bw(  ) +
                           plt_theme +
                           guides( fill = guide_legend( title = NULL,label.position = "right",
                                                      label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
                           theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
                           scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                                             labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_metropolitanos, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_metropolitanos', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

## Gráficos pirámide poblacional METROPOLITANOS por ciudades---------------------------------------------

### Quito------------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional metropolitanos Quito'  )
aux<-(  tabla_metropolitanos_quito_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_metropolitanos_quito <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_metropolitanos_quito, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_metropolitanos_quito', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Ambato----------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional metropolitanos Ambato'  )
aux<-(  tabla_metropolitanos_ambato_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_metropolitanos_ambato <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_metropolitanos_ambato, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_metropolitanos_ambato', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Cuenca------------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional metropolitanos Cuenca'  )
aux<-(  tabla_metropolitanos_cuenca_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_metropolitanos_cuenca <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_metropolitanos_cuenca, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_metropolitanos_cuenca', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Guayaquil---------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional metropolitanos Guayaquil'  )
aux<-(  tabla_metropolitanos_gye_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_metropolitanos_gye <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_metropolitanos_gye, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_metropolitanos_gye', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Loja-------------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional metropolitanos Loja'  )
aux<-(  tabla_metropolitanos_loja_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_metropolitanos_loja <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_metropolitanos_loja, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_metropolitanos_loja', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Machala----------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional metropolitanos Machala'  )
aux<-(  tabla_metropolitanos_machala_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_metropolitanos_machala <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_metropolitanos_machala, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_metropolitanos_machala', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Portoviejo--------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional metropolitanos Portoviejo'  )
aux<-(  tabla_metropolitanos_prtvj_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_metropolitanos_prtvj <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_metropolitanos_prtvj, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_metropolitanos_prtvj', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )


#Gráfico de barras cargo METROPOLITANOS------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Agentes de Control Municipal o Metropolitano'  )

aux <- (  tabla_metropolitanos_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
           group_by(  cargo_coescop  ) %>%
           mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
           ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_metropolitanos <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
                           labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
                           theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_metropolitanos, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_metropolitanos', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

## Gráficos de barras carg METROPOLITANOS por ciudades----------------------------------------------

### Quito-------------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Agentes de Control Municipal o Metropolitano de Quito'  )

aux <- (  tabla_metropolitanos_quito_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_metropolitanos_quito <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_metropolitanos_quito, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_metropolitanos_quito', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Ambato------------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Agentes de Control Municipal o Metropolitano de Ambato'  )

aux <- (  tabla_metropolitanos_ambato_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_metropolitanos_ambato <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )
  #theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_metropolitanos_ambato, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_metropolitanos_ambato', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Cuenca-------------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Agentes de Control Municipal o Metropolitano de Cuenca'  )

aux <- (  tabla_metropolitanos_cuenca_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_metropolitanos_cuenca <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_metropolitanos_cuenca, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_metropolitanos_cuenca', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Guayaquil----------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Agentes de Control Municipal o Metropolitano de Guayaquil'  )

aux <- (  tabla_metropolitanos_gye_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_metropolitanos_gye <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_metropolitanos_gye, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_metropolitanos_gye', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Loja--------------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Agentes de Control Municipal o Metropolitano de Loja'  )

aux <- (  tabla_metropolitanos_loja_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_metropolitanos_loja <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )
  #theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_metropolitanos_loja, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_metropolitanos_loja', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Machala-----------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Agentes de Control Municipal o Metropolitano de Machala'  )

aux <- (  tabla_metropolitanos_machala_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_metropolitanos_machala <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )
  #theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_metropolitanos_machala, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_metropolitanos_machala', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Portoviejo--------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Agentes de Control Municipal o Metropolitano de Portoviejo'  )

aux <- (  tabla_metropolitanos_prtvj_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_metropolitanos_prtvj <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )
 # theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_metropolitanos_prtvj, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_metropolitanos_prtvj', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )


#Gráfico pirámide poblacional CTE-------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional CTE'  )
aux<-(  tabla_cte_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
         #group_by(  sexo  ) %>%
         mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
         mutate(  n = n/N ) %>%
         ungroup(  ) %>%
         mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_cte <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
                xlab(  'Edad'  ) +
                ylab(  ''  ) +
                geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
                geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
                scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
                scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
                coord_flip(  ) +
                #theme_tufte(  )+
                theme_bw(  ) +
                plt_theme +
                guides( fill = guide_legend( title = NULL,label.position = "right",
                                           label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
                theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
                scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                                  labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_cte, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_cte', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

#Gráfico de barras cargo CTE------------------------------------------------------------------

message(  '\tGráfico de barras por cargo CTE'  )

aux <- (  tabla_cte_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
           group_by(  cargo_coescop  ) %>%
           mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
           ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_cte <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
                labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
                theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_cte, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_cte', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )


#Gráfico pirámide poblacional BOMBEROS----------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS'  )
aux<-(  tabla_bomberos_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
         #group_by(  sexo  ) %>%
         mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
         mutate(  n = n/N ) %>%
         ungroup(  ) %>%
         mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
                     xlab(  'Edad'  ) +
                     ylab(  ''  ) +
                     geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
                     geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
                     scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
                     scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
                     coord_flip(  ) +
                     theme_bw(  ) +
                     plt_theme +
                     guides( fill = guide_legend( title = NULL,label.position = "right",
                                                label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
                     theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
                     scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                                       labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

## Gráficos pirámide poblacional BOMBEROS por ciudades---------------------------------------------

### Quito------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Quito'  )
aux<-(  tabla_bomberos_quito_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_quito <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_quito, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_quito', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Ambato----------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Ambato'  )
aux<-(  tabla_bomberos_ambato_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_ambato <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_ambato, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_ambato', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Guayaquil---------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Guayaquil'  )
aux<-(  tabla_bomberos_gye_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_gye <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_gye, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_gye', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Ibarra-----------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Ibarra'  )
aux<-(  tabla_bomberos_ibarra_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_ibarra <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_ibarra, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_ibarra', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Machala----------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Machala'  )
aux<-(  tabla_bomberos_machala_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_machala <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_machala, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_machala', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Manta------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Manta'  )
aux<-(  tabla_bomberos_manta_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_manta <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_manta, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_manta', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Milagro----------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Milagro'  )
aux<-(  tabla_bomberos_milagro_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_milagro <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_milagro, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_milagro', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Portoviejo---------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Portoviejo'  )
aux<-(  tabla_bomberos_prtvj_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_prtvj <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_prtvj, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_prtvj', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Riobamba---------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Riobamba'  )
aux<-(  tabla_bomberos_riobamba_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_riobamba <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_riobamba, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_riobamba', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Santo Domingo----------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Santo Domingo'  )
aux<-(  tabla_bomberos_sto_dom_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_sto_dom <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_sto_dom, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_sto_dom', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Loja--------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Loja'  )
aux<-(  tabla_bomberos_loja_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_loja <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_loja, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_loja', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Cuenca------------------------------------------------------------------------------------------

message(  '\tGraficando pirámide poblacional BOMBEROS de Cuenca'  )
aux<-(  tabla_bomberos_cuenca_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
  #group_by(  sexo  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  mutate(  n = n/N ) %>%
  ungroup(  ) %>%
  mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_bomberos_cuenca <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab(  'Edad'  ) +
  ylab(  ''  ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                    labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_bomberos_cuenca, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_bomberos_cuenca', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

#Gráfico de barras cargo BOMBEROS------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos'  )

aux <- (  tabla_bomberos_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
           group_by(  cargo_coescop  ) %>%
           mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
           ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
                     labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
                     theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )


## Gráfico de barras cargo BOMBEROS por ciudad------------------------------------------------------

### Quito-------------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Quito'  )

aux <- (  tabla_bomberos_quito_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_quito <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_quito, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_quito', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Ambato-----------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Ambato'  )

aux <- (  tabla_bomberos_ambato_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_ambato <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_ambato, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_ambato', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Guayaquil----------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Guayaquil'  )

aux <- (  tabla_bomberos_gye_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_gye <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )#+
  #theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_gye, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_gye', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Ibarra------------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Ibarra'  )

aux <- (  tabla_bomberos_ibarra_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_ibarra <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_ibarra, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_ibarra', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Machala-----------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Machala'  )

aux <- (  tabla_bomberos_machala_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_machala <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )
  #theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_machala, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_machala', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Manta-------------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Manta'  )

aux <- (  tabla_bomberos_manta_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_manta <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_manta, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_manta', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Milagro-----------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Milagro'  )

aux <- (  tabla_bomberos_milagro_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_milagro <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_milagro, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_milagro', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Portoviejo---------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Portoviejo'  )

aux <- (  tabla_bomberos_prtvj_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_prtvj <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_prtvj, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_prtvj', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Riobamba----------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Riobamba'  )

aux <- (  tabla_bomberos_riobamba_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_riobamba <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_riobamba, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_riobamba', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Santo Domingo------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Santo Domingo'  )

aux <- (  tabla_bomberos_sto_dom_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_sto_dom <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_sto_dom, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_sto_dom', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Loja--------------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Loja'  )

aux <- (  tabla_bomberos_loja_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_loja <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_loja, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_loja', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

### Cuenca------------------------------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Bomberos de Cuenca'  )

aux <- (  tabla_bomberos_cuenca_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
  group_by(  cargo_coescop  ) %>%
  mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
  ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_bomberos_cuenca <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_bomberos_cuenca, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_bomberos_cuenca', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

#Gráfico pirámide poblacional ADUANEROS-------------------------------------------------------------

message(  '\tGraficando pirámide poblacional aduaneros'  )
aux<-(  tabla_aduaneros_edad_sexo %>% 
         select(  sexo, edad, n:= frecuencia )  ) %>%
         #group_by(  sexo  ) %>%
         mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
         mutate(  n = n/N ) %>%
         ungroup(  ) %>%
         mutate(  n = ifelse(  sexo == 'M', -n, n )  )


iess_pir_aduaneros<-ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
                    xlab(  'Edad'  ) +
                    ylab(  ''  ) +
                    geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
                    geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
                    scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
                    scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
                    coord_flip(  ) +
                    #theme_tufte(  )+
                    theme_bw(  ) +
                    plt_theme +
                    guides( fill = guide_legend( title = NULL,label.position = "right",
                                               label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
                    theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
                    scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                                      labels = c( "Mujeres", "Hombres" ) )

ggsave(  plot = iess_pir_aduaneros, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_aduaneros', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )


#Gráfico de barras cargo ADUANEROS------------------------------------------------------------------

message(  '\tGráfico de barras por cargo Cuerpo de Vigilancia Aduanera'  )

aux <- (  tabla_aduaneros_cargo %>% 
           select(  cargo_coescop, n:= frecuencia )  ) %>%
           group_by(  cargo_coescop  ) %>%
           mutate(  N = sum(  n, na.rm = TRUE )  ) %>%
           ungroup(  )

c <- c( "cargo_coescop","N" )
aux <- aux[,names( aux ) %in% c]
aux <- aux[!duplicated( aux ),]


iess_bar_aduaneros <- ggplot( aux, aes( x = cargo_coescop, y = N ) ) + 
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
                      labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
                      theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )


ggsave(  plot = iess_bar_aduaneros, 
        filename = paste0(  parametros$resultado_graficos, 'iess_bar_aduaneros', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Limpiar Memoria RAM--------------------------------------------------------------------------------
message(  paste(  rep( '-', 100  ), collapse = ''  )  )
rm(  list = ls(  )[ !(  ls(  ) %in% c(  'parametros'  )  ) ]  )
gc(  )

