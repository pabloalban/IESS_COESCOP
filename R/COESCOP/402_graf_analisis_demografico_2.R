message(  paste(  rep( '-', 100 ), collapse = '' ) )

message(  '\tGraficando demografía COESCOP' )

# Plantilla gráfica --------------------------------------------------------------------------------
source(  'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load(  file = paste0(  parametros$RData, 'IESS_tablas_contingencia_2.RData' ) )

# Funciones-----------------------------------------------------------------------------------------

## Función de la pirámide---------------------------------------------------------------------------
max_edad <- 100
min_edad <- 17

salto_y <- 3
salto_x <- 0.02
brks_y <- seq( -0.2,0.2,salto_x )
lbls_y <- paste0( as.character( c( seq( 0.2, 0, -salto_x ) * 100, seq( salto_x, 0.2, salto_x ) * 100 ) ), "%" )
brks_x <- seq( 15,100,salto_y )
lbls_x <- paste0( as.character( brks_x ) )

graf_piramide <- function( .data, brks_x, brks_y, lbls_x, lbls_y ){
  ggplot( .data, aes( x = edad, y = n, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = .data %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar( data = .data %>% filter(  sexo == 'M' ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip( ) +
  #theme_tufte( )+
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                               label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position="bottom" )+   #legend.position = c( 0.8, 0.2 )
  scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                     labels = c( "Mujeres", "Hombres" ) )
}

#Función para gráfico con cargos--------------------------------------------------------------------
graf_puestos <- function( .data, brks_x, brks_y, lbls_x, lbls_y ){
  ggplot( .data, aes( x = puesto, y = ben ) ) + 
  geom_bar( stat = "identity", fill = parametros$iess_green )+
  theme_bw( ) +
  plt_theme +
  geom_text( aes( label = ben ), 
             vjust = -0.2, 
             color = "black", 
             hjust = 0.5,
             position = position_dodge( 0.9 ),  
             angle = 0, 
             size = 3.0 ) + 
  labs( x = "Cargo COESCOP", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 20, vjust =1, hjust=0.9 ) )
}

# Gráficos de los agentes de Control----------------------------------------------------------------
message(  '\tGraficando pirámide poblacional y puestos de agentes de control' )

##Ambato
ggsave(  plot = graf_piramide( tab_control_ambato$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_control_ambato', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_control_ambato$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_control_ambato', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Duran
ggsave(  plot = graf_piramide( tab_control_duran$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_control_duran', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_control_duran$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_control_duran', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Esmeraldas
ggsave(  plot = graf_piramide( tab_control_esmeraldas$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_control_esmeraldas', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_control_esmeraldas$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_control_esmeraldas', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Guayaquil
ggsave(  plot = graf_piramide( tab_control_guayaquil$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_control_guayaquil', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_control_guayaquil$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_control_guayaquil', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Latacunga
ggsave(  plot = graf_piramide( tab_control_latacunga$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_control_latacunga', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_control_latacunga$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_control_latacunga', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Machala
ggsave(  plot = graf_piramide( tab_control_machala$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_control_machala', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_control_machala$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_control_machala', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Portoviejo
ggsave(  plot = graf_piramide( tab_control_p_viejo$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_control_p_viejo', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_control_p_viejo$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_control_p_viejo', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Quito
ggsave(  plot = graf_piramide( tab_control_quito$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_control_quito', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_control_quito$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_control_quito', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Santo Domingo
ggsave(  plot = graf_piramide( tab_control_s_domingo$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_control_s_domingo', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_control_s_domingo$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_control_s_domingo', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráficos de los agentes de Tránsito----------------------------------------------------------------
message(  '\tGraficando pirámide poblacional y puestos de agentes de tránsito' )

##Cuenca
ggsave(  plot = graf_piramide( tab_transito_cuenca$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_cuenca', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_cuenca$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_cuenca', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Duran
ggsave(  plot = graf_piramide( tab_transito_duran$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_duran', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_duran$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_duran', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Esmeraldas
ggsave(  plot = graf_piramide( tab_transito_esmeraldas$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_esmeraldas', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_esmeraldas$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_esmeraldas', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Guayaquil
ggsave(  plot = graf_piramide( tab_transito_guayaquil$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_guayaquil', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_guayaquil$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_guayaquil', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Ibarra
ggsave(  plot = graf_piramide( tab_transito_ibarra$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_ibarra', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_ibarra$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_ibarra', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Latacunga
ggsave(  plot = graf_piramide( tab_transito_latacunga$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_latacunga', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_latacunga$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_latacunga', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Machala
ggsave(  plot = graf_piramide( tab_transito_machala$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_machala', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_machala$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_machala', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Quito
ggsave(  plot = graf_piramide( tab_transito_quito$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_quito', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_quito$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_quito', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Santo Domingo
ggsave(  plot = graf_piramide( tab_transito_s_domingo$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_s_domingo', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_s_domingo$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_s_domingo', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Ambato
ggsave(  plot = graf_piramide( tab_transito_ambato$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_ambato', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_ambato$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_ambato', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Riobamba
ggsave(  plot = graf_piramide( tab_transito_riobamba$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_riobamba', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_riobamba$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_riobamba', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Loja
ggsave(  plot = graf_piramide( tab_transito_loja$piramide , brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_pir_transito_loja', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave(  plot = graf_puestos( tab_transito_loja$num_puestos, brks_x, brks_y, lbls_x, lbls_y ), 
         filename = paste0(  parametros$resultado_graficos, 'iess_bar_transito_loja', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Limpiar Memoria RAM--------------------------------------------------------------------------------
message(  paste(  rep( '-', 100  ), collapse = ''  )  )
rm(  list = ls(  )[ !(  ls(  ) %in% c(  'parametros'  )  ) ]  )
gc(  )