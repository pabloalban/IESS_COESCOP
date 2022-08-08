message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando demografía COESCOP' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_tablas_contingencia.RData' ) )


#Gráfico pirámide poblacional SNAI------------------------------------------------------------------

message( '\tGraficando pirámide poblacional SNAI' )
aux<-( tabla_snai_edad_sexo %>% 
       select( sexo, edad, n:= frecuencia) ) %>%
       group_by( sexo ) %>%
       mutate( N = sum( n, na.rm = TRUE) ) %>%
       mutate( n = n/N) %>%
       ungroup() %>%
       mutate( n = ifelse( sexo == 'M', -n, n) )


max_edad<-100
min_edad<-15

salto_y<-10
salto_x<-0.01
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))


iess_pir_snai<-ggplot(aux, aes(x = edad, y = n, fill = sexo)) +
               xlab( 'Edad' ) +
               ylab( '' ) +
               geom_bar( data = aux %>% filter(sexo == 'F'), stat = 'identity',colour="white", size=0.1) +
               geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
               scale_y_continuous(breaks = brks_y, labels = lbls_y) +
               scale_x_continuous(breaks = brks_x, labels = lbls_x) +
               coord_flip() +
               theme_bw() +
               plt_theme +
               guides(fill = guide_legend(title = NULL,label.position = "right",
                                         label.hjust = 0, label.vjust = 0.5))+
               theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
               scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                                 labels = c("Mujeres", "Hombres"))

ggsave( plot = iess_pir_snai, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_snai', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Gráfico de barras cargo SNAI------------------------------------------------------------------

message( '\tGráfico de barras por cargo SNAI' )

aux <- ( tabla_snai_cargo %>% 
         select( cargo_coescop, n:= frecuencia) ) %>%
         group_by( cargo_coescop ) %>%
         mutate( N = sum( n, na.rm = TRUE) ) %>%
         ungroup()

c <- c("cargo_coescop","N")
aux <- aux[,names(aux) %in% c]
aux <- aux[!duplicated(aux),]


iess_bar_snai <- ggplot(aux, aes(x = cargo_coescop, y = N)) + 
                 geom_bar(stat = "identity", fill = parametros$iess_green)+
                 theme_bw() +
                 plt_theme +
                 geom_text(aes(label=N), 
                              vjust=-0.9, 
                              color="black", 
                              hjust=0.5,
                              position = position_dodge(0.9),  
                              angle=0, 
                              size=4.0) + 
                 labs(x = "Cargo COESCOP", y = "Número de Servidores")
  
ggsave( plot = iess_bar_snai, 
        filename = paste0( parametros$resultado_graficos, 'iess_bar_snai', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Gráfico pirámide poblacional SNMLCF----------------------------------------------------------------

message( '\tGraficando pirámide poblacional SNMLCF' )
aux<-( tabla_snmlcf_edad_sexo %>% 
         select( sexo, edad, n:= frecuencia) ) %>%
         group_by( sexo ) %>%
         mutate( N = sum( n, na.rm = TRUE) ) %>%
         mutate( n = n/N) %>%
         ungroup() %>%
         mutate( n = ifelse( sexo == 'M', -n, n) )


iess_pir_snmlcf <- ggplot(aux, aes(x = edad, y = n, fill = sexo)) +
                   xlab( 'Edad' ) +
                   ylab( '' ) +
                   geom_bar( data = aux %>% filter(sexo == 'F'), stat = 'identity',colour="white", size=0.1) +
                   geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
                   scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                   scale_x_continuous(breaks = brks_x, labels = lbls_x) +
                   coord_flip() +
                   theme_bw() +
                   plt_theme +
                   guides(fill = guide_legend(title = NULL,label.position = "right",
                                              label.hjust = 0, label.vjust = 0.5))+
                   theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
                   scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                                     labels = c("Mujeres", "Hombres"))

ggsave( plot = iess_pir_snmlcf, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_snmlcf', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Gráfico de barras cargo SNAI------------------------------------------------------------------

message( '\tGráfico de barras por cargo SNMLCF' )

aux <- ( tabla_snmlcf_cargo %>% 
           select( cargo_coescop, n:= frecuencia) ) %>%
           group_by( cargo_coescop ) %>%
           mutate( N = sum( n, na.rm = TRUE) ) %>%
           ungroup()

c <- c("cargo_coescop","N")
aux <- aux[,names(aux) %in% c]
aux <- aux[!duplicated(aux),]


iess_bar_snmlcf <- ggplot(aux, aes(x = cargo_coescop, y = N)) + 
                   geom_bar(stat = "identity", fill = parametros$iess_green)+
                   theme_bw() +
                   plt_theme +
                   geom_text(aes(label=N), 
                            vjust=-0.9, 
                            color="black", 
                            hjust=0.5,
                            position = position_dodge(0.9),  
                            angle=0, 
                            size=4.0) + 
                   labs(x = "Cargo COESCOP", y = "Número de Servidores")

ggsave( plot = iess_bar_snai, 
        filename = paste0( parametros$resultado_graficos, 'iess_bar_snmlcf', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



#Gráfico pirámide poblacional METROPOLITANOS-----------------------------------------------------------

message( '\tGraficando pirámide poblacional metropolitanos' )
aux<-( tabla_metropolitanos_edad_sexo %>% 
         select( sexo, edad, n:= frecuencia) ) %>%
         group_by( sexo ) %>%
         mutate( N = sum( n, na.rm = TRUE) ) %>%
         mutate( n = n/N) %>%
         ungroup() %>%
         mutate( n = ifelse( sexo == 'M', -n, n) )


iess_pir_metropolitanos <- ggplot(aux, aes(x = edad, y = n, fill = sexo)) +
                           xlab( 'Edad' ) +
                           ylab( '' ) +
                           geom_bar( data = aux %>% filter(sexo == 'F'), stat = 'identity',colour="white", size=0.1) +
                           geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
                           scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                           scale_x_continuous(breaks = brks_x, labels = lbls_x) +
                           coord_flip() +
                           theme_bw() +
                           plt_theme +
                           guides(fill = guide_legend(title = NULL,label.position = "right",
                                                      label.hjust = 0, label.vjust = 0.5))+
                           theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
                           scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                                             labels = c("Mujeres", "Hombres"))

ggsave( plot = iess_pir_metropolitanos, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_metropolitanos', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Gráfico de barras cargo METROPOLITANOS------------------------------------------------------------------

message( '\tGráfico de barras por cargo Cuerpo de Agentes de Control Municipal o Metropolitano' )

aux <- ( tabla_metropolitanos_cargo %>% 
           select( cargo_coescop, n:= frecuencia) ) %>%
           group_by( cargo_coescop ) %>%
           mutate( N = sum( n, na.rm = TRUE) ) %>%
           ungroup()

c <- c("cargo_coescop","N")
aux <- aux[,names(aux) %in% c]
aux <- aux[!duplicated(aux),]


iess_bar_metropolitanos <- ggplot(aux, aes(x = cargo_coescop, y = N)) + 
                           geom_bar(stat = "identity", fill = parametros$iess_green)+
                           theme_bw() +
                           plt_theme +
                           geom_text(aes(label=N), 
                                    vjust=-0.9, 
                                    color="black", 
                                    hjust=0.5,
                                    position = position_dodge(0.9),  
                                    angle=0, 
                                    size=4.0) + 
                           labs(x = "Cargo COESCOP", y = "Número de Servidores")

ggsave( plot = iess_bar_snai, 
        filename = paste0( parametros$resultado_graficos, 'iess_bar_metropolitanos', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



#Gráfico pirámide poblacional CTE-------------------------------------------------------------------

message( '\tGraficando pirámide poblacional CTE' )
aux<-( tabla_cte_edad_sexo %>% 
         select( sexo, edad, n:= frecuencia) ) %>%
         group_by( sexo ) %>%
         mutate( N = sum( n, na.rm = TRUE) ) %>%
         mutate( n = n/N) %>%
         ungroup() %>%
         mutate( n = ifelse( sexo == 'M', -n, n) )


iess_pir_cte <- ggplot(aux, aes(x = edad, y = n, fill = sexo)) +
                xlab( 'Edad' ) +
                ylab( '' ) +
                geom_bar( data = aux %>% filter(sexo == 'F'), stat = 'identity',colour="white", size=0.1) +
                geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
                scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                scale_x_continuous(breaks = brks_x, labels = lbls_x) +
                coord_flip() +
                #theme_tufte()+
                theme_bw() +
                plt_theme +
                guides(fill = guide_legend(title = NULL,label.position = "right",
                                           label.hjust = 0, label.vjust = 0.5))+
                theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
                scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                                  labels = c("Mujeres", "Hombres"))

ggsave( plot = iess_pir_cte, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_cte', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Gráfico de barras cargo SNAI------------------------------------------------------------------

message( '\tGráfico de barras por cargo CTE' )

aux <- ( tabla_cte_cargo %>% 
           select( cargo_coescop, n:= frecuencia) ) %>%
           group_by( cargo_coescop ) %>%
           mutate( N = sum( n, na.rm = TRUE) ) %>%
           ungroup()

c <- c("cargo_coescop","N")
aux <- aux[,names(aux) %in% c]
aux <- aux[!duplicated(aux),]


iess_bar_cte <- ggplot(aux, aes(x = cargo_coescop, y = N)) + 
                geom_bar(stat = "identity", fill = parametros$iess_green)+
                theme_bw() +
                plt_theme +
                geom_text(aes(label=N), 
                          vjust=-0.9, 
                          color="black", 
                          hjust=0.5,
                          position = position_dodge(0.9),  
                          angle=0, 
                          size=4.0) + 
                labs(x = "Cargo COESCOP", y = "Número de Servidores")

ggsave( plot = iess_bar_snai, 
        filename = paste0( parametros$resultado_graficos, 'iess_bar_cte', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Gráfico pirámide poblacional BOMBEROS----------------------------------------------------------------

message( '\tGraficando pirámide poblacional BOMBEROS' )
aux<-( tabla_bomberos_edad_sexo %>% 
         select( sexo, edad, n:= frecuencia) ) %>%
         group_by( sexo ) %>%
         mutate( N = sum( n, na.rm = TRUE) ) %>%
         mutate( n = n/N) %>%
         ungroup() %>%
         mutate( n = ifelse( sexo == 'M', -n, n) )


iess_pir_bomberos <- ggplot(aux, aes(x = edad, y = n, fill = sexo)) +
                     xlab( 'Edad' ) +
                     ylab( '' ) +
                     geom_bar( data = aux %>% filter(sexo == 'F'), stat = 'identity',colour="white", size=0.1) +
                     geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
                     scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                     scale_x_continuous(breaks = brks_x, labels = lbls_x) +
                     coord_flip() +
                     theme_bw() +
                     plt_theme +
                     guides(fill = guide_legend(title = NULL,label.position = "right",
                                                label.hjust = 0, label.vjust = 0.5))+
                     theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
                     scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                                       labels = c("Mujeres", "Hombres"))

ggsave( plot = iess_pir_bomberos, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_bomberos', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Gráfico de barras cargo BOMBEROS------------------------------------------------------------------

message( '\tGráfico de barras por cargo Cuerpo de Bomberos' )

aux <- ( tabla_bomberos_cargo %>% 
           select( cargo_coescop, n:= frecuencia) ) %>%
           group_by( cargo_coescop ) %>%
           mutate( N = sum( n, na.rm = TRUE) ) %>%
           ungroup()

c <- c("cargo_coescop","N")
aux <- aux[,names(aux) %in% c]
aux <- aux[!duplicated(aux),]


iess_bar_bomberos <- ggplot(aux, aes(x = cargo_coescop, y = N)) + 
                     geom_bar(stat = "identity", fill = parametros$iess_green)+
                     theme_bw() +
                     plt_theme +
                     geom_text(aes(label=N), 
                                vjust=-0.9, 
                                color="black", 
                                hjust=0.5,
                                position = position_dodge(0.9),  
                                angle=0, 
                                size=4.0) + 
                     labs(x = "Cargo COESCOP", y = "Número de Servidores")

ggsave( plot = iess_bar_snai, 
        filename = paste0( parametros$resultado_graficos, 'iess_bar_bomberos', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Gráfico pirámide poblacional ADUANEROS-------------------------------------------------------------

message( '\tGraficando pirámide poblacional aduaneros' )
aux<-( tabla_aduaneros_edad_sexo %>% 
         select( sexo, edad, n:= frecuencia) ) %>%
         group_by( sexo ) %>%
         mutate( N = sum( n, na.rm = TRUE) ) %>%
         mutate( n = n/N) %>%
         ungroup() %>%
         mutate( n = ifelse( sexo == 'M', -n, n) )


iess_pir_aduaneros<-ggplot(aux, aes(x = edad, y = n, fill = sexo)) +
                    xlab( 'Edad' ) +
                    ylab( '' ) +
                    geom_bar( data = aux %>% filter(sexo == 'F'), stat = 'identity',colour="white", size=0.1) +
                    geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
                    scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                    scale_x_continuous(breaks = brks_x, labels = lbls_x) +
                    coord_flip() +
                    #theme_tufte()+
                    theme_bw() +
                    plt_theme +
                    guides(fill = guide_legend(title = NULL,label.position = "right",
                                               label.hjust = 0, label.vjust = 0.5))+
                    theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
                    scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                                      labels = c("Mujeres", "Hombres"))

ggsave( plot = iess_pir_aduaneros, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_aduaneros', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Gráfico de barras cargo ADUANEROS------------------------------------------------------------------

message( '\tGráfico de barras por cargo Cuerpo de Vigilancia Aduanera' )

aux <- ( tabla_aduaneros_cargo %>% 
           select( cargo_coescop, n:= frecuencia) ) %>%
           group_by( cargo_coescop ) %>%
           mutate( N = sum( n, na.rm = TRUE) ) %>%
           ungroup()

c <- c("cargo_coescop","N")
aux <- aux[,names(aux) %in% c]
aux <- aux[!duplicated(aux),]


iess_bar_aduaneros <- ggplot(aux, aes(x = cargo_coescop, y = N)) + 
                      geom_bar(stat = "identity", fill = parametros$iess_green)+
                      theme_bw() +
                      plt_theme +
                      geom_text(aes(label=N), 
                                vjust=-0.9, 
                                color="black", 
                                hjust=0.5,
                                position = position_dodge(0.9),  
                                angle=0, 
                                size=4.0) + 
                      labs(x = "Cargo COESCOP", y = "Número de Servidores")

ggsave( plot = iess_bar_snai, 
        filename = paste0( parametros$resultado_graficos, 'iess_bar_aduaneros', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Limpiar Memoria RAM--------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

