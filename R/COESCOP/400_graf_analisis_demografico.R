message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando población afiliada activa inicial SGO del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_demografico.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_rentas_2020.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_subsidios_prestaciones_2020.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_indemnizaciones_prestaciones_2020.RData' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_poblacion_afiliada_inicial.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_IVM_analisis_demografico.RData' ) ) 

# Graficos Afiliados Activos------------------------------------------------------------------------
unidad<-1e6
aux<-copy( pob_afi_ini[, .(anio, Afiliados)] )

x_lim <- c( 2005, 2020 )
x_brk <- 2005:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
#x_lbl[x_lbl=="2020"] <-"2020-04"

y_lim <- c( 500000, 3500000)
y_brk <- seq( y_lim[1], y_lim[2], 500000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pob_afi_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = Afiliados, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Afiliados' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ) )

# iess_afi_ini
ggsave( plot = iess_pob_afi_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_afi_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Masa salarial-------------------------------------------------------------------------------------
message( '\tGraficando masa salarial inicial SGO del IESS' )

# Carga de datos -----------------------------------------------------------------------------------
#load( file = paste0( parametros$RData_seg, 'IESS_masa_salarial_inicial.RData' ) )

unidad<-1e6
aux<-copy( masa_salarial_ini[ anio<=2020, .(anio, Masa_Anual)] )
#aux[, activos := activos / unidad ]

x_lim <- c( 2005, 2020 )
x_brk <- 2005:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 4000000000, 28000000000)
y_brk <- seq( y_lim[1], y_lim[2], 4000000000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_masa_salarial_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = Masa_Anual, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Masa Salarial' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

# iess_masa_salarial
ggsave( plot = iess_masa_salarial_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Graficando población afiliada activa inicial por edad y sexo SGO del IESS--------------------------
message( '\tGraficando población afiliada activa inicial por edad y sexo SGO del IESS' )
aux<-copy( pob_afi_edad_sexo_ini )
max_edad<-100
min_edad<-15
aux<-aux[edad<=95]  #Condición para extraer los datos

aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE),by=sexo]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-10
salto_x<-0.01
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_afiliados<-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
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
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_afiliados, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficos de la pirámide de Masa Salarial----------------------------------------------------------
message( '\tGraficando masa salarial por monto y sexo SGO del IESS' )
aux<-copy( masa_sal_edad_monto_ini )
aux<-aux[cat=="afi"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE),by=sexo]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x
salto_y<-200
salto_x<-0.05
brks_y <- seq(-0.40,0.40,salto_x)
lbls_y <- paste0(as.character(c(seq(0.40, 0, -salto_x)*100, seq(salto_x, 0.40, salto_x)*100)), "%")
brks_x <- seq(100,1600,salto_y)
nb <- length(brks_x)-1   
lbls_x <- c(paste0(formatC(c(brks_x[1:nb]),
                           digits = 0, 
                           format = 'f', 
                           big.mark = '.', 
                           decimal.mark = ',')),"mayor a 1.500")

iess_pir_masa_salarial<-ggplot(aux, aes(x = monto, y = n, fill=sexo)) +
  xlab( 'Salario (USD)' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ],
            stat = 'identity',
            colour="white",  
            size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], 
            stat = 'identity',
            colour="white",  
            size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_masa_salarial, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Subsidios------------------------------------------------------------------------------------------
message( '\tGraficando población beneficiaria a subsidios de RT' )
unidad<-1e6
message( '\tTabla de Subsidios' )
aux <- subsidios_rtr %>%
  mutate(anio=year(sr_fecha_generacion)) %>%
  arrange(anio) %>%
  group_by(anio) %>%
  mutate(monto=sum(sr_valor,na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sr_cedula,anio,.keep_all = TRUE) %>%
  group_by(anio) %>%
  mutate(numero=n()) %>%
  ungroup() %>%
  distinct(anio,.keep_all = TRUE) %>%
  select(anio,numero,monto)

aux[nrow(aux), 2:3 ] <- 3 * aux[nrow(aux), 2:3 ]

aux <- aux %>%
  mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
  mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
  select(anio,numero)


x_lim <- c( 2011, 2020 )
x_brk <- 2011:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 16000)
y_brk <- seq( y_lim[1], y_lim[2], 4000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_subsidios <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = numero, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_subsidios, 
        filename = paste0( parametros$resultado_graficos, 'iess_subsidios_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Subsidios por edad y sexo--------------------------------------------------------------------------
message( '\tGraficando subsidios por edad y sexo de RT' )
aux<-copy( subsidios_edad_sexo[,.(edad, personas,sexo)] )
min_edad <- min(aux$edad)
max_edad <- max(aux$edad)

aux[is.na(personas),personas:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(personas,na.rm = TRUE),by=sexo]))  # número total por sexo
aux[sexo=="MASCULINO", sexo:="H"]
aux[sexo=="FEMENINO", sexo:="M"]
aux[sexo=="H", personas:=-personas]
aux[sexo=="H", personas:=personas/N[1,2]]
aux[sexo=="M", personas:=personas/N[2,2]]

M <- data.frame((aux[,max(abs(personas),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-0.01
brks_y <- seq(-0.06,0.06, salto_y)
lbls_y <- paste0(as.character(c(seq(0.06, 0, -salto_y)*100, seq(salto_y, 0.06, salto_y)*100)), "%")
salto_x<-10
brks_x <- seq(min_edad,max_edad,salto_x)
lbls_x <- paste0(as.character(brks_x))

iess_subsidios_edad_sexo <- ggplot(aux, aes(x = edad, y = personas, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ],
            stat = 'identity',
            colour="white",
            size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ],
            stat = 'identity',
            colour="white", 
            size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom") +   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres")) +
  theme(legend.box.spacing = unit(-0.5,"cm"))

ggsave( plot = iess_subsidios_edad_sexo, 
        filename = paste0( parametros$resultado_graficos, 
                           'iess_subsidios_edad_sexo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Indemnizaciones------------------------------------------------------------------------------------
message( '\tGraficando población beneficiaria a indemnizaciones de RT' )
unidad<-1e6
aux <- indemnizaciones_rtr %>%
  mutate( anio = ano ) %>%
  filter(anio<='2020',
         tipo_prestacion=='ID') %>%
  group_by(anio) %>%
  mutate(monto=sum(valor_de_la_prestacion,na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(cedula,anio,.keep_all = TRUE) %>%
  group_by(anio) %>%
  mutate(numero=n()) %>%
  ungroup() %>%
  distinct(anio,.keep_all = TRUE) %>%
  select(anio,numero,monto) 

aux[nrow(aux), 2:3 ] <- 3 * aux[nrow(aux), 2:3 ]

aux <- aux %>%
  arrange(anio) %>%
  mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
  mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
  #mutate(anio=as.character(anio)) %>%
  select(anio,numero,creci_num,monto,creci_monto) 

x_lim <- c( 2011, 2020 )
x_brk <- 2011:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1250)
y_brk <- seq( y_lim[1], y_lim[2], 300 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_indemnizaciones <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = numero, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_indemnizaciones , 
        filename = paste0( parametros$resultado_graficos, 'iess_indemnizaciones_rtr', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Indemnizaciones por edad y sexo--------------------------------------------------------------------
message( '\tGraficando población beneficiaria a indemnizaciones por edad y sexo de RT' )
unidad<-1e6
aux<-copy( indemnizaciones_edad_sexo[,.(edad, personas, sexo)])
min_edad <- min(aux$edad)
max_edad <- max(aux$edad)+3

N <- data.frame((aux[,sum(personas,na.rm = TRUE),by=sexo]))  # número total por sexo
aux[sexo=="MASCULINO", sexo:="H"]
aux[sexo=="FEMENINO", sexo:="M"]
aux[sexo=="H", personas:=-personas]
aux[sexo=="H", personas:=personas/N[1,2]]
aux[sexo=="M", personas:=personas/N[2,2]]

M <- data.frame((aux[,max(abs(personas),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-0.01
brks_y <- seq(-0.08,0.08,salto_y)
lbls_y <- paste0(as.character(c(seq(0.08, 0, -salto_y)*100, seq(salto_y, 0.08, salto_y)*100)), "%")
salto_x<-10
brks_x <- seq(min_edad,max_edad,salto_x)
lbls_x <- paste0(as.character(brks_x))

iess_indemnizaciones_edad_sexo <- ggplot(aux, aes(x = edad, y = personas, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',
            colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',
            colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom") +   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, 
                               parametros$iess_green),
                    labels = c("Hombres", "Mujeres")) +
  theme(legend.box.spacing = unit(-0.5,"cm"))

ggsave( plot = iess_indemnizaciones_edad_sexo,
        filename = paste0( parametros$resultado_graficos, 'iess_indemnizaciones_edad_sexo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Pensiones provisionales por incapacidad temporal---------------------------------------------------
message( '\tGraficando pensiones provisionales por incapacidad temporal de RT' )
unidad<-1e6
aux<-copy( pensiones_provisionales_inc_tem)

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 200)
y_brk <- seq( y_lim[1], y_lim[2], 20 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pensiones_provisionales_inc_tem <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = numero, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
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

ggsave( plot = iess_pensiones_provisionales_inc_tem , 
        filename = paste0( parametros$resultado_graficos, 'iess_pensiones_provisionales_inc_tem_rtr', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Incapacidad permanente parcial---------------------------------------------------------------------
message( '\tGraficando pensionistas por incapacidad permanente parcial del fondo de RT' )
unidad<-1e6
aux<- base %>%
  filter(tipo_prestacion=='PP') %>%
  group_by(anio) %>%
  mutate(monto=sum(tot_ingr,na.rm = TRUE)) %>%
  #filter(mes=='12') %>%
  distinct(cedula,.keep_all = TRUE) %>%
  mutate(numero=n()) %>%
  distinct(anio,.keep_all = TRUE) %>%
  ungroup() %>%
  select(anio,numero,monto) %>%
  mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
  mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
  mutate(anio=as.numeric(anio)) %>%
  select(anio,numero,creci_num,monto,creci_monto)


x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 4700, 5200)
y_brk <- seq( y_lim[1], y_lim[2], 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_incapacidad_parcial <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = numero, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_incapacidad_parcial , 
        filename = paste0( parametros$resultado_graficos, 'iess_incapacidad_parcial_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Incapacidad Permanente Parcial por edad y sexo-----------------------------------------------------
message( '\tGraficando pensionistas por incapacidad permanente parcial por edad y sexo de RT' )
unidad<-1e6
aux<-copy(  incapacidad_parcial_edad_sexo[,.(edad, personas, sexo)])
aux <- aux[edad=="<null>", edad:=""]
aux[, edad:=as.double(edad)]
aux <- aux[!is.na(edad)]
min_edad <- min(aux$edad)
max_edad <- max(aux$edad)

N <- data.frame((aux[,sum(personas,na.rm = TRUE),by=sexo]))  # número total por sexo
aux[sexo=="Masculino", sexo:="H"]
aux[sexo=="Femenino", sexo:="M"]
aux[sexo=="H", personas:=-personas]
aux[sexo=="H", personas:=personas/N[1,2]]
aux[sexo=="M", personas:=personas/N[2,2]]

M <- data.frame((aux[,max(abs(personas),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-0.01
brks_y <- seq(-0.05,0.05,salto_y)
lbls_y <- paste0(as.character(c(seq(0.05, 0, -salto_y)*100, seq(salto_y, 0.05, salto_y)*100)), "%")
salto_x<-9
brks_x <- seq(min_edad,max_edad,salto_x)
lbls_x <- paste0(as.character(brks_x))

iess_incapacidad_parcial_edad_sexo <- ggplot(aux, aes(x = edad, y = personas, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',
            colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',
            colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom") +   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, 
                               parametros$iess_green),
                    labels = c("Hombres", "Mujeres")) +
  theme(legend.box.spacing = unit(-0.5,"cm"))

ggsave( plot = iess_incapacidad_parcial_edad_sexo,
        filename = paste0( parametros$resultado_graficos, 'iess_incapacidad_parcial_edad_sexo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Incapacidad permanente total---------------------------------------------------------------------
message( '\tGraficando pensionistas por incapacidad permanente total del fondo de RT' )
unidad<-1e6
aux<- prestaciones_pt %>%
  group_by(anio) %>%
  mutate(monto=sum(tot_ingr,na.rm = TRUE)) %>%
  #filter(mes=='12') %>%
  distinct(cedula,.keep_all = TRUE) %>%
  mutate(numero=n()) %>%
  distinct(anio,.keep_all = TRUE) %>%
  ungroup() %>%
  select(anio,numero,monto) %>%
  mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
  mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
  mutate(anio=as.integer(anio)) %>%
  select(anio,numero,creci_num,monto,creci_monto)

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 900, 1300)
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_incapacidad_total <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = numero, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_incapacidad_total , 
        filename = paste0( parametros$resultado_graficos, 'iess_incapacidad_total_rtr', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Incapacidad Permanente total por edad y sexo-------------------------------------------------------
message( '\tGraficando pensionistas por incapacidad permanente total por edad y sexo de RT' )
unidad<-1e6
aux<-copy(  incapacidad_total_edad_sexo[,.(edad, personas, sexo)])
aux <- aux[edad=="<null>", edad:=""]
aux[, edad:=as.double(edad)]
aux <- aux[!is.na(edad)]
min_edad <- min(aux$edad)
max_edad <- max(aux$edad)

N <- data.frame((aux[,sum(personas,na.rm = TRUE),by=sexo]))  # número total por sexo
aux[sexo=="Masculino", sexo:="H"]
aux[sexo=="Femenino", sexo:="M"]
aux[sexo=="H", personas:=-personas]
aux[sexo=="H", personas:=personas/N[1,2]]
aux[sexo=="M", personas:=personas/N[2,2]]

M <- data.frame((aux[,max(abs(personas),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-0.01
brks_y <- seq(-0.07,0.07,salto_y)
lbls_y <- paste0(as.character(c(seq(0.07, 0, -salto_y)*100, seq(salto_y, 0.07, salto_y)*100)), "%")
salto_x<-8
brks_x <- seq(min_edad,max_edad,salto_x)
lbls_x <- paste0(as.character(brks_x))

iess_incapacidad_total_edad_sexo <- ggplot(aux, aes(x = edad, y = personas, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',
            colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',
            colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,
                             label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom") +   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue,
                               parametros$iess_green),
                    labels = c("Hombres", "Mujeres")) +
  theme(legend.box.spacing = unit(-0.5,"cm"))

ggsave( plot = iess_incapacidad_total_edad_sexo,
        filename = paste0( parametros$resultado_graficos, 'iess_incapacidad_total_edad_sexo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# Incapacidad permanente absoluta--------------------------------------------------------------------
message( '\tGraficando pensionistas por incapacidad permanente absoluta del fondo de RT' )
unidad<-1e6
aux<- prestaciones_pa %>%
  group_by(anio) %>%
  mutate(monto=sum(tot_pension,na.rm = TRUE)) %>%
  #filter(mes=='12') %>%
  distinct(cedula,.keep_all = TRUE) %>%
  mutate(numero=n()) %>%
  distinct(anio,.keep_all = TRUE) %>%
  ungroup() %>%
  select(anio,numero,monto) %>%
  mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
  mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
  mutate(anio=as.integer(anio)) %>%
  select(anio,numero,creci_num,monto,creci_monto)

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 160)
y_brk <- seq( y_lim[1], y_lim[2], 20 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_incapacidad_absoluta <-  ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = numero, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_incapacidad_absoluta , 
        filename = paste0( parametros$resultado_graficos, 'iess_incapacidad_absoluta_rtr', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Incapacidad Permanente absoluta por edad y sexo----------------------------------------------------
message( '\tGraficando pensionistas por incapacidad permanente total por edad y sexo de RT' )
unidad<-1e6
aux<-copy(  incapacidad_absoluta_edad_sexo[,.(edad, personas, sexo)])
aux <- aux[edad=="<null>", edad:=""]
aux[, edad:=as.double(edad)]
aux <- aux[!is.na(edad)]
min_edad <- min(aux$edad)
max_edad <- max(aux$edad)

N <- data.frame((aux[,sum(personas,na.rm = TRUE),by=sexo]))  # número total por sexo
aux[sexo=="Masculino", sexo:="H"]
aux[sexo=="Femenino", sexo:="M"]
aux[sexo=="H", personas:=-personas]
aux[sexo=="H", personas:=personas/N[1,2]]
aux[sexo=="M", personas:=personas/N[2,2]]

M <- data.frame((aux[,max(abs(personas),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-0.02
brks_y <- seq(-0.16,0.16,salto_y)
lbls_y <- paste0(as.character(c(seq(0.16, 0, -salto_y)*100, seq(salto_y, 0.16, salto_y)*100)), "%")
salto_x<-8
brks_x <- seq(min_edad,81,salto_x)
lbls_x <- paste0(as.character(brks_x))


iess_incapacidad_absoluta_edad_sexo <-ggplot(aux, aes(x = edad, y = personas, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], 
            stat = 'identity',
            colour="white", 
            size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], 
            stat = 'identity',
            colour="white",
            size=0.1) +
  scale_y_continuous(breaks = brks_y, 
                     labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, 
                     labels = lbls_x,
                     limits = c(24,80)) +
  coord_flip() +
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,
                             label.position = "right", 
                             label.hjust = 0, 
                             label.vjust = 0.5))+
  theme(legend.position="bottom") +   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue,
                               parametros$iess_green),
                    labels = c("Hombres", "Mujeres")) +
  theme(legend.box.spacing = unit(-0.5,"cm"))

ggsave( plot = iess_incapacidad_absoluta_edad_sexo,
        filename = paste0( parametros$resultado_graficos, 'iess_incapacidad_absoluta_edad_sexo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensionistas por orfandad--------------------------------------------------------------------------
message( '\tGraficando pensionistas por orfandad del fondo de RT' )
unidad<-1e6
aux<- prestaciones_orfandad %>%
  group_by(anio) %>%
  mutate(monto=sum(tot_ingr,na.rm = TRUE)) %>%
  #filter(mes=='12') %>%
  distinct(cedula,.keep_all = TRUE) %>%
  mutate(numero=n()) %>%
  distinct(anio,.keep_all = TRUE) %>%
  ungroup() %>%
  select(anio,numero,monto) %>%
  mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
  mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
  mutate(anio=as.integer(anio)) %>%
  select(anio,numero,creci_num,monto,creci_monto)


x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 3000, 4000)
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pensionistas_orfadad <-  ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = numero, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_pensionistas_orfadad , 
        filename = paste0( parametros$resultado_graficos, 'iess_pensionistas_orfadad_rtr', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensionistas por orfandad por edad y sexo----------------------------------------------------------
message( '\tGraficando pensionistas por orfandad por edad y sexo de RT' )
unidad<-1e6
aux<-copy(  pensionistas_orfandad_edad_sexo[,.(edad, personas, sexo)])
aux <- aux[edad=="<null>", edad:=""]
aux[, edad:=as.double(edad)]
aux <- aux[!is.na(edad)]
aux<-aux[edad<18]

min_edad <- 1
max_edad <- 17

N <- data.frame((aux[,sum(personas,na.rm = TRUE),by=sexo]))  # número total por sexo
aux[sexo=="Masculino", sexo:="H"]
aux[sexo=="Femenino", sexo:="M"]
aux[sexo=="H", personas:=-personas]
aux[sexo=="H", personas:=personas/N[1,2]]
aux[sexo=="M", personas:=personas/N[2,2]]

M <- data.frame((aux[,max(abs(personas),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-0.02
brks_y <- seq(-0.12,0.12,salto_y)
lbls_y <- paste0(as.character(c(seq(0.12, 0, -salto_y)*100, seq(salto_y, 0.12, salto_y)*100)), "%")
salto_x<-2
brks_x <- seq(1,17,salto_x)
lbls_x <- paste0(as.character(brks_x))

iess_pensionistas_orfandad_edad_sexo <- ggplot(aux, aes(x = edad, y = personas, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',
            colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',
            colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,
                             label.position = "right", 
                             label.hjust = 0, 
                             label.vjust = 0.5))+
  theme(legend.position="bottom") +   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, 
                               parametros$iess_green),
                    labels = c("Hombres", "Mujeres")) +
  theme(legend.box.spacing = unit(-0.5,"cm"))

ggsave( plot = iess_pensionistas_orfandad_edad_sexo,
        filename = paste0( parametros$resultado_graficos,
                           'iess_pensionistas_orfandad_edad_sexo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensionistas por viudedad--------------------------------------------------------------------------
message( '\tGraficando pensionistas por viudedad del fondo de RT' )
unidad<-1e6
aux<- prestaciones_viudez %>%
  group_by(anio) %>%
  mutate(monto=sum(tot_ingr,na.rm = TRUE)) %>%
  #filter(mes=='12') %>%
  distinct(cedula,.keep_all = TRUE) %>%
  mutate(numero=n()) %>%
  distinct(anio,.keep_all = TRUE) %>%
  ungroup() %>%
  select(anio,numero,monto) %>%
  mutate(creci_num=100*(numero-lag(numero))/lag(numero)) %>%
  mutate(creci_monto=100*(monto-lag(monto))/lag(monto)) %>%
  mutate(anio=as.integer(anio)) %>%
  select(anio,numero,creci_num,monto,creci_monto)

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 3000, 4400)
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pensionistas_viudedad <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = numero, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_pensionistas_viudedad , 
        filename = paste0( parametros$resultado_graficos, 'iess_pensionistas_viudedad_rtr', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensionistas por viudedad por edad y sexo----------------------------------------------------------
message( '\tGraficando pensionistas por viudedad por edad y sexo de RT' )
unidad<-1e6
aux<-copy(  pensionistas_viudedad_edad_sexo[,.(edad, personas, sexo)])
aux <- aux[edad=="<null>", edad:=""]
aux[, edad:=as.double(edad)]
aux <- aux[!is.na(edad)]
min_edad <- 20
max_edad <- 100

N <- data.frame((aux[,sum(personas,na.rm = TRUE),by=sexo]))  # número total por sexo
aux[sexo=="Masculino", sexo:="H"]
aux[sexo=="Femenino", sexo:="M"]
aux[sexo=="H", personas:=-personas]
aux[sexo=="H", personas:=personas/N[2,2]]
aux[sexo=="M", personas:=personas/N[1,2]]

M <- data.frame((aux[,max(abs(personas),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-0.01
brks_y <- seq(-0.1,0.1,salto_y)
lbls_y <- paste0(as.character(c(seq(0.1, 0, -salto_y)*100, seq(salto_y, 0.1, salto_y)*100)), "%")

salto_x<-10
brks_x <- seq(min_edad,max_edad,salto_x)
lbls_x <- paste0(as.character(brks_x))

iess_pensionistas_viudedad_edad_sexo <- ggplot(aux, aes(x = edad, y = personas, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme_legend +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom") +   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres")) +
  theme(legend.box.spacing = unit(-0.5,"cm"))

ggsave( plot = iess_pensionistas_viudedad_edad_sexo,
        filename = paste0( parametros$resultado_graficos, 'iess_pensionistas_viudedad_edad_sexo_rtr', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Limpiar Memoria RAM--------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

