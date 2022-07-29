message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLeyendo Activo Fondo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )

activo_del_fondo <- read_xlsx( path = file, sheet = 'Tabla1', skip = 0, 
                               col_names = T)
#Análisis Componentes Activo
message( '\tLeyendo Análisis Componentes Activo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_componentes_activo <- read_xlsx( path = file, sheet = 'Tabla2', skip = 0,
                                          col_names = T)
#Leyendo Análisis Horizontal del Activo
message( '\tLeyendo Análisis Horizontal del Activo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_horizontal_activo <- read_xlsx( path = file, sheet = 'Tabla3', skip = 0,
                                         col_names = T)
#Leyendo Análisis Vertical del Activo
message( '\tLeyendo Análisis Vertical del Activo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_vertical_activo <- read_xlsx( path = file, sheet = 'Tabla4', skip = 0,
                                       col_names = T)
#Leyendo Cuentas por CObrar del FOndo RT
message( '\tLeyendo Cuentas por CObrar del FOndo RT' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
cuentas_cobrar_fondo <- read_xlsx( path = file, sheet = 'Tabla5', skip = 0,
                                   col_names = T)
#Leyendo Pasivos del Fondo
message( '\tLeyendo Pasivos del Fondo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
pasivos_fondo <- read_xlsx( path = file, sheet = 'Tabla6', skip = 0,
                            col_names = T)
# Leyendo Componentes del Pasivo del Fondo
message( '\tLeyendo Componentes del Pasivo del Fondo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
componentes_pasivos_fondo <- read_xlsx( path = file, sheet = 'Tabla7', skip = 0,
                                        col_names = T)
#Leyendo Análisis Horizontal del Pasivo
message( '\tLeyendo Análisis Horizontal del Pasivo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_horizontal_pasivos <- read_xlsx( path = file, sheet = 'Tabla8', skip = 0,
                                          col_names = T)
#Leyendo Análisis Vertical del Pasivo
message( '\tLeyendo Análisis Vertical del Pasivo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_vertical_pasivos <- read_xlsx( path = file, sheet = 'Tabla9', skip = 0,
                                        col_names = T)
#Leyendo Cuentas por Pagar del Fondo
message( '\tLeyendo Cuentas por Pagar del Fondo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
cuentas_pagar_fondo <- read_xlsx( path = file, sheet = 'Tabla10', skip = 0,
                                  col_names = T)
#Leyendo Patrimonio del Fondo
message( '\tLeyendo Patrimonio del Fondo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
patrimonio_fondo <- read_xlsx( path = file, sheet = 'Tabla11', skip = 0,
                               col_names = T)
#Leyendo Componentes del Patrimonio
message( '\tLeyendo Componentes del Patrimonio' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
componentes_patrimonio_fondo <- read_xlsx( path = file, sheet = 'Tabla12', skip = 0,
                                           col_names = T)
#Leyendo Análisis Horizontal del Patrimonio
message( '\tLeyendo Análisis Horizontal del Patrimonio' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_horizontal_patrimonio <- read_xlsx( path = file, sheet = 'Tabla13', skip = 0,
                                             col_names = T)
#Leyendo Análisis Vertical del Patrimonio
message( '\tLeyendo Análisis Vertical del Patrimonio' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_vertical_patrimonio <- read_xlsx( path = file, sheet = 'Tabla14', skip = 0,
                                           col_names = T)
#Leyendo Ingresos del Fondo
message( '\tLeyendo Ingresos del Fondo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
ingresos_fondo <- read_xlsx( path = file, sheet = 'Tabla15', skip = 0,
                             col_names = T)
#Leyendo Componentes de los Ingresos
message( '\tLeyendo Componentes de los Ingresos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
componentes_ingresos <- read_xlsx( path = file, sheet = 'comp_ingre', skip = 0,
                                   col_names = T)
#Leyendo Análisis Horizontal de los Ingresos
message( '\tLeyendo Análisis Horizontal de los Ingresos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_horizontal_ingresos <- read_xlsx( path = file, sheet = 'ana_hor_ing', skip = 0,
                                           col_names = T)
#Leyendo Análisis Vertical de los Ingresos
message( '\tLeyendo Análisis Vertical de los Ingresos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_vertical_ingresos <- read_xlsx( path = file, sheet = 'ana_ver_ing', skip = 0,
                                         col_names = T)
#Leyendo Ingresos por Aportes
message( '\tLeyendo Ingresos por Aportes' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
ingresos_aportes <- read_xlsx( path = file, sheet = 'Tabla19', skip = 0,
                               col_names = T)
#Leyendo Otros Ingresos
message( '\tLeyendo Otros Ingresos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
otros_ingresos <- read_xlsx( path = file, sheet = 'Tabla20', skip = 0,
                             col_names = T)
#Leyendo Gastos
message( '\tLeyendo Gastos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
gastos <- read_xlsx( path = file, sheet = 'Tabla21', skip = 0,
                     col_names = T)
#Leyendo Componentes del Gastos
message( '\tLeyendo Componentes del Gastos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
componentes_gastos <- read_xlsx( path = file, sheet = 'compo_gasto', skip = 0,
                                 col_names = T)
#Leyendo Análisis Horizontal del Gastos
message( '\tLeyendo Análisis Horizontal del Gastos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_horizontal_gastos <- read_xlsx( path = file, sheet = 'ana_hor_gastos', skip = 0,
                                         col_names = T)

#Leyendo Análisis Vertical del Gastos
message( '\tLeyendo Análisis Vertical del Gastos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_vertical_gastos <- read_xlsx( path = file, sheet = 'ana_ver_gastos', skip = 0,
                                       col_names = T)
# Leyendo Gastos Prestacionales
message( '\tLeyendo Gastos Prestacionales' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
gastos_prestacionales <- read_xlsx( path = file, sheet = 'gastos_prestacionales', skip = 0,
                                    col_names = T)
#Leyendo Otros Gastos Prestacionale
message( '\tLeyendo Otros Gastos Prestacionales' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
otros_gastos_prestacionales <- read_xlsx( path = file, sheet = 'Tabla26', skip = 0,
                                          col_names = T)
# Leyendo Otros Gastos
message( '\tLeyendo Otros Gastos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
gast_adm <- read_xlsx( path = file, sheet = 'gast_adm', skip = 0,
                       col_names = T)

#Lectura de ingresos vs gastos totales
message( '\tLeyendo ingresos vs gastos totales' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
ingresos_vs_gastos <- read_xlsx( path = file, sheet = 'ingresos_vs_gastos', skip = 0,
                                 col_names = T)

#Lectura de ingresos vs gastos adminstrativos
message( '\tLeyendo ingresos vs gastos administrativos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
ingresos_vs_gtos_adm <- read_xlsx( path = file, sheet = 'ingresos_vs_gtos_adm', skip = 0,
                                   col_names = T)

#Lectura de ingresos vs gastos adminstrativos
message( '\tLeyendo ingresos vs gastos administrativos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
relacion_patrimonio_beneficio <- read_xlsx( path = file, sheet = 'relacion_patrimonio_beneficio', skip = 0,
                                            col_names = T)

#Lectura desglose de pensiones de RT
message( '\tLeyendo ingresos vs gastos administrativos' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
gastos_pensiones_rt <- read_xlsx( path = file, sheet = 'gastos_pensiones_rt', skip = 0,
                                  col_names = T)


lista <- c('activo_del_fondo', 'analisis_componentes_activo', 'analisis_horizontal_activo'
           , 'analisis_vertical_activo', 'cuentas_cobrar_fondo', 'pasivos_fondo'
           ,'componentes_pasivos_fondo', 'analisis_horizontal_pasivos', 'analisis_vertical_pasivos'
           , 'cuentas_pagar_fondo', 'patrimonio_fondo', 'componentes_patrimonio_fondo'
           , 'analisis_horizontal_patrimonio', 'analisis_vertical_patrimonio', 'ingresos_fondo'
           , 'componentes_ingresos', 'analisis_horizontal_ingresos', 'analisis_vertical_ingresos'
           , 'ingresos_aportes', 'otros_ingresos', 'gastos', 'componentes_gastos'
           , 'analisis_horizontal_gastos', 'analisis_vertical_gastos', 'gastos_prestacionales'
           , 'otros_gastos_prestacionales', 'gast_adm', 'relacion_patrimonio_beneficio'
           , 'ingresos_vs_gastos', 'ingresos_vs_gtos_adm')
save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )
#-----------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
