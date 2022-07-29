message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de la mora patronal' )

#Cargando información financiera--------------------------------------------------------------------
file_1<-paste0(parametros$Data_seg, 'SENTENCIA CORTE CONST 1024/IESS-DNRGC-2022-0585-M/','base datos mora 05 marzo 2022-1.xlsx' )

options(java.parameters = "-Xmx1000m")
data <- read.xlsx(file_1)
#Caraga de recursos administrados por el BIESS------------------------------------------------------
parte_1 <- read_excel(file_1,
                               sheet = 1,
                               col_names = TRUE,
                               col_types = NULL,
                               n_max = 1000,
                               na = "",
                               skip = 0) %>% clean_names()

inver_corte <- read_excel(file,
                          sheet = 2,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          skip = 0) %>% clean_names()

rendimientos_netos <- read_excel(file,
                                 sheet = 3,
                                 col_names = TRUE,
                                 col_types = NULL,
                                 na = "",
                                 skip = 0) %>% clean_names() %>%
  mutate(corte_a=as.Date(corte_a,"%d/%m/%Y"))

ingresos <- read_excel(file,
                       sheet = 4,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()

gastos_opera <- read_excel(file,
                           sheet = 5,
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           skip = 0) %>% clean_names()

inv_instrumento <- read_excel(file,
                              sheet = 6,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0) %>% clean_names()

detalle_bonos <- read_excel(file,
                            sheet = 7,
                            col_names = TRUE,
                            col_types = NULL,
                            na = "",
                            skip = 0) %>% clean_names()

detalle_bonos_40 <- read_excel(file,
                               sheet = 8,
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0) %>% clean_names() %>%
  mutate(fecha_colocacion=as.Date(fecha_colocacion,"%d/%m/%Y"),
         vencimiento=as.Date(vencimiento,"%d/%m/%Y"),
         pago_del_periodo=as.Date(pago_del_periodo,"%d/%m/%Y"))

detalle_obligaciones <- read_excel(file,
                                   sheet = 9,
                                   col_names = TRUE,
                                   col_types = NULL,
                                   na = "",
                                   skip = 0) %>% clean_names()

detalle_titularizaciones <- read_excel(file,
                                       sheet = 10,
                                       col_names = TRUE,
                                       col_types = NULL,
                                       na = "",
                                       skip = 0) %>% clean_names()

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando inversiones en un solo data.frame' )

save( recurs_adm_biess,
      inver_corte,
      rendimientos_netos,
      ingresos,
      gastos_opera,
      inv_instrumento,
      detalle_bonos,
      detalle_bonos_40,
      detalle_obligaciones,
      detalle_titularizaciones,
      file = paste0( parametros$RData_seg, 'IESS_RTR_inversiones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()