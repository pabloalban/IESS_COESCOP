
message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los población COESCOP' )

#Cargando población de beneficiarios----------------------------------------------------------------

load(paste0(parametros$RData, "COESCOP_snai_cargos.RData")) #snai
load(paste0(parametros$RData, "COESCOP_snmlcf_cargos.RData")) #snmlcf
load(paste0(parametros$RData, "COESCOP_metropolitanos_cargos.RData")) #metropolitanos
load(paste0(parametros$RData, "COESCOP_cte_cargos.RData")) #cte
load(paste0(parametros$RData, "COESCOP_bomberos_cargos.RData")) #bomberos
load(paste0(parametros$RData, "COESCOP_aduaneros_cargos.RData")) #aduaneros

load(paste0(parametros$RData, "IESS_tablas_contingencia.RData")) #aduaneros

#Tablas de contingencia por edad y sexo-------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tResuminedo información en tablas de contingencia' )


#-------------------------------------------SNAI----------------------------------------------------

tabla_snai_edad_sexo <- snai %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                                         enddate = as.Date("31/03/2022","%d/%m/%Y"),
                                         units = "years",
                                         precise = TRUE ) )) %>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad ) %>%
  filter( edad > 17, edad < 70)

tabla_snai_cargo <- snai %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


#-----------------------------------------SNMLCF----------------------------------------------------

tabla_snmlcf_edad_sexo <- snmlcf %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad ) %>%
  filter( edad > 17, edad < 70)

tabla_snmlcf_cargo <- snmlcf %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)

#---------------------------------------METROPOLITANOS---------------------------------------------

tabla_metropolitanos_edad_sexo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad ) %>%
  filter( edad > 17, edad < 70)

tabla_metropolitanos_cargo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


#-------------------------------------------CTE--------------------------------------------------

tabla_cte_edad_sexo <- transito %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad ) %>%
  filter( edad > 17, edad < 70)

tabla_cte_cargo <- transito %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


#------------------------------------------BOMBEROS------------------------------------------------

tabla_bomberos_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad ) %>%
  filter( edad > 17, edad < 70)

tabla_bomberos_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


#------------------------------------------ADUANEROS-----------------------------------------------

tabla_aduaneros_edad_sexo <- aduaneros %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad ) %>%
  filter( edad > 17, edad < 70)


tabla_aduaneros_cargo <- aduaneros %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


#Guardando en un Rdata todas las tablas-------------------------------------------------------------
message( '\tGuardando en data.frame' )

tablas <- c('tabla_snmlcf_edad_sexo', 
            'tabla_snai_edad_sexo', 
            'tabla_metropolitanos_edad_sexo', 
            'tabla_cte_edad_sexo', 
            'tabla_bomberos_edad_sexo', 
            'tabla_aduaneros_edad_sexo',
            'tabla_snmlcf_cargo', 
            'tabla_snai_cargo', 
            'tabla_metropolitanos_cargo', 
            'tabla_cte_cargo', 
            'tabla_bomberos_cargo', 
            'tabla_aduaneros_cargo')

save(list = tablas , file = paste0( parametros$RData, 'IESS_tablas_contingencia.RData' ))


#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()

