
message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los población COESCOP' )

#Cargando población de beneficiarios----------------------------------------------------------------

load(paste0(parametros$RData, "COESCOP_snai_cargos.RData")) #snai
load(paste0(parametros$RData, "COESCOP_snmlcf_cargos.RData")) #snmlcf
load(paste0(parametros$RData, "COESCOP_metropolitanos_cargos.RData")) #metropolitanos
load(paste0(parametros$RData, "COESCOP_cte_cargos.RData")) #cte
load(paste0(parametros$RData, "COESCOP_bomberos_cargos.RData")) #bomberos
load(paste0(parametros$RData, "COESCOP_aduaneros_cargos.RData")) #aduaneros


#Tablas de contingencia por edad y sexo-------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tResumiendo información en tablas de contingencia' )


#-------------------------------------------SNAI----------------------------------------------------

tabla_snai_edad_sexo <- snai %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                                         enddate = as.Date("31/03/2022","%d/%m/%Y"),
                                         units = "years",
                                         precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_snai_cargo <- snai %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
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
  filter( edad > 17, edad < 70)%>%
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
  filter( edad > 17, edad < 70)%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_metropolitanos_ciudad <-  metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  group_by( sexo, ciudad)%>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, ciudad, .keep_all = TRUE ) %>%
  dplyr::select(ciudad, sexo,
                frecuencia)


##----------------METROPOLITANOS POR CIUDAD------------------------------------------------------

tabla_metropolitanos_quito_edad_sexo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad =='Quito')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_metropolitanos_quito_cargo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad =='Quito')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_metropolitanos_gye_edad_sexo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad == 'Guayaquil')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_metropolitanos_gye_cargo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad =='Guayaquil')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)

                                                  
tabla_metropolitanos_cuenca_edad_sexo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad == 'Cuenca')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )  

tabla_metropolitanos_cuenca_cargo <- metropolitanos%>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad =='Cuenca')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_metropolitanos_loja_edad_sexo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad == 'Loja')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad ) 

tabla_metropolitanos_loja_cargo <- metropolitanos%>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad =='Loja')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_metropolitanos_prtvj_edad_sexo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad == 'Portoviejo')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad ) 

tabla_metropolitanos_prtvj_cargo <- metropolitanos%>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad =='Portoviejo')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_metropolitanos_machala_edad_sexo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad == 'Machala')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_metropolitanos_machala_cargo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad =='Machala')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_metropolitanos_ambato_edad_sexo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad == 'Ambato')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_metropolitanos_ambato_cargo <- metropolitanos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( ciudad =='Ambato')%>%
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
  filter( edad > 17, edad < 70)%>%
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
  filter( edad > 17, edad < 70)%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)

tabla_bomberos_ciudad <-  bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  group_by( sexo, canton)%>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, canton, .keep_all = TRUE ) %>%
  dplyr::select(canton, sexo,
                frecuencia)
##-------------------------------BOMBEROS POR CIUDAD-----------------------------------------------

tabla_bomberos_ambato_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Ambato')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_ambato_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Ambato')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_quito_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Quito')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_quito_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Quito')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_riobamba_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Riobamba')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_riobamba_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Riobamba')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_cuenca_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Cuenca')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_cuenca_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Cuenca')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_machala_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Machala')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_machala_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Machala')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_manta_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Manta')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_manta_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Manta')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_gye_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Guayaquil')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_gye_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Guayaquil')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_sto_dom_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Santo Domingo')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_sto_dom_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Santo Domingo')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_ibarra_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Ibarra')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_ibarra_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Ibarra')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_loja_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Loja')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_loja_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Loja')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_milagro_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Milagro')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_milagro_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Milagro')%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select(cargo_coescop, sexo,
                frecuencia)


tabla_bomberos_prtvj_edad_sexo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton == 'Portoviejo')%>%
  group_by( edad, sexo ) %>%
  mutate( frecuencia = n() ) %>%
  ungroup() %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select(sexo,
                edad,
                frecuencia) %>%
  arrange( sexo, edad )

tabla_bomberos_prtvj_cargo <- bomberos %>%
  filter( !is.na(fecha_nacimiento)) %>%
  filter( fecha_nacimiento < as.Date("31/03/2022","%d/%m/%Y") ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = as.Date("31/03/2022","%d/%m/%Y"),
                               units = "years",
                               precise = TRUE ) )) %>%
  filter( edad > 17, edad < 70)%>%
  filter( canton =='Portoviejo')%>%
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
  filter( edad > 17, edad < 70)%>%
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
            'tabla_aduaneros_cargo',
            'tabla_metropolitanos_ciudad',
            'tabla_bomberos_ciudad',
            'tabla_metropolitanos_quito_edad_sexo',
            'tabla_metropolitanos_gye_edad_sexo',
            'tabla_metropolitanos_cuenca_edad_sexo',
            'tabla_metropolitanos_prtvj_edad_sexo',
            'tabla_metropolitanos_machala_edad_sexo',
            'tabla_metropolitanos_ambato_edad_sexo',
            'tabla_metropolitanos_loja_edad_sexo',
            'tabla_metropolitanos_quito_cargo',
            'tabla_metropolitanos_gye_cargo',
            'tabla_metropolitanos_cuenca_cargo',
            'tabla_metropolitanos_prtvj_cargo',
            'tabla_metropolitanos_machala_cargo',
            'tabla_metropolitanos_ambato_cargo',
            'tabla_metropolitanos_loja_cargo',
            'tabla_bomberos_riobamba_edad_sexo',
            'tabla_bomberos_ambato_edad_sexo',
            'tabla_bomberos_gye_edad_sexo',
            'tabla_bomberos_ibarra_edad_sexo',
            'tabla_bomberos_machala_edad_sexo',
            'tabla_bomberos_manta_edad_sexo',
            'tabla_bomberos_milagro_edad_sexo',
            'tabla_bomberos_prtvj_edad_sexo',
            'tabla_bomberos_quito_edad_sexo',
            'tabla_bomberos_sto_dom_edad_sexo',
            'tabla_bomberos_loja_edad_sexo',
            'tabla_bomberos_cuenca_edad_sexo',
            'tabla_bomberos_riobamba_cargo',
            'tabla_bomberos_ambato_cargo',
            'tabla_bomberos_gye_cargo',
            'tabla_bomberos_ibarra_cargo',
            'tabla_bomberos_machala_cargo',
            'tabla_bomberos_manta_cargo',
            'tabla_bomberos_milagro_cargo',
            'tabla_bomberos_prtvj_cargo',
            'tabla_bomberos_quito_cargo',
            'tabla_bomberos_sto_dom_cargo',
            'tabla_bomberos_loja_cargo',
            'tabla_bomberos_cuenca_cargo')
            

save(list = tablas , file = paste0( parametros$RData, 'IESS_tablas_contingencia.RData' ))


#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()

