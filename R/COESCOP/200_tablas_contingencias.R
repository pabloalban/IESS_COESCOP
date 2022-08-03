
message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los población COESCOP' )
#Cargando población de beneficiarios----------------------------------------------------------------

load(paste0(parametros$RData, "COESCOP_snai.RData"))


#Tablas de contingencia por edad y sexo-------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tResuminedo información en tabals de contingencia' )


#Tablas de contingencia por edad y sexo-------------------------------------------------------------

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


#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( tabla_snai_edad_sexo,
      file = paste0( parametros$RData, 'IESS_tablas_contingencia.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()