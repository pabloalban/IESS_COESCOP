message(paste(rep("-", 100), collapse = ""))

message("\tCargando servidores COESCOP")

# Carga de bases------------------------------------------------------------------------------------
load( paste0( parametros$RData, "COESCOP_bomberos_cargos.RData" )) #bomberos
load( paste0( parametros$RData, "COESCOP_cte_cargos.RData") ) #cte
load( paste0( parametros$RData, "COESCOP_snai_cargos.RData") ) #snai
load( paste0( parametros$RData, "COESCOP_snmlcf_cargos.RData") ) #snmlcf
load( paste0( parametros$RData, "COESCOP_metropolitanos_cargos.RData") ) #metropolitanos
load( paste0( parametros$RData, "COESCOP_aduaneros_cargos.RData") ) #aduaneros
load( paste0( parametros$RData, "IESS_imposiciones_2022_12.RData") ) 



#Selección de variables-----------------------------------------------------------------------------

ad <- aduaneros %>%
  dplyr::select( cedula,
                 fecha_nacimiento,
                 sexo,
                 nombre,
                 cargo,
                 sueldo,
                 cargo_coescop ) %>%
  mutate( ciudad = NA ) %>%
  mutate( tipo = "aduanero")


bo <- bomberos %>%
  dplyr::select( cedula,
                 fecha_nacimiento,
                 sexo,
                 nombre,
                 cargo,
                 sueldo,
                 cargo_coescop,
                 ciudad:=canton ) %>%
  mutate( tipo = "bombero")


me <- metropolitanos %>%
  dplyr::select( cedula,
                 fecha_nacimiento,
                 sexo,
                 nombre,
                 cargo,
                 sueldo,
                 cargo_coescop,
                 ciudad) %>%
  mutate( tipo = "metropolitano")


snai <- snai %>%
  dplyr::select( cedula,
                 fecha_nacimiento,
                 sexo,
                 nombre,
                 cargo,
                 sueldo,
                 cargo_coescop ) %>%
  mutate( ciudad = NA ) %>%
  mutate( tipo = "snai")


sn <- snmlcf %>%
  dplyr::select( cedula,
                 fecha_nacimiento,
                 sexo,
                 nombre,
                 cargo,
                 sueldo,
                 cargo_coescop ) %>%
  mutate( ciudad = NA ) %>%
  mutate( tipo = "snmlcf")


cte <- transito %>%
  dplyr::select( cedula,
                 fecha_nacimiento,
                 sexo,
                 nombre,
                 cargo,
                 sueldo,
                 cargo_coescop ) %>%
  mutate( ciudad = NA ) %>%
  mutate( tipo = "cte")


#Factor de correción--------------------------------------------------------------------------------

factor <- 10561 / 6606

#Consolidación en una sola tabla--------------------------------------------------------------------

coescop <- rbind( ad,
                  bo,
                  me,
                  snai,
                  sn,
                  cte) %>%
  filter(cargo_coescop!="Administrativo" ) %>%
  left_join( ., imposiciones, by = 'cedula') %>%
  filter( !is.na(cedula) & fecha_nacimiento > as.Date('1950-01-01') & !is.na( numimp ) & fecha_nacimiento < as.Date('2004-01-01') ) %>%
  mutate( edad = round(age_calc(fecha_nacimiento,
                                enddate = as.Date("31/03/2022","%d/%m/%Y"),
                                units = "years",
                                precise = TRUE ) ) ) %>%
  filter( numimp > 5 )


#Guardar en Rdata-----------------------------------------------------------------------------------
save( coescop,
      factor,
      file = paste0( parametros$RData, 'IESS_consolidado_coescop.RData' ) )

# #-------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
