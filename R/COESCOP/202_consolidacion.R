message(paste(rep("-", 100), collapse = ""))

message("\tCargando servidores COESCOP")

# Carga de bases------------------------------------------------------------------------------------
load(paste0(parametros$RData, "COESCOP_bomberos_cargos.RData")) #bomberos
load(paste0(parametros$RData, "COESCOP_cte_cargos.RData")) #cte
load(paste0(parametros$RData, "COESCOP_snai_cargos.RData")) #snai
load(paste0(parametros$RData, "COESCOP_snmlcf_cargos.RData")) #snmlcf
load(paste0(parametros$RData, "COESCOP_metropolitanos_cargos.RData")) #metropolitanos
load(paste0(parametros$RData, "COESCOP_aduaneros_cargos.RData")) #aduaneros



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
                 ciudad ) %>%
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


#Consolidación en una sola tabla--------------------------------------------------------------------

coescop <- rbind( ad,
                  bo,
                  me,
                  snai,
                  sn,
                  cte) %>%
  filter(cargo_coescop!="Administrativo" )


#Guardar en Rdata-----------------------------------------------------------------------------------
save( reporte_resp_patronal,
      file = paste0( parametros$RData_seg, 'IESS_RTR_tablas_rp.RData' ) )

# #-------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
