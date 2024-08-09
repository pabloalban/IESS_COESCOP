message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura tabla de cotizaciones a enero de 2019' )

#Cargando de archivo txt---------------------------------------------------------------------------
file<-paste0(parametros$Data, 'KSPCOTAFILIADOS.txt' )

cotizaciones <- fread(file,
              fill=TRUE,
              sep="\t"
) %>% 
  as_tibble() %>% clean_names()


cotizaciones <- cotizaciones %>% 
  lazy_dt() %>%
  mutate( feciniafi = as.Date( feciniafi, "%Y-%m-%d" ),
          fecnacafi = as.Date( fecnacafi, "%Y-%m-%d" ),
          genafi = ifelse( genafi=='2', 'F', 'M' ) ) %>%
  dplyr::select( cedula:= numafi,
                 feciniafi,
                 fecnacafi,
                 genafi,
                 numimp ) %>%
  as_tibble()
  
  
  

# Guardando en un Rdata------------------------------------------------------------------------------
message("\tGuardando Rdatas")

save(cotizaciones,
     file = paste0(parametros$RData, "IESS_cotizaciones.RData")
)

# Borrando data.frames-------------------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% "parametros")])
gc()
