message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de imposicions al 31/12/2022' )

#Cargando de archivo txt---------------------------------------------------------------------------
file<-paste0('W:/Data/CES/Tablas Pablo A/KSPCOTAFILIADOS_2023_01.dsv' )


my_cols <- c("NUMAFI", "NUMIMPTOT", "NUMIMP")


imposiciones <- fread( file, 
                       select = my_cols,
                       #nrows = 100000,
                       header ="auto") %>% clean_names() %>%
  
  mutate( numafi = as.character( numafi ) ) 


imposiciones <- imposiciones[-c(2553177)] %>%
  mutate( n = nchar(numafi) ) %>%
  mutate( cedula = if_else( nchar(numafi)==9,
                            paste0('0', numafi ),
                            numafi ) ) %>%
  dplyr::select( cedula,
                 numimp )



# Guardando en un Rdata------------------------------------------------------------------------------
message("\tGuardando Rdatas")

save(imposiciones,
     file = paste0(parametros$RData, "IESS_imposiciones_2022_12.RData")
)

# Borrando data.frames-------------------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% "parametros")])
gc()