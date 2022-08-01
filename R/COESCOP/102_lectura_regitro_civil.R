message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de registro civil' )

#Cargando de archivo txt--------------------------------------------------------
file<-paste0(parametros$Data, 'KSPCOTREGCIV.txt' )

dat1 <- fread(file,
              fill=TRUE,
              sep=";",
              nrows = 9858082,
              select = c(1:7)
) %>% 
  as_tibble() %>% clean_names()



dat2 <- fread(file,
             fill=TRUE,
             sep=";",
             skip=9858083,
             select = c(1:7)
             ) %>% as_tibble() %>%
  clean_names()

colnames(dat2) <- colnames(dat1)

rc <- rbind( dat1,
             dat2 ) %>%
  mutate(fecnacper = as.Date(fecnacper,"%d/%m/%Y")) %>%
  mutate(fecdefper = as.Date(fecdefper,"%d/%m/%Y")) %>%
  mutate( genper = ifelse( genper == '2', 'F', 'M' )) %>%
  dplyr::select( cedula:= cedideusu,
                 nombre:= nomper,
                 sexo := genper,
                 fecha_nacimiento:= fecnacper,
                 nacionalidad := nacper,
                 estado_civil := estcivper,
                 fecha_defuncion := fecdefper )

rc$nombre <- str_replace(rc$nombre, c("Ã‘"), "Ñ")
rc$nombre <- str_replace(rc$nombre, c("Ã‘"), "Ñ")
rc$nombre <- str_replace(rc$nombre, c("Ã‘"), "Ñ")
rc$nombre <- str_replace(rc$nombre, c("Ã‘"), "Ñ")
      
# Guardando en un Rdata------------------------------------------------------------------------------
message("\tGuardando Rdatas")

save(rc,
     file = paste0(parametros$RData, "IESS_Reg_Civil.RData")
)

# Borrando data.frames-------------------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% "parametros")])
gc()
