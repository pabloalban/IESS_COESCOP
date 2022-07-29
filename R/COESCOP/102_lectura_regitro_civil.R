message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de registro civil' )

#Cargando de archivo txt--------------------------------------------------------
file<-paste0(parametros$Data, 'KSPCOTREGCIV.txt' )

rc <- read.table(file,
                  sep = ";",
                  dec = ".",
                  na = "",
                  header = TRUE,
                  skip = 0,
                 fill = TRUE,
                 #nrows = 1000,
                 colClasses = c('character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character',
                                
                                'character')
) %>%
  clean_names()




      
# Guardando en un Rdata------------------------------------------------------------------------------
message("\tGuardando Rdatas")

save(rc,
     file = paste0(parametros$RData, "IESS_Reg_Civil.RData")
)

# Borrando data.frames-------------------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% "parametros")])
gc()
