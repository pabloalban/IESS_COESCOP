message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCreación de las tablas del análisis demográfico' )

#Carga de función tildes a latex--------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Carga de datos-----------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_tablas_contingencia.RData"))

#Tabla SNAI rangos de edad y sexo------------------------------------------------------
message( '\tTabla de servidores publicos del SNAI por rango de edad y sexo, en marzo 2022' )

cortes_edad<-c(17,seq(20,70,10) )
etiquetas_edad<-c(paste0( "\\left[", formatC( c(18,seq(20,60,10)), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-",formatC( c(seq(20,70,10)), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"\\right]"))
aux  <- tabla_snai_edad_sexo %>%
        mutate(rango_edad=cut(edad, breaks = cortes_edad,
                               labels = etiquetas_edad,
                               #include.lowest = TRUE,
                               right = TRUE)) %>%
        group_by(sexo,rango_edad) %>%
        mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(sexo,rango_edad,.keep_all = TRUE) %>%
        mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
        mutate(dist=beneficiarios/N) %>%
        select(sexo,beneficiarios,rango_edad,dist) %>%
        arrange(rango_edad,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
        select(rango_edad,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
        select(rango_edad,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_edad') %>%
        select(rango_edad,M_ben,M_dist,F_ben,F_dist) %>%
        mutate(M_dist=100*M_dist,
               F_dist=100*F_dist,
               rango_edad=as.character(rango_edad))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_snai_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Tabla SNAI de sexo y cargos-------------------------------------------------------------------------
message( '\tTabla de servidores publicos del SNAI por cargo y sexo, en marzo 2022' )

aux  <- tabla_snai_cargo %>%
  group_by(cargo_coescop, sexo) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(cargo_coescop, sexo ,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,cargo_coescop,dist) %>%
  arrange(cargo_coescop,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(cargo_coescop,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(cargo_coescop,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='cargo_coescop') %>%
  select(cargo_coescop,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         cargo_coescop=as.character(cargo_coescop))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_snai_cargo_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


#Tabla SNMLCF rangos de edad y sexo------------------------------------------------------
message( '\tTabla de servidores publicos del SNMLCF por rango de edad y sexo, en marzo 2022' )

aux  <- tabla_snmlcf_edad_sexo %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                        labels = etiquetas_edad,
                        #include.lowest = TRUE,
                        right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sexo,rango_edad,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,rango_edad,dist) %>%
  arrange(rango_edad,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(rango_edad,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(rango_edad,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_edad') %>%
  select(rango_edad,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         rango_edad=as.character(rango_edad))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_snmlcf_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Tabla SNMLCF de sexo y cargos-------------------------------------------------------------------------
message( '\tTabla de servidores publicos del SNMLCF por cargo y sexo, en marzo 2022' )

aux  <- tabla_snmlcf_cargo %>%
  group_by(cargo_coescop, sexo) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(cargo_coescop, sexo ,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,cargo_coescop,dist) %>%
  arrange(cargo_coescop,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(cargo_coescop,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(cargo_coescop,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='cargo_coescop') %>%
  select(cargo_coescop,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         cargo_coescop=as.character(cargo_coescop))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_snmlcf_cargo_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Tabla METROPOLITANOS rangos de edad y sexo------------------------------------------------------
message( '\tTabla de servidores publicos del Cuerpo de Agentes de Control Municipal o Metropolitano por rango de edad y sexo, en marzo 2022' )

aux  <- tabla_metropolitanos_edad_sexo %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                        labels = etiquetas_edad,
                        #include.lowest = TRUE,
                        right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sexo,rango_edad,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,rango_edad,dist) %>%
  arrange(rango_edad,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(rango_edad,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(rango_edad,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_edad') %>%
  select(rango_edad,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         rango_edad=as.character(rango_edad))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_metropolitanos_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Tabla METROPOLITANOS de sexo y cargos----------------------------------------------------------------
message( '\tTabla de servidores publicos del Cuerpo de Agentes de Control Municipal o Metropolitano por cargo y sexo, en marzo 2022' )

aux  <- tabla_metropolitanos_cargo %>%
  group_by(cargo_coescop, sexo) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(cargo_coescop, sexo ,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,cargo_coescop,dist) %>%
  arrange(cargo_coescop,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(cargo_coescop,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(cargo_coescop,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='cargo_coescop') %>%
  select(cargo_coescop,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         cargo_coescop=as.character(cargo_coescop))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_metropolitanos_cargo_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


#Tabla CTE rangos de edad y sexo------------------------------------------------------
message( '\tTabla de servidores publicos del CTE por rango de edad y sexo, en marzo 2022' )

aux  <- tabla_cte_edad_sexo %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                        labels = etiquetas_edad,
                        #include.lowest = TRUE,
                        right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sexo,rango_edad,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,rango_edad,dist) %>%
  arrange(rango_edad,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(rango_edad,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(rango_edad,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_edad') %>%
  select(rango_edad,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         rango_edad=as.character(rango_edad))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_cte_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Tabla CTE de sexo y cargos----------------------------------------------------------------
message( '\tTabla de servidores publicos del CTE por cargo y sexo, en marzo 2022' )

aux  <- tabla_cte_cargo %>%
  group_by(cargo_coescop, sexo) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(cargo_coescop, sexo ,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,cargo_coescop,dist) %>%
  arrange(cargo_coescop,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(cargo_coescop,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(cargo_coescop,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='cargo_coescop') %>%
  select(cargo_coescop,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         cargo_coescop=as.character(cargo_coescop))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )

aux_xtable <- tildes_a_latex(aux_xtable)

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_cte_cargo_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


#Tabla BOMBEROS rangos de edad y sexo------------------------------------------------------
message( '\tTabla de servidores publicos del Cuerpo de Bomberos por rango de edad y sexo, en marzo 2022' )

aux  <- tabla_bomberos_edad_sexo %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                        labels = etiquetas_edad,
                        #include.lowest = TRUE,
                        right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sexo,rango_edad,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,rango_edad,dist) %>%
  arrange(rango_edad,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(rango_edad,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(rango_edad,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_edad') %>%
  select(rango_edad,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         rango_edad=as.character(rango_edad))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_bomberos_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Tabla BOMBEROS de sexo y cargos----------------------------------------------------------------
message( '\tTabla de servidores publicos del Cuerpo de Bomberos por cargo y sexo, en marzo 2022' )

aux  <- tabla_bomberos_cargo %>%
  group_by(cargo_coescop, sexo) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(cargo_coescop, sexo ,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,cargo_coescop,dist) %>%
  arrange(cargo_coescop,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(cargo_coescop,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(cargo_coescop,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='cargo_coescop') %>%
  select(cargo_coescop,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         cargo_coescop=as.character(cargo_coescop))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )

aux_xtable <- tildes_a_latex(aux_xtable)

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_bomberos_cargo_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Tabla ADUANEROS rangos de edad y sexo------------------------------------------------------
message( '\tTabla de servidores publicos del Cuerpo de Vigilancia Aduanera por rango de edad y sexo, en marzo 2022' )

aux  <- tabla_aduaneros_edad_sexo %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                        labels = etiquetas_edad,
                        #include.lowest = TRUE,
                        right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sexo,rango_edad,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,rango_edad,dist) %>%
  arrange(rango_edad,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(rango_edad,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(rango_edad,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='rango_edad') %>%
  select(rango_edad,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         rango_edad=as.character(rango_edad))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_aduaneros_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Tabla ADUANEROS de sexo y cargos----------------------------------------------------------------
message( '\tTabla de servidores publicos del Cuerpo de Vigilancia Aduanera por cargo y sexo, en marzo 2022' )

aux  <- tabla_aduaneros_cargo %>%
  group_by(cargo_coescop, sexo) %>%
  mutate(beneficiarios= sum(frecuencia, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(cargo_coescop, sexo ,.keep_all = TRUE) %>%
  mutate(N= sum(beneficiarios, na.rm = TRUE )) %>%
  mutate(dist=beneficiarios/N) %>%
  select(sexo,beneficiarios,cargo_coescop,dist) %>%
  arrange(cargo_coescop,sexo)


auxa <- spread(select(aux,-dist),sexo,value = c(beneficiarios)) %>%
  select(cargo_coescop,M_ben:=M,F_ben:=F)
auxb <- spread(select(aux,-beneficiarios),sexo,value = c(dist)) %>%
  select(cargo_coescop,M_dist:=M,F_dist:=F)

aux<-left_join(auxa,auxb,by='cargo_coescop') %>%
  select(cargo_coescop,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         cargo_coescop=as.character(cargo_coescop))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_aduaneros_cargo_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))


#Limpiando memoria RAM------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
