message(paste(rep("-", 100), collapse = ""))

message("\tCargando servidores COESCOP")

# Carga de bases------------------------------------------------------------------------------------
load(paste0(parametros$RData, "COESCOP_bomberos.RData")) #bomberos 3936
load(paste0(parametros$RData, "COESCOP_cte.RData")) #cte 5108
load(paste0(parametros$RData, "COESCOP_snai.RData")) #snai 2979
load(paste0(parametros$RData, "COESCOP_snmlcf.RData")) #snmlcf 189 
load(paste0(parametros$RData, "COESCOP_metropolitanos.RData")) #metropolitanos 6789
load(paste0(parametros$RData, "COESCOP_aduaneros.RData")) #aduaneros 1670

#Bomberos-------------------------------------------------------------------------------------------
bomberos["cargo_coescop"] <- NA
bomberos$cargo <- tolower(bomberos$cargo) #transformar a minúsculas
bomberos$grado <- tolower(bomberos$grado) #transformar a minúsculas
bomberos$cargo_coescop <- as.character( bomberos$cargo_coescop )

#Administrativos

bomberos[grep("abogad", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("asesor", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("administrativo", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("secretaria", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("conductor", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("contador", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("talento", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("prevencion", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("analista", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("financier", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("asistente", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("tecnico", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("juridico", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("comunicador", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("conductor", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("auxiliar", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("limpieza", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("maquinista", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("radio", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("conserje", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("tesorer", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("servicio", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("social", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("compras", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("académico ", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("guar ", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("guuar ", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("presupuesto", bomberos$grado),]$cargo_coescop <- ('Administrativo')
bomberos[grep("comunicacion", bomberos$grado),]$cargo_coescop <- ('Administrativo')
bomberos[grep("public", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("chof", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("mecanico", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("recauda", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("contratacion", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("central", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("desarrollo", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("imagen", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("mantenimiento", bomberos$cargo),]$cargo_coescop <- ('Administrativo')
bomberos[grep("personal", bomberos$cargo),]$cargo_coescop <- ('Administrativo')

#Técnicos operativos
bomberos[grep(c("jefe cuerpo"), bomberos$cargo),]$cargo_coescop <- ('Jefe de Bomberos') #inspector de brigada
bomberos[grep(c("segundo"), bomberos$cargo),]$cargo_coescop <- ('Subjefe de Bomberos') #inspector de brigada


bomberos[grep(c("bombero 1"), bomberos$cargo),]$cargo_coescop <- ('Bombero 1') #Bombero 1
bomberos[grep(c("bombero 2"), bomberos$cargo),]$cargo_coescop <- ('Bombero 2') #Bombero 2
bomberos[grep(c("bombero 3"), bomberos$cargo),]$cargo_coescop <- ('Bombero 3') #Bombero 3
bomberos[grep(c("bombero 4"), bomberos$cargo),]$cargo_coescop <- ('Bombero 4') #Bombero 4

bomberos[grep(c("bomberil 1"), bomberos$cargo),]$cargo_coescop <- ('Bombero 1') #Bombero 1
bomberos[grep(c("bomberil 2"), bomberos$cargo),]$cargo_coescop <- ('Bombero 2') #Bombero 1
bomberos[grep(c("bomberil 3"), bomberos$cargo),]$cargo_coescop <- ('Bombero 3') #Bombero 1
bomberos[grep(c("bomberil 4"), bomberos$cargo),]$cargo_coescop <- ('Bombero 4') #Bombero 1

#Directivos
bomberos[grep(c("subjefe"), bomberos$cargo),]$cargo_coescop <- ('Subjefe de Bomberos')
bomberos[grep(c("sub jefe"), bomberos$cargo),]$cargo_coescop <- ('Subjefe de Bomberos')
bomberos[grep(c("sub inspector"), bomberos$cargo),]$cargo_coescop <- ('Subinspector de Estación')

bomberos <- bomberos %>%
  mutate(cargo_coescop = ifelse( grepl("jefe",cargo) & is.na(cargo_coescop), "Jefe de Bomberos", cargo_coescop) )

bomberos <- bomberos %>%
  mutate(cargo_coescop = ifelse( grepl("inspector",cargo) & is.na(cargo_coescop), "Inspector de Brigada", cargo_coescop) )

bomberos <- bomberos %>%
  mutate(cargo_coescop = ifelse( grepl("insp",cargo) & is.na(cargo_coescop), "Inspector de Brigada", cargo_coescop) )

bomberos <- bomberos %>%
  mutate(cargo_coescop = ifelse( grepl("cabo",cargo) & is.na(cargo_coescop), "Bombero 1", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("suboficial",cargo) & is.na(cargo_coescop), "Bombero 2", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("sargento",cargo) & is.na(cargo_coescop), "Bombero 2", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("sub oficial",cargo) & is.na(cargo_coescop), "Bombero 2", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("teniente coronel",cargo) & is.na(cargo_coescop), "Inspector de Brigada", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("subteniente",cargo) & is.na(cargo_coescop), "Bombero 3", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("sub teniente",cargo) & is.na(cargo_coescop), "Bombero 3", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("teniente",cargo) & is.na(cargo_coescop), "Bombero 4", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("capitan",cargo) & is.na(cargo_coescop), "Subinspector de Estación", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("mayor",cargo) & is.na(cargo_coescop), "Subinspector de Estación", cargo_coescop) ) %>%
  mutate(cargo_coescop = ifelse( grepl("coronel",cargo) & is.na(cargo_coescop), "Inspector de Brigada", cargo_coescop) )
  
bomberos <- bomberos %>%
  mutate(cargo_coescop = ifelse( grepl("bombero",cargo) & is.na(cargo_coescop), "Bombero 1", cargo_coescop) )
  
bomberos <- bomberos %>%
  mutate( cargo_coescop = ifelse(is.na( cargo_coescop ), "Administrativo", cargo_coescop ))



table(bomberos$cargo_coescop)
#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en data.frame' )

save( bomberos,
      file = paste0( parametros$RData, 'COESCOP_bomberos_cargos.RData' ) )


#CTE------------------------------------------------------------------------------------------------

transito["cargo_coescop"] <- NA
transito$cargo <- tolower(transito$cargo) #transformar a minúsculas

transito[grep(c("agente 1"), transito$cargo),]$cargo_coescop <- ('Agente de tránsito 1') #Agente 1
transito[grep(c("agente 2"), transito$cargo),]$cargo_coescop <- ('Agente de tránsito 2') #Agente 2
transito[grep(c("agente 3"), transito$cargo),]$cargo_coescop <- ('Agente de tránsito 3') #Agente 3
transito[grep(c("agente 4"), transito$cargo),]$cargo_coescop <- ('Agente de tránsito 4') #Agente 4

transito[grep(c("sub-inspector i"), transito$cargo),]$cargo_coescop <- ('Subinspector de tránsito 1') #Subinspector de tránsito 1
transito[grep(c("sub-inspector ii"), transito$cargo),]$cargo_coescop <- ('Subinspector de tránsito 2') #Subinspector de tránsito 2

transito[grep(c("sub-prefecto"), transito$cargo),]$cargo_coescop <- ('Subprefecto') #Subprefecto
transito[grep(c("prefecto jefe"), transito$cargo),]$cargo_coescop <- ('Prefecto Jefe') #Prefecto Jefe
transito[grep(c("prefecto comandante"), transito$cargo),]$cargo_coescop <- ('Prefecto Comandante') #Prefecto comandante

transito <- transito %>%
  mutate(cargo_coescop = ifelse( grepl("prefecto",cargo) & is.na(cargo_coescop), "Prefecto", cargo_coescop) )  %>%
  mutate(cargo_coescop = ifelse( grepl("inspector",cargo) & is.na(cargo_coescop) , "Inspector", cargo_coescop) )

transito <- transito %>%
  mutate( cargo_coescop = if_else(is.na( cargo_coescop ),
                                  "Administrativo", 
                                  cargo_coescop ) )
cte <- transito

save( cte,
      file = paste0( parametros$RData, 'COESCOP_cte_cargos.RData' ) )

#SNAI-----------------------------------------------------------------------------------------------

snai["cargo_coescop"] <- NA
snai$cargo <- tolower(snai$cargo) #transformar a minúsculas

snai[grep(c("agente de seguridad penitenciaria 1"), snai$cargo),]$cargo_coescop <- ('Agente de Seguridad Penitenciaria 1') #Agente 1
snai[grep(c("agente de seguridad penitenciaria 2"), snai$cargo),]$cargo_coescop <- ('Agente de Seguridad Penitenciaria 2') #Agente 2
snai[grep(c("agente de seguridad penitenciaria 3"), snai$cargo),]$cargo_coescop <- ('Agente de Seguridad Penitenciaria 3') #Agente 3

snai[grep(c("subinspector"), snai$cargo),]$cargo_coescop <- ('Subinspector de Seguridad Penitenciaria') #subinspector
snai[grep(c("inspector de seguridad penitenciaria"), snai$cargo),]$cargo_coescop <- ('Inspector de Seguridad Penitenciaria') #inspector
snai[grep(c("subjefe"), snai$cargo),]$cargo_coescop <- ('Subjefe de Seguridad Penitenciaria') #subjefe

snai <- snai %>%
  mutate( cargo_coescop = ifelse(is.na( cargo_coescop ), "Administrativo", cargo_coescop ))

save( snai,
      file = paste0( parametros$RData, 'COESCOP_snai_cargos.RData' ) )


#SNMLCF---------------------------------------------------------------------------------------------

snmlcf["cargo_coescop"] <- NA
snmlcf$cargo <- tolower(snmlcf$cargo) #transformar a minúsculas
snmlcf$cargo_coescop <- as.character( snmlcf$cargo_coescop )

snmlcf[grep(c("perito"), snmlcf$cargo),]$cargo_coescop <- ('Analista 2') #Analista 2
snmlcf[grep(c("medico legal 1"), snmlcf$cargo),]$cargo_coescop <- ('Analista 1') #Analista 1

snmlcf[grep(c("medico legal 3"), snmlcf$cargo),]$cargo_coescop <- ('Especialista') #Especialista

snmlcf[grep(c("disector"), snmlcf$cargo),]$cargo_coescop <- ('Asistente') #asistente

snmlcf[grep(c("director general"), snmlcf$cargo),]$cargo_coescop <- ('Directivo') #directivos
snmlcf[grep(c("directora general"), snmlcf$cargo),]$cargo_coescop <- ('Directivo') #directivos


snmlcf <- snmlcf %>%
  mutate(cargo_coescop = ifelse( grepl("analista",cargo) & grepl("3-OTROS REGIMENES ESPECIALES",regimen_laboral_al_que_pertenece), "Analista 2", cargo_coescop) )

snmlcf <- snmlcf %>%
  mutate( cargo_coescop = ifelse(is.na( cargo_coescop ), "Administrativo", cargo_coescop ))

save( snmlcf,
      file = paste0( parametros$RData, 'COESCOP_snmlcf_cargos.RData' ) )

#-Metropolitanos------------------------------------------------------------------------------------

metropolitanos["cargo_coescop"] <- NA
metropolitanos$cargo <- tolower(metropolitanos$cargo) #transformar a minúsculas
metropolitanos$cargo_coescop <- as.character(metropolitanos$cargo_coescop)


metropolitanos[grep(c("agente de control metropolitano 1"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Metropolitano 1') #Agente de Control Metropolitano 1
metropolitanos[grep(c("agente de control metropolitano 2"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Metropolitano 2') #Agente de Control Metropolitano 2
metropolitanos[grep(c("agente de control metropolitano 3"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Metropolitano 3') #Agente de Control Metropolitano 3
metropolitanos[grep(c("agente de control metropolitano 4"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Metropolitano 4') #Agente de Control Metropolitano 4
metropolitanos[grep(c("acmi"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Metropolitano 1') #Agente de Control Metropolitano 1
metropolitanos[grep(c("acm ii"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Metropolitano 2') #Agente de Control Metropolitano 2
metropolitanos[grep(c("policia metropolitano"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Metropolitano 1') #Agente de Control Metropolitano 1

metropolitanos[grep(c("agente de control municipal 1"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Municipal 1') #Agente de Control Municipal 1
metropolitanos[grep(c("policia municipal"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Municipal 1') #Agente de Control Municipal 1
metropolitanos[grep(c("agente de control y vigilancia municipal"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Municipal 1') #Agente de Control Municipal 1

metropolitanos[grep(c("subinspector 1"), metropolitanos$cargo),]$cargo_coescop <- ('Subinspector de Control Metropolitano') #Subinspector de Control Metropolitano
metropolitanos[grep(c("inspector 1"), metropolitanos$cargo),]$cargo_coescop <- ('Inspector de Control Metropolitano') #inspector de Control Metropolitano
metropolitanos[grep(c("inspector 2"), metropolitanos$cargo),]$cargo_coescop <- ('Subjefe de Control Metropolitano') #Subjefe de Control Metropolitano
metropolitanos[grep(c("inspector 3"), metropolitanos$cargo),]$cargo_coescop <- ('Jefe de Control Metropolitano') #jefe de Control Metropolitano

metropolitanos[grep(c("agente transito 1"), metropolitanos$cargo),]$cargo_coescop <- ('Agente de Control Metropolitano 1') #Agente de Control Metropolitano 1

metropolitanos <- metropolitanos %>%
  mutate(cargo_coescop = ifelse( grepl("agente de control municipal",cargo) & is.na( cargo_coescop ), "Agente de Control Municipal 1", cargo_coescop) )

metropolitanos <- metropolitanos %>%
  mutate(cargo_coescop = ifelse( grepl("agente de control  metropolitano",cargo) & is.na( cargo_coescop ), "Agente de Control Metropolitano 1", cargo_coescop) ) 


metropolitanos <- metropolitanos %>%
  mutate( cargo_coescop = ifelse(is.na( cargo_coescop ), "Administrativo", cargo_coescop ))

save( metropolitanos,
      file = paste0( parametros$RData, 'COESCOP_metropolitanos_cargos.RData' ) )

#Adueneros------------------------------------------------------------------------------------------

aduaneros["cargo_coescop"] <- NA
aduaneros$cargo <- tolower(aduaneros$cargo) #transformar a minúsculas
#aduaneros$grado <- tolower(aduaneros$grado) #transformar a minúsculas

#aduaneros[grep(c("abogado"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("analista"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("asesor"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("asistente"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("administrativo"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("chofer"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("conserje"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("oficinista"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("secretaria"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("servidor publico"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo
#aduaneros[grep(c("tesorero"), aduaneros$cargo),]$cargo_coescop <- ('Administrativo') #Administrativo

aduaneros[grep(c("inspector de vigilancia 1"), aduaneros$cargo),]$cargo_coescop <- ('Inspector de Vigilancia 1') #Inspector de Vigilancia 1
aduaneros[grep(c("inspector de vigilancia 2"), aduaneros$cargo),]$cargo_coescop <- ('Inspector de Vigilancia 2') #Inspector de Vigilancia 1
aduaneros[grep(c("inspector de vigilancia 3"), aduaneros$cargo),]$cargo_coescop <- ('Inspector de Vigilancia 3') #Inspector de Vigilancia 1

aduaneros[grep(c("vigilante aduanero 1"), aduaneros$cargo),]$cargo_coescop <- ('Vigilante Aduanero 1') #Vigilante Aduanero 1
aduaneros[grep(c("vigilante aduanero 2"), aduaneros$cargo),]$cargo_coescop <- ('Vigilante Aduanero 2') #Vigilante Aduanero 2

aduaneros <- aduaneros %>%
  mutate( cargo_coescop = ifelse(is.na( cargo_coescop ), "Administrativo", cargo_coescop ))

save( aduaneros,
      file = paste0( parametros$RData, 'COESCOP_aduaneros_cargos.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()

