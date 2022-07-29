message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo población inicial de pensionistas RT del IESS' )

#Cargar prestaciones mensuales de RTR---------------------------------------------------------------
load(paste0( parametros$RData_seg, 'IESS_RTR_rentas.RData' ))

#Calculo de la edad de los beneficiarios al 31/12/2018----------------------------------------------
base <- as.data.table(base) %>%
        lazy_dt() %>%
        filter(tipo_seguro=='RT',
        anio=='2018',
        mes=='12') %>%
        mutate(edad=round(age_calc(fecha_nacimiento,
                                   enddate = fecha_siniestro,
                                   units = "years",
                                   precise = TRUE))) %>%
        as.data.table()


#Edades de la poblacion inicial---------------------------------------------------------------------
x<-rbind(data.table(x=c(0:105),sexo='M'),data.frame(x=c(0:105),sexo='F'))


#Beneficiarios de incapacidad PA y PT de RTR, a diciembre de 2018, por edad-------------------------

ben_pa_pt<-base %>%
           lazy_dt() %>%
           filter(tipo_seguro=='RT',
                 tipo_prestacion %in% c('PA','PT'),
                 anio=='2018',
                 mes=='12') %>%
           group_by(edad,sexo) %>%
           mutate(lx=n()) %>%
           distinct(edad,sexo, .keep_all = TRUE) %>%
           select(x:=edad,sexo,lx) %>%
           arrange(sexo,x) %>%
           full_join(.,x,by=c('x','sexo')) %>%
           mutate(estado='PA_PT') %>% as.data.table()

#Beneficiarios de incapacidad PP RTR, a diciembre de 2018, por edad--------------------------------
ben_pp<-base %>%
        lazy_dt() %>%
        filter(tipo_seguro=='RT',
               tipo_prestacion %in% c('PP'),
               anio=='2018',
               mes=='12') %>%
        group_by(edad,sexo) %>%
        mutate(lx=n()) %>%
        distinct(edad,sexo, .keep_all = TRUE) %>%
        select(x:=edad,sexo,lx) %>%
        arrange(x) %>%
        full_join(.,x,by=c('x','sexo')) %>%
        mutate(estado='PP') %>% as.data.table()

#Beneficiarios de orfandad RTR, a diciembre de 2018, por edad---------------------------------------
aux <-  base %>%
        lazy_dt() %>%
        filter(beneficiario=='ORFANDAD',
                anio=='2018',
                mes=='12') %>%
        distinct(cedula, .keep_all = TRUE) %>%
        as.data.table()

Total_mo <- NROW(aux)
        
ben_mo<-aux %>%
        lazy_dt() %>%
        filter(edad<18) %>% #se filtra a los custodios
        mutate(n=n()) %>%
        group_by(edad,sexo) %>%
        mutate(lx=n()*Total_mo/n) %>%  #se corrige el número de huerfanos, por los custodios
        ungroup() %>%
        distinct(edad,sexo, .keep_all = TRUE) %>%
        select(x:=edad,sexo,lx) %>%
        arrange(x) %>%
        full_join(.,x,by=c('x','sexo')) %>%
        mutate(estado='MO',
               lx=ifelse(is.na(lx),0,lx)) %>%
        as.data.table()
#Beneficiarios de viudedad RTR, a diciembre de 2018, por edad---------------------------------------
ben_mv<-base %>%
        lazy_dt() %>%
        filter(beneficiario=='VIUDEDAD',
               anio=='2018',
               mes=='12') %>%
        group_by(edad,sexo) %>%
        mutate(lx=n()) %>%
        ungroup() %>%
        distinct(edad,sexo, .keep_all = TRUE) %>%
        select(x:=edad,sexo,lx) %>%
        arrange(x) %>%
        full_join(.,x,by=c('x','sexo')) %>%
        mutate(estado='MV',
               lx=ifelse(is.na(lx),0,lx)) %>%
        as.data.table()

#Consolidar base en un Rdata------------------------------------------------------------------------
pob_ini_rtr<-rbind(ben_pa_pt,ben_pp,ben_mo,ben_mv) %>%
             lazy_dt() %>%
             mutate(lx=ifelse(is.na(lx),0,lx)) %>%
             select(sexo,x,lx,estado) %>%
             as.data.table()

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando población inicial' )
save( pob_ini_rtr, file = paste0( parametros$RData_seg, 'IESS_RTR_poblacion_ini.RData' ) )

#Limpiando la memoria ram---------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

