message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo población inicial de pensionistas RT del IESS' )

#Cargar prestaciones mensuales de RTR---------------------------------------------------------------
load(paste0( parametros$RData_seg, 'IESS_RTR_poblacion_ini.RData' ))

#Ajuste de pensionistas iniciales de orfandad-------------------------------------------------------
MO<-as_tibble(pob_ini_rtr) %>%
    filter(estado=='MO')
#Hombres--------------------------------------------------------------------------------------------
MO_m<-MO %>% filter(sexo=='M') %>% filter(x<18)
hombres<-sum(MO_m$lx)

aux <- MO_m

mod<-smooth.spline(aux$x,
                   aux$lx,df=3) 

pred<-data.frame(x=c(0:17),
                 lx=predict(mod,c(0:17), deriv = 0)[["y"]])
MO_m <- pred %>% 
        mutate(sexo:='M',
               lx=ifelse(lx<0,0,lx)) %>%
        mutate(lx=lx*hombres/sum(lx,na.rm = TRUE)) %>%
        rbind(.,data.frame(x=c(18:105),lx=0,sexo='M'))
MO_m[which(MO_m$x=='1'),]$lx <-  MO_m[which(MO_m$x=='1'),]$lx + 5     
#Gráfica del ajuste --------------------------------------------------------------------------------
plot(aux$x,aux$lx)
lines(pred$x,pred$lx)

#Mujeres--------------------------------------------------------------------------------------------
MO_f<-MO %>% filter(sexo=='F') %>% filter(x<18)
mujeres<-sum(MO_f$lx)

aux <- MO_f

mod<-smooth.spline(aux$x,
                   aux$lx,df=3) 

pred<-data.frame(x=c(0:17),
                 lx=predict(mod,c(0:17), deriv = 0)[["y"]])
MO_f <- pred %>% 
        mutate(sexo:='F',
               lx=ifelse(lx<0,0,lx)) %>%
        mutate(lx=lx*mujeres/sum(lx,na.rm = TRUE)) %>%
        rbind(.,data.frame(x=c(18:105),lx=0,sexo='F'))
MO_f[which(MO_f$x=='1'),]$lx <-  MO_f[which(MO_f$x=='1'),]$lx + 7    
#Gráfica del ajuste --------------------------------------------------------------------------------
plot(aux$x,aux$lx)
lines(pred$x,pred$lx)

#Ajuste de pensionistas iniciales de viudedad-------------------------------------------------------
MV<-as_tibble(pob_ini_rtr) %>%
    filter(estado=='MV')
#Hombres--------------------------------------------------------------------------------------------
MV_m<-MV %>% filter(sexo=='M')
hombres<-sum(MV_m$lx)

aux <- MV_m

mod<-smooth.spline(aux$x,
                   aux$lx,df=6) 

pred<-data.frame(x=c(0:105),
                 lx=predict(mod,c(0:105), deriv = 0)[["y"]])
MV_m <- pred %>% 
        mutate(sexo:='M',
               lx=ifelse(lx<0,0,lx)) %>%
        mutate(lx=lx*hombres/sum(lx,na.rm = TRUE))

#Gráfica del ajuste --------------------------------------------------------------------------------
plot(aux$x,aux$lx)
lines(pred$x,pred$lx)

#Mujeres--------------------------------------------------------------------------------------------
MV_f<-MV %>% filter(sexo=='F')
mujeres<-sum(MV_f$lx)

aux <- MV_f

mod<-smooth.spline(aux$x,
                   aux$lx,df=6) 

pred<-data.frame(x=c(0:105),
                 lx=predict(mod,c(0:105), deriv = 0)[["y"]])
MV_f <- pred %>% 
        mutate(sexo:='F',
               lx=ifelse(lx<0,0,lx)) %>%
        mutate(lx=lx*mujeres/sum(lx,na.rm = TRUE)) 

#Gráfica del ajuste --------------------------------------------------------------------------------
plot(aux$x,aux$lx)
lines(pred$x,pred$lx)


#Consolidar base en un Rdata------------------------------------------------------------------------
pob_ini_rtr <-  as_tibble(pob_ini_rtr) %>%
                filter(!(estado %in% c('MO','MV')))

aux1<-rbind(MO_f,MO_m) %>% mutate(estado:='MO')
aux2<-rbind(MV_f,MV_m) %>% mutate(estado:='MV')
pob_ini_rtr <- rbind(pob_ini_rtr,aux1,aux2)%>% as.data.table()
            
#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando población inicial' )
save( pob_ini_rtr, file = paste0( parametros$RData_seg, 'IESS_RTR_poblacion_inicial.RData' ) )

#Limpiando la memoria ram---------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
