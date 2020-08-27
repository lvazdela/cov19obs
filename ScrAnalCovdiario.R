#_______________________________________________________________
#Análisis de la base de datos bdcovdiario.
#Se depuró la hoja "HOJA DIARIA" del libro "Base de datos 29.xlsx" en ScrDepuCovdiario.R
#El objeto bdcovdiario se guarda en Rdata/bdcovdiario.rda
#
#_______________________________________________________________


# ANÁLISIS de pruebas de laboratorio ---------------------------

library(tidyverse)
library(lubridate)

load('Rdata/bdcovdiario.rda')
load('Rdata/bdcoving.rda')

#Código para obtener bdlabos, que es un resumen de laboratorios durante la estancia de los pacientes
names(bdcovdiario)
nomlabos <- bdcovdiario %>% select(c(14,15, 21:46, 54:57, 59:62,75:77)) %>% names
nomlabos

mymin <- function(x){
  round(ifelse(sum(is.na(x)) == length(x),NA, min(x, na.rm = TRUE)), 2)
}
mymax <- function(x){
  round(ifelse(all(is.na(x)),NA, max(x, na.rm = TRUE)), 2)
}
mydif <- function(x){
  mimin <- ifelse(length(x)> 1,last(na.omit(x)) - first(na.omit(x)), 0)
  mimin <- ifelse(all(is.na(x)), NA, mimin)
  return(round(mimin, 2))
}

mydifpor <- function(x){
  primero = first(na.omit(x))
  dif <- ifelse(length(x)> 1,last(na.omit(x)) - first(na.omit(x)), 0)
  porc <- ifelse(primero == 0, dif, dif*100/primero)
  return(round(porc, 2))
}


bdlabos <- bdcovdiario %>% group_by(nss) %>%arrange(fechaest)%>%
  summarise(numestudios = n(), across(all_of(nomlabos), list(prom = ~ round(mean(.x, na.rm = TRUE),2),
                                                             #de = ~sd(.x, na.rm = TRUE), no usaré la desvest.
                                                             
                                                             min = ~mymin(.x),
                                                             max = ~mymax(.x),
                                                             dif = ~mydif(.x),
                                                             difpor = ~mydifpor(.x))))



#este código fue para probar el comportamiento de las funciones.
gbdcovdiario <- bdcovdiario %>% group_by(nss)%>%arrange(fechaest)
gbdcovdiario %>% filter(nss == '10796901792M1965OR')%>%select(c('lugaring',all_of(c('fechaest', nomlabos))))
bdcoving %>% filter(nss == '10796901792M1965OR')%>%select(motivoegre)
rm(gbdcovdiario)

#la idea es tener columnas de promedio, min, max, dif y difpor, luego agrupar por labo y obtener resúmenes

bdporlabos <- bdlabos %>%
  pivot_longer(
    cols = -nss,
    names_to = c('analito', '.value'),
    names_pattern = '(\\w+)_(\\w+)'  #También funciona '(.*)_(.*)
  )
bdporlabos %>% filter(analito == 'dimd')

#Este código salió muy bien, pero la realidad es que la base así no es de mucha ayuda, pero es importante
#no olvidarlo. Lo obtuve del help de pivot_longer.

#Revisar regresión en analitos durante estancia, seleccionaré un paciente con 5 mediciones
gbdcovdiario <- bdcovdiario %>% group_by(nss)%>%mutate(n = n())%>% ungroup()%>%
  filter(n >= 5)
gbdcovdiario %>% filter(nss == '62674411266F1951PE')%>% #lm(bun ~fechaest, data = .)
  ggplot(aes(x = fechaest, y = temp))+
  geom_point()

#uno info de bdcoving y bdporlabos:
bdcov <- bdcoving %>% select(nss, motivoegre, sato2, sato2sin)
bdcovid <- inner_join(bdcov, bdlabos, by = 'nss')
table(bdcovid$motivoegre)
bdcovid$motivoegre[which(bdcovid$motivoegre == 1)] <- 3
levels(bdcovid$motivoegre)
bdcovid$motivoegre <- factor(bdcovid$motivoegre, levels = c(2, 3))
levels(bdcovid$motivoegre)
t.test(satcono2_min ~ motivoegre, data = bdcovid)
names(bdcovid)

library(broom)
prueba <- bdcovid %>% mutate(difox = sato2 - sato2sin) %>%
  do(tidy(t.test(difox ~ motivoegre, data = .)))

tidy(t.test(sato2 ~ motivoegre, data = bdcovid))$p.value
t.test(sato2 ~ motivoegre, data = bdcovid)
table(bdcovid$motivoegre)
