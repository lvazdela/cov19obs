#Depuración de la base de evolución en internamiento de los pacientes.
#21 de julio de 2020.


library(tidyverse)
library(caret)

#Me di cuenta tarde que read.csv importó nas como cadenas vacías '', pongo na.strings = ''
bdcovdiario <- read.csv('bases-originales/covdiario.csv', na.strings = '')
names(bdcovdiario)

#se importaron columnas después de las últimas, las quito
bdcovdiario <- bdcovdiario %>% select(-starts_with('X'))
names(bdcovdiario)

bdcovdiario <- bdcovdiario%>%rename(fecha = FECHA, colesterol = colesterol.)
names(bdcovdiario)

#Trabajo las fechas, que queden como date.
library(lubridate)
bdcovdiario <- bdcovdiario%>% mutate_at(vars(starts_with('fecha')), dmy)
#sale un error que no se pudieron cambiar
bdcovdiario %>% filter(!is.Date(fecha)|!is.Date(fechaest))%>%select(nombre, fecha, fechaest)
#no salen fechas malas, checo los nas
bdcovdiario %>% filter(is.na(fechaest)|is.na(fechaest))%>%select(nombre, fecha, fechaest)
#puso nas cuando no pudo cambiarlos, los checo en la base original, veo que hay errores,
#los trateré de cazar con el siguiente código:
indmalfec <- which(is.na(bdcovdiario$fecha)) 
indmalfec
indmalfec + 3 # para que cheque con el número de fila de Excel.
#corrijo a mano:
bdcovdiario$fecha[indmalfec[1]] <- as.Date('28-05-2020','%d-%m-%Y')
bdcovdiario$fecha[indmalfec[2]] <- as.Date('15-06-2020','%d-%m-%Y')
bdcovdiario$fecha[indmalfec[3]] <- as.Date('15-06-2020','%d-%m-%Y')
bdcovdiario$fecha[indmalfec[4]] <- as.Date('12-06-2020','%d-%m-%Y')
sum(is.na(bdcovdiario$fecha))
#Veo que hay fechas mayores al día de hoy 21 de julio 2020 (el código solo servirá para este día)
malfec2 <- which(bdcovdiario$fecha > today())
malfec2
malfec2 +3
bdcovdiario$fecha[malfec2[1]] <- as.Date('26-04-2020','%d-%m-%Y')
bdcovdiario$fecha[indmalfec[2]] <- as.Date('26-05-2020','%d-%m-%Y')
bdcovdiario$fecha[indmalfec[3]] <- as.Date('09-06-2020','%d-%m-%Y')
bdcovdiario$fecha[indmalfec[4]] <- as.Date('04-06-2020','%d-%m-%Y')


indmalfec2 <- which(is.na(bdcovdiario$fechaest)|bdcovdiario$fechaest > today()) 
indmalfec2
indmalfec2 + 3
bdcovdiario$fechaest[indmalfec2[1]] <- as.Date('26-04-2020','%d-%m-%Y' )
bdcovdiario$fechaest[indmalfec2[2]] <- as.Date('26-05-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[3]] <- as.Date('09-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[4]] <- as.Date('26-05-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[5]] <- as.Date('04-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[6]] <- as.Date('09-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[7]] <- as.Date('10-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[7]+1] <- as.Date('05-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[8]] <- as.Date('08-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[9]] <- as.Date('09-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[10]] <- as.Date('05-06-2020','%d-%m-%Y')

indmalfec2[10:length(indmalfec2)] + 3
bdcovdiario$fechaest[indmalfec2[11]] <- as.Date('15-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[12]] <- as.Date('02-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[13]] <- as.Date('07-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[14]] <- as.Date('02-06-2020','%d-%m-%Y')

indmalfec2[15:length(indmalfec2)] + 3
bdcovdiario$fechaest[indmalfec2[15]] <- as.Date('12-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[16]] <- as.Date('12-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[17]] <- as.Date('27-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[18]] <- as.Date('30-06-2020','%d-%m-%Y')
names(indmalfec2) <- 1:length(indmalfec2)
indmalfec2 +3
bdcovdiario$fechaest[indmalfec2[19]] <- as.Date('01-07-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[20]] # no hubo fecha en el excel
bdcovdiario$fechaest[indmalfec2[21]] <- as.Date('23-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[22]] <- as.Date('29-06-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[23]] <- as.Date('25-05-2020','%d-%m-%Y')
bdcovdiario$fechaest[indmalfec2[24]] <- as.Date('17-06-2020','%d-%m-%Y')
which(is.na(bdcovdiario$fechaest)|bdcovdiario$fechaest > today()) 
#solo quedó como NA el 685 (indmalfec2[20])

varnocamb <- nearZeroVar(bdcovdiario)
varnocamb
#todos con dx neumonía.
table(bdcovdiario[,13])

colnames(bdcovdiario)
table(bdcovdiario[,16])
which(bdcovdiario$neumonia == '100')
#Reviso el excel, se ve que copió y pegó datos. Los diferentess de uno son fc, que está junto, corresponden con
#a valores de uno de la fc.
unosfc <- which(bdcovdiario$fc == '1'|bdcovdiario$fc =='|')
unosfc
# copio los valores que estaban en neumonia a la fc
bdcovdiario$fc[unosfc] <- bdcovdiario$neumonia[unosfc]
#convierto en unos los valores de neumonía que tenían la fc:
bdcovdiario$neumonia[unosfc] <- 1
table(bdcovdiario$neumonia)
# sigue habiendo |
bdcovdiario$neumonia[which(bdcovdiario$neumonia == '|')] <- 1
table(bdcovdiario$vnoinv)
#guardo bdcovdiario sin rasurar variables
save(bdcovdiario, file = 'Rdata/bdcovdiario.rda')


#corro varnocamb otra vez:
varnocamb <- nearZeroVar(bdcovdiario)
varnocamb
nomsincamb <- bdcovdiario%>%select(varnocamb)%>%names
nomsincamb
#reviso las var que tienen varianza cercana a 0
table(bdcovdiario$neumonia)
#todos tienen dx de neumonía.
table(bdcovdiario$vnoinv)
#Solo 6 veces se usó, la dejaré porque creo que es importante, pero no la tomaré en cuenta si hago predicciones.
nomsincamb
table(bdcovdiario$ggt)
# hay un solo valor con 40, no tiene caso dejarla
table(bdcovdiario$il6)
#un solo valor con 38, no la dejaré
# a excepción de vnoinv, eliminaré a las demás:
nomsincamb <- nomsincamb[-2]
varnocamb <- varnocamb[-2]
varnocamb
nomsincamb

# BDCOVDI -----------------------------------------------------------------

#Elimino estas variables que no cambian.

bdcovdi <- bdcovdiario%>%select(-all_of(varnocamb))
names(bdcovdi)

#voy a checar los nas de todas las variables:
cuantosna <- function(x){
  
  sum(is.na(x))/length(x)
}


bdnas <- bdcovdi%>%summarise(  across(everything(),  ~sum(is.na(.x)) )) #Ahora hay que usar across en summarise_all y los demás
# la observo y está bien eliminar a los de arriba de 0.7
bdnas <- as.data.frame(t(bdnas))
nomfilas <- row.names(bdnas)
bdnas <- bdnas %>% rename(totalnas = V1)%>%mutate(porcentaje = round(totalnas/dim(bdcovdi)[1]*100,3),
                                                  len = dim(bdcovdi)[1])
row.names(bdnas) <- nomfilas
bdnas <- arrange(bdnas,desc(porcentaje))
save(bdnas, file = 'Rdata/bdnas.rda')

hartosna <- function(x){
  
  sum(is.na(x))/length(x) < 0.7
}

bdcovdi <- bdcovdi%>%select_if(hartosna)
names(bdcovdi)
str(bdcovdi)

#lo mismo que coviding, errores en variables numéricas. Usamos la misma estrategia:
patron <- '[^\\d\\.]' #cualquier cosa menos dígitos o números
nomcadenas <- bdcovdi%>%select_if(is.character)%>%names
nomcadenas
nomcadenas <- nomcadenas[c(-4:-1, -20)]
nomcadenas
fpatron <- function(x){
  str_which(x, pattern = patron)
}

indcambios <- apply(bdcovdi[, nomcadenas],2,fpatron )
indcambios
nomcadenas
#la fc ya está bien, me la salto.

bdcovdi$satcono2[indcambios$satcono2] # pusieron el porciento, lo quito
bdcovdi$satcono2[indcambios$satcono2] <- str_sub(bdcovdi$satcono2[indcambios$satcono2],1,2)
bdcovdi$satcono2[indcambios$satcono2]

bdcovdi$satsino2[indcambios$satsino2]
bdcovdi$satsino2[indcambios$satsino2] <- str_sub(bdcovdi$satsino2[indcambios$satsino2], 1,2)
bdcovdi$satsino2[indcambios$satsino2]

bdcovdi$urea[indcambios$urea] #dice no en expediente, le pongo NA
bdcovdi$urea[indcambios$urea] <- NA
bdcovdi$urea[indcambios$urea]

bdcovdi$creat[indcambios$creat] #usaron comas en ves de puntos.
bdcovdi$creat[indcambios$creat] <- str_replace_all(bdcovdi$creat[indcambios$creat], ',','.')
bdcovdi$creat[indcambios$creat]

bdcovdi$hb[indcambios$hb] # más difícil, tiene comas pero en uno hay coma con punto.
bdcovdi$hb[indcambios$hb] <- str_replace_all(bdcovdi$hb[indcambios$hb], ',|,\\.','.') #no funcionó, el ,. se convirtio en ..
bdcovdi$hb[indcambios$hb] <- str_replace(bdcovdi$hb[indcambios$hb], '\\.{2}','.')
bdcovdi$hb[indcambios$hb]

nomcadenas
bdcovdi$plaq[indcambios$plaq] # hay espacios en blanco entre los números
bdcovdi$plaq[indcambios$plaq] <- str_replace(bdcovdi$plaq[indcambios$plaq], ' ', '')
bdcovdi$plaq[indcambios$plaq]

bdcovdi$leucos[indcambios$leucos] #lo mismo
bdcovdi$leucos[indcambios$leucos] <- str_replace(bdcovdi$leucos[indcambios$leucos], ' ', '')
bdcovdi$leucos[indcambios$leucos]

nomcadenas
bdcovdi$basos[indcambios$basos] #puso o en vez de 0
bdcovdi$basos[indcambios$basos] <- 220
bdcovdi$basos[indcambios$basos]

bdcovdi$neutros[indcambios$neutros]
bdcovdi$neutros[indcambios$neutros] <- 792
bdcovdi$neutros[indcambios$neutros]

bdcovdi$tgo[indcambios$tgo]
bdcovdi$tgo[indcambios$tgo] <- 41
bdcovdi$tgo[indcambios$tgo]

nomcadenas
bdcovdi$dimd[indcambios$dimd]
bdcovdi$dimd[indcambios$dimd] <- NA
bdcovdi$dimd[indcambios$dimd]

bdcovdi$ph[indcambios$ph] #comas
bdcovdi$ph[indcambios$ph] <- str_replace(bdcovdi$ph[indcambios$ph], ',', '.')
bdcovdi$ph[indcambios$ph]

bdcovdi$hco3[indcambios$hco3]
bdcovdi$hco3[indcambios$hco3] <- c(NA,'20')
bdcovdi$hco3[indcambios$hco3]

nomcadenas
bdcovdi$colesterol[indcambios$colesterol]
bdcovdi$colesterol[indcambios$colesterol] <- 1 
bdcovdi$colesterol[indcambios$colesterol]

bdcovdi$tp[indcambios$tp] #escribió "INDOSIFICABLE", checo y le pongo 30
bdcovdi$tp[indcambios$tp] <- 30
bdcovdi$tp[indcambios$tp]

#Checo si solo hay números otra vez en las variables numéricas
camb <- apply(bdcovdi[, nomcadenas],2,fpatron )
camb
#sigue no habiendo números en plaq, leucos y paco2
bdcovdi$plaq[camb$plaq]       #trailing spaces
bdcovdi$leucos[camb$leucos]   #id
bdcovdi$paco2[camb$paco2]     #humm, no lo arreglé, le pongo na
bdcovdi$paco2[camb$paco2] <- NA
bdcovdi$paco2[camb$paco2] 

#quito los espacios
bdcovdi <- bdcovdi %>% mutate_at(vars(all_of(nomcadenas)), str_trim)
# checo otra Vez
camb2 <- apply(bdcovdi[, nomcadenas],2,fpatron )
camb2

#ya no salen cadena que no son números o puntos.
bdcovdi2 <- bdcovdi %>% mutate_at(vars(all_of(nomcadenas)), as.numeric) #siguen errores.

#modifico patrón
patron2 <- '[^\\d\\.]|\\.{2,}' 

fpatron2 <- function(x){
  str_which(x, pattern = patron2)
}
prueba <- append(prueba, c('1.2', '1,3', '2..2', '2.2.3'))
prueba
str_view_all(prueba, patron2)

nopuntos <- apply(bdcovdi[, nomcadenas],2,fpatron2 )
nopuntos
#pH con  puntos juntos, veo si hay separados:

fpatron3 <- function(x){
 cuales <- ifelse(str_count(x, '\\.') > 1, TRUE, FALSE) #necesito escapar el punto, si no no funciona, lo que resulta es la cantidad de caracteres en la cadena
 which(cuales)
}
puntossep <- apply(bdcovdi[, nomcadenas],2,fpatron3 )
puntossep

#hay puntos separados, los checo:
bdcovdi$creat[puntossep$creat] # puso '.2.77'
bdcovdi$creat[puntossep$creat] <- 2.77
bdcovdi$creat[puntossep$creat]

bdcovdi$ph[puntossep$ph]
bdcovdi$ph[puntossep$ph] <- 7.46

#veo si ya no hay errores
bdcovdi2 <- bdcovdi %>% mutate_at(vars(all_of(nomcadenas)), as.numeric)

#finalmente ya no reporta errores.
bdcovdi <- bdcovdi2
#borro covdi2
remove(bdcovdi2)

#checo la estrutura
str(bdcovdi)
#veo que los que quiero como factores, son integer, a excepcion de pas y pad, los convierto a numéricos
bdcovdi <- bdcovdi %>% mutate_at(vars(c('pas','pad')), as.numeric)
str(bdcovdi)

#los integer los cambio a factores
bdcovdi <- bdcovdi %>% mutate_if(is.integer, as.factor)
str(bdcovdi)

#cambio txhashosp a factor (sale como caracter)
bdcovdi$txhashosp <- as.factor(bdcovdi$txhashosp)
str(bdcovdi)
save(bdcovdi, file = 'Rdata/bdcovdi.rda')
