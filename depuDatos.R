
# MANEJO BASE DE DATOS "Base de datos 9.xlsx" -----------------------------

# Hoja con datos de ingreso de pacientes con dx de covid al umae IMSS San José.
#29 de junio 2020
#La base "Base de datos 9.xlsx", se pasó a "covid19_R.xlsx". La primara hoja (ficha de ingreso)
#se pasó a coviding.csv.
#Tenía muchos errores, por lo que se entregó a Álvaro. Entregó otra base que le puso:
# "Base de datos 18_R.xlsx".

#Transoporto la tabla de Excel
library(tidyverse)
library(lubridate)

bdcoving <- read.csv('coviding.csv')
bdcoving <- bdcoving%>%slice(1:161)
bdcoving <- bdcoving%>%mutate_at(vars(starts_with('fecha')), dmy)

#Hay errores en algunas fechas:
colfechasn <- select(bdcoving, starts_with('fecha'))%>%names
colfechasn

which(is.na(bdcoving$fecha))
bdcoving$fecha[114] <- dmy('26/05/2020')
bdcoving$fecha[114]

which(is.na(bdcoving$fechacov1))
#no errores, solo que no están

which(is.na(bdcoving$fechacov2))
#hay muchos vacíos, no se si haya errores en los pocos que pusieron

which(is.na(bdcoving$fechahosp))
bdcoving$fechahosp[51] <- dmy('25/05/2020')

#Hay fechas posteriores junio:
colfechas <- select(bdcoving, starts_with('fecha'))
indmalasfechas <- apply(colfechas, 2, function(x){
  which(month(x) > 6)
})
dim(colfechas)
indmalasfechas$fecha

#se pasan a Alvaro para que las revise.
#motivo de egreso:
table(bdcoving$motivoing)
which(bdcoving$motivoing == 5)
which(bdcoving$motivoing > 3)
mean(bdcoving$motivoing == 3, na.rm = TRUE)
typeof(bdcoving$motivoing)
which(is.na(bdcoving$motivoing))


# MANEJO BASE DE DATOS "Base de datos 18_R.xlsx" --------------------------

#14/07/23

# library(readxl)
# bdcoving <- read_excel("Base de datos 18_R.xlsx", 
#                        sheet = "FICHA DE INGRESO")
# View(bdcoving)

library(tidyverse)
library(lubridate)

# El manejo de las fechas, se hará con lubridate.
#en el intento de arriba, no sabía cuando iniciaba el conteo de fechas de Excel, lo supe hasta después,
#por eso primero pasé a csv la hoja ficha de ingreso, y luego usé la función dmy.

# str(bdcoving)
# colfechasn <- bdcoving%>%select(starts_with('fecha'))%>%names
# colfechasn
# bdcoving <- bdcoving%>% mutate_at(vars(starts_with('fecha')), as.numeric)

#CONCLUSIÓN: NO RESULTÓ PRÁCTICO. En el excel, algunas fechas no están como de tipo fecha, sino como general
#readxl las convierte a caracter no al número correspondiente, mejor primero lo paso a .csv en Excel
#sobreescribo el bdcoving anterior.

bdcoving <- read.csv('bdcoving.csv')
dim(bdcoving) #checa con la base original

#Trabajamos con las fechas:
bdcoving <- bdcoving%>%mutate_at(vars(starts_with('fecha')), dmy)

#salen errorea cuando convierto a fecha, selecciono los NAs en fechas
colfechas <- select(bdcoving, starts_with('fecha'))
indmalasfechas <- apply(colfechas, 2, function(x){
  which(is.na(x))
})
indmalasfechas
# hay unas cuantas fechas incorrectas, pero las demás son NAs, porque no existen.
#las únicas incorrectas están en la variable fecha
indmalasfechas$fecha
bdcoving$nombre[indmalasfechas$fecha]
bdcoving$fecha[115] <- dmy('26/05/2020')
bdcoving$fecha[170] <- dmy('15/06/2020')
#estas fechas también las corregí en Base de datos 18.xls, pero no en Base de datos 18_R.xlsx.

#Como restar fechas en:
#https://blog.exploratory.io/5-most-practically-useful-operations-when-working-with-date-and-time-in-r-9f9eb8a17465
bdcoving <- bdcoving%>%mutate(diasest = fechalta - fechahosp, tiempoenf = fechalta - fechainisint)
class(bdcoving$diasest)
mean(bdcoving$diasest, na.rm = TRUE)
summary(as.numeric(bdcoving$diasest))

which.max(bdcoving$diasest)
bdcoving$nombre[which.max(bdcoving$diasest)]
#observo que el max de diasest es el registro 179 (101 días), lo reviso y veo que hay un error. Lo corrijo
# tiene 06/03/2020, pero en la variable fecha, está 03/06/2020, asumo que cambió el mes por el día.
bdcoving$fechahosp[179] <- bdcoving$fecha[179]
bdcoving <- bdcoving%>%mutate(diasest = fechalta - fechahosp, tiempoenf = fechalta - fechainisint)

summary(as.numeric(bdcoving$diasest))

#¿son iguales fecha y fechahosp?
with(bdcoving, identical(fecha, fechahosp))
diffechas <- with(bdcoving, which((fecha -fechahosp)!= 0))
fechasdif <- bdcoving%>%select(nombre,fecha,fechalta)%>%slice(diffechas)
dim(fechasdif)
View(fechasdif) # hay 29 que son diferentes, le preguntaré a Álvaro.

# ¿Porqué hay 0 días de estancia?
which.min(bdcoving$diasest)
#es el registro 80, se murió el mismo día.
dim(bdcoving)
names(bdcoving)
mean(bdcoving$sato2sin, na.rm = TRUE)
head(bdcoving$sato2sin)
names(bdcoving[,1:20])
summary(bdcoving$edad)
table(bdcoving$ocupacion)
sum(is.na(bdcoving$ocupacion))
summary(bdcoving$escolaridad)
table(bdcoving$escolaridad)
bdcoving$escolaridad[3]
is.na(bdcoving$escolaridad[3])
is.null(bdcoving$escolaridad[3])
bdcoving$escolaridad[3] == ""

#me doy cuenta que al pasar de excel, muchos faltantes están como "", en coving están como factor.
#después de ensayo y error, creo caracnul, para convertir "" en NA y dejar la variable como factor:

caracnul <- function(x){
  
    nuevo <- ifelse(as.character(x) == '', NA,as.character(x))
  factor(nuevo)
  
}

#Este código fue para hacer las pruebas:
is.factor(bdcoving$antihip)
antihip <- bdcoving$antihip
typeof(antihip)
levels(antihip)
antihip2 <-  caracnul(antihip)
levels(antihip2)
antihip2
antihip

#protejo bdcoving y reviso si funciona la función caracnul.
bdcoving2 <-bdcoving%>% mutate_if(is.factor, caracnul) #si funciona
sum(as.matrix(bdcoving2)== '', na.rm = TRUE)
sum(as.matrix(bdcoving)== '', na.rm = TRUE) #ACORDARSE: PONER na.rm = TRUE

#esto de abajo es innecesario, pensé que no funcionaba el código de arriba porque se me había olvidado
#poner na.rm = TRUE.
sum(unlist(apply(bdcoving, 2, function(x) sum(x == ''))),na.rm = TRUE) #ESTE SI FUNCIONA
sum(unlist(apply(bdcoving2, 2, function(x) sum(x == ''))),na.rm = TRUE)
#con la línea de código de arriba, veo que ya no hay "" en bdcoving2.
bdcoving <- bdcoving2

#¿Qué pasa si desde read.csv uso que '' sea NA
bd <- read.csv('bdcoving.csv', na.strings = '') # es una mejor alternativa.
levels(bd$ph)# pero por ejemplo, el ph sigue saliendo como factor y no quiero

#veo cuáles son factor:
varfactores <- bdcoving%>%select_if(is.factor)%>%names
varfactores

#es mejor si uso as.is en read.csv?
bd <- read.csv('bdcoving.csv', na.strings = '', as.is = TRUE)
typeof(bd$ph)
sum(is.na(bd$ph))
sum(is.na(as.numeric(bd$ph))) # hay errores en ph, ya que al pasarlos a número, por coerción pone NAs

which(is.na(as.numeric(bd$ph)))
which(is.na(bd$ph))
#Comparo visualmente los dos
bd$ph[264]
is.na(is.numeric(as.numeric(bd$ph[264])))
#checo que fila no sigue el patrón de pH
str_detect(bd$ph, '\\d\\.\\d')
which(!str_detect(bd$ph, '\\d\\.\\d'))



# SEGUNDO MANEJO BASE DE DATOS "Base de datos 18_R.xlsx" ------------------

#Aprendo de lo anterior, se detectaron errores que se anotaron en la página "apuntes para covid observacional"
# de la sección COVID-19

#abro bdcoving.csv, que viene de la hoja FICHA DE INGRESO, del libro "Base de datos 18_R.xlsx", pero 
#pongo que detecte como NAs a las cadenas vacías, que las variables se queden como son (no convertir a factores)
bdcoving <- read.csv('bases-originales/bdcoving.csv', na.strings = '', as.is = TRUE)
dim(bdcoving) #checa con la base original

#Trabajamos con las fechas:
bdcoving <- bdcoving%>%mutate_at(vars(starts_with('fecha')), dmy)

#salen errorea cuando convierto a fecha, selecciono los NAs en fechas
colfechas <- select(bdcoving, starts_with('fecha'))
indmalasfechas <- apply(colfechas, 2, function(x){
  which(is.na(x))
})
indmalasfechas
# hay unas cuantas fechas incorrectas, pero las demás son NAs, porque no existen.
#las únicas incorrectas están en la variable fecha
indmalasfechas$fecha
bdcoving$nombre[indmalasfechas$fecha]
bdcoving$fecha[115] <- dmy('26/05/2020')
bdcoving$fecha[170] <- dmy('15/06/2020')
#estas fechas también las corregí en Base de datos 18.xls, pero no en Base de datos 18_R.xlsx.

#Como restar fechas en:
#https://blog.exploratory.io/5-most-practically-useful-operations-when-working-with-date-and-time-in-r-9f9eb8a17465
bdcoving <- bdcoving%>%mutate(diasest = fechalta - fechahosp, tiempoenf = fechalta - fechainisint)
class(bdcoving$diasest)
mean(bdcoving$diasest, na.rm = TRUE)
summary(as.numeric(bdcoving$diasest))

which.max(bdcoving$diasest)
bdcoving$nombre[which.max(bdcoving$diasest)]
#observo que el max de diasest es el registro 179 (101 días), lo reviso y veo que hay un error. Lo corrijo
# tiene 06/03/2020, pero en la variable fecha, está 03/06/2020, asumo que cambió el mes por el día.
bdcoving$fechahosp[179] <- bdcoving$fecha[179]
bdcoving <- bdcoving%>%mutate(diasest = fechalta - fechahosp, tiempoenf = fechalta - fechainisint)

summary(as.numeric(bdcoving$diasest))

#preguntar a Álvaro el significado de la variable fecha, ya que es diferente a fechahosp

# ¿Porqué hay 0 días de estancia?
which.min(bdcoving$diasest)
#es el registro 80, se murió el mismo día.
str(bdcoving)

#Reviso las variables que son númericas, pero salieron como cadenas.
nomcadenas <- bdcoving%>%select_if(is.character)%>%names
nomcadenas

patron <- '[^\\d\\.]' #cualquier cosa menos dígitos o puntos
str_view_all(c(12, 12.5, '12,5', ' 12.5', '12a'), patron)
str_which(bdcoving$peso, pattern = patron)
which(indcambios)
bdcoving$peso[152]
#Esto funciona, ve si lo puedo automatizar

fpatron <- function(x){
  str_which(x, pattern = patron)
}
nomcadenas <- nomcadenas[-6:-1]
nomcadenas
indcambios <- apply(bdcoving[, nomcadenas],2,fpatron )
indcambios

#Voy cambiando de acuerdo a lo que vaya saliendo:
bdcoving$peso[indcambios$peso]
bdcoving$peso[indcambios$peso] <- NA

bdcoving$talla[indcambios$talla]
bdcoving$talla[indcambios$talla] <- NA
nomcadenas

bdcoving$txhosp_10[indcambios$txhosp_10]
indcambios$txhosp_10
bdcoving$txhosp_10[indcambios$txhosp_10] <- NA

bdcoving$txhosp_14[indcambios$txhosp_14]
bdcoving$txhosp_14[indcambios$txhosp_14] <- NA

bdcoving$txhosp_17[indcambios$txhosp_17]
bdcoving$txhosp_17[indcambios$txhosp_17] <- NA

bdcoving$txhosp_18[indcambios$txhosp_18]
bdcoving$txhosp_18[indcambios$txhosp_18] <- NA

nomcadenas
bdcoving$rescov1[indcambios$rescov1] #Sale que están pendientes, checar con Álvaro porque es el resultado de la prueba covid.
indcambios$rescov1
bdcoving$nombre[indcambios$rescov1]

bdcoving$motivoegre[indcambios$motivoegre]
bdcoving$motivoegre[indcambios$motivoegre] <- 4 # aparece como 'ALTA VOLUNTARIA', añado que el 4 es eso en Basededatos 18.xlsx
bdcoving$motivoegre[indcambios$motivoegre]

bdcoving$sato2[indcambios$sato2] #pusieron el signo de porciento
bdcoving$sato2[indcambios$sato2] <- str_sub(bdcoving$sato2[indcambios$sato2], 1, 2)
bdcoving$sato2[indcambios$sato2]

bdcoving$sato2sin[indcambios$sato2sin] #lo mismo que el anterior
bdcoving$sato2sin[indcambios$sato2sin] <- str_sub(bdcoving$sato2sin[indcambios$sato2sin], 1, 2)
bdcoving$sato2sin[indcambios$sato2sin]

bdcoving$creat[indcambios$creat]
bdcoving$creat[indcambios$creat] <- '0.81'
bdcoving$creat[indcambios$creat]

nomcadenas
bdcoving$hb[indcambios$hb]
indcambios$hb
bdcoving$hb[indcambios$hb] <- NA
bdcoving$hb[indcambios$hb]

bdcoving$plaq[indcambios$plaq] #tiene espacio el número: '160 000'
bdcoving$plaq[indcambios$plaq] <- str_replace_all(bdcoving$plaq[indcambios$plaq], ' ', '')
bdcoving$plaq[indcambios$plaq]

bdcoving$leucos[indcambios$leucos] #igual que el anterior
bdcoving$leucos[indcambios$leucos] <- str_replace_all(bdcoving$leucos[indcambios$leucos], ' ', '')
bdcoving$leucos[indcambios$leucos]

bdcoving$basof[indcambios$basof] #tiene caracter no numérico después del número
bdcoving$basof[indcambios$basof] <- str_sub(bdcoving$basof[indcambios$basof], 1, 3)
bdcoving$basof[indcambios$basof]
indcambios$basof

nomcadenas
bdcoving$na[indcambios$na] # en vez del uno puso |
indcambios$na
bdcoving$na[indcambios$na] <- 130

bdcoving$aat[indcambios$aat] #tiene caracter no numérico después del número
indcambios$aat
bdcoving$aat[indcambios$aat] <- 41

bdcoving$dimd[indcambios$dimd] # puso una 'E' y 'E.CURSO', a ver si se pueden recuperar.Checar con Álvaro

bdcoving$gdo_3[indcambios$gdo_3] #es el general de orina, casi no tiene datos, no entrará al estudio.

bdcoving$ph[indcambios$ph] #tiene una coma
bdcoving$ph[indcambios$ph] <- '7.43'

nomcadenas
bdcoving$hco3[indcambios$hco3] # tiene espacios
bdcoving$hco3[indcambios$hco3] <- '20'
bdcoving$hco3[indcambios$hco3]

bdcoving$ekg_7[indcambios$ekg_7] # no tienen casi datos, no se integrará al análisis

bdcoving$txhashosp[indcambios$txhashosp] # se queda como cadena

#el nombre de colesterol está mal, lo cambio.
bdcoving <- bdcoving%>%rename(colesterol = colesterol.)
bdcoving$colesterol[indcambios$colesterol.] #la | en vez del 1
indcambios$colesterol.
bdcoving$colesterol[indcambios$colesterol.] <- '1'
bdcoving$colesterol[indcambios$colesterol.]

bdcoving$tp[indcambios$tp] #escribió "INDOSIFICABLE", pongo un 99
bdcoving$tp[indcambios$tp] <- '99'
bdcoving$tp[indcambios$tp]

nomcadenas
typeof(indcambios)

#creo una base de datos con rescov1 y dimd que faltan, a ver si se pueden rescatar.
rescov <- bdcoving %>% slice(indcambios$rescov1)%>%
  mutate(lugar = indcambios$rescov1 + 2) %>% select(lugar, nombre, nss, rescov1)
dimerosd <- bdcoving %>% slice(indcambios$dimd)%>%
  mutate(lugar = indcambios$dimd + 2) %>%select(lugar, nombre, nss, dimd)
alvaro1 <- full_join(rescov, dimerosd)%>%arrange(lugar)
write.csv(alvaro1, 'alvaro1.csv', row.names = FALSE)
table(bdcoving$motivoegre)
table(bdcoving$medico)


#17/07/2020
# Álvaro regresó la base sin cambios, termino entonces con la depuración:
library(tidyverse)
nomcadenas
bdcoving$rescov1[indcambios$rescov1]
bdcoving$rescov1[indcambios$rescov1] <- NA
bdcoving$rescov1[indcambios$rescov1]

bdcoving$dimd[indcambios$dimd]
bdcoving$dimd[indcambios$dimd] <- NA
bdcoving$dimd[indcambios$dimd]
nomcadenas

hartona <- function(x){
  
 sum(is.na(x))/length(x) > 0.6
}
nomhartona <- bdcoving%>%select_if(hartona)%>%names
nomhartona
sum(is.na(bdcoving$dimd))
table(bdcoving$dimd)
nas <- bdcoving%>%summarise_all(~sum(is.na(.)))
nas <- t(nas)
head(nas)
