
# CREACIÓN DE BDCOVIN A PARTIR DE BDCOVING ---------------------------------------------


load('Rdata/bdcoving.rda')
library(tidyverse)
library(caret)


#Genero una base con la cantidad de NAs y casos válidos.
bdnasing <- bdcoving%>%summarise(across(everything(),  ~sum(is.na(.x)) )) #Ahora hay que usar across en summarise_all y los demás
bdnasing <- as.data.frame(t(bdnasing))
nomfilas <- row.names(bdnasing)
bdnasing <- bdnasing %>% rename(totalnas = V1)%>%mutate(porcentaje = round(totalnas/dim(bdcoving)[1]*100,3),
                                                  len = dim(bdcoving)[1], variable = nomfilas,
                                                  validos = len - totalnas)%>%
  select(variable, validos, totalnas, porcentaje, len)%>%arrange(desc(porcentaje))

save(bdnasing, file = 'Rdata/bdnasing.rda')

hartosna <- function(x){
 sum(is.na(x))/length(x) > 0.73 #valor decidido, checando bdnas
}
#Quito los Nas
nomhartosna <- bdcoving %>% select(where(hartosna))%>%names
nomhartosna
bdcovin <- bdcoving%>%select(!where(hartosna))
names(bdcovin)
str(bdcovin, list.len = 200)

varnocamb <- bdcovin%>% select(all_of(nearZeroVar(bdcovin)))%>%names
varnocamb
table(bdcovin$app_1, useNA = 'ifany')
table(bdcovin$rescov1, useNA = 'ifany')
bdcovin%>%as_tibble %>% filter (rescov1 %in% c(2,3,4))%>%select(motivoegre)%>%pull%>%table(.)

bdcovin%>%as_tibble %>% filter (rescov1%in% c(1,NA))%>%select(motivoegre)%>%pull%>%table(.)
bdcovin%>%as_tibble %>% filter (!is.na(rescov1))%>%select(motivoegre)%>%pull%>%table(.)
table(prueba =bdcovin$rescov1, motivoalta = bdcovin$motivoegre, useNA = 'ifany')
length(which(is.na(bdcovin$rescov1)))


# ANTECEDENTES PERSONALES PATOLÓGICOS ----------------------------------------------------

#__________________________________________________________________________________
# Código para:
# 1) una tabla con la frecuencia de antecedentes personales patológicos en la base
# 2) una tabla con la frecuencia de app por motivo de egreso
# 3) cálCulo de chis cuadradas de apps contra motivo de egreso
# 4) añadir las chis a la tabla2
# 5) código para cantidad de antecedentes por paciente
#__________________________________________________________________________________


library(tidyverse)
#cargo bdcoving
load('Rdata/bdcoving.rda')
app <- read_lines('bases-originales/nomapp.txt')
app <- str_split(app, '\t')[[1]]%>%str_replace_all('\\s|-', "_") #cuando copio y pego la cadena de encabezados de Excel, sale \t que es un tabulador

##creamos sobrepeso y obesidad (app_18, app_19)
bdcoving <- bdcoving %>%
  mutate(app_18 = as.factor(ifelse(imc >= 25 & imc < 30, 1, 2)),
         app_19 = as.factor(ifelse(imc >= 30, 1, 2)))
#añado has, porque en la base de excel está separada del conjunto de antecedentes personales patológicos (app),
app <- c('HAS', app, 'SOBREPESO', 'OBESIDAD')
app
nomappord <- bdcoving%>%select(starts_with('app'))%>%names
nomappord

# solo emplearemos los pacientes con resultado de la prueba covid
#bdcoving <- bdcoving %>% filter(rescov1 %in% c(1,2))


#Frecuencia global de antecedentes
frecapp <- bdcoving%>%
  filter(motivoegre %in% c(2,3), rescov1 == 1)%>% 
  pivot_longer(starts_with('app'), names_to = 'apps', values_to = 'valor')%>%
  group_by(apps, valor) %>%
    mutate(apps = factor(apps, levels = nomappord, labels = app),
         valor = factor(valor, levels = c(1,2), labels = c('Si', 'No')))%>%
  summarise(n = n())%>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%filter(valor == 'Si')%>%
  arrange(frec)%>%
  rename(Antecedentes = apps, Cantidad = n, Porcentaje = frec, Total = total)%>%
  select(Antecedentes, Cantidad, Porcentaje, Total)

#frecuencia de antecedentes acumulados por persona:
facanum <- function(x){
  as.numeric(as.character(x))
}

numappglob <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1)%>% 
  rowwise()%>%
  mutate(across(starts_with('app'), facanum) ) %>%
  mutate(numapp = (length(nomappord)-sum(is.na(c_across(starts_with('app')))))*2 - sum(c_across(starts_with('app')), na.rm = TRUE))%>%
  #mutate(numapp = 36 - sum(c_across(starts_with('app')), na.rm = TRUE))%>% el código de arriba toma en cuenta los nas, los valores salen diferentes
  select(motivoegre, numapp)%>%
  ungroup %>%
  mutate(numapp = case_when(
    numapp == 0 ~ 'No',
    numapp == 1 ~ 'Una',
    numapp > 1  ~ 'Más de una'
  ))%>%
  mutate(numapp = factor(numapp, levels = c('No', 'Una', 'Más de una')))%>%
  group_by(numapp)%>%
    summarise(n = n()) %>%
    mutate(frec = round(n/sum(n)*100, 2),
           total = sum(n),
           validos = sum(n)- sum(is.na(numapp)))%>%
    select(numapp, n, frec, validos) %>%
    rename(`Número de enfermedades previas` = numapp,
           Número = n,
           Porcentaje = frec,
           Total = validos)

# Frecuencia de app agrupado por motivoegre y chi cuadrada
# 1) hago una base con el motivo de egreso y las variables categóricas con las que quiero hacer la chi cuadrada
bdcat <- bdcoving%>%filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  select(c(motivoegre, starts_with('app')))
names(bdcat)
# 2) calculo la chi cuadrada para cada antecedente contra el motivo de egreso
listachis <- apply(bdcat[,-1],2, function(i){chisq.test(table(i, bdcat$motivoegre))$p.value})
listachis

# 3) hago una base de datos con los nombres de los antecedentes y el valor de p, para unirlo en la base de datos
dfchis <- data.frame(apps = app, p = round(listachis, 3))

# hago la tabla con las frecuencias por motivo de egreso, añadiendo el valor de p de la chi cuadrada
frecapp_egreso <- bdcoving%>%filter(motivoegre %in% c(2,3) & rescov1 == 1) %>%
  select(c(motivoegre, starts_with('app'))) %>%
  pivot_longer(starts_with('app'), names_to = 'apps', values_to = 'valor')%>%
  group_by(motivoegre, apps, valor) %>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')),
         apps = factor(apps, levels = nomappord, labels = app),
         valor = factor(valor, levels = c(1,2), labels = c('Si', 'No')))%>%
  summarise(n = n())%>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n)) %>%
  pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
  mutate(total = `n_Defunción` + `n_Mejoría`,
         pordef = round(`n_Defunción`/total*100, 1),
         pormejo = round(`n_Mejoría`/total*100, 1),
         muertos = paste0(str_pad(`n_Defunción`,4,side = 'right', pad = ' '), '(', pordef, '%)'),
         vivos = paste0(str_pad(`n_Mejoría`,4,side = 'right'), '(', pormejo, '%)')) %>% #usé unite con las numéricas.
  select(apps, valor, total, muertos, vivos) %>%
  left_join(dfchis, by = 'apps' ) %>%
  filter(!is.na(valor) )%>%
  select(apps, valor, total, muertos, vivos, p) %>%
  arrange(p, by_group = TRUE)%>%
  rename(Variable = apps,Valor = valor , Total = total, `Defunción` = muertos, `Mejoría` = vivos)

# cantidad de antecedentes por paciente con su chi cuadrada:
facanum <- function(x){
  as.numeric(as.character(x))
}
numappchi <- bdcoving %>%
  filter(motivoegre %in% c(2,3)  & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  rowwise()%>%
  mutate(across(starts_with('app'), facanum) ) %>%
  mutate(numapp = (length(nomappord)-sum(is.na(c_across(starts_with('app')))))*2 - sum(c_across(starts_with('app')), na.rm = TRUE))%>%
  #mutate(numapp = 36 - sum(c_across(starts_with('app')), na.rm = TRUE))%>%
  select(motivoegre, numapp)%>%
  ungroup %>%
  mutate(numapp = case_when(
    numapp == 0 ~ 'No',
    numapp == 1 ~ 'Una',
    numapp > 1  ~ 'Más de una'
  ))%>%
  mutate(numapp = factor(numapp, levels = c('No', 'Una', 'Más de una')))

str(numappchi)
tablanumapp <- table(`Número de enfermedades previas` = numappchi$numapp, Evolución = numappchi$motivoegre)
tablanumapp
tablapropnumapp <- round(prop.table(tablanumapp, margin = 2)*100,2)
tablapropnumapp
chinumapp <- chisq.test(table(numappchi$numapp, numappchi$motivoegre))$p.value
chinumapp <- round(chinumapp, 3)
chinumapp

numapps_egreso <-numappchi %>% group_by(motivoegre, numapp)%>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n),
         validos = sum(n)- sum(is.na(numapp)),
         chi = chinumapp)%>%
  select(numapp, motivoegre, n, frec, validos, chi) %>%
  rename(`Número de enfermedades previas` = numapp,
         Evolución = motivoegre,
         Número = n,
         Porcentaje = frec,
         Total = validos,
         `chi cuadrada` = chi)
#hice una v de cramer, pero salió de .12.
# install.packages('questionr')
# library(questionr)
# ls('package:questionr')         
# cramer.v(tablanumapp)
# detach(package:questionr)

# t con imc y motivoegre
bdcoving%>%filter(motivoegre %in% c(2,3)  & rescov1 == 1) %>%
  select(motivoegre, imc) %>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  t.test(imc ~ motivoegre, data = .)
         

# TRATAMIENTO ANTES DEL INTERNAMIENTO -------------------------------------

#__________________________________________________________________________________
# Código para:
# 1) una tabla con la frecuencia de los tratamientos previos en la base
# 2) una tabla con la frecuencia de tratamientos previos por motivo de egreso
# 3) cálCulo de chis cuadradas de tratamientos previos contra motivo de egreso
# 4) añadir las chis a la tabla2
# 5) código para cantidad de antecedentes por paciente
#__________________________________________________________________________________


library(tidyverse)
#cargo bdcoving
load('Rdata/bdcoving.rda')
txprev <- read_lines('bases-originales/nomtxprev.txt')
txprev <- str_split(txprev, '\t')[[1]] %>%
  str_trim()%>%
  str_replace_all('\\s|-', "_") #cuando copio y pego la cadena de encabezados de Excel, sale \t que es un tabulador
txprev
nomtxprevord <- bdcoving%>%select(starts_with('txprev'))%>%names
nomtxprevord

# solo emplearemos los pacientes con resultado de la prueba covid

#Frecuencia global de tratamientos previos
frectxprev <- bdcoving%>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1)%>%
  pivot_longer(starts_with('txprev'), names_to = 'txprevs', values_to = 'valor')%>%
  group_by(txprevs, valor) %>%
  mutate(txprevs = factor(txprevs, levels = nomtxprevord, labels = txprev), #AQUÍ CAMBIO txprev_n A NOMBRE DE LOS MEDICAMENTOS
         valor = factor(valor, levels = c(1,2), labels = c('Si', 'No')))%>%
  summarise(n = n())%>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%filter(valor == 'Si')%>%
  arrange(desc(frec))%>%
  rename(Antecedentes = txprevs, Cantidad = n, Porcentaje = frec, Total = total)%>%
  select(Antecedentes, Cantidad, Porcentaje, Total)

#Cantidad de tratamientosacumulados por persona:
facanum <- function(x){
  as.numeric(as.character(x))
}

numtxprevglob <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1)%>%
  mutate(across(starts_with('txprev'), facanum) ) %>%
  rowwise()%>%
  mutate(numtxprev = (length(nomtxprevord)-sum(is.na(c_across(starts_with('txprev')))))*2 - sum(c_across(starts_with('txprev')), na.rm = TRUE))%>%
  select(motivoegre, numtxprev) %>%
  ungroup %>%
  mutate(numtxprev = case_when(
    numtxprev == 0 ~ 'No',
    numtxprev == 1 ~ 'Uno',
    numtxprev > 1  ~ 'Más de uno'
  ))%>%
  mutate(numtxprev = factor(numtxprev, levels = c('No', 'Uno', 'Más de uno')))%>%
  group_by(numtxprev)%>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n),
         validos = sum(n)- sum(is.na(numtxprev)))%>%
  select(numtxprev, n, frec, validos) %>%
  rename(`Número de tratamientos previos` = numtxprev,
         Número = n,
         Porcentaje = frec,
         Total = validos)

# Checar problemas en txprev, poner el gato antes de ungroup()
#  table(numtxprevglob$numtxprev)
# indneg <- which(numtxprevglob$numtxprev %in% c(-18, -6)) 
# indneg +2 #lo añado al ScrDepuCoving.R





# Frecuencia de txprev agrupado por motivoegre y chi cuadrada
# 1) hago una base con el motivo de egreso y las variables categóricas con las que quiero hacer la chi cuadrada
bdcat <- bdcoving%>%filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  select(c(motivoegre, starts_with('txprev')))
names(bdcat)
# 2) calculo la chi cuadrada para cada tratamiento previo contra el motivo de egreso
listachis <- apply(bdcat[,2:23],2, function(i){chisq.test(table(i, bdcat$motivoegre))$p.value})
listachis

# 3) hago una base de datos con los nombres de los tratmientos previos y el valor de p, para unirlo en la base de datos
dfchis <- data.frame(apps = txprev, p = round(listachis, 3))

# hago la tabla con las frecuencias por motivo de egreso, añadiendo el valor de p de la chi cuadrada
frectxprev_egreso <- bdcoving%>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>%
  select(c(motivoegre, starts_with('txprev'))) %>%
  pivot_longer(starts_with('txprev'), names_to = 'apps', values_to = 'valor')%>%
  group_by(motivoegre, apps, valor) %>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')),
         apps = factor(apps, levels = nomtxprevord, labels = txprev),
         valor = factor(valor, levels = c(1,2), labels = c('Si', 'No')))%>%
  summarise(n = n())%>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n)) %>%
  pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
  mutate(total = `n_Defunción` + `n_Mejoría`,
         pordef = round(`n_Defunción`/total*100, 1),
         pormejo = round(`n_Mejoría`/total*100, 1),
         muertos = paste0(str_pad(`n_Defunción`,4,side = 'right', pad = ' '), '(', pordef, '%)'),
         vivos = paste0(str_pad(`n_Mejoría`,4,side = 'right'), '(', pormejo, '%)')) %>% #usé unite con las numéricas.
  select(apps, valor, total, muertos, vivos) %>%
  left_join(dfchis, by = 'apps' ) %>%
  filter(!is.na(valor) )%>%
  select(apps, valor, total, muertos, vivos, p) %>%
  arrange(p, by_group = TRUE)%>%
  rename(Variable = apps,Valor = valor , Total = total, `Defunción` = muertos, `Mejoría` = vivos)

# cantidad de antecedentes por paciente con su chi cuadrada:
facanum <- function(x){
  as.numeric(as.character(x))
}
numtxprevchi <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  rowwise()%>%
  mutate(across(starts_with('txprev'), facanum) ) %>%
  mutate(numtxprev = (length(nomtxprevord)-sum(is.na(c_across(starts_with('txprev')))))*2 - sum(c_across(starts_with('txprev')), na.rm = TRUE))%>%
  #mutate(numtxprev = 36 - sum(c_across(starts_with('txprev')), na.rm = TRUE))%>%
  select(motivoegre, numtxprev)%>%
  ungroup %>%
  mutate(numtxprev = case_when(
    numtxprev == 0 ~ 'No',
    numtxprev == 1 ~ 'Uno',
    numtxprev > 1  ~ 'Más de uno'
  ))%>%
  mutate(numtxprev = factor(numtxprev, levels = c('No', 'Uno', 'Más de uno')))
str(numtxprevchi)
tablanumtxprev <- table(`Número de tratamientos previos` = numtxprevchi$numtxprev, Evolución = numtxprevchi$motivoegre)
tablanumtxprev
tablapropnumtxprev <- round(prop.table(tablanumtxprev, margin = 2)*100,2)
tablapropnumtxprev
chinumtxprev <- chisq.test(table(numtxprevchi$numtxprev, numtxprevchi$motivoegre))$p.value
chinumtxprev <- round(chinumtxprev, 3)
chinumtxprev

numtxprevs <-numtxprevchi %>% group_by(motivoegre, numtxprev)%>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n),
         validos = sum(n)- sum(is.na(numtxprev)),
         chi = chinumtxprev)%>%
  select(numtxprev, motivoegre, n, frec, validos, chi) %>%
  rename(`Número de tratamientos previos` = numtxprev,
         Evolución = motivoegre,
         Número = n,
         Porcentaje = frec,
         Total = validos,
         `chi cuadrada` = chi)

#Código para probar si los resultados son confiables. Selecciono un tx y lo checo con chi.
# prueba <- bdcoving%>%filter(motivoegre %in% c(2,3)) %>%
#   mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))
# table(prueba$txprev_4, prueba$motivoegre)
# rm(prueba)

# TRATAMIENTOS DURANTE LA HOSPITALIZACIÓN ---------------------------------

library(tidyverse)
#cargo bdcoving
load('Rdata/bdcoving.rda')
txhosp <- read_lines('bases-originales/nomtxhosp.txt')
txhosp <- str_split(txhosp, '\t')[[1]] %>%
  str_trim()%>%
  str_replace_all('\\s|-', "_") #cuando copio y pego la cadena de encabezados de Excel, sale \t que es un tabulador
txhosp
nomtxhospord <- bdcoving%>%select(starts_with('txhosp'))%>%names
nomtxhospord

# solo emplearemos los pacientes con resultado de la prueba covid


#Frecuencia global de tratamientos hospitalarios
frectxhosp <- bdcoving%>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1)%>%
  pivot_longer(starts_with('txhosp'), names_to = 'txhosps', values_to = 'valor')%>%
  group_by(txhosps, valor) %>%
  mutate(txhosps = factor(txhosps, levels = nomtxhospord, labels = txhosp),
         valor = factor(valor, levels = c(1,2), labels = c('Si', 'No')))%>%
  summarise(n = n())%>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%filter(valor == 'Si')%>%
  arrange(desc(frec))%>%
  rename(Tratamientos = txhosps, Cantidad = n, Porcentaje = frec, Total = total)%>%
  select(Tratamientos, Cantidad, Porcentaje, Total)

#Cantidad de tx hospitalarios acumulados por persona:
facanum <- function(x){
  as.numeric(as.character(x))
}

numtxhospglob <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1)%>%
  mutate(across(starts_with('txhosp'), facanum) ) %>%
  rowwise()%>%
  mutate(numtxhosp = (length(nomtxhospord)-sum(is.na(c_across(starts_with('txhosp')))))*2 - sum(c_across(starts_with('txhosp')), na.rm = TRUE))%>%
  select(motivoegre, numtxhosp) %>%
  ungroup %>%
  # mutate(numtxhosp = case_when(
  #   numtxhosp == 0 ~ 'No',
  #   numtxhosp == 1 ~ 'Uno',
  #   numtxhosp > 1  ~ 'Más de uno'
  # ))%>%
  # mutate(numtxhosp = factor(numtxhosp, levels = c('No', 'Uno', 'Más de uno')))%>%
  group_by(numtxhosp)%>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n),
         validos = sum(n)- sum(is.na(numtxhosp)))%>%
  select(numtxhosp, n, frec, validos) %>%
  rename(`Número de fármacos administrados` = numtxhosp,
         Número = n,
         Porcentaje = frec,
         Total = validos)

# Checar problemas en txhosp, poner el gato antes de ungroup()
#  table(numtxhospglob$numtxhosp)
# indneg <- which(numtxhospglob$numtxhosp %in% c(-18, -6))
# indneg +2 #lo añado al ScrDepuCoving.R

# Frecuencia de txhosp agrupado por motivoegre y chi cuadrada
# 1) hago una base con el motivo de egreso y las variables categóricas con las que quiero hacer la chi cuadrada
bdcat <- bdcoving%>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  select(c(motivoegre, starts_with('txhosp')))
names(bdcat)
# 2) calculo la chi cuadrada para cada tratamiento previo contra el motivo de egreso
listachis <- apply(bdcat[,2:21],2, function(i){chisq.test(table(i, bdcat$motivoegre))$p.value})
listachis

# 3) hago una base de datos con los nombres de los tratmientos previos y el valor de p, para unirlo en la base de datos
dfchis <- data.frame(apps = txhosp, p = round(listachis, 3))

# hago la tabla con las frecuencias por motivo de egreso, añadiendo el valor de p de la chi cuadrada
frectxhosp_egreso <- bdcoving%>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>%
  select(c(motivoegre, starts_with('txhosp'))) %>%
  pivot_longer(starts_with('txhosp'), names_to = 'apps', values_to = 'valor')%>%
  group_by(motivoegre, apps, valor) %>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')),
         apps = factor(apps, levels = nomtxhospord, labels = txhosp),
         valor = factor(valor, levels = c(1,2), labels = c('Si', 'No'))) %>%
  summarise(n = n())%>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n)) %>%
  pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
  mutate(total = `n_Defunción` + `n_Mejoría`,
         pordef = round(`n_Defunción`/total*100, 1),
         pormejo = round(`n_Mejoría`/total*100, 1),
         muertos = paste0(str_pad(`n_Defunción`,4,side = 'right', pad = ' '), '(', pordef, '%)'),
         vivos = paste0(str_pad(`n_Mejoría`,4,side = 'right'), '(', pormejo, '%)')) %>% #usé unite con las numéricas.
  select(apps, valor, total, muertos, vivos) %>%
  left_join(dfchis, by = 'apps' ) %>%
  filter(!is.na(valor) )%>%
  select(apps, valor, total, muertos, vivos, p) %>%
  arrange(p, by_group = TRUE)%>%
  rename(`Tratamientos previos` = apps,Valor = valor , Total = total, `Defunción` = muertos, `Mejoría` = vivos)


# cantidad de tx hospitalarios por paciente con su chi cuadrada:
facanum <- function(x){
  as.numeric(as.character(x))
}
numtxhospchi <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  rowwise()%>%
  mutate(across(starts_with('txhosp'), facanum) ) %>%
  mutate(numtxhosp = (length(nomtxhospord)-sum(is.na(c_across(starts_with('txhosp')))))*2 - sum(c_across(starts_with('txhosp')), na.rm = TRUE))%>%
  #mutate(numtxhosp = 36 - sum(c_across(starts_with('txhosp')), na.rm = TRUE))%>%
  select(motivoegre, numtxhosp)%>%
  ungroup %>%
  mutate(numtxhosp = case_when(
    numtxhosp == 0 ~ 'No',
    #numtxhosp == 1 ~ 'Uno', #en este caso, no hubo unos, si lo dejo, da error en la chi.
    numtxhosp > 1  ~ 'Más de uno'
  ))%>%
  mutate(numtxhosp = factor(numtxhosp, levels = c('No', 'Más de uno')))

str(numtxhospchi)
tablanumtxhosp <- table(`Número de tratamientos previos` = numtxhospchi$numtxhosp, Evolución = numtxhospchi$motivoegre)
tablanumtxhosp
tablapropnumtxhosp <- round(prop.table(tablanumtxhosp, margin = 2)*100,2)
tablapropnumtxhosp
chinumtxhosp <- chisq.test(table(numtxhospchi$numtxhosp, numtxhospchi$motivoegre))$p.value
chinumtxhosp <- round(chinumtxhosp, 3)
chinumtxhosp

numtxhosps <-numtxhospchi %>% group_by(motivoegre, numtxhosp)%>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n),
         validos = sum(n)- sum(is.na(numtxhosp)),
         chi = chinumtxhosp)%>%
  select(numtxhosp, motivoegre, n, frec, validos, chi) %>%
  rename(`Número de tratamientos previos` = numtxhosp,
         Evolución = motivoegre,
         Número = n,
         Porcentaje = frec,
         Total = validos,
         `chi cuadrada` = chi)

#_______________________________________________________________
# El cálculo de cantidad de tx intrahospitalarios no es muy útil, sería mejor ver combinaciones.
#Uniré los tratamientos en una cadena y lo convertiré en factor
# se usa el verbo unite()
combtxhosp <- bdcoving %>%
 filter(motivoegre %in% c(2,3) & rescov1 == 1) %>%
 select(c(motivoegre, starts_with('txhosp'))) %>%
 unite('txhosp_com', starts_with('txhosp'))
table(combtxhosp$txhosp_com)

# demasiadas combinaciones. otra estrategia, poner todos los txhosp en una variable y filtrar los 1.
fcomb <- function(x){
  cad <- str_c(x, sep = ',', collapse = TRUE)
  return(cad)
}
#cargo nobres abreviados de los tx hospitalarios:
txhospabr <- read_lines('bases-originales/nomtxhospabr.txt')
txhospabr <- str_split(txhospabr, '\t')[[1]] %>%
  str_trim()%>%
  str_replace_all('\\s|-', "_") #cuando copio y pego la cadena de encabezados de Excel, sale \t que es un tabulador
txhospabr

combtxhosp <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>%
  select(c(nss, motivoegre, starts_with('txhosp'))) %>%
  rename_with( ~ txhospabr, starts_with('txhosp')) %>%
  pivot_longer(c(3:22), names_to = 'txhosps', values_to = 'valor') %>%
  filter(valor == 1)%>%
  group_by(nss, motivoegre) %>%
  arrange(txhosps) %>%
  summarise(comb = str_c(txhosps,  collapse = ',')) %>% ungroup() %>%
  group_by(comb) %>%
  summarise(n = n(),
            mort = sum(motivoegre == 3),
            propmort = round(100*sum(motivoegre == 3)/n(), 1)) %>%
  arrange(desc(n))# %>%filter(n > 1)

# veo el comportamiento de la asociación de colchicina + clopidogrel, colchicina, colchicina sin clopidogrel
comb2 <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  select(c(nss, motivoegre, starts_with('txhosp'))) %>%
  #mutate(across(starts_with('txhosp'), facanum)) %>%
  mutate(colchiclopi = case_when(
    txhosp_9 == 1 & txhosp_13 == 1 ~ 'colchi+clopi',
    txhosp_9 == 1 & txhosp_13 == 2 ~ 'colchicina',
    txhosp_9 == 2 & txhosp_13 == 1 ~ 'clopidogrel',
    TRUE ~ 'otros'
  )) %>%
  select(nss, motivoegre, colchiclopi)
  
tcomb2 <- table(comb2$motivoegre, comb2$colchiclopi)
tcomb2
chisq.test(tcomb2)


#selecciono en combtxhosp solo las colchis.
bdcolchis <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>%
  select(c(nss, motivoegre, starts_with('txhosp'))) %>%
  rename_with( ~ txhospabr, starts_with('txhosp')) %>%
  rename(aacolchi = colchi) %>%
  pivot_longer(c(3:22), names_to = 'txhosps', values_to = 'valor') %>%
  filter(valor == 1)%>%
  group_by(nss, motivoegre) %>%
  arrange(txhosps) %>%
  summarise(comb = str_c(txhosps,  collapse = ',')) %>% ungroup() %>%
  group_by(comb) %>%
  summarise(n = n(),
            mort = sum(motivoegre == 3),
            propmort = round(100*sum(motivoegre == 3)/n(), 1)) %>%
  arrange(desc(n)) %>%
  slice(str_which(comb, 'colchi')) %>%
  arrange(propmort)



# VARIABLES SOCIODEMOGRÁFICAS ---------------------------------------------

library(tidyverse)
load('Rdata/bdcoving.rda')
names(bdcoving)

#varsocdemo <- c('edad', 'peso', 'talla', 'sexo', 'ocupacion', 'escolaridad', 'nivsoc')

#Descripción edad, peso, talla e imc.
bdvarnumglob <- bdcoving %>%filter(motivoegre %in% c(2,3) & rescov1 == 1)%>%
  select(edad, peso, talla, imc)%>%
  summarise(across(everything(), list(
    Promedio = ~ round(mean(.x, na.rm = TRUE),1),
    `Desviación estándar` = ~ round(sd(.x, na.rm = TRUE),1)
  )))%>%
  pivot_longer(cols = everything(),
               names_sep = '_',
               names_to = c('Variable', '.value'))%>%
  unite('Promedio±DE', c(Promedio, `Desviación estándar`), sep = ' ± ')

#Análisis edad, peso, talla, imc por motivoegre y cálculo de t.

bdnum <- bdcoving%>%filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  select(c(motivoegre, edad, peso, talla, imc))
names(bdnum)
# 2) calculo la t para cada variable contra el motivo de egreso
listates <- apply(bdnum[,-1],2, function(i){t.test(i ~ bdnum$motivoegre)$p.value})
listates
# 3) hago una base de datos con los nombres de los antecedentes y el valor de p, para unirlo en la base de datos
dftes <- data.frame(Variable = c('edad', 'peso', 'talla', 'imc'), p = round(listates, 3))

bdvarnummotiegre <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  select(motivoegre, edad, peso, talla, imc)%>%
  group_by(motivoegre) %>%
  summarise(across(everything(), list(
    Promedio = ~ round(mean(.x, na.rm = TRUE),2),
    `Desviación estándar` = ~ round(sd(.x, na.rm = TRUE),2)
  )))%>%
  pivot_longer(cols = -motivoegre,
               names_sep = '_',
               names_to = c('Variable', '.value')) %>%
  left_join(dftes, by = 'Variable')%>%
  #rename(expression(italic(p))= p)
  unite('Promedio±DE', c(Promedio, `Desviación estándar`), sep = ' ± ') %>%
  select(Variable, motivoegre, `Promedio±DE`, p  )%>%
  rename(`Motivo de egreso` = motivoegre)%>%
  arrange(Variable)

#_____________________________________________________________________________________________#
#Análisis de'sexo', 'ocupacion', 'escolaridad', 'nivsoc'
#Cada variable tiene diferentes factores, hago una dataframe para cada variable y luego las uno

frecsexo <- bdcoving%>%
  filter(motivoegre %in% c(2,3), rescov1 == 1)%>%
  select(sexo)%>%
 pivot_longer(sexo, names_to = 'apps', values_to = 'valor') %>%
  group_by(apps, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%
  arrange(frec)%>%
  rename(Variable = apps,Valor = valor , Cantidad = n, Porcentaje = frec, Total = total)%>%
  select(Variable, Valor, Cantidad, Porcentaje, Total)

#la ocupación tiene un 7 que no está codificado, como es un solo caso lo filtro.
table(bdcoving$ocupacion)

frecocup <- bdcoving%>%
  filter(motivoegre %in% c(2,3),rescov1 == 1, ocupacion != 7)%>%
  select(ocupacion)%>%
  mutate(ocupacion = case_when(
    ocupacion == 1 ~ 'Personal sanitario',
    ocupacion == 2 ~ 'Trabajo de oficina',
    ocupacion == 3 ~ 'Trabajo al aire libre',
    ocupacion == 4 ~ 'Trabajo en espacio público',
    ocupacion == 5 ~ 'Trabajo en casa',
    ocupacion == 6 ~ 'No trabaja'
  ))%>%
  rename( `Ocupación` = ocupacion) %>%
  pivot_longer(`Ocupación`, names_to = 'apps', values_to = 'valor') %>%
  group_by(apps, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%
  arrange(frec)%>%
  rename(Variable = apps,Valor = valor , Cantidad = n, Porcentaje = frec, Total = total)%>%
  select(Variable, Valor, Cantidad, Porcentaje, Total)

#Ahora escolaridad
table(bdcoving$escolaridad)

frecescol <- bdcoving%>%
  filter(motivoegre %in% c(2,3),rescov1 == 1)%>%
  select(escolaridad)%>%
  mutate(escolaridad = case_when(
    escolaridad == 1 ~ 'Analfabeta',
    escolaridad == 2 ~ 'Primaria',
    escolaridad == 3 ~ 'Secundaria',
    escolaridad == 4 ~ 'Bachillerato',
    escolaridad == 5 ~ 'Licenciatura',
    escolaridad == 6 ~ 'Posgrado'
  ))%>%
  rename( Escolaridad = escolaridad) %>%
  na.omit %>%
  pivot_longer(Escolaridad, names_to = 'apps', values_to = 'valor') %>%
  group_by(apps, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%
  arrange(frec)%>%
  rename(Variable = apps,Valor = valor , Cantidad = n, Porcentaje = frec, Total = total)%>%
  select(Variable, Valor, Cantidad, Porcentaje, Total)

#Ahora nivel socioeconómico
table(bdcoving$nivsoc)

# frecnivsoc <- bdcoving%>%
#   filter(motivoegre %in% c(2,3),rescov1 == 1)%>%
#   select(nivsoc)%>%
#   mutate(nivsoc = case_when(
#     nivsoc == 1 ~ 'Bajo',
#     nivsoc == 2 ~ 'Medio-bajo',
#     nivsoc == 3 ~ 'Medio-alto',
#     nivsoc == 4 ~ 'Alto'
#   ))%>%
#   na.omit %>%
#   rename( `Nivel socioeconómico` = nivsoc) %>%
#   pivot_longer(`Nivel socioeconómico`, names_to = 'apps', values_to = 'valor') %>%
#   group_by(apps, valor) %>%
#   summarise(n = n()) %>%
#   mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%
#   arrange(frec)%>%
#   rename(Variable = apps,Valor = valor , Cantidad = n, Porcentaje = frec, Total = total)%>%
#   select(Variable, Valor, Cantidad, Porcentaje, Total)

# nivsoc con dos niveles:
frecnivsoc <- bdcoving%>%
  filter(motivoegre %in% c(2,3),rescov1 == 1)%>%
  select(nivsoc)%>%
  mutate(nivsoc = case_when(
    nivsoc %in% c(1,2) ~ 'Bajo, medio-bajo',
    nivsoc %in% c(3,4) ~ 'Medio-alto, alto'
  ))%>%
  na.omit %>%
  rename( `Nivel socioeconómico` = nivsoc) %>%
  pivot_longer(`Nivel socioeconómico`, names_to = 'apps', values_to = 'valor') %>%
  group_by(apps, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%
  arrange(frec)%>%
  rename(Variable = apps,Valor = valor , Cantidad = n, Porcentaje = frec, Total = total)%>%
  select(Variable, Valor, Cantidad, Porcentaje, Total)

#Uno las dataframe:
dbvarsocdemglob <- bind_rows(frecsexo,frecocup,frecescol,frecnivsoc)
 
#_________________________________________________________________________________

#Igual que arriba, pero distribuidas por motivoegre

frecsexoegre <- bdcoving%>%
  filter(motivoegre %in% c(2,3), rescov1 == 1)%>%
  select(motivoegre, sexo)%>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción'))) %>%
  pivot_longer(sexo, names_to = 'apps', values_to = 'valor') %>%
  group_by(apps,motivoegre, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n)) %>%
  pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
  mutate(total = `n_Defunción` + `n_Mejoría`,
         pordef = round(`n_Defunción`/total*100, 1),
         pormejo = round(`n_Mejoría`/total*100, 1),
         muertos = paste0(str_pad(`n_Defunción`,4,side = 'right', pad = ' '), '(', pordef, '%)'),
         vivos = paste0(str_pad(`n_Mejoría`,4,side = 'right'), '(', pormejo, '%)')) %>% #usé unite con las numéricas.
  select(apps, valor, total, muertos, vivos) %>%
  #left_join(dfchis, by = 'apps' ) %>%
  filter(!is.na(valor) )%>%
  select(apps, valor, total, muertos, vivos) %>%
  #arrange(p, by_group = TRUE)%>%
  rename(Variable = apps,Valor = valor , Total = total, `Defunción` = muertos, `Mejoría` = vivos)

#la ocupación tiene un 7 que no está codificado, como es un solo caso lo filtro.
table(bdcoving$ocupacion)

frecocupegre <- bdcoving%>%
  filter(motivoegre %in% c(2,3),rescov1 == 1, ocupacion != 7)%>%
  select(motivoegre, ocupacion)%>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción'))) %>%
  mutate(ocupacion = case_when(
    ocupacion == 1 ~ 'Personal sanitario',
    ocupacion == 2 ~ 'Trabajo de oficina',
    ocupacion == 3 ~ 'Trabajo al aire libre',
    ocupacion == 4 ~ 'Trabajo en espacio público',
    ocupacion == 5 ~ 'Trabajo en casa',
    ocupacion == 6 ~ 'No trabaja'
  ))%>%
  rename( `Ocupación` = ocupacion) %>%
  pivot_longer(`Ocupación`, names_to = 'apps', values_to = 'valor') %>%
  group_by(motivoegre, apps, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n)) %>%
  pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
  mutate(total = `n_Defunción` + `n_Mejoría`,
         pordef = round(`n_Defunción`/total*100, 1),
         pormejo = round(`n_Mejoría`/total*100, 1),
         muertos = paste0(str_pad(`n_Defunción`,4,side = 'right', pad = ' '), '(', pordef, '%)'),
         vivos = paste0(str_pad(`n_Mejoría`,4,side = 'right'), '(', pormejo, '%)')) %>% #usé unite con las numéricas.
  select(apps, valor, total, muertos, vivos) %>%
  #left_join(dfchis, by = 'apps' ) %>%
  filter(!is.na(valor)) %>%
  #arrange(p, by_group = TRUE)%>%
  rename(Variable = apps,Valor = valor , Total = total, `Defunción` = muertos, `Mejoría` = vivos)

#Ahora escolaridad
table(bdcoving$escolaridad)

frecescolegre <- bdcoving%>%
  filter(motivoegre %in% c(2,3),rescov1 == 1)%>%
  select(motivoegre, escolaridad)%>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción'))) %>%
  mutate(escolaridad = case_when(
    escolaridad == 1 ~ 'Analfabeta',
    escolaridad == 2 ~ 'Primaria',
    escolaridad == 3 ~ 'Secundaria',
    escolaridad == 4 ~ 'Bachillerato',
    escolaridad == 5 ~ 'Licenciatura',
    escolaridad == 6 ~ 'Posgrado'
  ))%>%
  rename( Escolaridad = escolaridad) %>%
  na.omit %>%
  pivot_longer(Escolaridad, names_to = 'apps', values_to = 'valor') %>%
  group_by(motivoegre, apps, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n)) %>%
  pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
  mutate(total = `n_Defunción` + `n_Mejoría`,
         pordef = round(`n_Defunción`/total*100, 1),
         pormejo = round(`n_Mejoría`/total*100, 1),
         muertos = paste0(str_pad(`n_Defunción`,4,side = 'right', pad = ' '), '(', pordef, '%)'),
         vivos = paste0(str_pad(`n_Mejoría`,4,side = 'right'), '(', pormejo, '%)')) %>% #usé unite con las numéricas.
  select(apps, valor, total, muertos, vivos) %>%
  #left_join(dfchis, by = 'apps' ) %>%
  filter(!is.na(valor) )%>%
  select(apps, valor, total, muertos, vivos) %>%
  #arrange(p, by_group = TRUE)%>%
  rename(Variable = apps,Valor = valor , Total = total, `Defunción` = muertos, `Mejoría` = vivos)


#Ahora nivel socioeconómico
table(bdcoving$nivsoc)

# frecnivsocegre <- bdcoving%>%
#   filter(motivoegre %in% c(2,3),rescov1 == 1)%>%
#   select(motivoegre, nivsoc)%>%
#   mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción'))) %>%
#   mutate(nivsoc = case_when(
#     nivsoc == 1 ~ 'Bajo',
#     nivsoc == 2 ~ 'Medio-bajo',
#     nivsoc == 3 ~ 'Medio-alto',
#     nivsoc == 4 ~ 'Alto'
#   ))%>%
#   na.omit %>%
#   rename( `Nivel socioeconómico` = nivsoc) %>%
#   pivot_longer(`Nivel socioeconómico`, names_to = 'apps', values_to = 'valor') %>%
#   group_by(motivoegre, apps, valor) %>%
#   summarise(n = n()) %>%
#   mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%
#   arrange(valor)%>%
#   rename(Variable = apps,Valor = valor , Cantidad = n, Porcentaje = frec, Total = total)%>%
#   select(Variable, motivoegre, Valor, Cantidad, Porcentaje, Total, ) %>%
#   rename(`Motivo de egreso` = motivoegre)

# nivsoc con solo dos niveles.
frecnivsocegre <- bdcoving%>%
  filter(motivoegre %in% c(2,3),rescov1 == 1)%>%
  select(motivoegre, nivsoc)%>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción'))) %>%
  mutate(nivsoc = case_when(
    nivsoc %in% c(1,2) ~ 'Bajo, medio-bajo',
    nivsoc %in% c(3,4) ~ 'Medio-alto, alto'
  ))%>%
  #na.omit %>%
  rename( `Nivel socioeconómico` = nivsoc) %>%
  pivot_longer(`Nivel socioeconómico`, names_to = 'apps', values_to = 'valor') %>%
  group_by(motivoegre, apps, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2),
         total = sum(n)) %>%
  pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
  mutate(total = `n_Defunción` + `n_Mejoría`,
         pordef = round(`n_Defunción`/total*100, 1),
         pormejo = round(`n_Mejoría`/total*100, 1),
         muertos = paste0(str_pad(`n_Defunción`,4,side = 'right', pad = ' '), '(', pordef, '%)'),
         vivos = paste0(str_pad(`n_Mejoría`,4,side = 'right'), '(', pormejo, '%)')) %>% #usé unite con las numéricas.
  select(apps, valor, total, muertos, vivos) %>%
  #left_join(dfchis, by = 'apps' ) %>%
  filter(!is.na(valor) )%>%
  select(apps, valor, total, muertos, vivos) %>%
  #arrange(p, by_group = TRUE)%>%
  rename(Variable = apps,Valor = valor , Total = total, `Defunción` = muertos, `Mejoría` = vivos)


#Uno las dataframe:
dbvarsocdemegre <- bind_rows(frecsexoegre,frecocupegre,frecescolegre,frecnivsocegre)
class(dbvarsocdemegre)


#______________________________________________________________________________________
#Creo una función que haga una cadena con la chi y blancos según los levels.
fchipasocdem <- function(x, y){
  #p <- round(fisher.test(table(x, y), simulate.p.value = TRUE)$p.value, 3)
  p <- round(chisq.test(table(x, y))$p.value, 3)
  if (!is.factor(x)){
    x <- as.factor(x)
  } 
  blancos <- length(levels(x)) -1
  if (p < 0.001){
    p <- '< 0.001'
  }
  cad <- c(p, rep('', blancos))
  return(cad)
}


bdsexo <-bdcoving %>%
  filter(motivoegre %in% c(2,3), rescov1 == 1) %>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3)))%>%
  select(motivoegre, sexo)
chisexo <-   fchipasocdem(bdsexo$sexo, bdsexo$motivoegre)
chisexo

bdocup <- bdcoving %>%
  filter(motivoegre %in% c(2,3), rescov1 == 1, ocupacion != 7) %>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3)),
         ocupacion = factor(ocupacion, levels = c(1,2,3,4,5,6)))%>%
  select(motivoegre, ocupacion)
chiocup <- fchipasocdem(bdocup$ocupacion, bdocup$motivoegre)
chiocup
chisq.test(bdocup$ocupacion, bdocup$motivoegre)

table(bdcoving$escolaridad)
bdescol <- bdcoving %>%
  filter(motivoegre %in% c(2,3), rescov1 == 1) %>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3)),
         escolaridad = factor(escolaridad, levels = c(1,2,3,4,5,6)))%>%
  select(motivoegre, escolaridad)
chiescol <- fchipasocdem(bdescol$escolaridad, bdescol$motivoegre)
chiescol
chisq.test(bdescol$motivoegre, bdescol$escolaridad)
table(bdescol$motivoegre, bdescol$escolaridad)

# table(bdcoving$nivsoc)
# bdnivsoc <- bdcoving %>%
#   filter(motivoegre %in% c(2,3), rescov1 == 1, nivsoc != 4) %>%
#   mutate(motivoegre = factor(motivoegre, levels = c(2,3)),
#          nivsoc = factor(nivsoc, levels = c(1,2,3)))%>%
#   select(motivoegre, nivsoc)
# chinivsoc <- fchipasocdem(bdnivsoc$nivsoc, bdnivsoc$motivoegre)
# chinivsoc <- c(chinivsoc, '') #porque quité el nivel 4
# chinivsoc
# chisq.test(bdnivsoc$motivoegre, bdnivsoc$nivsoc)
# table(bdnivsoc$motivoegre, bdnivsoc$nivsoc)

#nivsoc con solo dos niveles.
bdnivsoc <- bdcoving %>%
  filter(motivoegre %in% c(2,3), rescov1 == 1) %>%
  mutate(nivsoc = case_when(
    nivsoc %in% c(1,2) ~ 'Bajo, medio-bajo',
    nivsoc %in% c(3,4) ~ 'Medio-alto, alto'
  ))%>%
  mutate(motivoegre = factor(motivoegre, levels = c(2,3)),
         nivsoc = as.factor(nivsoc))%>%
  select(motivoegre, nivsoc)
chinivsoc <- fchipasocdem(bdnivsoc$nivsoc, bdnivsoc$motivoegre)
chinivsoc
chisq.test(bdnivsoc$motivoegre, bdnivsoc$nivsoc)
table(bdnivsoc$motivoegre, bdnivsoc$nivsoc)

chisvarsocdem <- c(chisexo, chiocup, chiescol, chinivsoc)
length(chisvarsocdem)
dim(dbvarsocdemegre)

#Añado a dbvarsocdem la columna de chi
dbvarsocdemegre <- dbvarsocdemegre %>%
  ungroup %>%
  mutate(Chi = chisvarsocdem)
class(dbvarsocdemegre) # me di cuenta que hay que desagrupar

# VARIABLES CLÍNICA Y PARACLÍNICAS --------------------------------------------------

load('Rdata/bdcoving.rda')
library(tidyverse)
library(knitr)
names(bdcoving)

bdcov2 <- bdcoving %>%
  mutate(cov2 = factor(cov2, levels = c(1,2), labels = c('Si', 'No'))) 

kable(table(bdcov2$cov2), col.names = c('Segunda prueba', 'Número'))

#detach('package:knitr')
table(bdcoving$neumonia) #todos con neumonía.

str(bdcoving, list.len = 200)
varclincat <- bdcoving%>%
  select(c(84:157, 159)) %>%
  select(where(is.factor))%>%
  names
varclincat
lvarclincat <- apply(bdcoving[,varclincat], 2, function(x) table(x, useNA = 'always'))
lvarclincat  
lvarclincat[[3]]['NA']

frecvclinglob <- bdcoving%>%
  filter(motivoegre %in% c(2,3), rescov1 == 1)%>%
    pivot_longer(all_of(varclincat),names_to = 'apps', values_to = 'valor') %>%
  group_by(apps, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%
  arrange(apps)%>%
  rename(Variable = apps,Valor = valor , Cantidad = n, Porcentaje = frec, Total = total)%>%
  select(Variable, Valor, Cantidad, Porcentaje, Total)

bdvarclin <- bdcoving %>%
  filter(motivoegre %in% c(2,3), rescov1 == 1)%>%
  select(c(all_of(varclincat), motivoegre))%>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(txhashosp = case_when(txhashosp == '1' ~ 'Diurético',
                               txhashosp == '1,2,5,7' ~ 'Diur,araII,baldos,calcioant',
                               txhashosp == '2' ~ 'ARAII',
                               txhashosp == '2,4' ~ 'ARAII',
                               txhashosp == '3' ~ 'IECA',
                               txhashosp == '3, 7' ~ 'ARAII,calcioant',
                               txhashosp == '4' ~ 'Betabloqueador',
                               txhashosp == '6' ~ 'Sin medicamento',
                               txhashosp %in% c('7','8,7') ~ 'Calcioantagonista',
                               TRUE ~ txhashosp),
         anticoag = case_when(anticoag == '1' ~ 'Profilaxis',
                              anticoag == '2' ~ 'Anticoagulante',
                              TRUE ~ NA_character_),
         controltahosp = case_when(controltahosp == '1' ~ 'Si',
                                   controltahosp == '2' ~ 'No'),
         dislip = ifelse(dislip == '1', 'Si', 'No'),
         neumonia = ifelse(neumonia == '1', 'Si', 'No'),
         piel = ifelse(piel == '1', 'Si', 'No'),
         sintoma1 = case_when(sintoma1 == '1' ~ 'Tos',
                              sintoma1 == '2' ~ 'Fiebre',
                              sintoma1 == '3' ~ 'Cefalea',
                              sintoma1 == '4' ~ 'Anosmia',
                              sintoma1 == '5' ~ 'Malestar general',
                              sintoma1 == '6' ~ 'Mareo',
                              sintoma1 == '7' ~ 'Debilidad muscular',
                              sintoma1 == '8' ~ 'Diarrea',
                              sintoma1 == '9' ~ 'Pérdida gusto',
                              sintoma1 == '10' ~ 'Erupciones cutáneas',
                              sintoma1 == '11' ~ 'Disnea',
                              sintoma1 == '12' ~ 'Dolor u opresión torácica',
                              sintoma1 == '13' ~ 'Incapacidad',),
         tipoing = case_when(tipoing == 1 ~ 'Piso',
                             tipoing == 2 ~ 'UCI', 
                             tipoing == 3 ~ 'Urgencias'),
         ventilador = case_when(ventilador == 1 ~ 'Si',
                                ventilador == 2 ~ 'No',
                                TRUE ~ NA_character_),
         ventnoinv = case_when(ventnoinv == 1 ~ 'Si',
                               ventnoinv == 2 ~ 'No',
                               TRUE ~ NA_character_),
         motivoegre = ifelse(motivoegre == 2, 'Mejoría', 'Defunción'))

frecvclin2 <- bdvarclin %>% pivot_longer(all_of(varclincat),names_to = 'apps', values_to = 'valor') %>%
  group_by(apps, valor) %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%
  arrange(apps)%>%
  rename(Variable = apps,Valor = valor , Cantidad = n, Porcentaje = frec, Total = total)%>%
  select(Variable, Valor, Cantidad, Porcentaje, Total)

#Después de revisar la tabla, selecciono las variables que vale la pena hacer la chi,reduzco dimensiones
# en valores pequeños.

#Ejemplo de chi cuadrada con anticoag
tbanticoag <- table(bdvarclin$anticoag, bdvarclin$motivoegre) 
addmargins(tbanticoag, margin = 1)
prop.table(tbanticoag, margin = 2)
chisq.test(tbanticoag) 
#_____________________
# Creo las chis.
nompachisvclin <- bdvarclin %>%
  select(c(-starts_with(c('ekg', 'gdo')), -txhashosp, -piel, -ventnoinv, -neumonia, -motivoegre)) %>% names
nompachisvclin
listachisvclin <- apply(bdvarclin[, nompachisvclin],2, function(i){chisq.test(table(i, bdvarclin$motivoegre))$p.value})
listachisvclin <- round(listachisvclin, 3)
listachisvclin
listachisvclin <- data.frame(apps = nompachisvclin, p = listachisvclin)

# En este código, encontré la manera de presentar de mejor manera las chis
str(bdvarclin)
bdvarclinred_nonas <- bdvarclin %>%
  #select(c(-starts_with(c('ekg', 'gdo')), -txhashosp, -piel,-neumonia, -ventnoinv)) %>%
  select(c(all_of(nompachisvclin), motivoegre)) %>%
  mutate(sintoma1 = ifelse(sintoma1 %in% c('Debilidad muscular', 'Incapacidad','Diarrea'), 'Otros', sintoma1)) %>%
  pivot_longer(1:6, names_to = 'apps', values_to = 'valor' ) %>%
  group_by(motivoegre, apps, valor) %>%
  na.omit %>%
  summarise(n = n()) %>%
  mutate(frec = round(n/sum(n)*100, 2), total = sum(n))%>%
  pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
  mutate(total = `n_Defunción` + `n_Mejoría`,
         pordef = round(`n_Defunción`/total*100, 1),
         pormejo = round(`n_Mejoría`/total*100, 1),
         muertos = paste0(str_pad(`n_Defunción`,4,side = 'right', pad = ' '), '(', pordef, '%)'),
         vivos = paste0(str_pad(`n_Mejoría`,4,side = 'right'), '(', pormejo, '%)')) %>% #usé unite con las numéricas.
  select(apps, valor, total, muertos, vivos) %>%
  left_join(listachisvclin, by = 'apps') %>%
  arrange(p)%>%
  rename(Variable = apps,Valor = valor , Total = total, `Defunción` = muertos, `Mejoría` = vivos)

#Añadiré las chis a la base anterior:
#este código formatea la variable chi a caracter, los menores de 0.001, pero decido que es mejor dejarla
# como númerica. el código para calcular la chi está antes de hacer la base, para usar left_join en el código
# nompachisvclin <- bdvarclin %>%
#   select(c(-starts_with(c('ekg', 'gdo')), -txhashosp, -piel, -ventnoinv, -neumonia, -motivoegre)) %>% names
# nompachisvclin
# listachisvclin <- apply(bdvarclin[, nompachisvclin],2, function(i){chisq.test(table(i, bdvarclin$motivoegre))$p.value})
# listachisvclin <- round(listachisvclin, 3)
# listachisvclin
# listachisvclin[which(listachisvclin < 0.001)] <- '< 0.001'
# listachisvclin
# lvclin <- listachisvclin
# lvclin
# #Doy el orden y espacios:
# listachisvclin2 <- c(lvclin['anticoag'], '', lvclin['controltahosp'], '',
#                      lvclin['dislip'], '', lvclin['sintoma1'], rep('', 6),
#                      lvclin['tipoing'], rep('', 2), lvclin['ventilador'], '')
# listachisvclin2

#agrego las chis a la base:
bdvarclinred_nonas$Chi <- listachisvclin2

#_________________________________________________________________________
#Código para trabajar las variables cuantitativas de laboratorios y otros.

# Datos globales
str(bdcoving, list.len = 200)
nomvar <- bdcoving %>% select(where(is.numeric)) %>% names
nomvar
nomvar <- nomvar[-c(1:5, 39:40, 52)]
nomvar
validos <- function(x){
  390 - sum(is.na(x))
}
bdvparaclinglob <- bdcoving %>%filter(motivoegre %in% c(2,3) & rescov1 == 1)%>%
  select(all_of(nomvar))%>%
  summarise(across(everything(), list(
    Validos = ~ validos(.x),
    Promedio = ~ round(mean(.x, na.rm = TRUE),1),
    `Desviación estándar` = ~ round(sd(.x, na.rm = TRUE),1)
  )))%>%
  pivot_longer(cols = everything(),
               names_sep = '_',
               names_to = c('Variable', '.value'))%>%
  unite('Promedio ± DE', c(Promedio, `Desviación estándar`), sep = ' ± ')



#Datos por motivoegre y sus tes.
#Selecciono las variables con válidos mayor a 100

nomvar2 <- bdvparaclinglob %>%
  filter(Validos > 100) %>%
  select(Variable) %>% pull
nomvar2


bdnum2 <- bdcoving%>%filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  select(c(motivoegre, all_of(nomvar2)))
names(bdnum2)
# 2) calculo la t para cada variable contra el motivo de egreso
listates2 <- apply(bdnum2[,-1],2, function(i){t.test(i ~ bdnum2$motivoegre)$p.value})
listates2
# 3) hago una base de datos con los nombres de los antecedentes y el valor de p, para unirlo en la base de datos
dftes2 <- data.frame(Variable = nomvar2, p = round(listates, 3))

validos2 <- function(x){
  length(x) - sum(is.na(x))
}

bdvarnum2motiegre <- bdcoving %>%
  filter(motivoegre %in% c(2,3) & rescov1 == 1) %>% 
  mutate(motivoegre = factor(motivoegre, levels = c(2,3), labels = c('Mejoría', 'Defunción')))%>%
  select(c(motivoegre, all_of(nomvar2)))%>%
  group_by(motivoegre) %>%
  summarise(across(everything(), list(
    Validos = ~ validos2(.x),
    Promedio = ~ round(mean(.x, na.rm = TRUE),2),
    `Desviación estándar` = ~ round(sd(.x, na.rm = TRUE),2)
  )))%>%
  pivot_longer(cols = -motivoegre,
               names_sep = '_',
               names_to = c('Variable', '.value')) %>%
  left_join(dftes2, by = 'Variable')%>%
  #rename(expression(italic(p))= p)
  unite('Promedio ± DE', c(Promedio, `Desviación estándar`), sep = ' ± ') %>%
  select(Variable, Validos, motivoegre, `Promedio ± DE`, p  )%>%
  rename(`Motivo de egreso` = motivoegre)%>%
  arrange(Variable)
