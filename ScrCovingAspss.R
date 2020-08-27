#_____________________________________________________________________
# Script para cambiar coving a SPSS
# se usa la librería haven
# renombramos las variables
#_____________________________________________________________________

library(tidyverse)
#cargo bdcoving
load('Rdata/bdcoving.rda')
#dar significado a nombre de variables:
app <- read_lines('bases-originales/nomapp.txt')
app <- str_split(app, '\t')[[1]]%>%str_replace_all('\\s|-', "_") #cuando copio y pego la cadena de encabezados de Excel, sale \t que es un tabulador
#añado has, porque en la base de excel no aparece con el prefijo app,
app <- c('HAS', app)
app
#No creo buena idea copiar y pegar al editor, guardo los encabezados de Excel en un txt con el bloc de notas, previamente editando los muy largos o con acentos.
txprev <- read_lines('bases-originales/nomvar.txt')
txprev
txprev <- str_split(txprev, '\t')[[1]]%>%str_trim()%>%str_replace_all('\\s|,', "_") #
txprev <- str_replace(txprev, '_{2,}|-_','_')
txprev 
#tx hosp igual que txprev, le pongo el prefijo txhosp
txhosp <- read_lines('bases-originales/nomtxhosp.txt')
txhosp
txhosp <-str_split(txhosp,'\t')[[1]]%>%str_trim()%>%str_replace_all('\\s|,', "_")
txhosp <- str_replace(txhosp, '_{2,}|-_','_')
txhosp
txhosp <- paste0('txhosp',txhosp)
txhosp

#Cambio los nombres de las variables:
bdcovingspss <- bdcoving %>% rename_with(~c(app, txprev, txhosp), starts_with(c('app', 'txprev', 'txhosp')))
#Exporto bdcoving a SPSS
library(haven)
write_sav(bdcovingspss,'bases-originales/bdcoving.sav')
