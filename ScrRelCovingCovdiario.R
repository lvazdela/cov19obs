
# ANÁLISIS RELACIÓN bdcoving, bdcovdiario ---------------------------------

#_______________________________________________________________________________
#03/08/2020
#Scipt para depurar la relación entre bdcoving y bdcovdiario.
#Debe haber los mismos pacientes en ambas bases
#
#_________________________________________________________________________________

library(tidyverse)
library(lubridate)

load('Rdata/bdcovdiario.rda')
load('Rdata/bdcoving.rda')


#Código para checar si hay diferencias entre los pacientes de bdcoving y bdcovdiario
pacbdcoving1 <- bdcoving %>% select(nss) %>% arrange(nss)
dim(pacbdcoving1)
pacbdcoving2 <- bdcoving %>% select(nss) %>% arrange(nss)%>%distinct
dim(pacbdcoving2)
#descubro que hay un repetido en bdcoving
pacrepe <- bdcoving %>% select(nss) %>% group_by(nss) %>% summarise(n = n())
pacrepe <- pacrepe %>% filter(n > 1) %>% select(nss) %>% pull
pacrepe
which(bdcoving$nss == pacrepe)+2

#comparo los nss de ambas bases
dim(bdcovdiario)
pacbdcovdi <- bdcovdiario %>% distinct( across('nss'), .keep_all = TRUE) %>% select(nss)
dim(pacbdcovdi)
dim(pacbdcoving2)

diferentes <- setdiff( pacbdcoving2$nss ,pacbdcovdi$nss) #estan en coving, pero no en covdiario
diferentes
diferentes2 <- setdiff(pacbdcovdi$nss, pacbdcoving2$nss) #están en covdiario, pero no en coving
diferentes2

which(bdcoving$nss %in% diferentes) +2
which(bdcovdiario$nss %in% diferentes2) +2
