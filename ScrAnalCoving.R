
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
  select(variable, validos, totalnas, porcentaje, len)
#row.names(bdnasing) <- nomfilas
bdnasing <- arrange(bdnasing,desc(porcentaje))
save(bdnasing, file = 'Rdata/bdnasing.rda')

hartosna <- function(x){
 sum(is.na(x))/length(x) > 0.73 #valor decidido, checando bdnas
}
#Quito los Nas
bdcovin <- bdcoving%>%select(!where(hartosna))
names(bdcovin)
str(bdcovin, list.len = 200)

varnocamb <- bdcovin%>% select(all_of(nearZeroVar(bdcovin)))%>%names
varnocamb
table(bdcovin$app_1, useNA = 'ifany')
table(bdcovin$cov2, useNA = 'ifany')
