load('Rdata/bdcoving.rda')
library(tidyverse)
library(caret)
varnocamb <- nearZeroVar(bdcoving, saveMetrics = FALSE)
varnocamb
bdcov1 <- bdcoving %>% select(- varnocamb)
dim(bdcov1)
dim(bdcoving)
sum(is.na(bdcov1$motivoegre))