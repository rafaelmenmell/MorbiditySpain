#Voy a intentar predecir el tiempo de ingreso con treeboost

if(Sys.info()['sysname']=="windows"){
  install.packages("MorbiditySpain_0.1.0.zip",repos = NULL,type = "binary")
} else {
  install.packages("MorbiditySpain_0.1.0.tar.gz",repos = NULL, type="source")
}
devtools::install_github("hafen/geofacet")
library(readr)
library(dplyr)
library(MorbiditySpain)
library(lubridate)
library(ggplot2)
library(geofacet)
library(zoo)
library(randomForest)

if (file.exists("morbi_data.rds")==FALSE){
  ll <- GetMorbiData(y1 = 2005,y2 = 2015)
  saveRDS(ll,"morbi_data.rds")
} else {
  ll <- readRDS("morbi_data.rds")
}

#en la comunidad de madrid
llpeq <- ll %>% FilterProvincia(28) %>% filter(year(fecha_ingreso)>=2014)
rm(ll)
llpeq <- llpeq %>% AddDiagnosis1() %>% AddDiagnosis2()

llpeq <- llpeq[complete.cases(llpeq),]

set.seed(333)
N <- nrow(llpeq)
sample_size <- floor(0.75*N)
train_ind <- sample(1:N,sample_size)
train <- llpeq[train_ind,]
test <- llpeq[-train_ind,]

fit <- randomForest(estancia ~ sexo + diag_in + fecha_ingreso + edad +  diag1 + diag2,data=train,importance=TRUE,ntree=50,nodesize=3,sampsize=sample_size*.3)
