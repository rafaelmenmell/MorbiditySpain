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
library(h2o)

if (file.exists("morbi_data.rds")==FALSE){
  ll <- GetMorbiData(y1 = 2005,y2 = 2015)
  saveRDS(ll,"morbi_data.rds")
} else {
  ll <- readRDS("morbi_data.rds")
}

#en la comunidad de madrid
llpeq <- ll %>% filter(lubridate::year(fecha_ingreso)>=2015)
rm(ll)
llpeq <- llpeq %>% FilterEmergency() %>% AddDiagnosis1() %>% AddDiagnosis2()

llpeq <- llpeq[complete.cases(llpeq),]

set.seed(333)
N <- nrow(llpeq)
sample_size <- floor(0.75*N)
train_ind <- sample(1:N,sample_size)
train <- llpeq[train_ind,]
test <- llpeq[-train_ind,]

train <- train %>% select(estancia,sexo,fecha_ingreso,edad,diag1,diag2,prov_hosp)
test <- test %>% select(estancia,sexo,fecha_ingreso,edad,diag1,diag2,prov_hosp)
#fit <- randomForest(estancia ~ sexo + diag_in + edad + diag2,data=train[1:100000,],proximity=FALSE)

h2o.init()

h2o_train <- as.h2o(train)
h2o_test <- as.h2o(test)

## run our first predictive model
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = h2o_train,        ## the H2O frame for training
  validation_frame = h2o_test,      ## the H2O frame for validation (not required)
  x=2:7,                        ## the predictor columns, by column index
  y=1,                          ## the target index (what we are predicting)
  model_id = "estancia_resto",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 333)                ## Set the random seed so that this can be
##  reproduced.

## run our first predictive model
rf2 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = h2o_train,        ## the H2O frame for training
  validation_frame = h2o_test,      ## the H2O frame for validation (not required)
  x=2:7,                        ## the predictor columns, by column index
  y=1,                          ## the target index (what we are predicting)
  model_id = "estancia_resto",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 333)                ## Set the random seed so that this can be
##  reproduced.

summary(rf1)
rf1@model$validation_metrics

prediction <- h2o.predict(object = rf1,newdata = h2o_test)
test$prediction <- as.data.frame(prediction)$predict
RMSE <- sqrt(mean((test$prediction - test$estancia)^2,na.rm=TRUE))

gbm1 <- h2o.gbm(
  training_frame = h2o_train,        ## the H2O frame for training
  validation_frame = h2o_test,      ## the H2O frame for validation (not required)
  x=2:7,                        ## the predictor columns, by column index
  y=1,                          ## the target index (what we are predicting)
  model_id = "gbm_estancia",     ## name the model in H2O
  seed = 333)                ## Set the random seed for reproducability


gbm2 <- h2o.gbm(
  training_frame = h2o_train,        ## the H2O frame for training
  validation_frame = h2o_test,      ## the H2O frame for validation (not required)
  x=2:7,                        ## the predictor columns, by column index
  y=1,                          ## the target index (what we are predicting)
  model_id = "gbm_estancia",     ## name the model in H2O
  seed = 333,
  ntrees = 1000)                ## Set the random seed for reproducability

gbm1@model$validation_metrics
gbm2@model$validation_metrics


h2o.shutdown()

train.sencillo <- train %>% group_by(diag2,edad) %>% summarise(estancia.media=mean(estancia))
testsencillo <- merge(test, train.sencillo, by=c("edad","diag2"))
RMSE <- sqrt(mean((testsencillo$estancia.media - testsencillo$estancia)^2,na.rm=TRUE))
