#grid search with h2o

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

llpeq <- llpeq %>% select(estancia,prov_hosp,sexo,fecha_ingreso,edad,diag1,diag2)

h2o.init(nthreads = -1, #Number of threads -1 means use all cores on your machine
         max_mem_size = "2G")  #max mem size is the maximum memory to allocate to H2O

h2o_llpeq <- as.h2o(llpeq)
splits <- h2o.splitFrame(data = h2o_llpeq,
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 333)  #setting a seed will guarantee reproducibility

train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

## Hyper-Parameter Search

## Construct a large Cartesian hyper-parameter space
ntrees_opts <- c(1000) ## early stopping will stop earlier
max_depth_opts <- seq(1,20)
min_rows_opts <- c(1,5,10,20,50,100)
learn_rate_opts <- seq(0.001,0.01,0.001)
sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_per_tree_opts = seq(0.3,1,0.05)
#nbins_cats_opts = seq(100,10000,100) ## no categorical features in this dataset

hyper_params = list( ntrees = ntrees_opts,
                     max_depth = max_depth_opts,
                     min_rows = min_rows_opts,
                     learn_rate = learn_rate_opts,
                     sample_rate = sample_rate_opts,
                     col_sample_rate = col_sample_rate_opts,
                     col_sample_rate_per_tree = col_sample_rate_per_tree_opts
                     #,nbins_cats = nbins_cats_opts
)

## Search a random subset of these hyper-parmameters, 50 modelos
## Search a random subset of these hyper-parmameters (max runtime and max models are enforced, and the search will stop after we don't improve much over the best 5 random models)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 1800, max_models = 20, stopping_metric = "AUTO", stopping_tolerance = 0.00001, stopping_rounds = 5, seed = 333)

gbm.grid <- h2o.grid("gbm",
                     grid_id = "mygrid",
                     x = 2:7,
                     y = 1,
                     
                     training_frame = train,
                     validation_frame = valid,
                     nfolds = 0,

                     distribution="gaussian", ## best for MSE loss, but can try other distributions ("laplace", "quantile")
                     
                     ## stop as soon as mse doesn't improve by more than 0.1% on the validation set,
                     ## for 2 consecutive scoring events
                     stopping_rounds = 2,
                     stopping_tolerance = 1e-3,
                     stopping_metric = "MSE",
                     
                     score_tree_interval = 100, ## how often to score (affects early stopping)
                     seed = 333, ## seed to control the sampling of the Cartesian hyper-parameter space
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

gbm.sorted.grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "mse")
print(gbm.sorted.grid)

best_model <- h2o.getModel(gbm.sorted.grid@model_ids[[1]])
summary(best_model)

scoring_history <- as.data.frame(best_model@model$scoring_history)
plot(scoring_history$number_of_trees, scoring_history$training_MSE, type="p") #training mse
points(scoring_history$number_of_trees, scoring_history$validation_MSE, type="l") #validation mse

## get the actual number of trees
ntrees <- best_model@model$model_summary$number_of_trees
print(ntrees)

## Construct a large Cartesian hyper-parameter space
ntrees_opts <- c(500) ## early stopping will stop earlier
max_depth_opts <- seq(1,10)
min_rows_opts <- c(20,50,100)
learn_rate_opts <- seq(0.001,0.01,0.001)
sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_per_tree_opts = seq(0.3,1,0.05)
#nbins_cats_opts = seq(100,10000,100) ## no categorical features in this dataset

hyper_params = list( ntrees = ntrees_opts,
                     max_depth = max_depth_opts,
                     min_rows = min_rows_opts,
                     learn_rate = learn_rate_opts,
                     sample_rate = sample_rate_opts,
                     col_sample_rate = col_sample_rate_opts,
                     col_sample_rate_per_tree = col_sample_rate_per_tree_opts
                     #,nbins_cats = nbins_cats_opts
)

search_criteria = list(strategy = "RandomDiscrete", max_models = 20)

gbm.grid2 <- h2o.grid("gbm",
                     grid_id = "mygrid2",
                     x = 2:7,
                     y = 1,
                     
                     training_frame = train,
                     validation_frame = valid,
                     nfolds = 0,
                     
                     distribution="gaussian", ## best for MSE loss, but can try other distributions ("laplace", "quantile")
                     
                     ## stop as soon as mse doesn't improve by more than 0.1% on the validation set,
                     ## for 2 consecutive scoring events
                     stopping_rounds = 2,
                     stopping_tolerance = 1e-3,
                     stopping_metric = "MSE",
                     
                     score_tree_interval = 100, ## how often to score (affects early stopping)
                     seed = 333, ## seed to control the sampling of the Cartesian hyper-parameter space
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

gbm.sorted.grid <- h2o.getGrid(grid_id = "mygrid2", sort_by = "mse")
print(gbm.sorted.grid)

best_model <- h2o.getModel(gbm.sorted.grid@model_ids[[1]])
summary(best_model)

scoring_history <- as.data.frame(best_model@model$scoring_history)
plot(scoring_history$number_of_trees, scoring_history$training_MSE, type="p") #training mse
points(scoring_history$number_of_trees, scoring_history$validation_MSE, type="l") #validation mse

## get the actual number of trees
ntrees <- best_model@model$model_summary$number_of_trees
print(ntrees)

#######deeeeeep learning

