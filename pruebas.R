if(Sys.info()['sysname']=="windows"){
  install.packages("MorbiditySpain_0.1.0.zip",repos = NULL,type = "binary")
} else {
  install.packages("MorbiditySpain_0.1.0.tar.gz",repos = NULL, type="source")
}
library(readr)
library(dplyr)
library(MorbiditySpain)
library(lubridate)

if (file.exists("morbi_data.rds")==FALSE){
  ll <- GetMorbiData(y1 = 2005,y2 = 2015)
  saveRDS(ll,"morbi_data.rds")
} else {
  ll <- readRDS("morbi_data.rds")
}
ll <- ll %>% FilterProvincia(28) %>% FilterEmergency() %>% filter(edad<17)
ll2 <- ll %>% FilterDiagnosis2(57)
ll_diag1 <- AddDiagnosis1(ll2)
ll_diag1 <- AddDiagnosis2(ll_diag1)
ll_diag1 <- AddDiagnosis3(ll_diag1)

ll.month <- ll %>% ReduceData(provincia = TRUE,date = "month",diag = "diag1")

ll.month %>% SetPrevalence()

