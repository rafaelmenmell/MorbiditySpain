install.packages("MorbiditySpain_0.1.0.zip",repos = NULL,type = "binary")
library(readr)
library(dplyr)
library(MorbiditySpain)

ll <- GetMorbiData(y1 = 2005,y2 = 2015)

ll <- ll %>% FilterProvincia(28) %>% FilterEmergency() %>% filter(edad<17)
ll2 <- ll %>% FilterDiagnosis2(57)
ll_diag1 <- AddDiagnosis1(ll2)
ll_diag1 <- AddDiagnosis2(ll_diag1)
ll_diag1 <- AddDiagnosis3(ll_diag1)
