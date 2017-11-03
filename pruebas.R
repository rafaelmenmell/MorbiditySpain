install.packages("MorbiditySpain_0.1.0.zip",repos = NULL,type = "binary")
library(readr)
library(dplyr)
library(MorbiditySpain)

# ll <- GetMorbiData(y1 = 2005,y2 = 2006)

ll %>% FilterProvincia(28) %>% FilterEmergency() %>% FilterDiagnosis1(1)
ll %>% FilterProvincia(28) %>% FilterDiagnosis2(113)
