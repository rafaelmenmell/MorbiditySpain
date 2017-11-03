#generar el fichero con datos auxiliares
#ver http://r-pkgs.had.co.nz/data.html#data-sysdata

provincias <- read.csv("provincias_code.txt",header=TRUE,sep=";")
diag1 <- read.csv("diccionario_mas_general.txt",header=TRUE,sep=";")
diag2 <- read.csv("diccionario_general.txt",header = TRUE,sep=";")

save(list = c("provincias","diag1","diag2"),file = "MorbiditySpain/R/sysdata.rda")
