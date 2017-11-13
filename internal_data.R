#generar el fichero con datos auxiliares
#ver http://r-pkgs.had.co.nz/data.html#data-sysdata
library(tidyr)

provincias <- read.csv("provincias_code.txt",header=TRUE,sep=";",stringsAsFactors = FALSE)
diag1 <- read.csv("diccionario_mas_general.txt",header=TRUE,sep=";",stringsAsFactors = FALSE)
diag2 <- read.csv("diccionario_general.txt",header = TRUE,sep=";",stringsAsFactors = FALSE)
poblacion <- read.csv(file = "poblacion_provincias.csv",header = TRUE,sep = ";",stringsAsFactors = FALSE,skip = 1)

pob.df <- data.frame(tipo=expand.grid(c("total","males","females"),provincias$nombre)[,1],provincia=expand.grid(c("total","males","females"),provincias$nombre)[,2])
for (i in 2000:2015){
  pob.df$col <- as.numeric(poblacion[poblacion$X==i,-1][-length(poblacion[poblacion$X==i,-1])])
  colnames(pob.df) <- c(colnames(pob.df)[colnames(pob.df)!="col"],i)
}
pob.df <- gather(pob.df,key = year,value = pob,3:18)
poblacion <- pob.df

# morbi.data <- GetMorbiData(y1 = 2005,y2 = 2015)

save(list = c("provincias","diag1","diag2","poblacion"),file = "MorbiditySpain/R/sysdata.rda")
