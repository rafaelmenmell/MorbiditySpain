#prueba real
#prevalencia por provincias de ingresos en urgencias de menores de 16 años relacionadas con alcohol y drogas. 2005-2015

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

ll <- GetMorbiData(y1 = 2005,y2 = 2015)

ll2 <- ll %>% FilterEmergency() %>% filter(edad<17) %>% FilterDiagnosis2(35)
ll2 <- ll2 %>% AddDiagnosis3() %>% ReduceData(provincia = TRUE,date = "year",diag = "diag3")
diag2.35 <- unique(ll2$diag3)
diag2.35 <- diag2.35[grepl("alcohol",tolower(diag2.35))]
ll2 <- ll2 %>% filter(diag3 %in% diag2.35)
ll2 <- ll2 %>% SetPrevalence()
ll2 <- ll2 %>% dplyr::group_by(prov,fecha) %>% dplyr::summarise(total=sum(total.prev))
ll2$code <- sprintf("%02d",ll2$prov)
ll2$year <- year(ll2$fecha)
prov.graf <- geofacet::spain_prov_grid1
ll2 <- full_join(ll2,prov.graf,by="code")

ll2.media <- mean(ll2$total,na.rm=TRUE)

g <- ggplot(data=ll2) + geom_bar(aes(x=year,y=total),stat="identity",position="dodge") + geom_hline(yintercept = ll2.media,color="red") + facet_geo(~ name, grid = "spain_prov_grid1") +labs(title="Prevalencia de ingresos urgentes relacionados con alcohol en menores",subtitle="Casos por cada 1000 habitantes",caption="Encuesta de morbilidad.2005-2015") + xlab("") + ylab("") + theme_bw()

# Esguinces en hombres/mujeres entre 30-45 años

lesiones <- ll %>% FilterEmergency() %>% filter(edad>=30 & edad<=45) %>% FilterDiagnosis2(96) %>% AddDiagnosis3()
lesiones <- lesiones %>% ReduceData(provincia = TRUE,date = "day",diag = "diag3",sex=TRUE)

lesiones.y <- lesiones %>% group_by(diag=diag3,sex=sex) %>% summarise(total=sum(total))
esguinces <- lesiones.y %>% group_by(diag) %>% summarise(tt=sum(total)) %>% top_n(10,tt)
esguinces <- esguinces$diag
lesiones.y <- lesiones.y %>% filter(diag %in% esguinces)
lesiones.y$sex <- factor(x = lesiones.y$sex,labels = c("Hombre","Mujer"))

g2 <- ggplot(data=lesiones.y) + geom_bar(aes(x=sex,y=total),stat="identity",position="dodge") + facet_wrap(~diag,nrow = 2,ncol = 5,scales = "free") + labs(title="Prevalencia de ingresos urgentes relacionados con esguinces por sexo",subtitle="Casos totales",caption="Encuesta de morbilidad.2005-2015") + xlab("Sexo") + ylab("") + theme_bw()

#Myth buster

partos <- ll %>% FilterEmergency() %>% FilterDiagnosis2(77) %>% AddDiagnosis3() %>% ReduceData(provincia = FALSE,date = "day",sex = FALSE)
#los datos de 2014 son kk
partos <- partos %>% filter(year(fecha)!=2004)
library(lunar)
partos$phase <- lunar.phase(partos$fecha,name=8)
partos <- partos %>% group_by(phase) %>% summarise(total=sum(total))

g3 <- ggplot(partos) + geom_bar(aes(x=phase,y=total),stat="identity",position = "dodge") + labs(title="Número de partos y fase lunar",subtitle="Casos totales",caption="Encuesta de morbilidad.2005-2015") + xlab("Fase Lunar") + ylab("") + theme_bw()
