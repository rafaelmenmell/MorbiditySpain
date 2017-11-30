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
library(dygraphs)
library(xts)
library(forecast)

if (file.exists("morbi_data.rds")==FALSE){
  ll <- GetMorbiData(y1 = 2005,y2 = 2015)
  saveRDS(ll,"morbi_data.rds")
} else {
  ll <- readRDS("morbi_data.rds")
}

ll.gripe <- ll %>% FilterProvincia(28) %>% FilterEmergency() %>% filter(edad<=16) %>% FilterDiagnosis2(57) %>% ReduceData(provincia = FALSE,date="day",sex = FALSE)
#2014 no
ll.gripe <- ll.gripe %>% filter(year(fecha)!=2004)
#faltan los días con 0 casos
fechas <- seq(min(ll.gripe$fecha),max(ll.gripe$fecha),by="1 day")
fechas <- data.frame(fecha=fechas)
ll.gripe <- full_join(fechas,ll.gripe,by="fecha")

ll.gripe[is.na(ll.gripe$total),]$total <- 0

ll.gripe$yday <- yday(ll.gripe$fecha)
ll.gripe.clim <- ll.gripe %>% group_by(yday) %>% summarise(mean=mean(total,na.rm=TRUE))

ll.gripe <- full_join(ll.gripe,ll.gripe.clim,by="yday")
#climatologia suabizada 11 dias
ll.gripe$clima.suave <- rollmean(ll.gripe$mean,k = 21,fill = NA)
clima.suave <- ll.gripe %>% filter(year(fecha)==2012) %>% select(yday,clima.suave)

xts.gripe <- xts(x = ll.gripe$total,order.by = ll.gripe$fecha)
dy <- dygraph(xts.gripe) %>% dyRangeSelector()
g4 <- ggplot(ll.gripe) + geom_line(aes(x=fecha,y=total))

y <- ts(ll.gripe$total, start=2005, frequency=365.25)
#grafico de autocorrelaciones
ggAcf(y,lag.max = 365)
#trainig_data 2005-2014, test 2015
train <- window(y,start=2005,end=c(2014,12))
test <- window(y,start=c(2015,1))
#descomponemos
train %>% decompose(type="multiplicative") %>% 
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition")

train  %>%
  stl(s.window=365, robust=TRUE) %>%
  autoplot

fit <- stl(train,  s.window=365, robust=TRUE)
fit %>% seasadj() %>% naive(h = 365) %>% autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")
fit %>% forecast(method="naive") %>% autoplot() + ylab("New orders index")
fcast <- stlf(eeadj, method='naive')
fc <- ses(train,h = 365)
round(accuracy(fc),2)
autoplot(fc) +
  forecast::autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")


fc <- holt(train, h=365)
fc2 <- holt(train, damped=TRUE, phi = 0.9, h=365)
autoplot(train) +
  forecast::autolayer(fc, PI=FALSE, series="Holt's method") +
  forecast::autolayer(fc2, PI=FALSE, series="Damped Holt's method") +
  ggtitle("Forecasts from Holt's method") +
  xlab("Year") + ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))


train %>% diff(lag=365) %>% ggtsdisplay
train %>% diff(lag=365) %>% diff() %>% ggtsdisplay

fit <- train %>%  Arima(order=c(2,0,1), xreg=fourier(train, K=4))
autoplot(test) + autolayer(forecast(fit, h=365.25, xreg=fourier(train, K=4, h=365.25)))
fit %>% forecast(h=365) %>% autoplot

####prueba de predicciones concatenadas con el año anterior, alcance 10
fechas <- seq(as.Date("2006-01-01"),as.Date("2015-12-20"),by="1 day")
predicciones <- data.frame(fecha=fechas,pred=NA)
for (i in 1:length(fechas)){
  f1 <- fechas[i]
  print(f1)
  f2 <- f1 - days(365)
  train <- ll.gripe %>% filter(fecha<f1) %>% filter(fecha>=f2)
  #train <- xts(train$total,order.by = train$fecha)
  y <- ts(train$total, start = 2015,frequency=365.25)
  fit <- y %>%  Arima(order=c(2,0,1), xreg=fourier(y, K=4))
  pred <- fit %>% forecast(h=1, xreg=fourier(y, K=4, h=10))
  pred <- as.numeric(pred$mean[10])
  f3 <- f1 + days(10)
  predicciones[predicciones$fecha==f3,]$pred <- pred
}

ll.gripe.pred <- inner_join(ll.gripe,predicciones,by="fecha")
xts.pred <- xts(x = ll.gripe.pred[,c("total","pred")],order.by = ll.gripe.pred$fecha)
dy2 <- dygraph(xts.pred) %>% dyRangeSelector() %>% dySeries(name="total",color="red") %>% dySeries(name="pred",color = "blue")
