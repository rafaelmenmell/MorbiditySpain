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

ll.gripe$week <- week(ll.gripe$fecha)
ll.gripe.semanal <- ll.gripe %>% group_by(year=year(fecha),week=week) %>% summarise(sum=sum(total))

y <- ts(ll.gripe.semanal$sum, start=2005, frequency=52)

ggseasonplot(y, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Total") + ggtitle("Seasonal plot: casos semanales de gripe")

ggseasonplot(y, polar=TRUE) +
  ylab("Total") + ggtitle("Polar seasonal plot: casos semanales de gripe")

ggsubseriesplot(y) + ylab("Total") +
  ggtitle("Seasonal subseries plot: casos semanales de gripe")

ggAcf(y,lag.max = 520)

y2 <- window(y,start=2005,end=c(2014,52))
# Plot some forecasts
autoplot(y2) +
  forecast::autolayer(meanf(y2, h=52), PI=FALSE, series="Mean") +
  forecast::autolayer(naive(y2, h=52), PI=FALSE, series="Naïve") +
  forecast::autolayer(snaive(y2, h=52), PI=FALSE, series="Seasonal naïve") +
  ggtitle("Forecasts for weekly flue cases") +
  xlab("Year") + ylab("Total") +
  guides(colour=guide_legend(title="Forecast"))

res <- residuals(naive(y2))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")
ggAcf(res) + ggtitle("ACF of residuals")
Box.test(res, lag=52, fitdf=0)

checkresiduals(naive(y2))


y2 <- window(y,start=2005,end=c(2014,52))
fit1 <- meanf(y2,h=52)
fit2 <- rwf(y2,h=52)
fit3 <- snaive(y2,h=52)
autoplot(window(y, start=2005)) +
  forecast::autolayer(fit1, series="Mean", PI=FALSE) +
  forecast::autolayer(fit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(fit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Total") +
  ggtitle("Forecasts for weekly flue cases") +
  guides(colour=guide_legend(title="Forecast"))

y3 <- window(y, start=2008)
accuracy(fit1, y3)
accuracy(fit2, y3)
accuracy(fit3, y3)

e <- tsCV(y, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(rwf(y, drift=TRUE))^2, na.rm=TRUE))


e <- matrix(NA_real_, nrow = 582, ncol = 8)
for (h in seq_len(8)){
  e[, h] <- tsCV(y, forecastfunction = naive, h = h)
}

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

autoplot(naive(y2))

fit.y <- tslm(y2 ~ trend + season)
summary(fit.y)

autoplot(y2, series="Data") +
  forecast::autolayer(fitted(fit.y), series="Fitted") +
  xlab("Year") + ylab("Total") +
  ggtitle("Weekly Flue Cases")

fourier.y <- tslm(y2 ~ trend + fourier(y2, K=2))
summary(fourier.y)

checkresiduals(fit.y)
checkresiduals(fourier.y)

fit <- tslm(y2 ~ trend + season)
cbind(Fitted=fitted(fit), Residuals=residuals(fit)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

y2 <- window(y,start=2005,end=c(2014,52))
fit.y <- tslm(y2 ~ trend + season)
fcast <- forecast(fit.y)
autoplot(fcast) +
  ggtitle("Forecasts of weekly fule cases")

ma5 <- ma(y, 3)
autoplot(y, series="Data") +
  forecast::autolayer(ma(y,5), series="5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("Weekly flue cases") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

autoplot(y, series="Data") +
  forecast::autolayer(ma(y, 52), series="52-MA") +
  xlab("Year") + ylab("Total") +
  ggtitle("Weekly flue cases") +
  scale_colour_manual(values=c("Data"="grey","52-MA"="red"),
                      breaks=c("Data","52-MA"))

y %>% decompose(type="multiplicative") %>% 
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of flue cases")

y %>%
  stl(s.window=52, robust=TRUE) %>%
  autoplot

fit <- stl(y,s.window=52, robust=TRUE)

fit %>% seasadj() %>% naive() %>% autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

fit %>% forecast(method="naive") %>% autoplot() + ylab("New orders index")
