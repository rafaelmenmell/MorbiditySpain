data <- MorbiditySpain::GetMorbiData(y1 = 2014,y2 = 2015)

data.ejemplo <- data %>% MorbiditySpain::FilterProvincia(provincia = 28) %>% MorbiditySpain::FilterEmergency() %>% MorbiditySpain::FilterDiagnosis2(diagnosis_id = 77) %>% MorbiditySpain::AddDiagnosis3()

data.ejemplo2 <- data %>% MorbiditySpain::FilterProvincia(provincia = 28) %>% MorbiditySpain::FilterEmergency() %>% MorbiditySpain::ReduceData()


#cuál es el día con mas ingresos urgencias en madri

data <- MorbiditySpain::GetMorbiData()
data.ejemplo2 <- data %>% MorbiditySpain::FilterProvincia(provincia = 28) %>% MorbiditySpain::FilterEmergency() %>% MorbiditySpain::ReduceData() %>% MorbiditySpain::SetPrevalence(pop = "total")

#ggplot(data.ejemplo2 %>% dplyr::filter(fecha>="2005-01-01"),aes(x=fecha,y=total)) + geom_line()
library(dygraphs)
library(xts)
data.ejemplo2 <- data.ejemplo2 %>% dplyr::filter(fecha>="2005-01-01")
xts <- xts(x = data.ejemplo2$total.prev,order.by = data.ejemplo2$fecha)
dygraph(data = xts) %>% dyRangeSelector()

data <- MorbiditySpain::GetMorbiData(y1 = 1978,y2 = 1979)
