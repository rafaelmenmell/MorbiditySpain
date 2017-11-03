ReadZip <- function(year){
  #descomprime
  filezip <- sprintf("https://github.com/rafaelmenmell/MorbiditySpain/raw/master/data/datos_morbi%s.zip",substr(year,3,4))
  temp <- tempfile()
  download.file(filezip,temp)
  fileu <- unzip(temp,list=TRUE)$Name
  unzip(temp)
  unlink(temp)
  #data <- read.fwf(fileu,widths = c(8,2,1,2,1,6,4,1,3,2,2,6,8,8),colClasses=rep("character",14))
  data <- read_fwf(fileu,fwf_widths(c(8,2,1,2,1,6,4,1,3,2,2,6,8,8)))
  colnames(data) <- c("numero","prov_hosp","sexo","prov_res","diag_in","fecha_alta","diag_ppal","motivo_alta","edad_anyos","edad_meses","edad_dias","estancia","elevacion","filler")
  #vamos a hacer unos cast para reducir el tamaño del data frame
  data$numero <- as.integer(data$numero)
  data$prov_hosp <- as.integer(data$prov_hosp)
  data$sexo <- as.integer(data$sexo)
  data$prov_res <- as.integer(data$prov_res)
  data$fecha_alta <- ISOdate(year=as.integer(substr(data$fecha_alta,1,2))+2000,month = as.integer(substr(data$fecha_alta,3,4)),day = as.integer(substr(data$fecha_alta,5,6)))
  data$motivo_alta <- as.integer(data$motivo_alta)
  data$edad_anyos <- as.integer(data$edad_anyos)
  data$edad_meses <- as.integer(data$edad_meses)
  data$edad_dias <- as.integer(data$edad_dias)
  data$estancia <- as.integer(data$estancia)
  data$fecha_ingreso <- as.Date(data$fecha_alta-data$estancia*24*60*60)
  data$edad <- as.integer(round(data$edad_anyos+data$edad_meses/12+data$edad_dias/365))
  data$edad_anyos <- NULL
  data$edad_meses <- NULL
  data$edad_dias <- NULL
  data$filler <- NULL
  data$elevacion <- NULL
  data$numero <- NULL
  data$fecha_alta <- NULL
  unlink(fileu)
  return(data)
}


GetMorbiData <- function(y1=2005,y2=2015){
  ys <- y1:y2
  data.m <- vector("list",length(ys))
  n <- 1
  for (y in ys){
    #print(y)
    data.m[[n]] <- suppressMessages(ReadZip(y))
    n <- n+1
  }
  data.m <- bind_rows(data.m)
  return(data.m)
}

FilterProvincia <- function(data,provincia){
  if (!(provincia %in% 1:52)){
    print(provincias)
    stop("provincia must be an integer between 1 and 52")
  }
  data <- data %>% filter(prov_hosp==provincia)
  return(data)
}

FilterEmergency <- function(data){
  data <- data %>% filter(diag_in==2)
  return(data)
}

FilterDiagnosis1 <- function(data,diagnosis_id){
  if (!(diagnosis_id %in% 1:17)) {
    print(diag1)
    stop("diagnosis_id must be an integer between 1 and 17")
  }
  dd <- diag1 %>% filter(id == diagnosis_id)
  data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
  data <- data %>% filter(temp >= dd$start) %>% filter(temp <= dd$end) %>% select(-temp)
  return(data)
}

FilterDiagnosis2 <- function(data,diagnosis_id){
  if (!(diagnosis_id %in% 1:123)) {
    print(diag2)
    stop("diagnosis_id must be an integer between 1 and 123")
  }
  dd <- diag2 %>% filter(id == diagnosis_id)
  if (!dd$V) {
    data <- data %>% filter(grepl("V",diag_ppal) == FALSE)
    data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
    data <-  data %>% filter(temp >= dd$start) %>% filter(temp <= dd$end) %>% select(-temp)
  } else {
    data <- data %>% filter(grepl("V",diag_ppal) == TRUE)
    data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,2))
    data <- data  %>% filter(temp >= as.numeric(gsub("V","",dd$start))) %>% filter(temp <= as.numeric(gsub("V","",dd$end))) %>% select(-temp)
  }
  return(data)
}


