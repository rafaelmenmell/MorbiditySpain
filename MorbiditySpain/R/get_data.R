ReadZip <- function(year){
  #descomprime
  #filezip <- sprintf("https://github.com/rafaelmenmell/MorbiditySpain/raw/master/data/datos_morbi%s.zip",substr(year,3,4))
  filezip <- sprintf("ftp://www.ine.es/temas/micordatos/datos_%s.zip",year,3,4)
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
  data$fecha_alta <- ISOdate(year=as.integer(substr(data$fecha_alta,1,2))+trunc(year/100)*100,month = as.integer(substr(data$fecha_alta,3,4)),day = as.integer(substr(data$fecha_alta,5,6)))
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
  data <- data %>% dplyr::filter(prov_hosp==provincia)
  return(data)
}

FilterEmergency <- function(data){
  data <- data %>% dplyr::filter(diag_in==2)
  return(data)
}

FilterDiagnosis1 <- function(data,diagnosis_id){
  if (!(diagnosis_id %in% 1:17)) {
    print(diag1)
    stop("diagnosis_id must be an integer between 1 and 17")
  }
  dd <- diag1 %>% dplyr::filter(id == diagnosis_id)
  data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
  data <- data %>% dplyr::filter(temp >= dd$start) %>% dplyr::filter(temp <= dd$end) %>% select(-temp)
  return(data)
}

FilterDiagnosis2 <- function(data,diagnosis_id){
  if (!(diagnosis_id %in% 1:123)) {
    print(diag2)
    stop("diagnosis_id must be an integer between 1 and 123")
  }
  dd <- diag2 %>% dplyr::filter(id == diagnosis_id)
  if (!dd$V) {
    data <- data %>% dplyr::filter(grepl("V",diag_ppal) == FALSE)
    data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
    data <-  data %>% dplyr::filter(temp >= dd$start) %>% dplyr::filter(temp <= dd$end) %>% select(-temp)
  } else {
    data <- data %>% dplyr::filter(grepl("V",diag_ppal) == TRUE)
    data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,2))
    data <- data  %>% dplyr::filter(temp >= as.numeric(gsub("V","",dd$start))) %>% dplyr::filter(temp <= as.numeric(gsub("V","",dd$end))) %>% select(-temp)
  }
  return(data)
}

AddDiagnosis1 <- function(data){
  data$diag1 <- NA
  for (i in 1:nrow(diag1)){
    cat(sprintf("%s de %s\r",i,nrow(diag1)))
    start <- diag1[i,]$start
    end <- diag1[i,]$end
    id <- diag1[i,]$id
    data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
    if(nrow(data[data$temp>=start & data$temp<=end,])>0){
      data[data$temp>=start & data$temp<=end,]$diag1 <- id
    }
  }
  data <- data %>% select(-temp)
  return(data)
}

AddDiagnosis2 <- function(data){
  data$diag2 <- NA
  for (i in 1:nrow(diag2)){
    cat(sprintf("%s de %s\r",i,nrow(diag2)))
    start <- as.numeric(gsub("V","",diag2[i,]$start))
    end <- as.numeric(gsub("V","",diag2[i,]$end))
    id <- diag2[i,]$id
    if(!diag2[i,]$V){
      data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
      if (nrow(data[!grepl("V",data$diag_ppal) & data$temp>=start & data$temp<=end,])>0){
        data[!grepl("V",data$diag_ppal) & data$temp>=start & data$temp<=end,]$diag2 <- id
      }
    } else {
      data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,2))
      if (nrow(data[grepl("V",data$diag_ppal) & data$temp>=start & data$temp<=end,])>0){
        data[grepl("V",data$diag_ppal) & data$temp>=start & data$temp<=end,]$diag2 <- id
      }
    }
  }
  data <- data %>% select(-temp)
  return(data)
}

TraduceCodigoEspecifico <- function(codigo){
  if (nchar(codigo)==4){
    if (grepl("V",codigo)==FALSE){
      codigo <- paste(substr(codigo, 1, 3), ".", substr(codigo, 4, 4), sep = "")
    }
  }
  url <- sprintf("http://icd9cm.chrisendres.com/index.php?srchtype=diseases&srchtext=%s&Submit=Search&action=search",codigo)
  info <- readLines(url)
  info <- info[grepl(codigo,info)][2]
  info <- strsplit(info,codigo)[[1]][2]
  info <- gsub("</div>","",info)
  return(info)
}

AddDiagnosis3 <- function(data){
  codes <- unique(data$diag_ppal)
  data$diag3 <- NA
  for (code in codes){
    i <- 1
    cat(sprintf("%s de %s\r",i,length(codes)))
    diag3 <- TraduceCodigoEspecifico(code)
    data[data$diag_ppal==code,]$diag3 <- diag3
    i <- i + 1
  }
  return(data)
}

