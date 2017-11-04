ReduceData <- function(data,provincia=TRUE,date="day",diag=NULL){
  if(provincia){
    data <- data %>% dplyr::group_by(prov)
  }
  if (!(date %in% c("day","month","year"))){
    stop("We only know about daily, monthly,yearly reduction")
  }
  if(date=="day"){
    data <- data %>% dplyr::group_by(fecha=fecha_ingreso,add = TRUE)
  }
  if(date=="month"){
    data <- data %>% dplyr::group_by(month=as.Date(fecha_ingreso,format="%Y%m"),add=TRUE)
  }
  if(date=="year"){
    data <- data %>% dplyr::group_by(year=year(as.Date(fecha_ingreso)),add=TRUE)
  }
  if(!is.null(diag)){
    if (diag=="diag1"){
      if (!("diag1" %in% colnames(data))){
        data <- data %>% AddDiagnosis1()
      }
      data <- data %>% dplyr::group_by(diag1=diag1,add=TRUE)
    }
    if (diag=="diag2"){
      if (!("diag2" %in% colnames(data))){
        data <- data %>% AddDiagnosis2()
      }
      data <- data %>% dplyr::group_by(diag2=diag2,add=TRUE)
    }
    if (diag=="diag3"){
      if (!("diag3" %in% colnames(data))){
        data <- data %>% AddDiagnosis3()
      }
      data <- data %>% dplyr::group_by(diag3=diag3,add=TRUE)
    }
    if (!(diag %in% c("diag1","diag2","diag3"))){
      stop("We only know about diag1, diag2, diag3 reduction")
    }
  }
  data <- data %>% dplyr::summarise(total=n())
}
