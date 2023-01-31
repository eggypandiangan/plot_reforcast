library(tidyr)
library(plyr)
library(readxl)
library(ggplot2)
library(openxlsx)
library(plotrix)
library(openair)
library(gridExtra)
# library(Metrics)

ubah_nzom <- function(nama_zom){
  if (class(nama_zom)=='character') {
    nama_zom[nama_zom=="ActiveX VT_ERROR: "] <- 'ACEH_01'
    nzom_baru <- paste0(gsub("[[:digit:]]","",nama_zom),sprintf("%02d",as.numeric(gsub("[^0-9]+", " ", nama_zom, fixed = F))))
  } else {
    nzom_baru <- norm[match(nama_zom,norm$NOZOM_NASIONAL),'NOZOM_PROVINSI']
  }
  return(nzom_baru)
}

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

load('nzom.RData')

rmse <- function(actual,model){
  hasil <- sqrt(mean((actual - model)^2))
  return(hasil)
}