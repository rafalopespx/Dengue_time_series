library(microdatasus)
library(read.dbc)
library(vroom)
library(purrr)
library(furrr)
library(dplyr)
library(ggplot2)

fetch_datasus_all<-function(year_start, year_end, uf, information_system){
  pre<-fetch_datasus(year_start = year_start, 
                     year_end = year_end, 
                     # uf = uf, 
                     information_system = paste0(information_system, "-PRELIMINAR"))
  final<-fetch_datasus(year_start = year_start, 
                       year_end = year_end, 
                       # uf = uf, 
                       information_system = paste0(information_system, "-FINAL"))
  data_final<-dplyr::bind_rows(pre, final)
  return(data_final)
}

dengue_list<-list()
for (i in 2013:2020) {
  dengue_list[[i]]<-fetch_datasus_all(year_start = i, year_end = i, information_system = "SINAN-DENGUE")
}




