library(microdatasus)
library(read.dbc)
library(vroom)
library(dplyr)
library(parallel)
library(foreach)
library(doParallel)
library(readr)
library(foreign)
library(lubridate)

setwd("~/Desktop/Dengue_cases/")

## Organizing everything in a working dataframe
### dbcs
# DBC files from the microdatasus package
dbc_names<-list.files("../Dengue Cases/ftp_sinan/")
dbc_names<-stringr::str_c("../Dengue Cases/ftp_sinan/", dbc_names)

dbc_files<-lapply(dbc_names, read.dbc)

# for(i in 1:length(dbc_files)){
#   
#   write_csv(dbc_files[[i]], 
#             file = xzfile(paste0("ftp_sinan_aggregated/sinan_dengue_20", years_dbc[i], ".csv.xz"), 
#                           compression = -9))
#   print(paste0(years_dbc[i], " Done!"))
# }
dbc_files <- dbc_files %>% 
  bind_rows()
dbc_files <- dbc_files %>% 
  select(DT_NOTIFIC, DT_SIN_PRI, DT_ENCERRA, DT_OBITO, 
         DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_MN_RESI, ID_MUNICIP, 
         CLASSI_FIN, EVOLUCAO, SOROTIPO, CRITERIO, RESUL_PCR_) %>% 
  mutate(IDADE = ifelse(
    substr(NU_IDADE_N, 1, 1) == "4",
    ifelse(substr(NU_IDADE_N, 2, 2) == "1",
           substr(NU_IDADE_N, 2, 4), 
           substr(NU_IDADE_N, 3, 4)
    ),
    0)
  )

write_csv(dbc_files,
          file = xzfile("Data/dbc_final_aggregated_2013_2020.csv.xz", compression = -9))

### dbfs
# DBF files from the e-SIC request
dbf_names<-list.files("../Dengue Cases/raw_files_dengue/dbf/")
dbf_names<-stringr::str_c("../Dengue Cases/raw_files_dengue/dbf/", dbf_names)

dbf_files<-lapply(dbf_names, read.dbf)

# for(i in 1:length(dbf_files)){
#   data_to_write<-dbf_files[[i]] # It has be like this to avoid stack overflow!
#   write_csv(data_to_write, 
#             file = xzfile(paste0("dbf_sinan_aggregated/sinan_dengue_", years_dbf[i], ".csv.xz"), 
#                           compression = -9))
#   print(paste0(years_dbf[i], " Done!"))
# }
years_dbf<-seq(2000, 2018, 1)

names(dbf_files)<-years_dbf

uniform_names<-c("DT_NOTIFIC", "DT_SIN_PRI", "DT_ENCERRA", "DT_OBITO",
                 "DT_NASC", "NU_IDADE_N", "CS_SEXO","CS_RACA","CS_ESCOL_N",
                 "ID_MN_RESI","ID_MUNICIP","CLASSI_FIN","EVOLUCAO","SOROTIPO","CRITERIO" ,"RESUL_PCR_")

for (i in names(dbf_files)[1:7]) {
 data<-dbf_files[[i]]  
 data <- data %>%
   select(DT_NOTIFIC, DT_SIN_PRI, CON_DT_ENC, CON_DT_OBI,
          DT_NASC, NU_IDADE, CS_SEXO, CS_RACA, CS_ESCOLAR, ID_MN_RESI, ID_MUNICIP,
          CON_CLASSI, CON_EVOLUC, RESUL_VIRA, CON_CRITER, RESUL_PCR)
 names(data)<-uniform_names
 dbf_files[[i]]<-data
}

for (i in names(dbf_files)[8:18]){
  data<-dbf_files[[i]] %>% 
    select(DT_NOTIFIC, DT_SIN_PRI, DT_ENCERRA, DT_OBITO, 
           DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_MN_RESI, ID_MUNICIP, 
           CLASSI_FIN, EVOLUCAO, SOROTIPO, CRITERIO, RESUL_PCR_)
  names(data)<-uniform_names
  dbf_files[[i]]<-data
}

columns_2018<-dbf_files$`2018`

columns_2018 <- columns_2018 %>% 
  mutate(IDADE = ifelse(
    substr(NU_IDADE_N, 1, 1) == "4",
    ifelse(substr(NU_IDADE_N, 2, 2) == "1",
           substr(NU_IDADE_N, 2, 4), 
           substr(NU_IDADE_N, 3, 4)
    ),
    0),
    DT_NASC = DT_SIN_PRI - lubridate::years(IDADE)
    ) %>% 
  select(DT_NOTIFIC, DT_SIN_PRI, DT_ENCERRA, DT_OBITO, 
         DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_MN_RESI, ID_MUNICIP, 
         CLASSI_FIN, EVOLUCAO, SOROTIPO, CRITERIO, RESUL_PCR_)
  

dbf_files$`2018` <- columns_2018

names_dbf_list<-lapply(dbf_files, names)
names_dbf_list <- names_dbf_list %>% 
  bind_rows()

mutate_idade<-function(x){
  x <- x %>% 
    mutate(IDADE = ifelse(
      substr(NU_IDADE_N, 1, 1) == "4" | substr(NU_IDADE_N, 1, 1) == "A",
      ifelse(substr(NU_IDADE_N, 2, 2) == "1",
             substr(NU_IDADE_N, 2, 4), 
             substr(NU_IDADE_N, 3, 4)
      ),
      0),
      NU_IDADE_N = as.factor(NU_IDADE_N),
      IDADE = as.integer(IDADE),
      CS_RACA = as.factor(CS_RACA),
      CS_SEXO = as.factor(CS_SEXO),
      CS_ESCOL_N = as.factor(CS_ESCOL_N), 
      ID_MN_RESI = as.factor(ID_MN_RESI),
      ID_MUNICIP = as.factor(ID_MUNICIP),
      CLASSI_FIN = as.factor(CLASSI_FIN),
      EVOLUCAO = as.factor(EVOLUCAO), 
      SOROTIPO = as.factor(SOROTIPO),
      CRITERIO = as.factor(CRITERIO), 
      RESUL_PCR_ = as.factor(RESUL_PCR_)
    )
  return(x)
}

dbf_files <- lapply(dbf_files, mutate_idade)

dbf_files <- dbf_files %>% 
  bind_rows()

write_csv(dbf_files,
          file = xzfile("Data/dbf_final_aggregated_2000_2018.csv.xz", compression = -9))
  

# columns_2007<-dbf_files$`2007`
# 
# columns_2007<-columns_2007 %>% 
#   select(DT_NOTIFIC, DT_SIN_PRI, DT_ENCERRA, DT_OBITO, 
#          DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_MN_RESI, ID_MUNICIP, 
#          CLASSI_FIN, EVOLUCAO, SOROTIPO, CRITERIO, RESUL_PCR_)
# 
# dbf_files$`2007`<-columns_2007
# 
# columns_2008<-dbf_files$`2008`
# 
# columns_2008 <- columns_2008 %>% 
#   select(DT_NOTIFIC, DT_SIN_PRI, DT_ENCERRA, DT_OBITO, 
#          DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_MN_RESI, ID_MUNICIP, 
#          CLASSI_FIN, EVOLUCAO, SOROTIPO, CRITERIO, RESUL_PCR_)
# 
# dbf_files$`2008`<-columns_2008
# 
# for (i in names(dbf_files)[10:14]) {
#   data<-dbf_files[[i]] %>% 
#     # data <- data %>% 
#     select(DT_NOTIFIC, DT_SIN_PRI, DT_ENCERRA, DT_OBITO, 
#            DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_MN_RESI, ID_MUNICIP, 
#            CLASSI_FIN, EVOLUCAO, SOROTIPO, CRITERIO, RESUL_PCR_)
#   names(data)<-uniform_names
#   dbf_files[[i]]<-data
# }
# 
# colums_2014<- dbf_files$`2014`
# 
# colums_2014 <- colums_2014 %>% 
#   select(DT_NOTIFIC, DT_SIN_PRI, DT_ENCERRA, DT_OBITO, 
#          DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_MN_RESI, ID_MUNICIP, 
#          CLASSI_FIN, EVOLUCAO, SOROTIPO, CRITERIO, RESUL_PCR_)
# 
# dbf_files$`2014`<-colums_2014
# 
# for (i in names(dbf_files)[16:17]) {
#   data<-dbf_files[[i]]
#   data<-data %>% 
#     select(DT_NOTIFIC, DT_SIN_PRI, DT_ENCERRA, DT_OBITO, 
#            DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_MN_RESI, ID_MUNICIP, 
#            CLASSI_FIN, EVOLUCAO, SOROTIPO, CRITERIO, RESUL_PCR_)
#   names(data)<-uniform_names
#   dbf_files[[i]]<-data
# }
# 
# columns_2017<-dbf_files$`2017`
# 
# columns_2017 <- columns_2017 %>% 
#   select(DT_NOTIFIC, DT_SIN_PRI, DT_ENCERRA, DT_OBITO, 
#          DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_MN_RESI, ID_MUNICIP, 
#          CLASSI_FIN, EVOLUCAO, SOROTIPO, CRITERIO, RESUL_PCR_)
# 
# dbf_files$`2017` <- columns_2017