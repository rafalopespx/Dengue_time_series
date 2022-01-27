library(vroom)
library(dplyr)
library(readr)
library(foreign)
library(lubridate)
library(multidplyr)

setwd("~/Desktop/Dengue_cases/")

dbf_files<-vroom("Data/dbf_final_aggregated_2000_2018.csv.xz")
dbc_files<-vroom("Data/dbc_final_aggregated_2013_2020.csv.xz")

dbc_files <- dbc_files %>% 
  mutate(NU_IDADE_N = as.factor(NU_IDADE_N), 
         IDADE = as.numeric(IDADE))

dbf_files<-dbf_files %>% 
  mutate(NU_IDADE_N = as.factor(NU_IDADE_N), 
         IDADE = as.numeric(IDADE))

## All Together version
### From 2013 to 2020 dbc files, due to being more updated
### From 2012 to 2000 dbf files
dbf_files_sin_pri <- dbf_files %>%
  filter(year(DT_SIN_PRI) < 2013)

dbf_files_notific<-dbf_files %>% 
  filter(year(DT_NOTIFIC) < 2013)

final_all_notific <- rbind(dbf_files_notific, dbc_files)
## 20,898,363 lines, with dbfs from 2000 to 2013 and dbcs from 2013 to 2020, by Notification date
write_csv(final_all_notific, 
          file = xzfile("Data/all_final_aggregated_notific_2000_2020.csv.xz", compression = -9))

final_all_sin_pri <- rbind(dbf_files_sin_pri, dbc_files)
## 20,917,525 lines, with dbfs from 2000 to 2013 and dbcs from 2013 to 2020, by onset of symptoms date
write_csv(final_all_sin_pri, 
          file = xzfile("Data/all_final_aggregated_sin_pri_2000_2020.csv.xz", compression = -9))

# inner_all<-inner_join(dbc_files, dbf_files)
# ## 8,214,275 lines, with dbf from 2000 to 2018 and dbc from 2013 to 2020
# 
# full_all<-full_join(dbc_files, dbf_files)
# ## 21,391,376 lines, with dbf from 2000 to 2018 and dbc from 2013 to 2020

final_list<-list(dbc = dbc_files, dbf = dbf_files)
final_list<-final_list %>% 
  bind_rows(.id = "type")
## 29,549,225 lines, without any filtering for year on dates of symptom onset or notification date

write_csv(final_list, 
          file = xzfile("Data/all_final_aggregated_2000_2020.csv.xz", compression = -9))


