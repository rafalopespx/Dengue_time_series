rm(list=ls())

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(rEDM)){install.packages("rEDM"); library(rEDM)}


## Loading Dengue Cases files
final_all_sin_pri<-vroom("Data/all_final_aggregated_sin_pri_2000_2020.csv.xz")
br_municipalities<-vroom("Data/municipalities_br.csv")

## Loading temperature files
t2m.file.list<-list.files("Data/era5land_data/")
t2m.file.list<-str_c("Data/era5land_data/", t2m.file.list)

# Files t2m
t2m_2009_2020<-lapply(t2m.file.list, vroom)
t2m_2009_2020<- t2m_2009_2020 %>% 
  bind_rows()
# Putting the values in Celsius scale and selection of columns with date correction
# t2m
t2m_2010_2019 <- t2m_2009_2020 %>% 
  mutate(date = ymd(date),
         code_muni = substr(code_muni, 1, 6), 
         temp_mean = weighted_mean - 273.15, 
         temp_max = weighted_max - 273.15,
         temp_min = weighted_min - 273.15,
  ) %>% 
  filter(year(date) > 2009 & year(date) < 2020) %>%
  dplyr::select(date, name_muni, code_muni, abbrev_state, code_state,
                temp_mean, temp_max, temp_min)

#### Correcting code muni, other problems ####
## Cities with code duplicated
cities_dup <- t2m_2010_2019 %>% 
  distinct(code_muni,name_muni)
cities_dup <- cities_dup %>% 
  group_by(code_muni) %>% 
  filter(n()>1)

## Cities missing mean temp
cities_missingtemp <- t2m_2010_2019 %>% 
  filter(is.na(temp_mean)) %>% 
  distinct(code_muni,name_muni)

## Now filtering the temp mean for the cities missing and duplicated
t2m_2010_2019 <-
  t2m_2010_2019 %>% 
  filter(!code_muni %in% cities_dup$code_muni) %>% 
  filter(!code_muni %in% cities_missingtemp$code_muni) 

vroom_write(t2m_2010_2019, file = "Data/t2m_muni_2010_2019.csv.xz")

# Dengue time series
dengue_cases_2010_2019<- final_all_sin_pri %>%
  filter(year(DT_SIN_PRI) > 2009 & year(DT_SIN_PRI) < 2020) %>% # Used the years that we have, so try to use before than 2009
  mutate(code_muni = ID_MN_RESI) %>% 
  group_by(DT_SIN_PRI, code_muni) %>% 
  summarise(N=n())

## changing cidades satelites to Brasilia code ##
dengue_cases_2010_2019 <-
  dengue_cases_2010_2019 %>% 
  mutate(
    code_muni = case_when(
      str_sub(code_muni,1,2)=="53"~530010,
      TRUE ~ code_muni
    )
  )

# collapsing again
dengue_cases_2010_2019<-
  dengue_cases_2010_2019 %>%
  group_by(DT_SIN_PRI, code_muni) %>% 
  summarise(N=sum(N))

dengue_cases_2010_2019<-
  dengue_cases_2010_2019 %>% 
  left_join(br_municipalities, by = "code_muni") %>% 
  setNames(c("date", "code_muni", "Cases", "name_muni")) %>% 
  mutate(code_muni = as.character(code_muni))

## Salving dengue cases 2009 to 2020
vroom_write(dengue_cases_2010_2019, file ="Data/dengue_cases_2010_2019.csv.xz")
# Saved!

# Complete, dengue hospitalizations and t2m
dengue_t2m<-t2m_2010_2019 %>% 
  mutate(code_muni = as.character(code_muni)) %>%
  left_join(dengue_cases_2010_2019, by = c("code_muni", "date", "name_muni"))

dengue_t2m<-dengue_t2m %>% 
  mutate(Cases = replace_na(Cases, 0)) # zero-inflated Cases Series

vroom_write(x = dengue_t2m, file = "Data/dengue_t2m_muni_2010_2019.csv.xz")
# Saved!

