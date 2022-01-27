rm(list=ls())

library(microdatasus)
library(read.dbc)
library(vroom)
library(purrr)
library(furrr)
library(dplyr)
library(ggplot2)
library(viridis)
library(lubridate)
library(scales)

source("Scripts/pega_pop_datasus_fx_regiao.R")

# final_all_notific<-vroom("Data/all_final_aggregated_notific_2000_2020.csv.xz")
final_all_sin_pri<-vroom("Data/all_final_aggregated_sin_pri_2000_2020.csv.xz")

plt_sin_pri<-final_all_sin_pri %>% 
  filter(year(DT_SIN_PRI)>= 2000 & year(DT_SIN_PRI) <= 2021) %>% 
  mutate(year = year(DT_SIN_PRI), 
         SEM_SIN = epiweek(DT_SIN_PRI)) %>% 
  group_by(DT_SIN_PRI, year, CLASSI_FIN) %>%
  summarise(N=n())

plt_notific<-final_all_notific %>% 
  filter(year(DT_NOTIFIC)>= 2000 & year(DT_NOTIFIC) <= 2021) %>% 
  mutate(year = year(DT_NOTIFIC), 
         SEM_NOT = epiweek(DT_NOTIFIC)) %>% 
  group_by(DT_NOTIFIC, year, CLASSI_FIN) %>%
  summarise(N=n())

plot_data<- ggplot()+
  # geom_line(data = plt_notific, aes(x = DT_NOTIFIC, y = N, color = "Notification Date"))+
  geom_line(data = plt_sin_pri, aes(x = DT_SIN_PRI, y = N, color = "After 2000"))+
  geom_line(data = plt_before_2000, aes(x = DT_SIN_PRI, y = N, color = "Before 2000"))+
  scale_x_date(name = "Date")+
  # scale_color_viridis(option = "turbo")+
  # facet_wrap(year~.,scales = "free")+
  theme_minimal()+
  labs(title = "Dengue Cases Brazil", y = "Cases Per day", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank())
plot_data

before_2000 <- final_all_sin_pri %>% 
  filter(year(DT_SIN_PRI) < 2000 & year(DT_SIN_PRI) > 1990) %>% 
  filter(DT_SIN_PRI != DT_NASC) # Filtering out dt_sin_pri equal to dt_nasc

after_2000 <- final_all_sin_pri %>% 
  filter(year(DT_SIN_PRI) >= 2000 & year(DT_SIN_PRI) < 2021)

plt_before_2000 <- before_2000 %>% 
  mutate(year = year(DT_SIN_PRI), 
         SEM_SIN_PRI = epiweek(DT_SIN_PRI)) %>% 
  group_by(year) %>%
  summarise(N=n())

plt_after_2000 <- after_2000 %>% 
  mutate(year = year(DT_SIN_PRI),
         SEM_SIN_PRI = epiweek(DT_SIN_PRI)) %>% 
  group_by(DT_SIN_PRI, year) %>%
  summarise(N=n())

plot_before <- plt_before_2000 %>% 
  ggplot()+
  geom_line(aes(x = year, y = N, color = "Before 2000"))+
  scale_color_manual(values = "skyblue")+
  theme_minimal()+
  labs(title = "Dengue Cases Brazil, Before 2000", y = "Nº Cases", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank())
plot_before

plot_after <- plt_after_2000 %>% 
  ggplot()+
  geom_line(aes(x = DT_SIN_PRI, y = N, color = year))+
  # scale_x_date(name = "Date")+
  scale_color_viridis_c(option = "turbo")+
  # scale_color_manual(values = "firebrick1")+
  theme_minimal()+
  labs(title = "Dengue Cases Brazil, After 2000", y = "Nº Cases", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank())
plot_after

library(patchwork)
library(geobr)

# relacional_ibge_regsaud<-read_csv("Scripts/rl_municip_regsaud.csv")
# relacional_ibge_regsaud<-relacional_ibge_regsaud %>% 
#   setNames(c("code_muni", "code_health_region"))

municipality_br <- read_municipality(year = 2010, simplified = T, showProgress = F)

state_br<-read_state(year = 2010, simplified = T, showProgress = F)

# health_region <- read_health_region(year = 2013, simplified = T, showProgress = F)

# macrohealth_region <- read_health_region(year = 2013, simplified = T, showProgress = F, macro = T)

pop_muni_2000_2020<- seq(2000, 2020, 1) %>%
  map_df(tabnet_pop,qsexo='T', qnivel='municipio') %>% 
  select(codreg, ano, Total) %>%
  setNames(nm=c("code_muni", "year", "total_pop"))

pop_state_2000_2020 <- seq(2000, 2020, 1) %>% 
  map_df(tabnet_pop, qnivel='uf') %>% 
  select(codreg, ano, Total) %>% 
  setNames(nm=c("code_state", "year", "total_pop"))

# pop_health_2000_2020 <- seq(2000,2020, 1) %>% 
#   map_df(tabnet_pop, qnivel='regional') %>% 
#   select(codreg, nome_regiao, ano, Total)%>% 
#   setNames(nm = c("code_health_region", "name_health_region", "year", "total_pop"))

# pop_macrohealth_2000_2020 <- seq(2000,2020, 1) %>% 
#   map_df(tabnet_pop, qnivel='macro')
# 
# pop_macrohealth_2000_2020<-pop_macrohealth_2000_2020 %>% 
#   select(codreg, nome_regiao, ano, Total) %>% 
#   setNames(nm = c("code_health_macroregion", "name_health_macroregion", "year", "total_pop"))

# before_2000 <- final_all_sin_pri %>% 
#   filter(year(DT_SIN_PRI) < 2000 & year(DT_SIN_PRI) >= 1990) %>% 
#   filter(DT_SIN_PRI != DT_NASC) %>%  # Filtering out dt_sin_pri equals to dt_nasc
#   mutate(code_6 = substr(ID_MN_RESI, 1,6))

after_2000 <- final_all_sin_pri %>% 
  filter(year(DT_SIN_PRI) >= 2000 & year(DT_SIN_PRI) < 2021) %>% 
  mutate(code_muni = ID_MN_RESI, 
         code_state = substr(ID_MN_RESI, 1, 2))

plt_after_2000 <- after_2000 %>% 
  mutate(year = year(DT_SIN_PRI), 
         SEM_SIN_PRI = epiweek(DT_SIN_PRI)) %>% 
  group_by(code_state, year) %>%
  summarise(Cases=n())

plt_after_2000 <- state_br %>% 
  mutate(code_state = as.character(code_state)) %>%
  left_join(plt_after_2000, by = c("code_state"))

plt_after_2000 <- plt_after_2000 %>% 
  left_join(pop_state_2000_2020, by = c("code_state", "year")) %>% 
  mutate(attack_rate = Cases/total_pop)

no_axis <- theme(axis.title = element_blank(),
                 axis.text =  element_blank(),
                 axis.ticks = element_blank())

viridis_lims<- pretty(plt_after_2000$attack_rate, shrink.sml = 10, n = 5)

map_after <- ggplot()+
  geom_sf(data = state_br, size=.1)+
  geom_sf(data = plt_after_2000, 
          aes(fill = round(attack_rate, 3)), 
          size = .1, na.rm = T)+
  scale_fill_viridis(option = "turbo", name = "Attack Rate", limits = c(0.00, 0.06), breaks = viridis_lims)+
  no_axis+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Dengue Attack Rate, by State, by year")+
  facet_wrap(year~., ncol = 7)
map_after

ggsave(plot = map_after, filename = "Plots/map_after_2000_states.png", 
       width = 9, height = 7, dpi = 300)
