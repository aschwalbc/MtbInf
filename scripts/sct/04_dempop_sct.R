## Mtb-Inf: Scotland

# Packages ==========
library(data.table)
library(rio)
library(here)
library(tidyverse)
library(fst)

# 1. Demographic data ==========
# 1.1 Population
WPP <- import(here("sources", "sct", "ONSpop_1961-2023.csv")) # ONS Population Estimates 2022

WPP <- WPP %>% 
  mutate(iso3 = 'GBR') %>% 
  select(iso3, year = Year, ageWPP = Age_band, pop = Population) %>%
  mutate(ageWPP = case_when(ageWPP == '0-4' ~ '00-04', ageWPP == '5-9' ~ '05-09', TRUE ~ ageWPP)) %>% 
  arrange(ageWPP) %>% 
  filter(year <= 2023) %>% 
  mutate(ageARI = case_when(ageWPP %in% c('00-04','05-09','10-14') ~ '00-14',
    ageWPP %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15-44',
    ageWPP %in% c('45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+') ~ '45+')) %>% 
  group_by(iso3, year) %>% 
  mutate(fpop = pop / sum(pop))

# 1.2 Births
WPPa <- WPP %>% 
  select(iso3, year, pop) %>% 
  summarise(pop = sum(pop), .groups = 'drop')

WPPb <- import(here("sources", "sct", "NRSbirths_1961-2023.csv")) %>% 
  left_join(WPPa, by = c('iso3', 'year')) %>% 
  mutate(birthrate = births / pop) %>% 
  mutate(ageWPP = '00-04') %>% 
  select(iso3, year, birthrate, ageWPP)
rm(WPPa)

# 1.3 Merges
WPP <- WPP %>% 
  left_join(WPPb, by = c('iso3', 'year', 'ageWPP')) %>% 
  select(-pop)
rm(WPPb)

input <- c("mARI_rev_mix_sct.Rdata", "ARI_rev_mix_sct.Rdata")
output <- c("mARI_rev_mix_pop_sct.Rdata", "ARI_rev_mix_pop_sct.Rdata")

for (i in 1:length(input)) {
  ARI <- as.data.table(import(here("data", "ari", input[i])))
  
  ARI <- ARI %>%
    left_join(WPP, by = c("iso3", "year", "ageARI"), relationship = 'many-to-many') %>% 
    arrange(iso3, year, ageWPP, ageARI) %>% 
    filter(!is.na(ageWPP)) 

  export(ARI, here("data","ari",output[i]))
}

write.fst(ARI, here("data", "ari", "ARI_rev_mix_pop_sct.fst"))

rm(list=ls())
