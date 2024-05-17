## Analysis code for Mtb Inf Burden
## Authors: A Schwalb
## RScript 04: DemPop.R

# Packages ==========
library(data.table)
library(rio)
library(here)
library(tidyverse)

# 1. Demographic data ==========
# 1.1 Population
WPP <- import(here("sources", "pop", "WPP_Pop_1950-2100.csv"))

WPP <- WPP %>% # Population data
  select(iso3 = ISO3_code, year = Time, ageWPP = AgeGrp, pop = PopTotal) %>%
  mutate(iso3 = na_if(iso3, "")) %>% 
  filter(!is.na(iso3)) %>%
  filter(year <= 2022) %>% 
  mutate(ageWPP = case_when(ageWPP == "0-4" ~ "00-04", ageWPP == "5-9" ~ "05-09",
    ageWPP %in% c('80-84','85-89','90-94','95-99','100+') ~ '80+', TRUE ~ ageWPP)) %>%
  group_by(iso3, year, ageWPP) %>% 
  summarise(pop = sum(pop) * 1e3) %>% 
  mutate(ageARI = case_when(ageWPP %in% c('00-04','05-09','10-14') ~ '00-14',
    ageWPP %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15-44',
    ageWPP %in% c('45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+') ~ '45+')) %>% 
  group_by(iso3, year) %>% 
  mutate(fpop = pop / sum(pop))

# 1.2 Births
WPPb <- import(here("sources", "pop", "WPP_Births_1950-2100.csv"))

WPPb <- WPPb %>% # Birth data
  select(iso3 = ISO3_code, year = Time, births = Births, pop = TPopulation1Jan) %>%
  mutate(iso3 = na_if(iso3, "")) %>% 
  filter(!is.na(iso3)) %>%
  filter(year <= 2022) %>%
  mutate(births = births * 1e3, pop = pop * 1e3) %>%
  mutate(birthrate = births / pop) %>% 
  mutate(ageWPP = '00-04') %>% 
  select(iso3, year, birthrate, ageWPP)

# 1.3 Merges
WPP <- WPP %>% 
  left_join(WPPb, by = c('iso3', 'year', 'ageWPP')) %>% 
  select(-pop)
rm(WPPb)

input <- c("mARI_rev_mix.Rdata", "ARI_rev_mix.Rdata")
output <- c("mARI_rev_mix_pop.Rdata", "ARI_rev_mix_pop.Rdata")

for (i in 1:length(input)) {
  ARI <- as.data.table(import(here("data", "ari", input[i])))
  
  ARI <- ARI %>%
    left_join(WPP, by = c("iso3", "year", "ageARI"), relationship = 'many-to-many') %>% 
    arrange(iso3, year, ageWPP, ageARI) %>% 
    filter(!is.na(ageWPP)) 

  export(ARI, here("data","ari",output[i]))
}

rm(list=ls())
