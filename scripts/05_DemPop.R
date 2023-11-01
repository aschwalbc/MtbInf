## Analysis code for Schwalb et al. 2023
## Distributed under CC BY 4.0
## RScript 05: DemPop.R

# Packages ==========
library(data.table) # Faster than data frame
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
library(ggplot2) # To build comparative plots

# 1. Load data ==========
ARI <- import(here("data","ari","ARI_IHME_norev_mix.Rdata"))
ARI_nomix <- import(here("data","ari","ARI_IHME_norev_nomix.Rdata"))
ARIrev <- import(here("data","ari","ARI_IHME_rev_mix.Rdata"))
ARIrev_nomix <- import(here("data","ari","ARI_IHME_rev_nomix.Rdata"))
ARIwho <- import(here("data","ari","ARI_WHO_norev_nomix.Rdata"))
WPP <- import(here("data","sources","pop","WPP_Pop_1950-2100.csv"))
WPPb <- import(here("data","sources","pop","WPP_Births_1950-2100.csv"))
WPPda <- import(here("data","sources","pop","WPP_Deaths_1950-2021.csv"))
WPPdb <- import(here("data","sources","pop","WPP_Deaths_2022-2080.csv"))

# 2. Data curation ==========
# 2.1 Population, births, and mortality
WPP$ISO3_code <- na_if(WPP$ISO3_code,"")
WPPb$ISO3_code <- na_if(WPPb$ISO3_code,"")
WPPda$ISO3_code <- na_if(WPPda$ISO3_code,"")
WPPdb$ISO3_code <- na_if(WPPdb$ISO3_code,"")

WPP <- WPP %>% # Population data
  rename(iso3 = ISO3_code, year = Time, agegp = AgeGrp, pop = PopTotal) %>%
  select(iso3,year,agegp,pop) %>%
  mutate(iso3 = na_if(iso3,"")) %>% 
  filter(!is.na(iso3)) %>%
  filter(year <= 2050) %>%
  mutate(agegp = case_when(
    agegp == "0-4" ~ "00-04", agegp == "5-9" ~ "05-09",
    agegp %in% c('80-84','85-89','90-94','95-99','100+') ~ '80+',
    TRUE ~ agegp)) %>%
  group_by(iso3,year,agegp) %>% 
  summarise(pop = sum(pop)*1e3) %>% 
  mutate(acat = case_when(
    agegp %in% c('00-04','05-09','10-14') ~ '0-14',
    agegp %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15-44',
    agegp %in% c('45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+') ~ '45+')) %>% 
  group_by(iso3,year) %>% 
  mutate(fpop = pop/sum(pop))

WPPb <- WPPb %>% # Birth data
  rename(iso3 = ISO3_code, year = Time, births = Births, pop = TPopulation1Jan) %>%
  select(iso3,year,births,pop) %>%
  filter(!is.na(iso3)) %>%
  filter(year <= 2050) %>%
  mutate(births = births*1e3, pop = pop*1e3) %>%
  mutate(birthrate = births/pop) %>% 
  select(-pop) %>% 
  mutate(agegp = '00-04')

WPPda <- WPPda %>% # Mortality data (1950-2021)
  rename(iso3 = ISO3_code, year = Time, agegp = AgeGrpStart, mort = DeathTotal) %>%
  select(iso3,year,agegp,mort) %>%
  filter(!is.na(iso3)) %>%
  mutate(agegp = cut(agegp, breaks = c(-Inf,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,Inf), 
                     labels = c('00-04','05-09','10-14','15-19','20-24','25-29','30-34','35-39',
                                '40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'))) %>%
  group_by(iso3,year,agegp) %>%
  summarise(mort = sum(mort)*1e3) %>% 
  mutate(acat = case_when(
    agegp %in% c('00-04','05-09','10-14') ~ '0-14',
    agegp %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15-44',
    agegp %in% c('45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+') ~ '45+'))

WPPdb <- WPPdb %>% # Mortality data (2022-2080)
  rename(iso3 = ISO3_code, year = Time, agegp = AgeGrpStart, mort = DeathTotal) %>%
  select(iso3,year,agegp,mort) %>%
  filter(!is.na(iso3)) %>%
  filter(year <= 2050) %>%
  mutate(agegp = cut(agegp, breaks = c(-Inf,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,Inf), 
                     labels = c('00-04','05-09','10-14','15-19','20-24','25-29','30-34','35-39',
                                '40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'))) %>%
  group_by(iso3,year,agegp) %>%
  summarise(mort = sum(mort)*1e3) %>% 
  mutate(acat = case_when(
    agegp %in% c('00-04','05-09','10-14') ~ '0-14',
    agegp %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15-44',
    agegp %in% c('45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+') ~ '45+'))

WPPd <- rbind(WPPda,WPPdb)
rm(WPPda,WPPdb)

isos <- unique(ARI$iso3)

WPP <- WPP %>%
  left_join(WPPb, by=c("iso3","year","agegp")) %>%
  inner_join(WPPd, by=c("iso3","year","agegp", "acat")) %>%
  filter(iso3 %in% isos) %>%
  mutate(mortrate = mort/pop) %>% 
  select(-births, -mort)
rm(WPPb,WPPd,isos)

WPPt <- WPP %>% 
  filter(year == 2014) %>% 
  group_by(iso3) %>% 
  summarise(pop = sum(pop)) %>% 
  filter(pop < 5e5)

isos <- sort(c(unique(WPPt$iso3), "TLS"))

ARIrev <- ARIrev %>%
  filter(!iso3 %in% isos) %>% 
  inner_join(WPP, by=c("year","iso3","acat"), relationship = 'many-to-many') %>%
  arrange(iso3,replicate,year,acat,agegp)

ARIrev_nomix <- ARIrev_nomix %>%
  filter(!iso3 %in% isos) %>% 
  inner_join(WPP, by=c("year","iso3","acat"), relationship = 'many-to-many') %>%
  arrange(iso3,replicate,year,acat,agegp)

ARI <- ARI %>%
  filter(!iso3 %in% isos) %>% 
  inner_join(WPP, by=c("year","iso3","acat"), relationship = 'many-to-many') %>%
  arrange(iso3,replicate,year,acat,agegp)

ARI_nomix <- ARI_nomix %>%
  filter(!iso3 %in% isos) %>% 
  inner_join(WPP, by=c("year","iso3","acat"), relationship = 'many-to-many') %>%
  arrange(iso3,replicate,year,acat,agegp)

isos <- sort(c("ABW","AIA","AND","ANT","ASM","ATG","BES","BHS","BLZ","BMU","BRB","BRN","COK",
               "CUW","CYM","DMA","FSM","GRD","GRL","GUM","HKG","ISL","KIR","KNA","LCA","MAC",
               "MCO","MDV","MHL","MLT","MNP","MSR","NCL","NIU","NRU","PLW","PYF","SMR","STP",
               "SXM","SYC","TCA","TKL","TLS","TON","TUV","VCT","VGB","VIR","VUT","WLF","WSM"))

isoWPP <- unique(WPP$iso3)
agegpWPP <- unique(WPP$agegp)
yearsWPP <- 1934:1949

WPPexp <- expand.grid(iso3 = isoWPP, year = yearsWPP, agegp = agegpWPP)

WPP_1950 <- WPP %>% 
  filter(year == 1950)

WPPext <- merge(WPPexp, WPP_1950, by = c("iso3", "agegp"), all.x = TRUE) %>% 
  select(!year.y) %>% 
  rename(year = year.x) %>% 
  arrange(iso3,year,agegp) %>% 
  select(iso3,year,agegp,pop,acat,fpop,mortrate,birthrate)

WPP <- WPP %>% 
  rbind(WPPext) %>% 
  arrange(iso3,year,agegp)

ARIwho <- ARIwho %>%
  filter(!iso3 %in% isos) %>% 
  inner_join(WPP, by=c("year","iso3","acat"), relationship = 'many-to-many') %>%
  arrange(iso3,replicate,year,acat,agegp)
rm(WPP, WPPt, isos)

isoihme <- unique(ARI$iso3)
isowho <- unique(ARIwho$iso3)

setdiff(isowho, isoihme)

export(ARIrev,here("data","ari","ARI_IHME_rev_mix_pop.Rdata")) # IHME - Reversion - Mixing
export(ARIrev_nomix,here("data","ari","ARI_IHME_rev_nomix_pop.Rdata")) # IHME - Reversion - No mixing
export(ARI,here("data","ari","ARI_IHME_norev_mix_pop.Rdata")) # IHME - No reversion - Mixing
export(ARI_nomix,here("data","ari","ARI_IHME_norev_nomix_pop.Rdata")) # # IHME - No reversion - No mixing
export(ARIwho,here("data","ari","ARI_WHO_norev_nomix_pop.Rdata")) # WHO - No reversion - No mixing
rm(list=ls())
