## Analysis code for Schwalb et al. 2024
## Adapted from Dodd et al. 2023
## Distributed under CC BY 4.0
## RScript 03: Mixing.R

# Packages ==========
library(data.table)
library(rio)
library(here)
library(tidyverse)
library(ggplot2)
library(viridis)

# 1. Data prep ==========
# 1.1 Mixing matrices
MIX <- import(here("data","sources","others","Mixing.Rdata")) # Mixing matrices by Prem et al. 2021

MIX <- MIX %>% 
  filter(setting == 'overall' & location_contact == 'all') %>% # Filter for all contact locations (work, home, etc.)
  select(iso3 = iso3c, AO = age_contactor, AI = age_cotactee, ctx = mean_number_of_contacts) %>% 
  mutate(AO = gsub(AO, pattern=" to ", replacement="-"), AI = gsub(AI, pattern=" to ", replacement="-")) %>% 
  mutate(AO = case_when(AO == '0-4' ~ '00-04', AO == '5-9' ~ '05-09', TRUE ~ AO),
         AI = case_when(AI == '0-4' ~ '00-04', AI == '5-9' ~ '05-09', TRUE ~ AI))

# 1.2 WHO incidence per age
WHOinc <- import(here("data","sources","who","WHOinc_2022.csv")) # WHO age and sex specific incidence

WHOinc <- WHOinc %>%
  filter(risk_factor == 'all') %>% 
  filter(!age_group %in% c('all', '0-14', '15plus', '18plus')) %>% # Filter out unnecessary age groups
  group_by(iso3, year, ageWHO = age_group) %>%
  summarise(val = sum(best), lo = sum(lo), hi = sum(hi)) %>% # Summarise over sex
  mutate(ageWHO = case_when(ageWHO == '0-4' ~ '00-04', ageWHO == '5-14' ~ '05-14', ageWHO == '65plus' ~ '65+', TRUE ~ ageWHO))

# 1.3 Age group populations
WPP <- import(here("data","sources","others","WPP_2022.csv")) # World Population Prospects 2022

WPP <- WPP %>% 
  select(iso3 = ISO3_code, year = Time, ageWPP = AgeGrp, pop = PopTotal) %>%
  mutate(ageWPP = case_when(ageWPP == '0-4' ~ '00-04', ageWPP == '5-9' ~ '05-09', TRUE ~ ageWPP)) %>% 
  mutate(iso3 = na_if(iso3,""), pop = pop * 1e3) %>% 
  filter(!is.na(iso3)) %>%
  filter(year == 2022) 

# 1.4 Age group definitions
AGEkey <- data.frame(ageWPP = unique(WPP$ageWPP)) %>% 
  mutate(ageWHO = case_when(ageWPP %in% c('00-04') ~ "00-04", ageWPP %in% c('05-09','10-14') ~ '05-14', 
    ageWPP %in% c('15-19','20-24') ~ '15-24', ageWPP %in% c('25-29','30-34') ~ '25-34', 
    ageWPP %in% c('35-39','40-44') ~ '35-44', ageWPP %in% c('45-49','50-54') ~ '45-54',
    ageWPP %in% c('55-59','60-64') ~ '55-64', TRUE ~ '65+')) %>%
  mutate(ageMIX = case_when(ageWPP %in% c('75-79','80-84','85-89','90-94','95-99','100+') ~ '75+', TRUE ~ ageWPP)) %>% 
  mutate(ageARI = case_when(ageWHO %in% c('00-04','05-14') ~ "00-14",
    ageWHO %in% c('15-24','25-34','35-44') ~ "15-44", ageWHO %in% c('45-54','55-64','65+') ~ "45+"))

# 2. Relative TB incidence ==========
WHOkey <- import(here("data","sources","who","WHOkey.csv")) %>% # WHO countries ISO codes and regions
  select(iso3, reg = g_whoregion)

WPP <- WPP %>%
  left_join(AGEkey, by = 'ageWPP') %>%
  group_by(iso3, ageWHO) %>%
  summarise(pop = sum(pop))

WHOinc <- WHOinc %>%
  left_join(WPP, by = c("iso3", "ageWHO")) %>%
  inner_join(AGEkey %>% distinct(ageWHO, ageARI) %>% select(ageWHO, ageARI), by = "ageWHO") %>% 
  group_by(iso3, year, ageARI) %>%
  summarise(val = sum(val), lo = sum(lo), hi = sum(hi), pop = sum(pop)) %>% 
  mutate(pcTB = val / pop * 1e5) %>% # TB incidence per capita
  group_by(iso3, year) %>% 
  mutate(refpcTB = ifelse(ageARI == '00-14', pcTB, NA)) %>% 
  mutate(refpcTB = ifelse(!is.na(refpcTB), refpcTB, first(na.omit(refpcTB)))) %>% 
  mutate(relpcTB = pcTB / refpcTB) %>% 
  mutate(relpcTB = ifelse(!is.finite(relpcTB), NA, relpcTB)) %>% 
  left_join(WHOkey, by = 'iso3')

WHOinc_reg <- WHOinc %>%
  group_by(reg, ageARI) %>%
  summarise(relpcTB_reg = mean(relpcTB, na.rm = TRUE))

WHOinc <- WHOinc %>% 
  left_join(WHOinc_reg, by = c('reg', 'ageARI')) %>% 
  mutate(relpcTB = ifelse(is.na(relpcTB), relpcTB_reg, relpcTB)) %>% 
  within(rm(refpcTB, pcTB, relpcTB_reg))
rm(WHOinc_reg)
  
F1 <- ggplot() +
  geom_line(WHOinc, mapping = aes(x = ageARI, y = relpcTB, group = iso3)) +
  facet_wrap(~reg) +
  geom_hline(yintercept = 1, col = 2) +
  scale_y_sqrt() +
  ylab('Square-root of relative TB incidence per capita') +
  xlab('Age category') +
  ggtitle('Relative TB incidence per capita (WHO estimates)') +
  theme_light()

# 3. Mixing matrices ==========
MIX <- MIX %>% 
  left_join(unique(AGEkey[, c("ageMIX", "ageARI")]), by = c("AO" = "ageMIX")) %>%
  rename(ageAO = ageARI) %>%
  left_join(unique(AGEkey[, c("ageMIX", "ageARI")]), by = c("AI" = "ageMIX")) %>%
  rename(ageAI = ageARI) %>% 
  group_by(iso3, ageAO, ageAI) %>% 
  summarise(ctx = sum(ctx)) %>% 
  inner_join(WHOkey, by = 'iso3')
  
MIXreg <- MIX %>%
  group_by(reg, ageAO, ageAI) %>%
  summarise(ctx = mean(ctx))

F2 <- ggplot() +
  facet_wrap(~reg) +
  geom_tile(MIXreg, mapping = aes(x = ageAO, y = ageAI, fill = ctx)) +
  scale_fill_viridis() +
  theme(legend.position = 'bottom') +
  xlab('Age of contactor') +
  ylab('Age of contactee') +
  ggtitle('Regional average contact patterns')
rm(MIXreg)

# 4. Age-adjusted ARIs ========== 
WHOinci <- WHOinc %>%
  select(iso3, reg, ageARI, val, pop, relpcTB) %>%
  left_join(MIX, by = c("iso3", "reg", "ageARI" = "ageAI")) %>% 
  within(rm(year)) %>%
  filter(!is.na(ctx))
rm(MIX)

WHOinc[WHOinc$acats %in% c("0-14"),"relpcTB"] <- 0

WHOinc <- WHOinc %>%
  mutate(ari = relpcTB * ctx) %>%
  group_by(g_whoregion,iso3,acato) %>%
  summarise(ari = sum(ari))

ARIref <- WHOinc %>%
  filter(acato == "0-14") %>%
  rename(refari = ari) %>%
  select(iso3,refari)
  
WHOinc <- WHOinc %>%
  left_join(ARIref, by=c("iso3","g_whoregion")) %>%
  mutate(relari = ari/refari) %>%
  rename(acat = acato) %>%
  mutate(acat = factor(acat,levels=acats,ordered=TRUE)) %>%
  arrange(g_whoregion,iso3,acat) %>%
  select(iso3,g_whoregion,acat,relari)
rm(ARIref,acats)

relARI_reg <- WHOinc %>%
  group_by(g_whoregion,acat) %>%
  summarise(relari = mean(relari, na.rm = TRUE))

relARI_glob <- WHOinc %>%
  group_by(acat) %>%
  summarise(relari = mean(relari, na.rm = TRUE))

WHOinc_reg <- WHOinc %>% 
  left_join(relARI_reg, by=c("g_whoregion","acat")) %>% 
  select(-relari.x) %>% 
  rename(relari = relari.y)

# ARIrev <- as.data.table(import(here("data","gp","GPruns_IHME_rev.Rdata"))) # IHME Reversion - All runs
# ARI <- as.data.table(import(here("data","gp","GPruns_IHME_norev.Rdata"))) # IHME No reversion - All runs
# ARIwho <- as.data.table(import(here("data","gp","GPruns_WHO_norev.Rdata"))) # WHO No reversion - All runs
ARIrev <- as.data.table(import(here("data","gp","GP_IHME_rev.Rdata"))) # IHME Reversion - Median
ARI <- as.data.table(import(here("data","gp","GP_IHME_norev.Rdata"))) # IHME No reversion - Median
ARIwho <- as.data.table(import(here("data","gp","GP_WHO_norev.Rdata"))) # WHO No reversion - Median

F3 <- ggplot() +
  geom_line(WHOinc, mapping = aes(acat,relari,group=iso3)) +
  facet_wrap(~g_whoregion) +
  geom_hline(yintercept=1,col=2) +
  xlab('Age') +
  ylab('Relative ARI') +
  ggtitle('Relative ARI implied by mixing')

ARIrev_reg <- ARIrev %>% 
  left_join(WHOkey, by="iso3") %>% 
  left_join(WHOinc_reg, by=c("iso3","g_whoregion"), relationship = "many-to-many") %>% 
  select(-country, -iso2, -iso_numeric)

ARIrev <- ARIrev %>%
  left_join(WHOinc, by="iso3", relationship = "many-to-many")

ARI_reg <- ARI %>% 
  left_join(WHOkey, by="iso3") %>% 
  left_join(WHOinc_reg, by=c("iso3","g_whoregion"), relationship = "many-to-many") %>% 
  select(-country, -iso2, -iso_numeric)

ARI <- ARI %>%
  left_join(WHOinc, by="iso3", relationship = "many-to-many")

ARIwho_reg <- ARIwho %>% 
  left_join(WHOkey, by="iso3") %>% 
  left_join(WHOinc_reg, by=c("iso3","g_whoregion"), relationship = "many-to-many") %>% 
  select(-country, -iso2, -iso_numeric)

ARIwho <- ARIwho %>%
  left_join(WHOinc, by="iso3", relationship = "many-to-many")
  
ARIrev_comp_reg <- ARIrev_reg %>%
  filter(!is.na(relari))

ARIrev_comp <- ARIrev %>%
  filter(!is.na(relari))

ARI_comp_reg <- ARI_reg %>%
  filter(!is.na(relari))

ARI_comp <- ARI %>%
  filter(!is.na(relari))

ARIwho_comp_reg <- ARIwho_reg %>%
  filter(!is.na(relari))

ARIwho_comp <- ARIwho %>%
  filter(!is.na(relari))

ARIrev_na_acat_reg <- ARIrev_reg %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari) %>%
  # select(year,iso3,lari,replicate) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARIrev_na_acat <- ARIrev %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari) %>%
  # select(year,iso3,lari,replicate) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARI_na_acat_reg <- ARI_reg %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari) %>%
  # select(year,iso3,lari,replicate) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARI_na_acat <- ARI %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari) %>%
  # select(year,iso3,lari,replicate) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARIwho_na_acat_reg <- ARIwho_reg %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari) %>%
  # select(year,iso3,lari,replicate) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARIwho_na_acat <- ARIwho %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari) %>%
  # select(year,iso3,lari,replicate) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARIrev_na_reg <- ARIrev_reg %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,acat) %>%
  # select(year,iso3,lari,acat,replicate) %>%
  bind_rows(ARIrev_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,g_whoregion,acat) %>%
  # select(year,iso3,lari,g_whoregion,acat,replicate) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARIrev_na <- ARIrev %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,acat) %>%
  # select(year,iso3,lari,acat,replicate) %>%
  bind_rows(ARIrev_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,g_whoregion,acat) %>%
  # select(year,iso3,lari,g_whoregion,acat,replicate) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARI_na_reg <- ARI_reg %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,acat) %>%
  # select(year,iso3,lari,acat,replicate) %>%
  bind_rows(ARI_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,g_whoregion,acat) %>%
  # select(year,iso3,lari,g_whoregion,acat,replicate) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARI_na <- ARI %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,acat) %>%
  # select(year,iso3,lari,acat,replicate) %>%
  bind_rows(ARI_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,g_whoregion,acat) %>%
  # select(year,iso3,lari,g_whoregion,acat,replicate) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARIwho_na_reg <- ARIwho_reg %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,acat) %>%
  # select(year,iso3,lari,acat,replicate) %>%
  bind_rows(ARIwho_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,g_whoregion,acat) %>%
  # select(year,iso3,lari,g_whoregion,acat,replicate) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARIwho_na <- ARIwho %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,acat) %>%
  # select(year,iso3,lari,acat,replicate) %>%
  bind_rows(ARIwho_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,g_whoregion,acat) %>%
  # select(year,iso3,lari,g_whoregion,acat,replicate) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

rm(relARI_glob,relARI_reg,WHOkey,ARIrev,ARIrev_reg,ARIrev_na_acat,ARIrev_na_acat_reg,
   ARI,ARI_reg,ARI_na_acat,ARI_na_acat_reg,ARIwho,ARIwho_reg,ARIwho_na_acat,ARIwho_na_acat_reg)

ARIrevmix_reg <- ARIrev_comp_reg %>%
  bind_rows(ARIrev_na_reg) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari) %>%
  mutate(ari = exp(lari)) %>% 
  select(year,iso3,g_whoregion,acat,ari) %>%
  # select(year,iso3,g_whoregion,acat,ari,replicate) %>%
  arrange(iso3,year)

ARIrevmix <- ARIrev_comp %>%
  bind_rows(ARIrev_na) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari) %>%
  mutate(ari = exp(lari)) %>% 
  select(year,iso3,g_whoregion,acat,ari) %>%
  # select(year,iso3,g_whoregion,acat,ari,replicate) %>%
  arrange(iso3,year)

ARImix_reg <- ARI_comp_reg %>%
  bind_rows(ARI_na_reg) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari) %>%
  mutate(ari = exp(lari)) %>% 
  select(year,iso3,g_whoregion,acat,ari) %>%
  # select(year,iso3,g_whoregion,acat,ari,replicate) %>%
  arrange(iso3,year)

ARImix <- ARI_comp %>%
  bind_rows(ARI_na) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari) %>%
  mutate(ari = exp(lari)) %>% 
  select(year,iso3,g_whoregion,acat,ari) %>%
  # select(year,iso3,g_whoregion,acat,ari,replicate) %>%
  arrange(iso3,year)

ARIwhomix_reg <- ARIwho_comp_reg %>%
  bind_rows(ARIwho_na_reg) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari) %>%
  mutate(ari = exp(lari)) %>% 
  select(year,iso3,g_whoregion,acat,ari) %>%
  # select(year,iso3,g_whoregion,acat,ari,replicate) %>%
  arrange(iso3,year)

ARIwhomix <- ARIwho_comp %>%
  bind_rows(ARIwho_na) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari) %>%
  mutate(ari = exp(lari)) %>% 
  select(year,iso3,g_whoregion,acat,ari) %>%
  # select(year,iso3,g_whoregion,acat,ari,replicate) %>%
  arrange(iso3,year)

ARIrevnomix <- ARIrevmix %>%
  group_by(year, iso3) %>% 
  # group_by(year, iso3, replicate) %>% 
  mutate(arix = ari[acat == "0-14"], ari = ifelse(acat != "0-14", arix, ari)) %>% 
  select(-arix)

ARInomix <- ARImix %>%
  group_by(year, iso3) %>% 
  # group_by(year, iso3, replicate) %>% 
  mutate(arix = ari[acat == "0-14"], ari = ifelse(acat != "0-14", arix, ari)) %>% 
  select(-arix)

ARIwhonomix <- ARIwhomix %>%
  group_by(year, iso3) %>% 
  # group_by(year, iso3, replicate) %>% 
  mutate(arix = ari[acat == "0-14"], ari = ifelse(acat != "0-14", arix, ari)) %>% 
  select(-arix)
  
rm(WHOinc,WHOinc_reg,ARIrev_comp,ARIrev_comp_reg,ARIrev_na,ARIrev_na_reg,
   ARI_comp,ARI_comp_reg,ARI_na,ARI_na_reg,ARIwho_comp,ARIwho_comp_reg,ARIwho_na,ARIwho_na_reg)

# IHME Reversion
# export(ARIrevmix,here("data","ari","ARI_IHME_rev_mix.Rdata")) # Mixing
# export(ARIrevmix_reg,here("data","ari","ARI_IHME_rev_mixreg.Rdata")) # Mixing (Regional estimates)
# export(ARIrevnomix,here("data","ari","ARI_IHME_rev_nomix.Rdata")) # No mixing

export(ARIrevmix,here("data","ari","mARI_IHME_rev_mix.Rdata")) # Median - Mixing
export(ARIrevmix_reg,here("data","ari","mARI_IHME_rev_mixreg.Rdata")) # Median - Mixing (Regional estimates)
export(ARIrevnomix,here("data","ari","mARI_IHME_rev_nomix.Rdata")) # Median - No mixing

# IMHE No reversion
# export(ARImix,here("data","ari","ARI_IHME_norev_mix.Rdata")) # Mixing
# export(ARImix_reg,here("data","ari","ARI_IHME_norev_mixreg.Rdata")) # Mixing (Regional estimates)
# export(ARInomix,here("data","ari","ARI_IHME_norev_nomix.Rdata")) # No mixing

export(ARImix,here("data","ari","mARI_IHME_norev_mix.Rdata")) # Median - Mixing
export(ARImix_reg,here("data","ari","mARI_IHME_norev_mixreg.Rdata")) # Median - Mixing (Regional estimates)
export(ARInomix,here("data","ari","mARI_IHME_norev_nomix.Rdata")) # Median - No mixing

# WHO No reversion
# export(ARIwhomix,here("data","ari","ARI_WHO_norev_mix.Rdata")) # Mixing
# export(ARIwhomix_reg,here("data","ari","ARI_WHO_norev_mixreg.Rdata")) # Mixing (Regional estimates)
# export(ARIwhonomix,here("data","ari","ARI_WHO_norev_nomix.Rdata")) # No mixing

export(ARIwhomix,here("data","ari","mARI_WHO_norev_mix.Rdata")) # Median - Mixing
export(ARIwhomix_reg,here("data","ari","mARI_WHO_norev_mixreg.Rdata")) # Median - Mixing (Regional estimates)
export(ARIwhonomix,here("data","ari","mARI_WHO_norev_nomix.Rdata")) # Median - No mixing

rm(list=ls())
