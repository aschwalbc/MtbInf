## Analysis code for Schwalb et al. 2023
## Adapted from Dodd et al. 2023
## Distributed under CC BY 4.0
## RScript 04: Mixing.R

# Packages ==========
library(data.table) # Faster than data.frame, allows use of j operator (:=)
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(naniar) # Visualise missingness
library(tidyverse) # To use tidyverse
library(ggplot2) # To build plots
library(viridis) # To enhance colour scales

# 1. Load data ==========
MIX <- import(here("data","sources","others","Mixing.Rdata")) # Mixing matrices by Prem et al. 2021
WHOinc <- import(here("data","sources","who","WHOinc_2019.csv")) # WHO age and sex specific incidence
WPP <- import(here("data","sources","others","WPP_2020.Rdata")) # World Population Prospects 2020
WHOkey <- import(here("data","sources","who","WHOkey.csv")) # WHO countries ISO codes and regions
ARIrev <- as.data.table(import(here("data","gp","GP_IHME_rev.Rdata"))) # IHME Reversion
ARI <- as.data.table(import(here("data","gp","GP_IHME_norev.Rdata"))) # IHME No reversion
# ARIwho <- as.data.table(import(here("data","gp","GP_WHO_norev.Rdata"))) # WHO No reversion
ARIwho <- as.data.table(import(here("data","gp","GP_ARI_FULL.Rdata"))) # WHO No reversion

# 2. Data curation ==========
MIX <- MIX %>% 
  filter(location_contact == "all") %>% # Filter for all contact locations (work, home, etc.)
  rename(iso3=iso3c, age_contactee=age_cotactee, ctx=mean_number_of_contacts) %>% # Rename
  within(rm(location_contact, setting)) # Remove unnecessary variables

WHOinc <- WHOinc %>%
  filter(!age_group %in% c("all","0-14","15plus","18plus")) %>% # Filter out unnecessary age groups
  filter(risk_factor == "all") %>% # Filter out risk factors
  rename(val=best) %>% # Rename variable
  group_by(iso3,year,age_group) %>% # Group by ISO, year, and age group
  summarise(val = sum(val), lo = sum(lo), hi = sum(hi)) # Aggregate over sex

AGEkey <- data.frame("AgeGrp"=unique(WPP$AgeGrp)) %>% # Create database of age group keys
  mutate(age_group = case_when( # Create variable from 'AgeGrp'
    AgeGrp %in% c('0-4') ~ "0-4",
    AgeGrp %in% c('5-9','10-14') ~ "5-14",
    AgeGrp %in% c('15-19','20-24') ~ "15-24",
    AgeGrp %in% c('25-29','30-34') ~ "25-34",
    AgeGrp %in% c('35-39','40-44') ~ "35-44",
    AgeGrp %in% c('45-49','50-54') ~ "45-54",
    AgeGrp %in% c('55-59','60-64') ~ "55-64",
    AgeGrp %in% c('65-69','70-74','75-79','80+') ~ "65plus")) %>%
  mutate(acat = gsub(age_group, pattern="plus", replacement="+")) %>% # Remove 'plus'
  mutate(acats = case_when(
    acat %in% c('0-4','5-14') ~ "0-14",
    acat %in% c('15-24','25-34','35-44') ~ "15-44",
    acat %in% c('45-54','55-64','65+') ~ "45+")) %>%
  mutate(cage = gsub(AgeGrp, pattern="75-79|80\\+", replacement="75+")) # 

acats <- unique(AGEkey$acats)

WPP <- WPP %>%
  merge(AGEkey) %>%
  group_by(iso3,age_group) %>%
  summarise(pop=sum(pop))

WHOinc <- WHOinc %>%
  left_join(WPP,by=c("iso3","age_group")) %>%
  inner_join(unique(AGEkey[,c("age_group","acats")]), by="age_group") %>%
  group_by(iso3,year,acats) %>%
  summarise(val=sum(val), lo=sum(lo), hi=sum(hi), pop=sum(pop)) %>%
  mutate(pcTB = 1e5*val/pop) # TB incidence per capita
rm(WPP)

WHOinc[is.na(WHOinc$pcTB),"pcTB"] <- 0
WHOinc$acats <- factor(WHOinc$acats, levels=acats, ordered=TRUE)

REFs <- WHOinc %>%
  filter(acats == "0-14") %>%
  mutate(refpcTB = pcTB) %>%
  select(iso3, refpcTB)

WHOinc <- WHOinc %>%
  inner_join(REFs, by=c("iso3","year")) %>%
  mutate(relpcTB = pcTB/refpcTB)
rm(REFs)

WHOinc[!is.finite(WHOinc$relpcTB),"relpcTB"] <- NA

WHOinc <- WHOinc %>%
  inner_join(WHOkey, by="iso3") %>%
  within(rm(country,iso2,iso_numeric,refpcTB))

WHOreg <- WHOinc %>%
  group_by(g_whoregion,iso3,acats) %>%
  summarise(relpcTB = mean(relpcTB,na.rm = TRUE))

WHOreg_sub <- WHOreg %>%
  filter(!acats %in% c("0-14"))

F1 <- ggplot() +
  geom_line(WHOreg_sub, mapping = aes(acats,relpcTB,group=iso3)) +
  facet_wrap(~g_whoregion) +
  geom_hline(yintercept=1,col=2) +
  scale_y_sqrt() +
  ylab('Square-root of relative per capita TB incidence') +
  xlab('Age category') +
  ggtitle('Relative per capita TB incidence (WHO estimates)') +
  theme_light()
rm(WHOreg,WHOreg_sub)

MIX <- MIX %>%
  mutate(AO = gsub(age_contactor, pattern=" to ", replacement="-")) %>%
  mutate(AI = gsub(age_contactee, pattern=" to ", replacement="-"))

MIX <- merge(MIX, unique(AGEkey[,c("cage","acats")]),
             by.x = "AO", by.y = "cage", all.x = TRUE) %>%
  rename(acato = acats)

MIX <- merge(MIX, unique(AGEkey[,c("cage","acats")]),
             by.x = "AI", by.y = "cage", all.x = TRUE) %>%
  rename(acati = acats)
rm(AGEkey)

MIX <- MIX %>%
  group_by(iso3,acato,acati) %>%
  summarise(ctx=sum(ctx)) %>%
  inner_join(WHOkey,by="iso3") %>%
  within(rm(country, iso2, iso_numeric))

MIXreg <- MIX %>%
  group_by(g_whoregion,acato,acati) %>%
  summarise(contacts = mean(ctx)) %>%
  mutate(acato = factor(acato,levels=acats,ordered=TRUE),
         acati = factor(acati,levels=acats,ordered=TRUE))

F2 <- ggplot() +
  geom_tile(MIXreg, mapping = aes(x=acato,y=acati,fill=contacts)) +
  scale_fill_viridis() +
  theme(legend.position='bottom') +
  facet_wrap(~g_whoregion) +
  xlab('Age of contactor') +
  ylab('Age of contactee') +
  ggtitle('Regional average contact patterns')
rm(MIXreg)

WHOinc <- WHOinc %>%
  select(iso3,acats,val,pop,relpcTB,g_whoregion) %>%
  merge(MIX,by.x = c('iso3','g_whoregion','acats'),
        by.y = c('iso3','g_whoregion','acati'),
        all.x = TRUE, all.y=FALSE) %>%
  within(rm(year)) %>%
  filter(!is.na(ctx))
rm(MIX)

length(unique(WHOinc$iso3)) # 177 countries

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
  select(year,iso3,lari,upper,lower) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARIrev_na_acat <- ARIrev %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari,upper,lower) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARI_na_acat_reg <- ARI_reg %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari,upper,lower) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARI_na_acat <- ARI %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari,upper,lower) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARIwho_na_acat_reg <- ARIwho_reg %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari,upper,lower) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARIwho_na_acat <- ARIwho %>%
  filter(is.na(relari)) %>%
  filter(is.na(acat)) %>%
  select(year,iso3,lari,upper,lower) %>%
  slice(rep(row_number(), each=3)) %>%
  mutate(acat = rep(c("0-14","15-44","45+"), length.out = n()))

ARIrev_na_reg <- ARIrev_reg %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,upper,lower,acat) %>%
  bind_rows(ARIrev_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,upper,lower,g_whoregion,acat) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARIrev_na <- ARIrev %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,upper,lower,acat) %>%
  bind_rows(ARIrev_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,upper,lower,g_whoregion,acat) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARI_na_reg <- ARI_reg %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,upper,lower,acat) %>%
  bind_rows(ARI_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,upper,lower,g_whoregion,acat) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARI_na <- ARI %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,upper,lower,acat) %>%
  bind_rows(ARI_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,upper,lower,g_whoregion,acat) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARIwho_na_reg <- ARIwho_reg %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,upper,lower,acat) %>%
  bind_rows(ARI_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,upper,lower,g_whoregion,acat) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

ARIwho_na <- ARIwho %>%
  filter(is.na(relari)) %>%
  filter(!is.na(acat)) %>%
  select(year,iso3,lari,upper,lower,acat) %>%
  bind_rows(ARI_na_acat) %>%
  inner_join(WHOkey, by="iso3") %>%
  select(year,iso3,lari,upper,lower,g_whoregion,acat) %>%
  left_join(relARI_reg, by=c("g_whoregion","acat"))

rm(relARI_glob,relARI_reg,WHOkey,ARIrev,ARIrev_reg,ARIrev_na_acat,ARIrev_na_acat_reg,
   ARI,ARI_reg,ARI_na_acat,ARI_na_acat_reg,ARIwho,ARIwho_reg,ARIwho_na_acat,ARIwho_na_acat_reg)

ARIrevmix_reg <- ARIrev_comp_reg %>%
  bind_rows(ARIrev_na_reg) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari, upper = upper+relari, lower = lower+relari) %>%
  mutate(ari = exp(lari), upper = exp(upper), lower = exp(lower)) %>% 
  select(year,iso3,g_whoregion,acat,ari,upper,lower) %>%
  arrange(iso3,year)

ARIrevmix <- ARIrev_comp %>%
  bind_rows(ARIrev_na) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari, upper = upper+relari, lower = lower+relari) %>%
  mutate(ari = exp(lari), upper = exp(upper), lower = exp(lower)) %>% 
  select(year,iso3,g_whoregion,acat,ari,upper,lower) %>%
  arrange(iso3,year)

ARImix_reg <- ARI_comp_reg %>%
  bind_rows(ARI_na_reg) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari, upper = upper+relari, lower = lower+relari) %>%
  mutate(ari = exp(lari), upper = exp(upper), lower = exp(lower)) %>% 
  select(year,iso3,g_whoregion,acat,ari,upper,lower) %>%
  arrange(iso3,year)

ARImix <- ARI_comp %>%
  bind_rows(ARI_na) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari, upper = upper+relari, lower = lower+relari) %>%
  mutate(ari = exp(lari), upper = exp(upper), lower = exp(lower)) %>% 
  select(year,iso3,g_whoregion,acat,ari,upper,lower) %>%
  arrange(iso3,year)

ARIwhomix_reg <- ARIwho_comp_reg %>%
  bind_rows(ARIwho_na_reg) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari, upper = upper+relari, lower = lower+relari) %>%
  mutate(ari = exp(lari), upper = exp(upper), lower = exp(lower)) %>% 
  select(year,iso3,g_whoregion,acat,ari,upper,lower) %>%
  arrange(iso3,year)

ARIwhomix <- ARIwho_comp %>%
  bind_rows(ARIwho_na) %>%
  mutate(relari = log(relari)) %>%
  mutate(lari = lari+relari, upper = upper+relari, lower = lower+relari) %>%
  mutate(ari = exp(lari), upper = exp(upper), lower = exp(lower)) %>% 
  select(year,iso3,g_whoregion,acat,ari,upper,lower) %>%
  arrange(iso3,year)

ARIrevnomix <- ARIrevmix %>%
  group_by(year, iso3) %>% 
  mutate(arix = ari[acat == "0-14"], upperx = upper[acat == "0-14"], lowerx = lower[acat == "0-14"],
         ari = ifelse(acat != "0-14", arix, ari), upper = ifelse(acat != "0-14", upperx, upper), lower = ifelse(acat != "0-14", lowerx, lower)) %>% 
  select(-arix, -upperx, -lowerx)

ARInomix <- ARImix %>%
  group_by(year, iso3) %>% 
  mutate(arix = ari[acat == "0-14"], upperx = upper[acat == "0-14"], lowerx = lower[acat == "0-14"],
         ari = ifelse(acat != "0-14", arix, ari), upper = ifelse(acat != "0-14", upperx, upper), lower = ifelse(acat != "0-14", lowerx, lower)) %>% 
  select(-arix, -upperx, -lowerx)

ARIwhonomix <- ARIwhomix %>%
  group_by(year, iso3) %>% 
  mutate(arix = ari[acat == "0-14"], upperx = upper[acat == "0-14"], lowerx = lower[acat == "0-14"],
         ari = ifelse(acat != "0-14", arix, ari), upper = ifelse(acat != "0-14", upperx, upper), lower = ifelse(acat != "0-14", lowerx, lower)) %>% 
  select(-arix, -upperx, -lowerx)
  
rm(WHOinc,WHOinc_reg,ARIrev_comp,ARIrev_comp_reg,ARIrev_na,ARIrev_na_reg,
   ARI_comp,ARI_comp_reg,ARI_na,ARI_na_reg,ARIwho_comp,ARIwho_comp_reg,ARIwho_na,ARIwho_na_reg)

# IHME Reversion
export(ARIrevmix,here("data","ari","ARI_IHME_rev_mix.Rdata")) # Mixing
export(ARIrevmix_reg,here("data","ari","ARI_IHME_rev_mixreg.Rdata")) # Mixing (Regional estimates)
export(ARIrevnomix,here("data","ari","ARI_IHME_rev_nomix.Rdata")) # No mixing

# IMHE No reversion
export(ARImix,here("data","ari","ARI_IHME_norev_mix.Rdata")) # Mixing
export(ARImix_reg,here("data","ari","ARI_IHME_norev_mixreg.Rdata")) # Mixing (Regional estimates)
export(ARInomix,here("data","ari","ARI_IHME_norev_nomix.Rdata")) # No mixing

# WHO No reversion
export(ARIwhomix,here("data","ari","ARI_WHO_norev_mix.Rdata")) # Mixing
export(ARIwhomix_reg,here("data","ari","ARI_WHO_norev_mixreg.Rdata")) # Mixing (Regional estimates)
export(ARIwhonomix,here("data","ari","ARI_WHO_norev_nomix.Rdata")) # No mixing
rm(list=ls())
