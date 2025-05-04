## Analysis code for Mtb Inf Burden
## Authors: P Dodd and A Schwalb
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
MIX <- import(here("sources", "others", "Mixing.Rdata")) # Mixing matrices by Prem et al. 2021

MIX <- MIX %>% 
  filter(setting == 'overall' & location_contact == 'all') %>% # Filter for all contact locations (work, home, etc.)
  select(iso3 = iso3c, AO = age_contactor, AI = age_cotactee, ctx = mean_number_of_contacts) %>% 
  mutate(AO = gsub(AO, pattern=" to ", replacement="-"), AI = gsub(AI, pattern=" to ", replacement="-")) %>% 
  mutate(AO = case_when(AO == '0-4' ~ '00-04', AO == '5-9' ~ '05-09', TRUE ~ AO),
         AI = case_when(AI == '0-4' ~ '00-04', AI == '5-9' ~ '05-09', TRUE ~ AI))

# 1.2 WHO incidence per age
WHOinc <- import(here("sources", "who", "WHOinc_2022.csv")) # WHO age and sex specific incidence

WHOinc <- WHOinc %>%
  filter(risk_factor == 'all') %>% 
  filter(!age_group %in% c('all', '0-14', '15plus', '18plus')) %>% 
  group_by(iso3, year, ageWHO = age_group) %>%
  summarise(val = sum(best), lo = sum(lo), hi = sum(hi)) %>% # Summarise over sex
  mutate(ageWHO = case_when(ageWHO == '0-4' ~ '00-04', ageWHO == '5-14' ~ '05-14', 
                            ageWHO == '65plus' ~ '65+', TRUE ~ ageWHO))

# 1.3 Age group populations
WPP <- import(here("sources", "pop", "WPP_Pop_1950-2100.csv")) # World Population Prospects 2022

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
WHOkey <- import(here("sources", "who", "WHOkey.csv")) %>% # WHO countries ISO codes and regions
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
rm(WPP)

WHOinc_reg <- WHOinc %>%
  group_by(reg, ageARI) %>%
  summarise(relpcTB_reg = mean(relpcTB, na.rm = TRUE))

WHOinc <- WHOinc %>% 
  left_join(WHOinc_reg, by = c('reg', 'ageARI')) %>% 
  mutate(relpcTB = ifelse(is.na(relpcTB), relpcTB_reg, relpcTB)) %>% 
  within(rm(refpcTB, pcTB, relpcTB_reg))
rm(WHOinc_reg)

png(here("plots", "03_relinc.png"), width = 8, height = 6, units = 'in', res = 1000)
ggplot() +
  facet_wrap(~reg) +
  geom_line(WHOinc, mapping = aes(x = ageARI, y = relpcTB, group = iso3), alpha = 0.5) +
  geom_hline(yintercept = 1, col = 2) +
  scale_y_sqrt(breaks = c(1, 10, 40, 80, 120, 160)) +
  scale_x_discrete(labels = c("0-15", "15-45", "45+")) +
  labs(y = 'Square-root of relative TB incidence per capita', x = 'Age group (Years)') +
  theme_bw() +
  theme(text = element_text(family = "Open Sans"))
dev.off()

# 3. Mixing matrices ==========
MIX <- MIX %>% 
  left_join(unique(AGEkey[, c("ageMIX", "ageARI")]), by = c("AO" = "ageMIX")) %>%
  rename(ageAO = ageARI) %>%
  left_join(unique(AGEkey[, c("ageMIX", "ageARI")]), by = c("AI" = "ageMIX")) %>%
  rename(ageAI = ageARI) %>% 
  group_by(iso3, ageAO, ageAI) %>% 
  summarise(ctx = sum(ctx)) %>% 
  inner_join(WHOkey, by = 'iso3')
rm(AGEkey)
  
MIXreg <- MIX %>%
  group_by(reg, ageAO, ageAI) %>%
  summarise(ctx = mean(ctx))

png(here("plots", "03_ctxpatterns.png"), width = 8, height = 6, units = 'in', res = 1000)
ggplot() +
  facet_wrap(~reg) +
  geom_tile(MIXreg, mapping = aes(x = ageAO, y = ageAI, fill = ctx)) +
  scale_fill_viridis(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  scale_y_discrete(labels = c("0-15", "15-45", "45+"), expand = c(0, 0)) +
  scale_x_discrete(labels = c("0-15", "15-45", "45+"), expand = c(0, 0)) +
  labs(x = 'Age of individual', y = 'Age of contact', fill = 'Average number of contacts') +
  theme_bw() +
  theme(legend.position = 'bottom', legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 10), legend.title = element_text(size = 10, margin = margin(b = 15, r = 5)),
        legend.key.width = unit(2, 'cm'), legend.key.height = unit(0.5, 'cm'),
        text = element_text(family = "Open Sans"))
dev.off()
rm(MIXreg)

# 4. Relative ARIs ========== 
relARI <- WHOinc %>%
  select(iso3, reg, ageARI, val, pop, relpcTB) %>%
  left_join(MIX, by = c("iso3", "reg", "ageARI" = "ageAI")) %>% 
  within(rm(year)) %>%
  filter(!is.na(ctx)) %>% 
  mutate(relpcTB = ifelse(ageARI == '00-14', 0, relpcTB)) %>%
  mutate(ari = relpcTB * ctx) %>% 
  group_by(reg, iso3, ageAO) %>% 
  summarise(ari = sum(ari)) %>% 
  group_by(iso3, ageAO) %>% 
  mutate(refari = ifelse(ageAO == '00-14', ari, NA)) %>% 
  group_by(iso3) %>%
  mutate(refari = ifelse(!is.na(refari), refari, first(na.omit(refari)))) %>%
  ungroup() %>% 
  mutate(relari = ari / refari) %>% 
  select(iso3, reg, ageARI = ageAO, relari) %>% 
  arrange(iso3, ageARI)
rm(WHOinc, MIX)

relARIreg <- relARI %>%
  group_by(reg, ageARI) %>%
  summarise(med = median(relari, na.rm = TRUE),
            lo = quantile(relari, 0.025), hi = quantile(relari, 0.975))
print(relARIreg) # Table S3

relARI %>% # Table S3
  group_by(ageARI) %>%
  summarise(med = median(relari, na.rm = TRUE),
            lo = quantile(relari, 0.025), hi = quantile(relari, 0.975))

png(here("plots", "03_relari.png"), width = 8, height = 6, units = 'in', res = 1000)
ggplot() +
  facet_wrap(~reg) +
  geom_line(relARI, mapping = aes(x = ageARI, y = relari, group = iso3), alpha = 0.5) +
  geom_line(relARIreg, mapping = aes(x = ageARI, y = med, group = reg), colour = "#FF0000") +
  geom_hline(yintercept = 1, col = 2) +
  scale_y_continuous(breaks = seq(1, 5, 1), limits = c(1, 5)) +
  scale_x_discrete(labels = c("0-15", "15-45", "45+")) +
  labs(y = 'Relative annual risk of infection', x = 'Age group (Years)') +
  theme_bw() +
  theme(text = element_text(family = "Open Sans"))
dev.off()

relARIreg <- relARI %>%
  group_by(reg, ageARI) %>%
  summarise(relari = mean(relari, na.rm = TRUE))

# 5. Age-adjusted ARIs ========== 
input <- c("GP_rev.Rdata", "GPruns_rev.Rdata")
output <- c("mARI_rev_mix.Rdata", "ARI_rev_mix.Rdata")

for (i in 1:length(input)) {
  ARI <- as.data.table(import(here("data", "gp", input[i])))

  ARI <- ARI %>% 
    left_join(WHOkey, by = 'iso3') %>% 
    left_join(relARI, by = c('iso3', 'reg'), relationship = 'many-to-many')
  
  ARIna <- ARI %>% 
    filter(is.na(relari)) %>% 
    within(rm(ageARI, relari)) %>% 
    left_join(relARIreg, by = c('reg'), relationship = 'many-to-many')
  
  ARI <- ARI %>% 
    filter(!is.na(relari)) %>% 
    rbind(ARIna) %>% 
    mutate(ari = exp(lari + (log(relari)))) %>% 
    select(year, iso3, reg, starts_with("rep"), ageARI, ari) %>% 
    arrange(iso3, year)
  
  export(ARI, here("data", "ari", output[i]))
}

ARI_UI <- ARI %>% 
  group_by(year, iso3, reg, ageARI) %>% 
  summarise(med = median(ari), lo = quantile(ari, 0.025), hi = quantile(ari, 0.975))
export(ARI_UI, here("data", "ari", "ARI_rev_mix_UI.Rdata"))

rm(list=ls())
