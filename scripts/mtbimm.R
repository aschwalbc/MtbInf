# Mtb to Immunoreactivity
# Packages ==========
library(rio)
library(here)
library(tidyverse)
library(data.table)

# 0. Reversion adjustment ==========
rev <- 2.9 # True ARI = 2-5x higher (Schwalb AJE 2023)
erev <- 1.5 # Increasing variance estimates by 50%

# 1. Country list ==========
# 1.1 Filtering countries with population size <500k in 2022
wpp <- import(here("sources", "pop", "WPP_Pop_1950-2100.csv"))

wpp <- wpp %>% # Population data
  select(iso3 = ISO3_code, year = Time, ageWPP = AgeGrp, pop = PopTotal) %>%
  mutate(iso3 = na_if(iso3, "")) %>%
  filter(!is.na(iso3)) %>%
  filter(year == 2022) %>%
  group_by(iso3, year) %>%
  summarise(pop = sum(pop) * 1e3) %>%
  filter(pop >= 500000) %>%
  pull(unique(iso3)) # 175 countries

# 2. Direct ARI estimates ==========
# 2.1 Cauthen et al.
cau <- as.data.table(import(here("sources", "survs", "ari_cauthen.csv")))

cau <- cau %>%
  filter(iso3 %in% wpp) %>%
  mutate(year = ceiling(year)) %>%
  mutate(ari = ari * 1e-2,
         age = {
           start <- regexpr("\\(", age)
           stop <- regexpr("\\)", age)
           as.numeric(substr(as.character(age), start = start + 1, stop = stop - 1))
         }) %>%
  mutate(var = ari / (age * n)) %>%
  mutate(E = sqrt(var) / ari) %>%
  mutate(ari = ari * rev, E = E * erev) %>%
  mutate(lari = log(ari)) %>%
  select(iso3, year, ari, E, lari, age) %>%
  na.omit() %>%
  mutate(iso3 = factor(iso3)) %>%
  arrange(iso3, year) %>%
  as.data.table()

# 2.2 Mtb surveys
irs <- as.data.table(import(here("sources", "survs", "ari_sysrev.csv")))

irs <- irs %>%
  filter(iso3 %in% wpp) %>%
  mutate(year = ceiling(year)) %>%
  mutate(ari = ari * 1e-2) %>%
  mutate(var = ari / (mage * n)) %>%
  mutate(var = if_else(!is.na(ari_hi), (1.96e-2 * (ari_hi - ari_lo))^2, ari / (mage * n))) %>%
  mutate(E = sqrt(var) / ari) %>%
  mutate(ari = ari * rev, E = E * erev) %>%
  mutate(lari = log(ari)) %>%
  select(iso3, year, ari, E, lari, age = mage) %>%
  na.omit() %>%
  mutate(iso3 = factor(iso3)) %>%
  arrange(iso3, year) %>%
  as.data.table()
rm(rev, erev)

# 2.3 ARI estimates
ari <- cau %>%
  rbind(irs) %>%
  mutate(sd = E / 1.96) %>%
  mutate(lo = exp(lari - sd), hi = exp(lari + sd)) %>%
  mutate(val = exp(lari)) %>%
  mutate(agegp = case_when(
    age < 5 ~ "00-05",
    age >= 5 & age < 10 ~ "05-10",
    age >= 10 & age < 15 ~ "10-15",
    age >= 15 & age < 20 ~ "15-20",
    age >= 20 & age < 25 ~ "20-25",
    age >= 25 & age < 30 ~ "25-30"
  )) %>%
  mutate(type = "ari") %>%
  select(iso3, year, agegp, type, val, lo, hi) %>%
  arrange(iso3, year, agegp)
rm(cau, irs)

# 3. Immunoreactivity prevalence estimates ==========
# 3.1 Cauthen et al.
cau <- as.data.table(import(here("sources", "survs", "ari_cauthen.csv")))

cau <- cau %>%
  filter(iso3 %in% wpp) %>%
  mutate(year = ceiling(year)) %>%
  mutate(ari = ari * 1e-2,
         age = {
           start <- regexpr("\\(", age)
           stop <- regexpr("\\)", age)
           as.numeric(substr(as.character(age), start = start + 1, stop = stop - 1))
         }) %>%
  mutate(var = ari / (age * n)) %>%
  mutate(E = sqrt(var) / ari) %>%
  mutate(lari = log(ari)) %>%
  mutate(sd = E / 1.96) %>%
  mutate(lo = exp(lari - sd), hi = exp(lari + sd)) %>%
  select(iso3, year, ari, lo, hi, age) %>%
  na.omit() %>%
  mutate(iso3 = factor(iso3)) %>%
  arrange(iso3, year) %>%
  as.data.table()

# 2.2 Mtb surveys
irs <- as.data.table(import(here("sources", "survs", "ari_sysrev.csv")))

irs <- irs %>%
  filter(iso3 %in% wpp) %>%
  mutate(year = ceiling(year)) %>%
  mutate(ari = ari * 1e-2,
         ari_lo = ari_lo * 1e-2,
         ari_hi = ari_hi * 1e-2) %>%
  mutate(var = ari / (mage * n)) %>%
  mutate(var = if_else(!is.na(ari_hi), (1.96e-2 * (ari_hi - ari_lo))^2, ari / (mage * n))) %>%
  mutate(E = sqrt(var) / ari) %>%
  mutate(lari = log(ari)) %>%
  mutate(lo = ifelse(is.na(ari_lo) | is.nan(ari_lo), exp(lari - (E / 1.96)), ari_lo),
         hi = ifelse(is.na(ari_hi) | is.nan(ari_hi), exp(lari + (E / 1.96)), ari_hi)) %>%
  select(iso3, year, ari, lo, hi, age = mage) %>%
  na.omit() %>%
  mutate(iso3 = factor(iso3)) %>%
  arrange(iso3, year) %>%
  as.data.table()

# 3.3 Immunoreactivity estimates
prev <- cau %>%
  rbind(irs) %>%
  mutate(year = ceiling(year)) %>%
  mutate(val = 1 - (1 - ari)^(age),
         lo = 1 - (1 - lo)^(age),
         hi = 1 - (1 - hi)^(age)) %>%
  mutate(agegp = case_when(
    age < 5 ~ "00-05",
    age >= 5 & age < 10 ~ "05-10",
    age >= 10 & age < 15 ~ "10-15",
    age >= 15 & age < 20 ~ "15-20",
    age >= 20 & age < 25 ~ "20-25",
    age >= 25 & age < 30 ~ "25-30"
  )) %>%
  mutate(type = "prev") %>%
  select(iso3, year, agegp, type, val, lo, hi) %>%
  arrange(iso3, year, agegp)
rm(cau, irs)

# 3.4 Immunoreactivity data
imm <- ari %>%
  rbind(prev) %>%
  arrange(iso3, year, agegp, type)
rm(ari, prev, wpp)

export(imm, here("outputs", "mtbimm", "imm.Rdata"))

# 4. Mtb infection estimates ==========
mtb <- import(here("outputs", "mtb_y20", "MTBiso_agepct.Rdata")) %>%
  filter(iso3 %in% unique(prv$iso3)) %>%
  filter(agegp %in% c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29")) %>%
  filter(var != "rec") %>%
  filter(year %in% unique(prv$year))
