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
# 1.1 Filtering countries with population size <500k in 2023
wpp <- import(here("sources", "pop", "WPP_Pop_1950-2100.csv"))

wpp <- wpp %>%
  select(iso3 = ISO3_code, year = Time, ageWPP = AgeGrp, pop = PopTotal) %>%
  mutate(iso3 = na_if(iso3, "")) %>%
  filter(!is.na(iso3)) %>%
  filter(year == 2023) %>%
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
  mutate(var = "ari") %>%
  select(iso3, year, agegp, var, val, lo, hi) %>%
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
  mutate(var = "prev") %>%
  select(iso3, year, agegp, var, val, lo, hi) %>%
  arrange(iso3, year, agegp)
rm(cau, irs)

# 3.4 Immunoreactivity data
imm <- ari %>%
  rbind(prev) %>%
  arrange(iso3, year, agegp, var)
rm(ari, prev, wpp)

export(imm, here("outputs", "mtbimm", "imm.Rdata"))

# 4. Mtb infection estimates ==========
# 4.0 Country-year data
imm <- import(here("outputs", "mtbimm", "imm.Rdata"))
iso <- as.character(unique(imm$iso3))
year <- unique(imm$year)

# 4.1 Mtb infection data
mtb <- import(here("outputs", "mtb_y20", "MTBiso_agepct.Rdata"))

mtb <- mtb %>%
  filter(iso3 %in% iso) %>%
  filter(year %in% year) %>%
  filter(agegp %in% c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29")) %>%
  mutate(agegp = case_when(
    agegp == "00-04" ~ "00-05",
    agegp == "05-09" ~ "05-10",
    agegp == "10-14" ~ "10-15",
    agegp == "15-19" ~ "15-20",
    agegp == "20-24" ~ "20-25",
    agegp == "25-29" ~ "25-30"
  )) %>%
  filter(var != "rec") %>%
  select(-reg) %>%
  mutate(var = case_when(
    var == "pI" ~ "inf",
    var == "prI" ~ "rec"
  ))
rm(iso, year)

# 5. Exploration =========
# 5.1 Combined data
mtbimm <- imm %>%
  rbind(mtb %>% semi_join(imm, by = c("iso3", "year", "agegp")))
rm(imm, mtb)

# 5.2 Ratios
# 5.2.1 Data subsets
ari <- mtbimm %>%
  filter(var == "ari") %>%
  select(iso3, year, agegp, val_ari = val, lo_ari = lo, hi_ari = hi)

prev <- mtbimm %>%
  filter(var == "prev") %>%
  select(iso3, year, agegp, val_prev = val, lo_prev = lo, hi_prev = hi)

inf <- mtbimm %>%
  filter(var == "inf") %>%
  select(iso3, year, agegp, val_inf = val, lo_inf = lo, hi_inf = hi)

rec <- mtbimm %>%
  filter(var == "rec") %>%
  select(iso3, year, agegp, val_rec = val, lo_rec = lo, hi_rec = hi)

# 5.2.2 Ratio calculations
# 5.2.2.1 ARI to total Mtb infection
rat_ariinf <- ari %>%
  inner_join(inf, by = c("iso3", "year", "agegp")) %>%
  mutate(
    var = "rat_ariinf",
    val = val_ari / val_inf,
    lo = lo_ari / lo_inf,
    hi = hi_ari / hi_inf
  ) %>%
  select(iso3, year, agegp, var, val, lo, hi)

# 5.2.2.2 ARI to recent Mtb infection
rat_arirec <- ari %>%
  inner_join(rec, by = c("iso3", "year", "agegp")) %>%
  mutate(
    var = "rat_arirec",
    val = val_ari / val_rec,
    lo = lo_ari / lo_rec,
    hi = hi_ari / hi_rec
  ) %>%
  select(iso3, year, agegp, var, val, lo, hi)
rm(ari)

# 5.2.2.3 Immunoreactivity prevalence to total Mtb infection
rat_previnf <- prev %>%
  inner_join(inf, by = c("iso3", "year", "agegp")) %>%
  mutate(
    var = "rat_previnf",
    val = val_prev / val_inf,
    lo = lo_prev / lo_inf,
    hi = hi_prev / hi_inf
  ) %>%
  select(iso3, year, agegp, var, val, lo, hi)

# 5.2.2.4 Immunoreactivity prevalence to recent Mtb infection
rat_prevrec <- prev %>%
  inner_join(rec, by = c("iso3", "year", "agegp")) %>%
  mutate(
    var = "rat_prevrec",
    val = val_prev / val_rec,
    lo = lo_prev / lo_rec,
    hi = hi_prev / hi_rec
  ) %>%
  select(iso3, year, agegp, var, val, lo, hi)
rm(prev, inf, rec)

# 5.3 Combined ratios
mtbimm <- mtbimm %>%
  rbind(rat_ariinf) %>%
  rbind(rat_arirec) %>%
  rbind(rat_previnf) %>%
  rbind(rat_prevrec) %>%
  arrange(iso3, year, agegp, var)
rm(list = ls(pattern = "^rat_"))

export(mtbimm, here("outputs", "mtbimm", "mtbimm.Rdata"))

# 5.4 Checks
mtbimm <- import(here("outputs", "mtbimm", "mtbimm.Rdata"))
summary(filter(mtbimm, var == "rat_ariinf")$val) # ARI to total Mtb infection [1.25; IQR: 1.00-1.55]
summary(filter(mtbimm, var == "rat_arirec")$val) # ARI to recent Mtb infection [2.28; IQR: 1.97-2.58]
summary(filter(mtbimm, var == "rat_previnf")$val) # Immunoreactivity prevalence to total Mtb infection [2.77; IQR: 2.14-3.66]
summary(filter(mtbimm, var == "rat_prevrec")$val) # Immunoreactivity prevalence to recent Mtb infection [5.30; IQR: 4.09-6.72]
