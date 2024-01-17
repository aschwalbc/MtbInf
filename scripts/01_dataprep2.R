## Analysis code for Schwalb et al. 2024
## Adapted from Houben & Dodd 2016
## Distributed under CC BY 4.0
## RScript 01: DataPrep.R

# Packages ==========
library(rio)
library(here)
library(tidyverse)
library(data.table)
library(naniar)
options(scipen = 999)

# 1. Indirect ARI estimates ==========
# 1.1 IHME TB prevalence
years <- 1990:2019
IHME <- list()

IHME <- rbindlist(lapply(years, function(year) {
  file_path <- here("data", "sources", "ihme", paste0(year, ".csv"))
  as.data.table(fread(file_path))
}))

IHMEkey <- import(here("data","sources","ihme","IHMEkey.csv")) # Provides ISO3

IHME <- IHME %>% 
  merge(IHMEkey, by = c('location_id', 'location_name')) %>%
  filter(measure_name == 'Prevalence', 
         cause_id %in% c(934, 946, 947), 
         metric_name == 'Number') %>%
  group_by(iso3, year) %>% 
  summarise(prev = sum(val), prev_lo = sum(lower), prev_hi = sum(upper))
rm(IHMEkey)

# 1.2 WHO TB estimates
WHOn <- as.data.table(import(here("data","sources","who","WHOest_2000-2022.csv")))
WHOo <- as.data.table(import(here("data","sources","who","WHOest_1990-2014.csv")))

WHO <- WHOo %>% 
  filter(year < 2000) %>% 
  rbind(WHOn, fill = TRUE) %>% 
  select(iso3, year,
         inc = e_inc_num, inc_lo = e_inc_num_lo, inc_hi = e_inc_num_hi, 
         cdr = c_cdr, cdr_lo = c_cdr_lo, cdr_hi = c_cdr_hi, 
         tbhiv = e_tbhiv_prct, tbhiv_lo = e_tbhiv_prct_lo, tbhiv_hi = e_tbhiv_prct_hi) %>% 
  bind_rows(as.data.frame(.) %>% filter(iso3 == 'SCG') %>% mutate(iso3 = 'SRB'),
            as.data.frame(.) %>% filter(iso3 == 'SCG') %>% mutate(iso3 = 'MNE'),
            as.data.frame(.) %>% filter(iso3 == 'SDN', year <= 2010) %>% mutate(iso3 = 'SSD')) %>%
  filter(iso3 != 'SCG') %>% 
  arrange(iso3, year)
rm(WHOo, WHOn)

# 1.3 UN population prospects
WPP <- import(here("data","sources","pop","WPP_Pop_1950-2100.csv"))

WPP <- WPP %>% 
  select(iso3 = ISO3_code, year = Time, agegp = AgeGrp, pop = PopTotal) %>% 
  mutate(iso3 = na_if(iso3, "")) %>%
  filter(!is.na(iso3) & year %in% years) %>% 
  group_by(iso3, year) %>% 
  summarise(pop = sum(pop) * 1e3) %>% 
  arrange(iso3, year)
rm(years)

# 1.4 IHME+WHO TB estimates
PREV <- IHME %>% 
  left_join(WPP, by = c('iso3', 'year')) %>% 
  mutate(prev = prev / pop * 1e5, prev_lo = prev_lo / pop * 1e5,
         prev_hi = prev_hi / pop * 1e5, .keep = c('unused')) %>% 
  left_join(WHO, by = c('iso3', 'year')) %>%
  mutate(iso3 = factor(iso3)) %>% 
  arrange(iso3, year) %>% 
  as.data.table()
rm(IHME, WPP, WHO)

# 1.5 Estimate adjustments
# 1.5.1 Styblo ratio
# LogNormal(1.678, 0.371)
mu <- 1.678
sig <- 0.371
stb <- exp(mu + 0.5 * sig^2)
vstb <- (exp(sig^2) - 1) * exp(2 * mu + sig^2)
rm(mu, sig)

# 1.5.2 Reversion adjustment
rev <- 3.5 # True ARI = 2-5x higher, 3.5 for average (Schwalb AJE 2023)
revE <- 1.5 # Increasing variance estimates by 50%

# 1.5.3 HIV in TB
HIV <- PREV %>% 
  mutate(cdr = cdr / 100, vcdr = (cdr_hi - cdr_lo)^2 / (100 * 3.92)^2,
         tbhiv = tbhiv / 100, vtbhiv = (tbhiv_hi - tbhiv_lo)^2 / (100 * 3.92)^2) %>% 
  select(iso3, year, cdr, vcdr, tbhiv, vtbhiv) %>% 
  filter(!is.na(tbhiv) & tbhiv > 1e-2) %>% # Drop if HIV in TB below 1%
  group_by(iso3) %>% 
  mutate(cdr = if_else(is.na(cdr), min(cdr, na.rm = TRUE), cdr), # Use min CDR
         vcdr = if_else(is.na(vcdr), max(vcdr, na.rm = TRUE), vcdr)) %>% # Use max CDR
  ungroup() %>% 
  mutate(vcdr = if_else(cdr > 1, 0, vcdr), # Sets null variance for CDR >100%
         cdr = if_else(cdr > 1, 1, cdr)) # Sets max CDR as 100%

# Smear-positivity of TB in people living with HIV (Factor f):
# f~U[0.3,0.4]/U[0.4,0.5]
# Factor f (mean) = ((0.4+0.3)/2)/((0.5+0.4)/2)
mf <- 0.780
# Factor f (variance) = ((1/12)*(0.4-0.3)^2)/((1/12)*(0.5-0.4)^2)
vf <- 0.00666 

# HIV on smear-positivity - Duration of TB disease by HIV and notification status (T):
# T1n~U[0.2,2] = HIV negative, notified
# T1u~U[1,4] = HIV negative, un-notified
# T2n~U[0.01,1] = HIV positive, notified
# T2u~U[0.01,0.22] = HIV positive, un-notified

HIV <- HIV %>% 
  mutate(T1 = cdr * (2 + 0.2) / 2 + (1 - cdr) * (4 + 1) / 2, # T1 = CDR*T1n + (1-CDR)*T1u 
         vT1 = vcdr * 0.25 * (2 + 0.2 - 4 - 1)^2 + cdr^2 * (2 - 0.2)^2 / 12 + (1 - cdr)^2 * (4 - 1)^2 / 12,
         T2 = cdr * (1 + 0.01) / 2 + (1 - cdr) * (0.2 + 0.01) / 2, # T2 = CDR*T2n + (1-CDR)*T2u
         vT2 = vcdr * 0.25 * (1 + 0.01 - 0.2 - 0.01)^2 + cdr^2 * (1 - 0.01)^2 / 12 + (1 - cdr)^2 * (0.2 - 0.01)^2 / 12) %>% 
  mutate(A = (tbhiv * T2 * mf + (1 - tbhiv) * T1), 
         B = (tbhiv * T2 + (1 - tbhiv)* T1)) %>% 
  mutate(S = A / B) %>% # Smear-positivity adjustment factor
  mutate(vS = vtbhiv * (tbhiv * (1 - tbhiv) * (1 - mf) * T1 / (A * B))^2 +
           (T1^2 * vT2 + T2^2 * vT1) * (tbhiv * (1 - tbhiv) * (1 - mf) / (A * B))^2 +
           vf * (tbhiv * T2 / A)^2) %>% # Variance of smear-positivity adjustment factor
  select(iso3, year, S, vS) %>% 
  mutate(iso3 = factor(iso3)) %>% 
  arrange(iso3, year) %>% 
  as.data.table()
rm(mf, vf)

# 1.5.4 Childhood TB
KID <- as.data.table(import(here("data","sources","others","TBinc_kids.csv")))
pKID <- as.data.table(import(here("data","sources","others","TBinc_kids_prop.csv"))) # Dodd et al. Lancet GH 2014

# Smear-positivity by age (Kunkel et al. BMC ID 2016)
YK <- 0.5e-2; vYK <- (1.9 * 1e-2 / 3.92)^2 # Children aged 0-4 (YK) = 0.5% (0.0 - 1.9)
OK <- 14e-2; vOK <- ((19.4 - 8.9) * 1e-2 / 3.92)^2 # Children aged 5-14 (OK) = 14.0% (8.9 - 19.4)
A <- 0.52; vA <- ((0.64 - 0.4) / 3.92)^2 # Adults (A) = 52.0% (40.0 - 64.0)

KID <- PREV %>% 
  filter(year == 2014) %>% 
  select(iso3, inc, inc_lo, inc_hi) %>% 
  left_join(KID, by = 'iso3') %>% 
  rename(k_inc = inc.num, k_inc_sd = inc.num.sd, k_pop = e.pop.014) %>% 
  mutate(pk = k_inc / inc) %>% # Proportion of TB in children
  mutate(vpk = pk^2 * (k_inc_sd^2 / k_inc^2 + ((inc_hi - inc_lo) / 3.92)^2 / inc^2)) %>% 
  mutate(pk = if_else(pk > 1, 0.1, pk), # Max extreme values to 10%
         vpk = if_else(vpk > 1e-2, 1e-2, vpk)) %>% # Max extreme values
  filter(!is.na(vpk)) %>% 
  left_join(pKID, by = 'iso3') %>% 
  within(rm(g_whoregion, country)) %>% 
  mutate(pu5 = a / (a + b), vpu5 = (1 - pu5) * pu5 / (a + b + 1)) %>% 
  mutate(mFr = pk * pu5 * YK + pk * (1 - pu5) * OK + (1 - pk) * A) %>%  # Fraction smear-positivity
  mutate(vFr = mFr^2 * (vpk * (pu5 * YK + (1 - pu5) * OK - A)^2 + vpu5 * (pk * YK - pk * OK)^2 +
          vYK * (pk * pu5)^2 + vOK * (pk * (1 - pu5))^2 + vA * (1 - pk)^2)) %>% 
  select(iso3, mFr, vFr) %>% 
  filter(!is.na(mFr)) %>% 
  mutate(iso3 = factor(iso3)) %>% 
  arrange(iso3) %>% 
  as.data.table()
rm(YK, vYK, OK, vOK, A, vA, pKID)

# 1.6 ARI estimation
GTB <- PREV %>% 
  left_join(HIV, by = c('iso3', 'year')) %>% 
  left_join(KID, by = 'iso3') %>% 
  mutate(mFr = if_else(is.na(mFr), median(mFr, na.rm = TRUE), mFr),
         vFr = if_else(is.na(vFr), median(vFr, na.rm = TRUE), vFr),
         vS = if_else(is.na(S), 0, vS), 
         S = if_else(is.na(S), 1, S)) %>% 
  select(iso3, year, prev, prev_lo, prev_hi, S, vS, mFr, vFr) %>% 
  mutate(iso3 = factor(iso3)) %>% 
  arrange(iso3) %>% 
  as.data.table()
rm(PREV, HIV, KID)

ARI <- GTB %>% 
  mutate(ari = prev * 1e-5 * S * mFr * stb * rev) %>% 
  mutate(lari = log(ari)) %>% 
  mutate(sd = (prev_hi - prev_lo) / (3.92 * prev)) %>% 
  mutate(E = sqrt(sd^2 + vstb / stb^2 + vS + vFr / mFr^2) * revE) %>% 
  select(iso3, year, ari, E, lari) %>% 
  mutate(type = 'Prevalence estimate')
rm(stb, vstb)

# 2. Direct ARI estimates ==========
# 2.1 Cauthen et al.
CAU <- as.data.table(import(here("data","sources","surveys","ARI_Cauthen.csv")))

CAU <- CAU %>% 
  rename_with(tolower) %>% 
  mutate(ari = ari * 1e-2,
         age = {start <- regexpr('\\(', age); stop <- regexpr('\\)', age)
         as.numeric(substr(as.character(age), start = start + 1, stop = stop - 1))}) %>% 
  mutate(var = ari / (age * n)) %>% 
  mutate(E = sqrt(var) / ari) %>% 
  mutate(ari = ari * rev, E = E * revE) %>% 
  mutate(lari = log(ari)) %>% 
  mutate(type = 'Mtb survey') %>% 
  select(iso3, year, ari, E, lari, type) %>% 
  na.omit() %>%
  mutate(iso3 = factor(iso3)) %>% 
  arrange(iso3, year) %>% 
  as.data.table()

# 2.2 Review of Mtb surveys
REV <- as.data.table(import(here("data","sources","surveys","ARI_SystRev.csv")))

REV <- REV %>% 
  rename_with(tolower) %>% 
  mutate(ari = ari * 1e-2,
         age = {start <- regexpr('\\(', age); stop <- regexpr('\\)', age)
         as.numeric(substr(as.character(age), start = start + 1, stop = stop - 1))}) %>%
  mutate(var = ari / (age * n)) %>% 
  mutate(var = if_else(!is.na(ari_hi), (1.96e-2 * (ari_hi - ari_lo))^2, ari / (age * n))) %>% 
  mutate(E = sqrt(var) / ari) %>% 
  mutate(ari = ari * rev, E = E * revE) %>% 
  mutate(lari = log(ari)) %>% 
  mutate(type = 'Mtb survey') %>% 
  select(iso3, year, ari, E, lari, type) %>% 
  na.omit() %>%
  mutate(iso3 = factor(iso3)) %>% 
  arrange(iso3, year) %>% 
  as.data.table()


  