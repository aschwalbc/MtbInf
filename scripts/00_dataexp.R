## Analysis code for Schwalb et al. 2024
## Distributed under CC BY 4.0
## RScript 00: DataExp.R

# Packages ==========
library(rio)
library(here)
library(tidyverse)
library(data.table)
library(naniar)
options(scipen = 999)

# 1. WHO estimates ==========
WHOn <- as.data.table(import(here("data","sources","who","WHOest_2000-2022.csv"))) %>% 
  mutate(type = 'new')

WHOo <- as.data.table(import(here("data","sources","who","WHOest_1990-2014.csv"))) %>% 
  mutate(type = 'old')

WHO <- WHOo %>% 
  rbind(WHOn, fill = TRUE) %>% 
  select(type, iso3, year,
         prev = e_prev_100k, prev_lo = e_prev_100k_lo, prev_hi = e_prev_100k_hi,
         inc = e_inc_num, inc_lo = e_inc_num_lo, inc_hi = e_inc_num_hi, 
         cdr = c_cdr, cdr_lo = c_cdr_lo, cdr_hi = c_cdr_hi, 
         tbhiv = e_tbhiv_prct, tbhiv_lo = e_tbhiv_prct_lo, tbhiv_hi = e_tbhiv_prct_hi) %>% 
  bind_rows(as.data.frame(.) %>% filter(iso3 == 'SCG') %>% mutate(iso3 = 'SRB'),
            as.data.frame(.) %>% filter(iso3 == 'SCG') %>% mutate(iso3 = 'MNE'),
            as.data.frame(.) %>% filter(iso3 == 'SDN', year <= 2010) %>% mutate(iso3 = 'SSD')) %>%
  filter(iso3 != 'SCG') %>% 
  mutate(iso3 = as.factor(iso3)) %>% 
  arrange(type, iso3, year)
rm(WHOo, WHOn)

WHOinc <- WHO %>%
  group_by(year, type) %>% 
  summarise(inc = sum(inc, na.rm = TRUE), 
            inc_lo = sum(inc_lo, na.rm = TRUE), 
            inc_hi = sum(inc_hi, na.rm = TRUE))

pWHOinc <- ggplot(WHOinc) +
  geom_line(aes(x = year, y = inc, colour = type)) +
  geom_ribbon(aes(x = year, ymin = inc_lo, ymax = inc_hi, fill = type), alpha = 0.2) +
  scale_color_manual(values = c("#FA8072", "turquoise"), labels = c("New", "Old")) +
  scale_fill_manual(values = c("#FA8072", "turquoise"), labels = c("New", "Old")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(x = 'Year', y = 'Total TB incidence', title = 'WHO TB incidence estimates',
       colour = 'Source', fill = 'Source') +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(here("plots", "dataexp", "WHO_inc.png"), plot = pWHOinc, width = 20, height = 15, units = "cm")

# 2. WHO prevalence/incidence ratios ==========
WHO2013 <- as.data.table(import(here("data","sources","who","ratios","TB_burden_countries_2013.csv")))
WHO2016 <- as.data.table(import(here("data","sources","who","ratios","TB_burden_countries_2016.csv")))
survey <- as.data.table(import(here("data","sources","surveys","tbprev_surv.csv")))

WHO2013 <- WHO2013 %>% 
  select(iso3, year, g_whoregion, starts_with('e_prev_num'), starts_with('e_inc_num')) %>% 
  filter(!e_prev_num < 10 | !e_inc_num < 10) %>% 
  mutate(previnc = e_prev_num / e_inc_num, previnc_lo = e_prev_num_lo / e_inc_num_lo, previnc_hi = e_prev_num_hi / e_inc_num_hi) %>% 
  mutate(type = '2013 DB') %>% 
  select(iso3, year, g_whoregion, type, previnc, previnc_lo, previnc_hi)

WHO2016 <- WHO2016 %>% 
  select(iso3, year, g_whoregion, starts_with('e_prev_num'), starts_with('e_inc_num')) %>% 
  filter(!e_prev_num < 10 | !e_inc_num < 10) %>% 
  mutate(previnc = e_prev_num / e_inc_num, previnc_lo = e_prev_num_lo / e_inc_num_lo, previnc_hi = e_prev_num_hi / e_inc_num_hi) %>% 
  mutate(type = '2016 DB') %>% 
  select(iso3, year, g_whoregion, type, previnc, previnc_lo, previnc_hi)

WHOratios <- WHO2013 %>% 
  rbind(WHO2016) %>% 
  group_by(iso3) %>%
  filter(!n() < 10) %>%
  ungroup()
rm(WHO2013, WHO2016)

iso <- unique(WHOratios$iso3)
surv <- unique(survey$iso3)

pdf(here("plots", "dataexp", "WHOratios.pdf"), height = 6, width = 10)
for(i in iso) {
  db <- filter(WHOratios, iso3 == i)
  p <- ggplot(db) +
    facet_wrap(~iso3) +
    geom_line(aes(x = year, y = previnc, colour = type)) +
    geom_ribbon(aes(x = year, ymin = previnc_lo, ymax = previnc_hi, fill = type), alpha = 0.2) +
    scale_color_manual(values = c("#72A8FA", "#FA8072"), labels = c("2013 DB", "2016 DB")) +
    scale_fill_manual(values = c("#72A8FA", "#FA8072"), labels = c("2013 DB", "2016 DB")) +
    scale_y_continuous(breaks = seq(0, 6, 0.5), limits = c(0, 6.5)) +
    labs(x = 'Year', y = 'Prevalence/incidence ratio', colour = 'Source', fill = 'Source') +
    theme_bw() +
    theme(legend.position = 'bottom')
  print(p)
}
dev.off()

pdf(here("plots", "dataexp", "WHOratios_surv.pdf"), height = 6, width = 10)
for(i in surv) {
  db <- filter(WHOratios, iso3 == i)
  p <- ggplot(db) +
    facet_wrap(~iso3) +
    geom_line(aes(x = year, y = previnc, colour = type)) +
    geom_ribbon(aes(x = year, ymin = previnc_lo, ymax = previnc_hi, fill = type), alpha = 0.2) +
    scale_color_manual(values = c("#72A8FA", "#FA8072"), labels = c("2013 DB", "2016 DB")) +
    scale_fill_manual(values = c("#72A8FA", "#FA8072"), labels = c("2013 DB", "2016 DB")) +
    scale_y_continuous(breaks = seq(0, 6, 0.5), limits = c(0, 6.5)) +
    labs(x = 'Year', y = 'Prevalence/incidence ratio', colour = 'Source', fill = 'Source') +
    theme_bw() +
    theme(legend.position = 'bottom')
  print(p)
}
dev.off()

gWHOratios <- WHOratios %>% 
  group_by(type, year) %>% 
  summarise(previnc = mean(previnc, na.rm = TRUE), 
            previnc_lo = mean(previnc_lo, na.rm = TRUE), 
            previnc_hi = mean(previnc_hi, na.rm = TRUE))

pgWHOratios <- ggplot(gWHOratios) +
  geom_line(aes(x = year, y = previnc, colour = type)) +
  geom_ribbon(aes(x = year, ymin = previnc_lo, ymax = previnc_hi, fill = type), alpha = 0.2) +
  scale_color_manual(values = c("#72A8FA", "#FA8072"), labels = c("2013 DB", "2016 DB")) +
  scale_fill_manual(values = c("#72A8FA", "#FA8072"), labels = c("2013 DB", "2016 DB")) +
  labs(x = 'Year', y = 'Prevalence/incidence ratio', colour = 'Source', fill = 'Source') +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(here("plots", "dataexp", "gWHOratios.png"), plot = pgWHOratios, width = 20, height = 15, units = "cm")

regWHOratios <- WHOratios %>% 
  group_by(type, year, g_whoregion) %>% 
  summarise(previnc = mean(previnc, na.rm = TRUE), 
            previnc_lo = mean(previnc_lo, na.rm = TRUE), 
            previnc_hi = mean(previnc_hi, na.rm = TRUE))

pregWHOratios <- ggplot(regWHOratios) +
  facet_wrap(~g_whoregion) +
  geom_line(aes(x = year, y = previnc, colour = type)) +
  geom_ribbon(aes(x = year, ymin = previnc_lo, ymax = previnc_hi, fill = type), alpha = 0.2) +
  scale_color_manual(values = c("#72A8FA", "#FA8072"), labels = c("2013 DB", "2016 DB")) +
  scale_fill_manual(values = c("#72A8FA", "#FA8072"), labels = c("2013 DB", "2016 DB")) +
  labs(x = 'Year', y = 'Prevalence/incidence ratio', colour = 'Source', fill = 'Source') +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(here("plots", "dataexp", "regWHOratios.png"), plot = pregWHOratios, width = 20, height = 15, units = "cm")

# 3. IHME estimates ==========
years <- 1990:2019
IHME <- list()

IHME <- rbindlist(lapply(years, function(year) {
  file_path <- here("data", "sources", "ihme", paste0(year, ".csv"))
  as.data.table(fread(file_path))
}))

IHME <- IHME %>% 
  filter(measure_name == 'Incidence', 
         cause_id %in% c(934, 946, 947), 
         metric_name == 'Number') %>%
  group_by(year) %>% 
  summarise(inc = sum(val), inc_lo = sum(lower), inc_hi = sum(upper)) %>% 
  mutate(type = 'ihme')

pIHMEinc <- ggplot(rbind(WHOinc, IHME)) +
  geom_line(aes(x = year, y = inc, colour = type)) +
  geom_ribbon(aes(x = year, ymin = inc_lo, ymax = inc_hi, fill = type), alpha = 0.2) +
  scale_color_manual(values = c("#72A8FA", "#FA8072", "#40E0D0"), labels = c("IHME", "WHO (New)", "WHO (Old)")) +
  scale_fill_manual(values = c("#72A8FA", "#FA8072", "#40E0D0"), labels = c("IHME", "WHO (New)", "WHO (Old)")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(x = 'Year', y = 'Total TB incidence', title = 'TB incidence estimates',
       colour = 'Source', fill = 'Source') +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(here("plots", "dataexp", "IHME_inc.png"), plot = pIHMEinc, width = 20, height = 15, units = "cm")

# 4. TB prevalence surveys ==========
IHME <- as.data.table(import(here("data","sources","ihme","IHME-GBD.csv")))
IHMEkey <- as.data.table(import(here("data","sources","ihme","IHMEkey.csv"))) 
survey <- as.data.table(import(here("data","sources","surveys","tbprev_surv.csv")))
WPP <- import(here("data","sources","pop","WPP_Pop_1950-2100.csv"))

IHME <- IHME %>%
  left_join(IHMEkey, by = c('location_id', 'location_name')) %>%
  select(iso3, year, age = age_name, cause_id, prev = val, lo = lower, hi = upper) %>%
  group_by(iso3, year, age) %>%
  summarise(prev = sum(prev), lo = sum(lo), hi = sum(hi), .groups = 'drop') %>% 
  group_by(iso3, year) %>% 
  summarise(prev5 = sum(prev, na.rm = TRUE),
            prev10 = sum(if_else(age %in% c('10-14 years', '15-19 years', '20-54 years', '+55 years'), prev, 0), na.rm = TRUE),
            prev15 = sum(if_else(age %in% c('15-19 years', '20-54 years', '+55 years'), prev, 0), na.rm = TRUE),
            lo5 = sum(lo, na.rm = TRUE),
            lo10 = sum(if_else(age %in% c('10-14 years', '15-19 years', '20-54 years', '+55 years'), lo, 0), na.rm = TRUE),
            lo15 = sum(if_else(age %in% c('15-19 years', '20-54 years', '+55 years'), lo, 0), na.rm = TRUE),
            hi5 = sum(hi, na.rm = TRUE),
            hi10 = sum(if_else(age %in% c('10-14 years', '15-19 years', '20-54 years', '+55 years'), hi, 0), na.rm = TRUE),
            hi15 = sum(if_else(age %in% c('15-19 years', '20-54 years', '+55 years'), hi, 0), na.rm = TRUE), .groups = 'drop') %>% 
  pivot_longer(cols = -c(iso3, year), names_to = c(".value", "agegp"), 
               names_pattern = "(prev|lo|hi)(\\d+)", values_to = c("prev", "lo", "hi")) %>%
  mutate(agegp = case_when(agegp == "5" ~ ">5", agegp == "10" ~ ">10", agegp == "15" ~ ">15"))
rm(IHMEkey)

WPP <- WPP %>% 
  select(iso3 = ISO3_code, year = Time, age = AgeGrp, pop = PopTotal) %>%
  mutate(iso3 = na_if(iso3, "")) %>%
  filter(!is.na(iso3) & year %in% 1990:2019) %>%
  group_by(iso3, year) %>%
  summarise(pop5 = sum(if_else(age != '0-4', pop, 0), na.rm = TRUE),
    pop10 = sum(if_else(age %in% c('10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', 
                                   '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79',
                                   '80-84', '85-89', '90-94', '95-99', '100+'), pop, 0), na.rm = TRUE),
    pop15 = sum(if_else(age %in% c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49',
                                   '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84',
                                   '85-89', '90-94', '95-99', '100+'), pop, 0), na.rm = TRUE)) %>% 
  pivot_longer(cols = starts_with("pop"), names_to = "agegp", values_to = "pop", names_prefix = "pop",
               names_transform = list(agegp = ~ case_when(.x == "5" ~ ">5", .x == "10" ~ ">10", .x == "15" ~ ">15"))) %>% 
  mutate(pop = pop * 1e3)

IHME <- IHME %>% 
  left_join(WPP, by = c('iso3', 'year', 'agegp')) %>% 
  mutate(prev = prev / pop * 1e5, lo = lo / pop * 1e5, hi = hi / pop * 1e5) %>% 
  mutate(type = 'ihme') %>% 
  within(rm(pop)) %>% 
  rbind(survey) %>% 
  select(iso3, year, agegp, type, prev, lo, hi) %>% 
  mutate(iso3 = as.factor(iso3)) %>% 
  arrange(iso3, year, agegp)
rm(WPP, survey)

iso <- unique(IHME$iso3)
age <- unique(IHME$agegp)

pdf(here("plots", "dataexp", "IHMETBsurv_prev.pdf"), height = 6, width = 10)
for(i in iso) {
  for(a in age) {
    db <- filter(IHME, iso3 == i, agegp == a)
    if(any(db$type != 'ihme' & !is.na(db$prev))){
      p<- ggplot() +
        geom_line(filter(db, type == 'ihme'), mapping = aes(x = year, y = prev), colour = '#FA8072') +
        geom_ribbon(filter(db, type == 'ihme'), mapping = aes(x = year, ymin = lo, ymax = hi), fill = '#FA8072', alpha = 0.2) +
        geom_point(filter(db, type != 'ihme'), mapping = aes(x = year, y = prev, shape = type), colour = '#A05249') +
        geom_errorbar(filter(db, type != 'ihme'), mapping = aes(x = year, ymin = lo, ymax = hi), colour = '#A05249', width = 0.8) +
        scale_shape_manual(values = c(16, 17), labels = c("Bact+", "Smear+")) +
        labs(x = 'Year', y = 'TB prevalence rate per 100k', title = 'IHME vs TB prevalence surveys',
             subtitle = paste(i, a, sep = ' - '), shape = 'Type') +
        theme_bw() +
        theme(legend.position = 'bottom')
      print(p)
    }
  }
}
dev.off()
