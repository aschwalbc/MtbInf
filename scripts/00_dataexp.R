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

# 2. IHME estimates ==========
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
