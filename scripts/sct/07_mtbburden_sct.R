## Mtb-Inf: Scotland

# Packages ==========
library(rio)
library(here)
library(tidyverse)
library(data.table)
library(rnaturalearth)
library(progress)
library(extrafont)
options(scipen = 1)

# 1. Load data ==========
scenario <- "y20" # Main analysis
# scenario <- "y50" # Sensitivity analysis

files <- list.files(here("outputs", paste0("sct_", scenario)), 
                    pattern = "\\.Rdata$", full.names = TRUE)

for(i in files) {
  name <- tools::file_path_sans_ext(basename(i))
  assign(name, import(i))
}
rm(files, i, name)

# 2. Tables ==========
# 2.1 Table 1 - Number of individuals with viable Mtb infection
tab1 <- MTBiso %>% 
  filter(year == 2023) %>% 
  filter(var %in% c('It', 'rIt')) %>% 
  mutate(across(c(val, lo, hi), ~ round(., 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(reg, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(reg, rIt, It) 

tab1ex <- MTBiso_kidnum %>% 
  filter(year == 2023) %>% 
  filter(agegp == '00-14') %>% 
  mutate(across(c(val, lo, hi), ~ round(., 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(reg, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(reg, krIt = rI, kIt = tI)

tab1 <- tab1 %>% 
  left_join(tab1ex, by = c('reg')) %>%
  select(reg, rIt, krIt, It, kIt) %>% 
  rename('WHO region' = reg ,'Recent infections' = rIt, 'Recent infections in children' = krIt,
         'All infections' = It, 'All infections in children' = kIt)
rm(tab1ex)

# 2.2 Table 2 - Proportion of population with viable Mtb infection
tab2 <- MTBiso %>% 
  filter(year == 2023) %>% 
  filter(var %in% c('pI', 'prI', 'rec')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 2))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.2f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(reg, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(reg, prI, pI, rec)

tab2ex <- MTBiso_kidpct %>% 
  filter(year == 2023) %>% 
  filter(agegp == '00-14') %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 2))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.2f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(reg, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(reg, pprI, ppI)

tab2 <- tab2 %>% 
  left_join(tab2ex, by = c('reg')) %>%
  select(reg, prI, pprI, pI, ppI, rec) %>% 
  rename('WHO region' = reg ,'Recent infection prevalence' = prI, 'Proportion in children (Recent)' = pprI,
         'All infection prevalence' = pI, 'Proportion in children (All)' = ppI,
         'Proportion recently infected' = rec)
rm(tab2ex)

# 2.3 Table S4 - Adjusted ARIs for top 30 high burden countries
tabs4 <- import(here("data", "ari", "ARI_rev_mix_UI_sct.Rdata")) %>% 
  filter(year == 2023) %>% 
  rename(val = lari, lo = lower, hi = upper) %>% 
  mutate(across(c(val, lo, hi), ~ exp(.))) %>%
  mutate(across(c(val, lo, hi), ~ round(. * 100, 2))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.2f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(iso3, ageARI, est) %>% 
  pivot_wider(names_from = ageARI, values_from = est) %>% 
  rename('Country' = iso3)

# 3. Plots ==========
# 3.3 Figure 4. Prevalence of infection by age group
age_labs <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", 
              "45-50", "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80+")

ggplot(filter(MTBiso_agepct, year == 2023, var != 'rec')) +
  geom_col(mapping = aes(x = agegp, y = val, fill = factor(var, levels = c("pI", "prI"))), position = "identity") +
  scale_fill_manual(values = c("pI" = "#900C3F", "prI" = "#FF5733"),
                    labels = c("pI" = "Distal infection", "prI" = "Recent infection")) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,0.002,0.0002)) +
  scale_x_discrete(labels = age_labs) + 
  labs(x = 'Age group (years)', y = expression('Percentage infected with viable '*italic('Mtb')*' infection'), fill = 'Type') +
  theme_bw() + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(vjust = -2), text = element_text(family = "Open Sans"))

# 3.4 Figure S1-6. Single country ARI trajectories
ARIreg <- ARIs %>%
  filter(type == "rev") %>%
  left_join(MTBiso %>% select(iso3, reg) %>% distinct(iso3, .keep_all = TRUE),by = "iso3")

for(i in 1:length(reg)){
  print(reg[i])
  ARIregs <- ARIreg %>% filter(reg == !!reg[i])
  
  png(here("plots", paste0("06_gplin_ari_", reg[i], ".png")), width = 16, height = 10.5, units = 'in', res = 1000)
  
  p <- ggplot(filter(ARIregs, is.na(source))) +
    facet_wrap(~iso3, ncol = ) +
    geom_line(aes(x = year, y = lari), colour = '#F9938C') +
    geom_ribbon(aes(x = year, ymin = lower, ymax = upper), fill = '#F9938C', alpha = 0.2) +
    geom_point(data = filter(ARIregs, !is.na(source)),
               aes(x = year, y = lari, shape = source), size = 1) +
    geom_errorbar(data = filter(ARIregs, !is.na(source)),
                  aes(x = year, ymin = lower, ymax = upper), linewidth = 0.1, width = 0) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 0.5, 0.05), labels = scales::label_percent()) +
    scale_x_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, 0.22)) + 
    labs(x = 'Year', y = 'Annual risk of infection', shape = 'Source') +
    theme_bw() + 
    theme(legend.position = 'bottom', panel.spacing = unit(0.75, "lines"),
          axis.title.x = element_text(vjust = -2), text = element_text(family = "Open Sans"))
  
  print(p)
  dev.off()
}

# 4. Extras ==========
# 4.1 Absolute number of infections by age group
png(here("plots", paste0("06x_reginf_agegpnum_", scenario, ".png")), width = 10, height = 6, units = 'in', res = 200)
ggplot(filter(MTBreg_agenum, year == 2022, var != 'tI')) +
  facet_wrap(~reg) +
  geom_col(mapping = aes(x = agegp, y = val, fill = factor(var, levels = c("S", "dI", "rI"))), position = "stack") +
  scale_fill_manual(values = c("S" = "#D3D3D3", "dI" = "#900C3F", "rI" = "#FF5733"),
                    labels = c("S" = "No infection", "dI" = "Distal infection", "rI" = "Recent infection")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
  scale_x_discrete(labels = age_labs) + 
  labs(x = 'Age group', y = expression('Absolute number infected with viable '*italic('Mtb')*' infection'), fill = 'Type') +
  theme_bw() + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(vjust = -2), text = element_text(family = "Open Sans"))
dev.off()

# 4.2 Proportion infected by country
png(here("plots", paste0("06x_inf_", scenario, ".png")), width = 7, height = 4, units = 'in', res = 200)
ggplot(data = filter(MTBiso, iso3 == 'ZAF', var %in% c('pI', 'prI'))) +
  geom_line(mapping = aes(x = year, y = val, colour = var)) +
  geom_ribbon(mapping = aes(x = year, ymin = lo, ymax = hi, fill = var), alpha = 0.2) +
  scale_colour_manual(values = c("pI" = "#900C3F", "prI" = "#FF5733"),
                      labels = c("pI" = "All infections", "prI" = "Recent infections")) +
  scale_fill_manual(values = c("pI" = "#900C3F", "prI" = "#FF5733"),
                    labels = c("pI" = "All infections", "prI" = "Recent infections")) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(1960, 2022, 20)) +
  scale_y_continuous(labels = scales::label_percent(), expand=c(0, 0), breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(1960, 2022)) +
  labs(title = 'ZAF', x = 'Year', y = 'Proportion infected (%)', colour = 'Type', fill = 'Type') +
  theme_bw() + 
  theme(legend.position = 'bottom', text = element_text(family = "Open Sans"))
dev.off()

# 4.3 Top 10 countries with highest number of Mtb infections
top10 <- c('IND', 'IDN', 'CHN', 'PHL', 'BGD', 'PAK', 'COD', 'NGA', 'VNM', 'ZAF')
png(here("plots", paste0("06x_infnum_topiso_", scenario, ".png")), width = 9, height = 4.5, units = 'in', res = 1000)
ggplot(filter(MTBiso, year == 2022, var %in% c('It', 'rIt'), iso3 %in% top10)) +
  geom_col(aes(x = reorder(iso3, -val * (var == 'It'), FUN = sum), y = val, fill = var), position = "identity") +
  scale_fill_manual(values = c("It" = "#900C3F", "rIt" = "#FF5733"),
                    labels = c("It" = "All infections", "rIt" = "Recent infections")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
  labs(x = NULL, y = expression('Absolute number of viable '*italic('Mtb')*' infections'), fill = 'Type') +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(family = "Open Sans"))
dev.off()  

# 4.4 Top 10 countries with highest prevalence of Mtb infections
prev10 <- c('PHL', 'PRK', 'TLS', 'GAB', 'KHM', 'MNG', 'IDN', 'CAF', 'MMR', 'PNG')
png(here("plots", paste0("06x_infpct_topiso_", scenario, ".png")), width = 9, height = 4.5, units = 'in', res = 1000)
ggplot(filter(MTBiso, year == 2022, var %in% c('pI', 'prI'), iso3 %in% prev10)) +
  geom_col(aes(x = reorder(iso3, -val * (var == 'pI'), FUN = sum), y = val, fill = var), position = "identity") +
  scale_fill_manual(values = c("pI" = "#900C3F", "prI" = "#FF5733"),
                    labels = c("pI" = "All infections", "prI" = "Recent infections")) +
  scale_y_continuous(labels = scales::label_percent(), breaks = seq(0, 0.25, 0.05)) +
  labs(x = NULL, y = expression('Prevalence of viable '*italic('Mtb')*' infection'), fill = 'Type') +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(family = "Open Sans"))
dev.off()  

# 4.5 WHO regional map
reg <- ne_countries(scale = 'small', type = 'countries', returnclass = 'sf') %>% 
  left_join(MTBiso %>% filter(year == 2022, var == 'prI'), by = c("iso_a3_eh" = "iso3"))

colours <- c('AMR' = "#E56E5A", 'EUR' = "#4C6A9C", 'EMR' = "#BC8E5A",
             'AFR' = "#A2559C", 'WPR' = "#B16215", 'SEA' = "#3B8E1E")

png(here("plots",paste0("06x_regmap.png")), width = 10, height = 5, units = 'in', res = 1000)
ggplot(data = reg) +
  geom_sf(aes(fill = reg)) +
  scale_fill_manual(values = colours, na.value = "#D3D3D3", na.translate = TRUE) +
  theme_void() +
  theme(legend.position = 'none', text = element_text(family = "Open Sans")) +
  coord_sf(crs = "+proj=robin +lon_0=0")
dev.off()

# 4.4 Country-level estimates
num <- MTBiso %>%
  filter(year == 2022) %>%
  filter(var %in% c('It', 'rIt')) %>%
  mutate(across(c(val, lo, hi), ~ round(./1e2) * 1e2)) %>% 
  mutate(across(c(val, lo, hi), ~ scales::comma(., accuracy = 1))) %>% 
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>%
  select(iso3, var, est, val) %>%
  pivot_wider(names_from = var, values_from = c(est, val), names_sep = "_") %>%
  arrange(iso3) %>%
  rename('Country' = iso3, 'Recent infections' = est_rIt, 'All infections' = est_It) %>%
  select(Country, `Recent infections`, `All infections`)

numk <- MTBiso_kidnum %>%
  filter(year == 2022) %>%
  filter(agegp == '00-14', var %in% c('rI', 'tI')) %>% 
  mutate(across(c(val, lo, hi), ~ round(./1e2) * 1e2)) %>% 
  mutate(across(c(val, lo, hi), ~ scales::comma(., accuracy = 1))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(iso3, var, est) %>% 
  pivot_wider(names_from = var, values_from = est, names_sep = "_") %>%
  arrange(iso3) %>%
  rename('Country' = iso3, 'Recent infections' = rI, 'All infections' = tI)

pct <- MTBiso %>%
  filter(year == 2022) %>% 
  filter(var %in% c('pI', 'prI')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 2))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.2f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>%
  select(iso3, var, est, val) %>%
  pivot_wider(names_from = var, values_from = c(est, val), names_sep = "_") %>%
  arrange(iso3) %>%
  rename('Country' = iso3, 'Recent infections' = est_prI, 'All infections' = est_pI) %>%
  select(Country, `Recent infections`, `All infections`)

pctk <- MTBiso_kidpct %>% 
  filter(year == 2022) %>% 
  filter(agegp == '00-14', var %in% c('pprI', 'ppI')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 2))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.2f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>%
  select(iso3, var, est) %>%
  pivot_wider(names_from = var, values_from = est, names_sep = "_") %>%
  arrange(iso3) %>%
  rename('Country' = iso3, 'Recent infections' = pprI, 'All infections' = ppI) %>%
  select(Country, `Recent infections`, `All infections`)
