## Analysis code for Schwalb et al. 2024
## Distributed under CC BY 4.0
## RScript 07: Results.R (Full run)

# Packages ==========
library(rio)
library(here)
library(tidyverse)
library(data.table)
library(rnaturalearth)
library(sf)
library(tmap)
library(progress)
options(scipen = 1)

# 1. Load data ==========
scenario <- "y20"
scenario <- "y50"

files <- list.files(here("outputs", paste0("mtb_", scenario)), 
                    pattern = "\\.Rdata$", full.names = TRUE)

for(i in files) {
  name <- tools::file_path_sans_ext(basename(i))
  assign(name, import(i))
}
rm(files, i, name)

# 2. Tables ==========
# 2.1 Table 1 - Proportion of population with viable Mtb infection
mtbprop <- MTBglb %>% 
  mutate(reg = 'GLOBAL') %>% 
  bind_rows(MTBreg) %>% 
  filter(year == 2022) %>% 
  filter(var %in% c('pI', 'prI', 'rec')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(reg, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(reg, prI, pI, rec)

mtbpropkid <- MTBglb_kidpct %>% 
  mutate(reg = 'GLOBAL') %>% 
  bind_rows(MTBreg_kidpct) %>% 
  filter(year == 2022) %>% 
  filter(agegp == '00-14') %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(reg, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(reg, pprI, ppI)

mtbprop <- mtbprop %>% 
  left_join(mtbpropkid, by = c('reg')) %>%
  mutate(reg = factor(reg, levels = c("AFR", "AMR", "EMR", "EUR", "WPR", "SEA", "GLOBAL"))) %>% 
  arrange(reg) %>% 
  select(reg, prI, pprI, pI, ppI, rec) %>% 
  rename('WHO region' = reg ,'Recent infection prevalence' = prI, 'Proportion in children (Rec)' = pprI,
         'All infection prevalence' = pI, 'Proportion in children (All)' = ppI,
         'Proportion recently infected' = rec)

# 2.2 Table 2 - Number of individuals with viable Mtb infection
mtbnum <- MTBglb %>% 
  mutate(reg = 'GLOBAL') %>% 
  bind_rows(MTBreg) %>% 
  filter(year == 2022) %>% 
  filter(var %in% c('It', 'rIt')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. / 1e6, 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(reg, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(reg, rIt, It) 

mtbnumkid <- MTBglb_kidnum %>% 
  mutate(reg = 'GLOBAL') %>% 
  bind_rows(MTBreg_kidnum) %>% 
  filter(year == 2022) %>% 
  filter(agegp == '00-14') %>% 
  mutate(across(c(val, lo, hi), ~ round(. / 1e6, 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(reg, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(reg, krIt = rI, kIt = tI)

mtbnum <- mtbnum %>% 
  left_join(mtbnumkid, by = c('reg')) %>%
  mutate(reg = factor(reg, levels = c("AFR", "AMR", "EMR", "EUR", "WPR", "SEA", "GLOBAL"))) %>% 
  arrange(reg) %>% 
  select(reg, rIt, krIt, It, kIt) %>% 
  rename('WHO region' = reg ,'Recent infections' = rIt, 'Recent infections in children' = krIt,
         'All infections' = It, 'All infections in children' = kIt)

# 2.3 Table S1 - Regional contribution to global viable Mtb infection burden
mtbreg <- MTBreg %>% 
  filter(year == 2022) %>% 
  filter(var %in% c('regIt','regrIt')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(reg, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  mutate(reg = factor(reg, levels = c("AFR", "AMR", "EMR", "EUR", "WPR", "SEA", "GLOBAL"))) %>% 
  select(reg, regrIt, regIt) %>% 
  rename('WHO region' = reg ,'Recent infections' = regrIt, 'All infections' = regIt)

# 2.4 Table S2 - Country-level estimates
mtbisoprop <- MTBiso %>% 
  filter(year == 2022) %>% 
  filter(var %in% c('pI', 'prI', 'rI')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(iso3, var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(iso3, prI, pI, rI) %>% 
  arrange(desc(prI)) %>% 
  rename('Country' = iso3 ,'Recent infection prevalence' = prI,
         'All infection prevalence' = pI, 'Proportion recently infected' = rI) %>% 
  slice_head(n = 20)

mtbisonum <- MTBiso %>%
  filter(year == 2022) %>%
  filter(var %in% c('It', 'rIt')) %>%
  mutate(across(c(val, lo, hi), ~ round(. / 1e6, 2))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>%
  select(iso3, var, est, val) %>%
  pivot_wider(names_from = var, values_from = c(est, val), names_sep = "_") %>%
  arrange(desc(as.numeric(val_rIt))) %>%
  rename('Country' = iso3, 'Recent infections' = est_rIt, 'All infections' = est_It) %>%
  select(Country, `Recent infections`, `All infections`) %>% 
  slice_head(n = 20)

# 3. Plots ==========
iso <- unique(MTBiso$iso3)

pdf(here("plots", paste0("07_inf_iso_", scenario, ".pdf")), height = 6, width = 10)
for(i in iso) {
  mtbdb <- filter(MTBiso, iso3 == i, var %in% c('pI', 'prI'))
  val2022 <- filter(mtbdb, year == 2022)
  p <- ggplot(data = mtbdb) +
    geom_line(mapping = aes(x = year, y = val, colour = var)) +
    geom_ribbon(mapping = aes(x = year, ymin = lo, ymax = hi, fill = var), alpha = 0.2) +
    scale_colour_manual(values = c("pI" = "#900C3F", "prI" = "#FF5733"),
                        labels = c("pI" = "All infections", "prI" = "Recent infections")) +
    scale_fill_manual(values = c("pI" = "#900C3F", "prI" = "#FF5733"),
                      labels = c("pI" = "All infections", "prI" = "Recent infections")) +
    scale_x_continuous(expand=c(0, 0), breaks = seq(1960, 2022, 20)) +
    scale_y_continuous(labels = scales::label_percent(), expand=c(0, 0), breaks = seq(0, 1, 0.2)) +
    coord_cartesian(ylim = c(0, 0.5), xlim = c(1960, 2022)) +
    labs(title = i, x = 'Year', y = 'Proportion infected (%)', colour = 'Type', fill = 'Type') +
    theme_bw() + 
    theme(legend.position = 'bottom')
  print(p)
}
dev.off()

pdf(here("plots", paste0("07_inf_agegp_", scenario, ".pdf")), height = 6, width = 10)
for(i in iso) {  
  mtbdb <- filter(MTBage_num, iso3 == i, var %in% c('S', 'dI', 'rI'), year == 2022)
  p <- ggplot() +
    geom_col(mtbdb, mapping = aes(x = agegp, y = val, fill = var), position = "stack") +
    scale_fill_manual(values = c("S" = "#D3D3D3", "dI" = "#900C3F", "rI" = "#FF5733"),
                      labels = c("S" = "Susceptible", "dI" = "Distally Infected (2+yrs)", "rI" = "Recently Infected (<2yrs)")) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title = i, x = 'Age group', y = 'Number', fill = 'Type') +
    theme_bw() + 
    theme(legend.position = 'bottom')
  print(p)
}
dev.off()

png(here("plots", paste0("07_recinf_", scenario, ".png")), width = 8, height = 5, units = 'in', res = 150)
ggplot() +
  geom_line(filter(MTBglb, var == 'rI'), mapping = aes(x = year, y = val, colour = 'Recent'), colour = '#FF5733') +
  geom_ribbon(filter(MTBglb, var == 'rI'), mapping = aes(x = year, ymin = lo, ymax = hi, fill = 'Recent'), fill = '#FF5733', alpha = 0.2) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(1960, 2050, 20)) +
  scale_y_continuous(labels = scales::label_percent(), expand=c(0, 0), breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(1960, 2022)) +
  labs(x = 'Year', y = 'Proportion recently infected / all infections (%)') +
  theme_bw() + 
  theme(legend.position = 'bottom')
dev.off()

MTB2022 <- MTBiso %>%
  filter(year == 2022, var == 'prI') %>%
  select(iso3, year, reg, val) %>% 
  mutate(prev = cut(val, breaks = c(-Inf, 0.0025, 0.01, 0.02, 0.05, 0.10),
                    labels = c('0.0 - 0.25%', '0.25 - 1.0%', '1.0 - 2.0%', 
                               '2.0 - 5.0%', '5.0 - 10.0%'),
                    include.lowest = TRUE)) %>% 
  mutate(prev = factor(prev, levels = c('0.0 - 0.25%', '0.25 - 1.0%', '1.0 - 2.0%', 
                                        '2.0 - 5.0%', '5.0 - 10.0%')))

world <- ne_countries(scale = 'medium', type = 'countries', returnclass = 'sf') %>% 
  left_join(MTB2022, by = c("iso_a3_eh" = "iso3"))


colours <- c('0.0 - 0.25%' = "#fee5d9", '0.25 - 1.0%' = "#fcae91", '1.0 - 2.0%' = "#fb6a4a",
             '2.0 - 5.0%' = "#a50f15", '5.0 - 10.0%' = "#67000d")

png(here("plots",paste0("07_mtbmap_", scenario, ".png")), width = 10, height = 5, units = 'in', res = 300)
ggplot(data = world) +
  geom_sf(aes(fill = prev)) +
  labs(title = expression("Global burden of recent viable " * italic("Mycobacterium tuberculosis") * " infection")) +
  scale_fill_manual(values = colours, na.value = "#F7F7F7", guide = guide_legend(title = "Prevalence")) +
  theme_void() +
  theme(legend.position = c(0.10, 0.25),
        legend.direction = "vertical",
        legend.justification = "center",
        legend.background = element_rect(fill = "#FFFFFF", colour = NA),
        legend.key = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = "+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dev.off()
