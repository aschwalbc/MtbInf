## Analysis code for Mtb Inf Burden
## Authors: A Schwalb 
## RScript 06: MtbBurden.R

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
tab1 <- MTBglb %>% 
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

tab1ex <- MTBglb_kidpct %>% 
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

tab1 <- tab1 %>% 
  left_join(tab1ex, by = c('reg')) %>%
  mutate(reg = factor(reg, levels = c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR", "GLOBAL"))) %>% 
  arrange(reg) %>% 
  select(reg, prI, pprI, pI, ppI, rec) %>% 
  rename('WHO region' = reg ,'Recent infection prevalence' = prI, 'Proportion in children (Rec)' = pprI,
         'All infection prevalence' = pI, 'Proportion in children (All)' = ppI,
         'Proportion recently infected' = rec)
rm(tab1ex)

# 2.2 Table 2 - Number of individuals with viable Mtb infection
tab2 <- MTBglb %>% 
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

tab2ex <- MTBglb_kidnum %>% 
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

tab2 <- tab2 %>% 
  left_join(tab2ex, by = c('reg')) %>%
  mutate(reg = factor(reg, levels = c("AFR", "AMR", "EMR", "EUR", "WPR", "SEA", "GLOBAL"))) %>% 
  arrange(reg) %>% 
  select(reg, rIt, krIt, It, kIt) %>% 
  rename('WHO region' = reg ,'Recent infections' = rIt, 'Recent infections in children' = krIt,
         'All infections' = It, 'All infections in children' = kIt)
rm(tab2ex)

# 2.3 Table S1 - Regional contribution to global viable Mtb infection burden
tabs1 <- MTBreg %>% 
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

# 2.4 Table S2 - Country-level estimates (proportion)
tabs2 <- MTBiso %>% 
  filter(year == 2022) %>% 
  filter(var %in% c('prI', 'pI', 'rec')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(iso3, var, est, val) %>%
  pivot_wider(names_from = var, values_from = c(est, val), names_sep = "_") %>%
  arrange(desc(as.numeric(val_prI))) %>%
  rename('Country' = iso3 ,'Recent infection prevalence' = est_prI,
         'All infection prevalence' = est_pI, 'Proportion recently infected' = est_rec) %>% 
  select(Country, `Recent infection prevalence`, `All infection prevalence`) %>% 
  slice_head(n = 20)

# 2.4 Table S3 - Country-level estimates (absolute number)
tabs3 <- MTBiso %>%
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
reg <- unique(MTBreg$reg)
iso <- unique(MTBiso$iso3)

# 3.1 Figure 1 - ARI estimates
ARInorev <- import(here("data", "gp", "GP_norev.Rdata")) %>% # Unadjusted ARIs
  mutate(ageARI = "all", type = 'norev', source = NA)

ARIrev <- import(here("data", "gp", "GP_rev.Rdata")) %>% # Reversion-adjusted ARIs
  mutate(ageARI = "all", type = 'rev', source = NA)

ARIrevmix <- import(here("data", "ari", "ARI_rev_mix_CI.Rdata")) %>% # Mixing-adjusted ARIs
  within(rm(reg)) %>% 
  mutate(type = 'mix', source = NA)

ARI <- import(here("data", "ari", "ARI_rev.Rdata")) %>% 
  mutate(source = type, type = 'rev', ageARI = 'all', sd = E/1.96) %>% 
  mutate(lower = lari - sd, upper = lari + sd) %>% 
  select(iso3, year, ageARI, lari, lower, upper, type, source)

ARIs <- rbind(ARInorev, ARIrev, ARIrevmix, ARI) %>% 
  mutate(type = factor(type, levels = c('norev', 'rev', 'mix'))) %>% 
  mutate(ageARI = factor(ageARI, levels = c('all', '00-14', '15-44', '45+'),
                         labels = c('All', '00-14', '15-44', '45+'))) %>% 
  mutate(source = factor(source, levels = c('prev', 'surv'),
                         labels = c('TB prevalence estimates', 'Immunoreactivity surveys')))
rm(ARInorev, ARIrev, ARIrevmix, ARI)

facetlabs <- c(norev = "Unadjusted ARIs", rev = "Reversion-adjusted ARIs", mix = "Mixing and reversion-adjusted ARIs")

png(here("plots", paste0("07_logari_", scenario, ".png")), width = 10, height = 6, units = 'in', res = 200)
ggplot(filter(ARIs, iso3 %in% c('IND', 'CHN', 'IDN'), is.na(source))) +
  facet_grid(rows = vars(iso3), cols = vars(type), labeller = labeller(type = facetlabs)) +
  geom_line(aes(x = year, y = lari, colour = ageARI)) +
  geom_ribbon(aes(x= year, ymin = lower, ymax = upper, fill = ageARI), alpha = 0.2) +
  geom_point(data = filter(ARIs, iso3 %in% c('IND', 'CHN', 'IDN'), !is.na(source)), 
             aes(x = year, y = lari, shape = source), size = 1) +
  geom_errorbar(data = filter(ARIs, iso3 %in% c('IND', 'CHN', 'IDN'), !is.na(source)), 
             aes(x = year, ymin = lower, ymax = upper), linewidth = 0.1) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-6, 0, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-6, 0)) + 
  labs(x = 'Year', y = 'Annual risk of infection (log)',
       colour = 'Age group', fill = 'Age group', shape = 'Source') +
  scale_colour_manual(values = c('All' = '#FDB827', '00-14' = '#8EC7D2', '15-44' = '#0D6986', '45+' = '#07485B')) +
  scale_fill_manual(values = c('All' = '#FDB827', '00-14' = '#8EC7D2', '15-44' = '#0D6986', '45+' = '#07485B')) +
  theme_bw() + 
  theme(legend.position = 'bottom', panel.spacing = unit(0.75, "lines"))
dev.off()

png(here("plots", paste0("07_ari_", scenario, ".png")), width = 10, height = 6, units = 'in', res = 200)
ggplot(filter(ARIs, iso3 %in% c('IND', 'CHN', 'IDN'))) +
  facet_grid(rows = vars(iso3), cols = vars(type), labeller = labeller(type = facetlabs)) +
  geom_line(aes(x = year, y = exp(lari), colour = ageARI))+
  geom_ribbon(aes(x= year, ymin = exp(lower), ymax = exp(upper), fill = ageARI), alpha = 0.2) +
  scale_y_continuous(labels = scales::label_percent(), expand=c(0, 0), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 0.3)) +
  labs(x = 'Year', y = 'Annual risk of infection',
       colour = 'Age group', fill = 'Age group') +
  theme_bw() + 
  theme(legend.position = 'bottom', panel.spacing = unit(0.75, "lines"))
dev.off()

png(here("plots", paste0("07_reginf_agegpnum_", scenario, ".png")), width = 10, height = 6, units = 'in', res = 200)
ggplot(filter(MTBreg_agenum, year == 2022, var != 'tI')) +
  facet_wrap(~reg) +
  geom_col(mapping = aes(x = agegp, y = val, fill = factor(var, levels = c("S", "dI", "rI"))), position = "stack") +
  scale_fill_manual(values = c("S" = "#D3D3D3", "dI" = "#900C3F", "rI" = "#FF5733"),
                    labels = c("S" = "Susceptible", "dI" = "Distally Infected (2+yrs)", "rI" = "Recently Infected (<2yrs)")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
  labs(x = 'Age group', y = 'Number', fill = 'Type') +
  theme_bw() + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

png(here("plots", paste0("07_reginf_agegppct_", scenario, ".png")), width = 10, height = 6, units = 'in', res = 200)
ggplot(filter(MTBreg_agepct, year == 2022, var != 'rec')) +
  facet_wrap(~reg) +
  geom_col(mapping = aes(x = agegp, y = val, fill = factor(var, levels = c("pI", "prI"))), position = "identity") +
  scale_fill_manual(values = c("pI" = "#900C3F", "prI" = "#FF5733"),
                    labels = c("pI" = "Distally Infected (2+yrs)", "prI" = "Recently Infected (<2yrs)")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = 'Age group', y = 'Number', fill = 'Type') +
  theme_bw() + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

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

png(here("plots", paste0("07_inf_", scenario, ".png")), width = 7, height = 4, units = 'in', res = 200)
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
  theme(legend.position = 'bottom')
dev.off()

top10iso <- c('IND', 'CHN', 'IDN', 'PHL', 'BGD', 'PAK', 'VNM', 'THA', 'MMR', 'COD')
png(here("plots", paste0("07_infnum_topiso_", scenario, ".png")), width = 8, height = 5, units = 'in', res = 200)
ggplot(filter(MTBiso, year == 2022, var %in% c('It', 'rIt'), iso3 %in% top10iso)) +
  geom_col(aes(x = reorder(iso3, -val * (var == 'It'), FUN = sum), y = val, fill = var), position = "identity") +
  scale_fill_manual(values = c("It" = "#900C3F", "rIt" = "#FF5733"),
                    labels = c("It" = "All infections", "rIt" = "Recent infections")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
  labs(x = NULL, y = 'Number', fill = 'Type') +
  theme_bw() +
  theme(legend.position = 'bottom')
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

png(here("plots", paste0("07_infage_", scenario, ".png")), width = 7, height = 4, units = 'in', res = 200)
ggplot(data = filter(MTBiso_agenum, iso3 == 'JPN', var %in% c('S', 'dI', 'rI'), year == 2022)) +
  geom_col(mapping = aes(x = agegp, y = val, fill = var), position = "stack") +
  scale_fill_manual(values = c("S" = "#D3D3D3", "dI" = "#900C3F", "rI" = "#FF5733"),
                    labels = c("S" = "Susceptible", "dI" = "Distally Infected (2+yrs)", "rI" = "Recently Infected (<2yrs)")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
  labs(title = 'JPN', x = 'Age group', y = 'Number', fill = 'Type') +
  theme_bw() + 
  theme(legend.position = 'bottom')
dev.off()

png(here("plots", paste0("07_recinf_", scenario, ".png")), width = 8, height = 5, units = 'in', res = 150)
ggplot() +
  geom_line(filter(MTBglb, var == 'rec'), mapping = aes(x = year, y = val, colour = 'Recent'), colour = '#FF5733') +
  geom_ribbon(filter(MTBglb, var == 'rec'), mapping = aes(x = year, ymin = lo, ymax = hi, fill = 'Recent'), fill = '#FF5733', alpha = 0.2) +
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
  left_join(MTB2022, by = c("iso_a3_eh" = "iso3")) %>% 
  filter(iso_a3 != 'ATA')


colours <- c('0.0 - 0.25%' = "#fee5d9", '0.25 - 1.0%' = "#fcae91", '1.0 - 2.0%' = "#fb6a4a",
             '2.0 - 5.0%' = "#a50f15", '5.0 - 10.0%' = "#67000d")

png(here("plots",paste0("07_mtbmap_", scenario, ".png")), width = 10, height = 5, units = 'in', res = 300)
ggplot(data = world) +
  geom_sf(aes(fill = prev)) +
  scale_fill_manual(values = colours, na.value = "#D3D3D3", guide = guide_legend(title = "Prevalence")) +
  theme_void() +
  theme(legend.position = c(0.20, 0.25),
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
  coord_sf(crs = "+proj=eck4 +lon_0=0 +datum=WGS84 +units=m +no_defs")
dev.off()


