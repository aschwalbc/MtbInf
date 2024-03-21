## Analysis code for Schwalb et al. 2024
## Distributed under CC BY 4.0
## RScript 06: mtbburden

# Packages ==========
library(rio)
library(here)
library(tidyverse)
library(data.table)

# 1. Population data ==========
POP <- import(here("data","sources","pop","WPP_Pop_1950-2100.csv"))

POP <- POP %>%
  select(iso3 = ISO3_code, year = Time, ageWPP = AgeGrp, pop = PopTotal) %>%
  mutate(iso3 = na_if(iso3, "")) %>% 
  filter(!is.na(iso3)) %>%
  filter(year <= 2050) %>%
  mutate(ageWPP = case_when(ageWPP == '0-4' ~ 'N0004', ageWPP == '5-9' ~ 'N0509', ageWPP == '10-14' ~ 'N1014', ageWPP == '15-19' ~ 'N1519',
    ageWPP == '20-24' ~ 'N2024', ageWPP == '25-29' ~ 'N2529', ageWPP == '30-34' ~ 'N3034', ageWPP == '35-39' ~ 'N3539', ageWPP == '40-44' ~ 'N4044',
    ageWPP == '45-49' ~ 'N4549', ageWPP == '50-54' ~ 'N5054', ageWPP == '55-59' ~ 'N5559', ageWPP == '60-64' ~ 'N6064', ageWPP == '65-69' ~ 'N6569', 
    ageWPP == '70-74' ~ 'N7074', ageWPP == '75-79' ~ 'N7579', ageWPP %in% c('80-84','85-89','90-94','95-99','100+') ~ 'N8000')) %>%
  group_by(iso3, year, ageWPP) %>% 
  summarise(pop = sum(pop) * 1e3) %>% 
  pivot_wider(id_cols = c("iso3", "year"), names_from = ageWPP, values_from = pop)

# 2. Mtb estimates ==========
MTB <- import(here("data","mtb","mMtb_rev_mix_pop_sc.Rdata")) %>% 
  mutate(year = time + 1950) %>% 
  left_join(POP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))))

MTBiso <- MTB %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-d]?$")))) %>% 
  mutate(rIt = rowSums(across(matches("^I\\d{4}[a-b]$")))) %>%
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  select(iso3, year, St, It, rIt, Nt) %>% 
  mutate(pI = It / Nt, prI = rIt / Nt, rI = prI / pI)

MTBage_pct <- MTB %>% 
  select(-starts_with('S')) %>% 
  mutate(pI0004 = rowSums(select(., matches("^I0004[az]?")), na.rm = TRUE)/N0004,
         pI0509 = rowSums(select(., matches("^I0509[az]?")), na.rm = TRUE)/N0509,
         pI1014 = rowSums(select(., matches("^I1014[az]?")), na.rm = TRUE)/N1014,
         pI1519 = rowSums(select(., matches("^I1519[az]?")), na.rm = TRUE)/N1519,
         pI2024 = rowSums(select(., matches("^I2024[az]?")), na.rm = TRUE)/N2024,
         pI2529 = rowSums(select(., matches("^I2529[az]?")), na.rm = TRUE)/N2529,
         pI3034 = rowSums(select(., matches("^I3034[az]?")), na.rm = TRUE)/N3034,
         pI3539 = rowSums(select(., matches("^I3539[az]?")), na.rm = TRUE)/N3539,
         pI4044 = rowSums(select(., matches("^I4044[az]?")), na.rm = TRUE)/N4044,
         pI4549 = rowSums(select(., matches("^I4549[az]?")), na.rm = TRUE)/N4549,
         pI5054 = rowSums(select(., matches("^I5054[az]?")), na.rm = TRUE)/N5054,
         pI5559 = rowSums(select(., matches("^I5559[az]?")), na.rm = TRUE)/N5559,
         pI6064 = rowSums(select(., matches("^I6064[az]?")), na.rm = TRUE)/N6064,
         pI6569 = rowSums(select(., matches("^I6569[az]?")), na.rm = TRUE)/N6569,
         pI7074 = rowSums(select(., matches("^I7074[az]?")), na.rm = TRUE)/N7074,
         pI7579 = rowSums(select(., matches("^I7579[az]?")), na.rm = TRUE)/N7579,
         pI8000 = rowSums(select(., matches("^I8000[az]?")), na.rm = TRUE)/N8000) %>% 
  mutate(prI0004 = rowSums(select(., matches("^I0004[ab]$")), na.rm = TRUE)/N0004,
         prI0509 = rowSums(select(., matches("^I0509[ab]$")), na.rm = TRUE)/N0509,
         prI1014 = rowSums(select(., matches("^I1014[ab]$")), na.rm = TRUE)/N1014,
         prI1519 = rowSums(select(., matches("^I1519[ab]$")), na.rm = TRUE)/N1519,
         prI2024 = rowSums(select(., matches("^I2024[ab]$")), na.rm = TRUE)/N2024,
         prI2529 = rowSums(select(., matches("^I2529[ab]$")), na.rm = TRUE)/N2529,
         prI3034 = rowSums(select(., matches("^I3034[ab]$")), na.rm = TRUE)/N3034,
         prI3539 = rowSums(select(., matches("^I3539[ab]$")), na.rm = TRUE)/N3539,
         prI4044 = rowSums(select(., matches("^I4044[ab]$")), na.rm = TRUE)/N4044,
         prI4549 = rowSums(select(., matches("^I4549[ab]$")), na.rm = TRUE)/N4549,
         prI5054 = rowSums(select(., matches("^I5054[ab]$")), na.rm = TRUE)/N5054,
         prI5559 = rowSums(select(., matches("^I5559[ab]$")), na.rm = TRUE)/N5559,
         prI6064 = rowSums(select(., matches("^I6064[ab]$")), na.rm = TRUE)/N6064,
         prI6569 = rowSums(select(., matches("^I6569[ab]$")), na.rm = TRUE)/N6569,
         prI7074 = rowSums(select(., matches("^I7074[ab]$")), na.rm = TRUE)/N7074,
         prI7579 = rowSums(select(., matches("^I7579[ab]$")), na.rm = TRUE)/N7579,
         prI8000 = rowSums(select(., matches("^I8000[ab]$")), na.rm = TRUE)/N8000) %>% 
  select(iso3, year, starts_with('pI'), starts_with('prI')) %>% 
  pivot_longer(cols = starts_with("pI") | starts_with("prI"), names_to = c("var", "agegp"),
               names_pattern = "^([a-z]+I)(\\d{4})$", values_to = "val") %>%
  mutate(agegp = str_replace(agegp, "(\\d{2})(\\d{2})", "\\1-\\2"),
         var = str_replace(var, "([a-z]+)I", "\\1I"))

MTBage_num <- MTB %>% 
  select(-starts_with('N')) %>% 
  mutate(dI0004 = rowSums(select(., matches("^I0004[cd]$")), na.rm = TRUE),
         dI0509 = rowSums(select(., matches("^I0509[cd]$")), na.rm = TRUE),
         dI1014 = rowSums(select(., matches("^I1014[cd]$")), na.rm = TRUE),
         dI1519 = rowSums(select(., matches("^I1519[cd]$")), na.rm = TRUE),
         dI2024 = rowSums(select(., matches("^I2024[cd]$")), na.rm = TRUE),
         dI2529 = rowSums(select(., matches("^I2529[cd]$")), na.rm = TRUE),
         dI3034 = rowSums(select(., matches("^I3034[cd]$")), na.rm = TRUE),
         dI3539 = rowSums(select(., matches("^I3539[cd]$")), na.rm = TRUE),
         dI4044 = rowSums(select(., matches("^I4044[cd]$")), na.rm = TRUE),
         dI4549 = rowSums(select(., matches("^I4549[cd]$")), na.rm = TRUE),
         dI5054 = rowSums(select(., matches("^I5054[cd]$")), na.rm = TRUE),
         dI5559 = rowSums(select(., matches("^I5559[cd]$")), na.rm = TRUE),
         dI6064 = rowSums(select(., matches("^I6064[cd]$")), na.rm = TRUE),
         dI6569 = rowSums(select(., matches("^I6569[cd]$")), na.rm = TRUE),
         dI7074 = rowSums(select(., matches("^I7074[cd]$")), na.rm = TRUE),
         dI7579 = rowSums(select(., matches("^I7579[cd]$")), na.rm = TRUE),
         dI8000 = rowSums(select(., matches("^I8000[cd]$")), na.rm = TRUE)) %>% 
  mutate(rI0004 = rowSums(select(., matches("^I0004[ab]$")), na.rm = TRUE),
         rI0509 = rowSums(select(., matches("^I0509[ab]$")), na.rm = TRUE),
         rI1014 = rowSums(select(., matches("^I1014[ab]$")), na.rm = TRUE),
         rI1519 = rowSums(select(., matches("^I1519[ab]$")), na.rm = TRUE),
         rI2024 = rowSums(select(., matches("^I2024[ab]$")), na.rm = TRUE),
         rI2529 = rowSums(select(., matches("^I2529[ab]$")), na.rm = TRUE),
         rI3034 = rowSums(select(., matches("^I3034[ab]$")), na.rm = TRUE),
         rI3539 = rowSums(select(., matches("^I3539[ab]$")), na.rm = TRUE),
         rI4044 = rowSums(select(., matches("^I4044[ab]$")), na.rm = TRUE),
         rI4549 = rowSums(select(., matches("^I4549[ab]$")), na.rm = TRUE),
         rI5054 = rowSums(select(., matches("^I5054[ab]$")), na.rm = TRUE),
         rI5559 = rowSums(select(., matches("^I5559[ab]$")), na.rm = TRUE),
         rI6064 = rowSums(select(., matches("^I6064[ab]$")), na.rm = TRUE),
         rI6569 = rowSums(select(., matches("^I6569[ab]$")), na.rm = TRUE),
         rI7074 = rowSums(select(., matches("^I7074[ab]$")), na.rm = TRUE),
         rI7579 = rowSums(select(., matches("^I7579[ab]$")), na.rm = TRUE),
         rI8000 = rowSums(select(., matches("^I8000[ab]$")), na.rm = TRUE)) %>% 
  select(iso3, year, starts_with('dI'), starts_with('rI'), starts_with('S')) %>% 
  pivot_longer(cols = -c(iso3, year), names_to = c("var", "agegp"),
               names_pattern = "^(rI|dI|S)(\\d{4})$", values_to = "val") %>%
  mutate(agegp = str_replace(agegp, "(\\d{2})(\\d{2})", "\\1-\\2"),
         var = case_when(str_detect(var, "^dI") ~ "dI", str_detect(var, "^rI") ~ "rI",
                         str_detect(var, "^S") ~ "S", TRUE ~ as.character(var))) %>% 
  mutate(var = factor(var, levels = c('S', 'dI', 'rI')))

MTBglb <- MTBiso %>% 
  group_by(year) %>% 
  summarise(St = sum(St), It = sum(It), rIt = sum(rIt), Nt = sum(Nt)) %>% 
  mutate(pI = It / Nt, prI = rIt / Nt, rI = prI / pI)

iso <- unique(MTB$iso3)

pdf(here("plots","mtb","Inf_iso.pdf"), height = 6, width = 10)
for(i in iso) {
  mtbdb <- filter(MTB, iso3 == i)
  val2019 <- filter(mtbdb, year == 2019)
  p <- ggplot() +
    geom_line(mtbdb, mapping = aes(x = year, y = pI, colour = 'All'), colour = 'black') +
    geom_line(mtbdb, mapping = aes(x = year, y = prI, colour = 'Recent'), colour = 'red') +
    geom_text(data = val2019, mapping = aes(x = 2019, y = pI, label = scales::percent(pI)), hjust = -0.1, vjust = 0, colour = 'black') +
    geom_text(data = val2019, mapping = aes(x = 2019, y = prI, label = scales::percent(prI)), hjust = -0.1, vjust = 0, colour = 'red') +
    scale_x_continuous(expand=c(0, 0), breaks = seq(1950, 2050, 25)) +
    scale_y_continuous(labels = scales::label_percent(), expand=c(0, 0), breaks = seq(0, 1, 0.2)) +
    coord_cartesian(ylim = c(0, 1), xlim = c(1950, 2050)) +
    labs(title = i, x = 'Year', y = 'Proportion infected (%)') +
    theme_bw() + 
    theme(legend.position = 'bottom')
  print(p)
}
dev.off()

pdf(here("plots","mtb","Inf_agegp.pdf"), height = 6, width = 10)
for(i in iso) {
  mtbdb <- filter(MTBage_num, iso3 == i, year == 2022)
  p <- ggplot() +
    geom_col(mtbdb, mapping = aes(x = agegp, y = val, fill = var), position = "stack") +
    scale_fill_manual(values = c("S" = "#D3D3D3", "dI" = "#900C3F", "rI" = "#FF5733"),
                      labels = c("S" = "Susceptible", "dI" = "Distally Infected", "rI" = "Recently Infected")) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M')) +
    labs(title = i, x = 'Age group', y = 'Number', fill = 'Type') +
    theme_bw() + 
    theme(legend.position = 'bottom')
  print(p)
}
dev.off()

pdf(here("plots","mtb","rInf.pdf"), height = 6, width = 10)
ggplot() +
  geom_line(gMTB, mapping = aes(x = year, y = rI, colour = 'Recent'), colour = 'red') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(1950, 2050, 25)) +
  scale_y_continuous(labels = scales::label_percent(), expand=c(0, 0), breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 0.6), xlim = c(1950, 2050)) +
  labs(x = 'Year', y = 'Proportion recently infected / all infections (%)') +
  theme_bw() + 
  theme(legend.position = 'bottom')
dev.off()
