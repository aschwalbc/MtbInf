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
MTB <- import(here("data","mtb","mMtb_rev_mix_pop_sc.Rdata"))

MTB <- MTB %>% 
  mutate(year = time + 1950) %>% 
  left_join(POP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5))))) %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-d]?$")))) %>% 
  mutate(rIt = rowSums(across(matches("^I\\d{4}[a-b]$")))) %>%
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  select(iso3, year, St, It, rIt, Nt) %>% 
  mutate(pI = It / Nt, prI = rIt / Nt, rI = prI / pI)

gMTB <- MTB %>% 
  group_by(year) %>% 
  summarise(St = sum(St), It = sum(It), rIt = sum(rIt), Nt = sum(Nt)) %>% 
  mutate(pI = It / Nt, prI = rIt / Nt, rI = prI / pI)

iso <- unique(MTB$iso3)

pdf(here("plots","mtb","propInf.pdf"), height = 6, width = 10)
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
