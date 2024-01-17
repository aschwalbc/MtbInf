# Quick Mtb check
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse

pop <- import(here("data","sources","pop","WPP_Pop_1950-2100.csv"))

pop <- pop %>% # Population data
  rename(iso3 = ISO3_code, year = Time, agegp = AgeGrp, pop = PopTotal) %>%
  select(iso3,year,agegp,pop) %>%
  mutate(iso3 = na_if(iso3,"")) %>% 
  filter(!is.na(iso3)) %>%
  filter(year == 2019) %>%
  mutate(agegp = case_when(
    agegp == '0-4' ~ 'N0004', agegp == '5-9' ~ 'N0509', agegp == '10-14' ~ 'N1014', agegp == '15-19' ~ 'N1519',
    agegp == '20-24' ~ 'N2024', agegp == '25-29' ~ 'N2529', agegp == '30-34' ~ 'N3034', agegp == '35-39' ~ 'N3539',
    agegp == '40-44' ~ 'N4044', agegp == '45-49' ~ 'N4549', agegp == '50-54' ~ 'N5054', agegp == '55-59' ~ 'N5559',
    agegp == '60-64' ~ 'N6064', agegp == '65-69' ~ 'N6569', agegp == '70-74' ~ 'N7074', agegp == '75-79' ~ 'N7579',
    agegp %in% c('80-84','85-89','90-94','95-99','100+') ~ 'N8000')) %>%
  group_by(iso3,year,agegp) %>% 
  summarise(pop = sum(pop)*1e3) %>% 
  pivot_wider(id_cols = c("iso3","year"), names_from = agegp, values_from = pop)

ode1 <- import(here("data","mtb","mMtb_IHME_rev_mix_pop_nosc.Rdata")) %>% 
  mutate(type = 'rev - mix - nosc')

ode2 <- import(here("data","mtb","mMtb_IHME_norev_mix_pop_nosc.Rdata")) %>% 
  mutate(type = 'norev - mix - nosc')

ode3 <- import(here("data","mtb","mMtb_IHME_rev_nomix_pop_nosc.Rdata")) %>% 
  mutate(type = 'rev - nomix - nosc')

ode4 <- import(here("data","mtb","mMtb_IHME_rev_mix_pop_sc.Rdata")) %>% 
  mutate(type = 'rev - mix - sc')

ode <- rbind(ode1, ode2, ode3, ode4) %>% 
  mutate(year = time + 1934) %>% 
  filter(year == 2019) %>% 
  left_join(pop, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5))))) %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-z]?$")))) %>%
  mutate(rIt = rowSums(across(matches("^I\\d{4}[ab]$")))) %>% 
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  mutate(pSt = round(St/Nt*1e2,3), pIt = round(It/Nt*1e2,3), prIt = round(rIt/Nt*1e2,3)) %>% 
  select(iso3, year, type, St, pSt, It, pIt, rIt, prIt, Nt) 

mtbres <- ode %>% 
  mutate(type = factor(type, levels = c('rev - mix - nosc', 'norev - mix - nosc',
                                        'rev - nomix - nosc', 'rev - mix - sc'))) %>% 
  group_by(type) %>% 
  summarise(wavg_St = sum(St)/sum(Nt),
            wavg_It = sum(It)/sum(Nt),
            wavg_rIt = sum(rIt)/sum(Nt))

order <- c('rev - mix - nosc', 'norev - mix - nosc',
           'rev - nomix - nosc', 'rev - mix - sc')
colours <- c("black", "red", "green", "blue")

ggplot(mtbres) + 
  geom_col(aes(x = type, y = wavg_It, fill = type)) +
  geom_text(aes(x = reorder(type, -wavg_It), y = wavg_It, label = round(wavg_It, 2)),
            vjust = -0.5, color = "black") +
  scale_fill_manual(values = setNames(colours, order)) +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(x = "type", y = "proportion infected")
  
ggplot(mtbres) + 
  geom_col(aes(x = type, y = wavg_rIt, fill = type)) +
  geom_text(aes(x = reorder(type, -wavg_rIt), y = wavg_rIt, label = round(wavg_rIt, 2)),
            vjust = -0.5, color = "black") +
  scale_fill_manual(values = setNames(colours, order)) +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(x = "type", y = "proportion infected")
