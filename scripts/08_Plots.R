## Analysis code for Schwalb et al. 2023
## Distributed under CC BY 4.0
## RScript 08: Plots.R

# Packages ==========
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
library(ggplot2) # To build comparative plots
library(scales) # Scale functions for visualisation
library(patchwork) # Plot composition
library(gganimate) # Adds animation to plot

# 1. Load data ==========
mtb <- import(here("data","mtb","JPN.Rdata"))
ari <- import(here("data","ari","ARIrevmix.Rdata"))

# 2. Data curation ==========
ari <- ari %>% 
  filter(iso3 == 'JPN' & acat == '0-14') %>% 
  select(iso3, year, ari) %>% 
  mutate(ari = round(ari*100, 2))

mtb <- mtb %>% 
  select(iso3, time, starts_with("I")) %>% 
  mutate(
    nI0004 = rowSums(select(., starts_with("I0004"))),
    nI0509 = rowSums(select(., starts_with("I0509"))),
    nI1014 = rowSums(select(., starts_with("I1014"))),
    nI1519 = rowSums(select(., starts_with("I1519"))),
    nI2024 = rowSums(select(., starts_with("I2024"))),
    nI2529 = rowSums(select(., starts_with("I2529"))),
    nI3034 = rowSums(select(., starts_with("I3034"))),
    nI3539 = rowSums(select(., starts_with("I3539"))),
    nI4044 = rowSums(select(., starts_with("I4044"))),
    nI4549 = rowSums(select(., starts_with("I4549"))),
    nI5054 = rowSums(select(., starts_with("I5054"))),
    nI5559 = rowSums(select(., starts_with("I5559"))),
    nI6064 = rowSums(select(., starts_with("I6064"))),
    nI6569 = rowSums(select(., starts_with("I6569"))),
    nI7074 = rowSums(select(., starts_with("I7074"))),
    nI7579 = rowSums(select(., starts_with("I7579"))),
    nI8000 = rowSums(select(., starts_with("I8000")))) %>% 
  select(iso3, time, starts_with("n")) %>% 
  select(iso3, time, nI0004, nI0509, nI1014, nI6569, nI7074, nI7579, nI8000) %>% 
  mutate(year = 1950+time) %>% 
  inner_join(ari) %>% 
  pivot_longer(cols = !c(iso3, year, time, ari), names_to = "state", values_to = "value") %>% 
  mutate(agegp = case_when(
    state %in% c('nI0004','nI0509','nI1014') ~ '0-14',
    state %in% c('nI6569','nI7074','nI7579','nI8000') ~ '65-80+'))

# 3. Plots ==========
ggplot(mtb) +
  geom_col(aes(x=state, y=value, fill=agegp)) +
  geom_text(aes(x = 2, y = 0.25, label = paste(ari,"%"))) +
  annotate("text", x = 6, y = 0.5, label = "ARI = 1.34%") +
  labs(title = paste(mtb$iso3,' - Year: {as.integer(frame_time)}'), x = 'Age group', y = 'Infected') +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  transition_time(year) +
  ease_aes('linear') +
  theme(legend.position = "bottom")

anim_save("JPNinf.gif", animation = last_animation())
