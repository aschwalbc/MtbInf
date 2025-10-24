library(data.table)
library(rio)
library(here)
library(tidyverse)
library(ggplot2)
library(viridis)

MIX <- import(here("sources", "others", "Mixing.Rdata")) # Mixing matrices by Prem et al. 2021

unique(MIX$setting) # overall, urban, rural
unique(MIX$location_contact) # home, work, school, others, all

MIX <- MIX %>% 
  rename(iso = iso3c, AO = age_contactor, AI = age_cotactee, loc = location_contact, ctx = mean_number_of_contacts) %>% 
  mutate(AO = gsub(AO, pattern=" to ", replacement="-"), AI = gsub(AI, pattern=" to ", replacement="-")) %>% 
  mutate(AO = case_when(AO == '0-4' ~ '00-04', AO == '5-9' ~ '05-09', TRUE ~ AO),
         AI = case_when(AI == '0-4' ~ '00-04', AI == '5-9' ~ '05-09', TRUE ~ AI))

MIXi <- MIX %>% 
  filter(AO %in% c('00-04', '05-09', '10-14')) %>% 
  filter(iso %in% c('IND', 'VNM')) %>% 
  group_by(iso, setting, loc, AO) %>% 
  summarise(ctx = sum(ctx)) 

ggplot(MIXi, aes(x = AO, y = ctx, fill = iso)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, colour = "white", linewidth = 0.2) +
  facet_grid(setting ~ loc, switch = "y") +
  labs(x = "Age group", y = "Average number of contacts per day", fill = "Country") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major.x = element_blank(), strip.placement = "outside", axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = NA, colour = NA), legend.position = "bottom")

rm(list=ls())
