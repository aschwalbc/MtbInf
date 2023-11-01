## Analysis code for Schwalb et al. 2023
## Distributed under CC BY 4.0
## RScript 07: MtbBurden.R

# Packages ==========
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
library(ggplot2) # To build comparative plots
library(data.table) # Faster than data.frame
library(patchwork) # Plot composition

# 1. Load data ==========
POP <- import(here("data","sources","pop","WPP_Pop_1950-2100.csv"))
WHOkey <- import(here("data","sources","who","WHOkey.csv"))
WHO <- import(here("data","sources","who","WHOest_2000-2021.csv"))
LTBI <- import(here("data","sources","others","pmed.1002152.csv"))
MTBbasIHME <- import(here("data","mtb","Mtb_IHME_norev_nomix_pop_nosc.Rdata"))
MTBbasWHO <- import(here("data","mtb","Mtb_WHO_norev_nomix_pop_nosc.Rdata"))

# 2. Data curation ==========
POP <- POP %>% # Population data
  rename(iso3 = ISO3_code, year = Time, agegp = AgeGrp, pop = PopTotal) %>%
  select(iso3,year,agegp,pop) %>%
  mutate(iso3 = na_if(iso3,"")) %>% 
  filter(!is.na(iso3)) %>%
  filter(year <= 2050) %>%
  mutate(agegp = case_when(
    agegp == '0-4' ~ 'N0004', agegp == '5-9' ~ 'N0509', agegp == '10-14' ~ 'N1014', agegp == '15-19' ~ 'N1519',
    agegp == '20-24' ~ 'N2024', agegp == '25-29' ~ 'N2529', agegp == '30-34' ~ 'N3034', agegp == '35-39' ~ 'N3539',
    agegp == '40-44' ~ 'N4044', agegp == '45-49' ~ 'N4549', agegp == '50-54' ~ 'N5054', agegp == '55-59' ~ 'N5559',
    agegp == '60-64' ~ 'N6064', agegp == '65-69' ~ 'N6569', agegp == '70-74' ~ 'N7074', agegp == '75-79' ~ 'N7579',
    agegp %in% c('80-84','85-89','90-94','95-99','100+') ~ 'N8000')) %>%
  group_by(iso3,year,agegp) %>% 
  summarise(pop = sum(pop)*1e3) %>% 
  pivot_wider(id_cols = c("iso3","year"), names_from = agegp, values_from = pop)

POP_1950 <- POP %>% 
  filter(year == 1950)

isoPOP <- POP_1950 %>%
  select(iso3) %>%
  distinct()

years <- data.frame(year_value = 1934:1950)

expPOP <- isoPOP %>%
  crossing(years) %>%
  left_join(POP_1950, by = "iso3") %>%
  arrange(iso3, year_value) %>%
  rename(year = year_value) %>% 
  select(!c(year.x, year.y)) %>% 
  filter(!year == 1950)

fullPOP <- POP %>% 
  rbind(expPOP) %>% 
  arrange(iso3, year)

gMTBbasIHME <- MTBbasIHME %>% 
  mutate(year = time + 1934) %>% 
  left_join(fullPOP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5))))) %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-z]?$")))) %>% 
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  group_by(year) %>% 
  summarise(St = sum(St), It = sum(It), Nt = sum(Nt)) %>% 
  mutate(pI = It/Nt*100) %>% 
  mutate(type = "ihme - no rev - no mix - no sc")

MTBbasIHME <- MTBbasIHME %>% 
  mutate(year = time + 1934) %>% 
  left_join(fullPOP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5))))) %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-z]?$")))) %>% 
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  mutate(pI = It/Nt*100) %>% 
  mutate(type = "ihme - no rev - no mix - no sc")

gMTBbasWHO <- MTBbasWHO %>% 
  mutate(year = time + 1934) %>% 
  left_join(fullPOP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5))))) %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-z]?$")))) %>% 
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  group_by(year) %>% 
  summarise(St = sum(St), It = sum(It), Nt = sum(Nt)) %>% 
  mutate(pI = It/Nt*100) %>% 
  mutate(type = "who - no rev - no mix - no sc")

MTBbasWHO <- MTBbasWHO %>% 
  mutate(year = time + 1934) %>% 
  left_join(fullPOP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5))))) %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-z]?$")))) %>% 
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  mutate(pI = It/Nt*100) %>% 
  mutate(type = "who - no rev - no mix - no sc")

# Top 30 - Absolute incidence
WHO <- WHO %>% 
  select(iso3, g_whoregion, year, e_inc_num) %>% 
  filter(year == 2014) %>% 
  arrange(desc(e_inc_num)) %>%
  slice_head(n = 30)

top30 <- unique(WHO$iso3)

MTBbasIHME30 <- MTBbasIHME %>% 
  filter(iso3 %in% top30) %>% 
  select(iso3, year, St, It, Nt, pI, type) %>% 
  filter(year == 2014)

MTBbasWHO30 <- MTBbasWHO %>% 
  filter(iso3 %in% top30) %>% 
  select(iso3, year, St, It, Nt, pI, type) %>% 
  filter(year == 2014)

MTBbas <- MTBbasWHO30 %>% 
  rbind(MTBbasIHME30)

ggplot(data = MTBbas) +
  facet_wrap(~type, nrow = 2) + 
  geom_col(mapping = aes(x = iso3, y = It)) + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  geom_text(mapping = aes(x = iso3, y = It, label = scales::number(It * 1e-6, accuracy = 0.1, suffix = "M")), 
            vjust = -0.5, size = 3) +
  theme_minimal()

ggplot(data = MTBbas) +
  facet_wrap(~type, nrow = 2) + 
  geom_col(mapping = aes(x = iso3, y = pI)) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) + 
  geom_text(mapping = aes(x = iso3, y = pI, label = scales::percent(scale = 1, pI, accuracy = 1)), 
            vjust = -0.5, size = 2) +
  theme_minimal()

# LTBI (PMED 2014)
LTBI <- LTBI %>% 
  rename(tbi = `All LTBI`) %>% 
  select(iso3, tbi) %>% 
  mutate(tbi = as.numeric(str_replace_all(str_extract(tbi, "\\d[\\d,]+"), ",", ""))) %>% 
  mutate(type = "ltbi")

isoLTBI <- unique(LTBI$iso3)
isoMTBI <- unique(MTBbasWHO$iso3)

setdiff(isoLTBI, isoMTBI)

pMTBIvsLTBI <- MTBbasWHO %>% 
  filter(year == 2014) %>% 
  select(iso3, It) %>% 
  mutate(type = "mtbi") %>% 
  left_join(LTBI, by = "iso3") %>% 
  mutate(pdiff = It/tbi)

summary(pMTBIvsLTBI$pdiff)

ggplot() +
  geom_col(data = pMTBIvsLTBI, aes(x = iso3, y = pdiff), colour = "#CE2931", fill = "#CE2931") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "ISO3", y = "Percentage difference (MTB/LTBI)") +
  theme_minimal() + 
  theme(axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, angle = 90))  

MTBIvsLTBI <- MTBbasWHO %>% 
  filter(year == 2014) %>% 
  select(iso3, It) %>% 
  rename(tbi = It) %>% 
  mutate(type = "mtbi") %>% 
  rbind(filter(LTBI, !iso3 %in% c("HKG","MAC")))

MTBIvsLTBIdiff <- MTBbasWHO %>% 
  filter(year == 2014) %>% 
  select(iso3, It) %>% 
  left_join(LTBI, by = "iso3") %>% 
  select(!type) %>% 
  mutate(diff = tbi - It) %>% 
  mutate(difftf = ifelse(diff>0,TRUE,FALSE)) %>% 
  mutate(pdiff = 1-(It/tbi))

MTBIvsLTBIdiff[MTBIvsLTBIdiff$difftf == FALSE,] # ARE GNQ HUN KWT LBN LUX POL SOM

ggplot() +
  geom_col(data = filter(MTBIvsLTBI, type == "ltbi"), aes(x = iso3, y = tbi), colour = "#CE2931", fill = "#FFFFFF") +
  geom_col(data = filter(MTBIvsLTBI, type == "mtbi"), aes(x = iso3, y = tbi), colour = "#CE2931", fill = "#CE2931") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(x = "ISO3", y = "Number infected (millions)") +
  theme_minimal() + 
  theme(axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, angle = 90))  

ggplot() +
  geom_col(data = filter(MTBIvsLTBI, type == "ltbi" & iso3 %in% top30), aes(x = iso3, y = tbi), colour = "#CE2931", fill = "#FFFFFF") +
  geom_col(data = filter(MTBIvsLTBI, type == "mtbi" & iso3 %in% top30), aes(x = iso3, y = tbi), colour = "#CE2931", fill = "#CE2931") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(x = "ISO3", y = "Number infected (millions)") +
  theme_minimal() + 
  theme(axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, angle = 90))  

ggplot() +
  geom_col(data = MTBIvsLTBIdiff, aes(x = iso3, y = diff), colour = "#CE2931", fill = "#CE2931") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(x = "ISO3", y = "Diff. LTBI vs TBI)") +
  theme_minimal() + 
  theme(axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, angle = 90))  

ggplot() +
  geom_col(data = MTBIvsLTBIdiff, aes(x = iso3, y = pdiff), colour = "#CE2931", fill = "#CE2931") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "ISO3", y = "Diff. LTBI vs TBI)") +
  theme_minimal() + 
  theme(axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, angle = 90)) 

ggplot() +
  geom_col(data = MTBIvsLTBIdiff, aes(x = reorder(iso3, diff, decreasing = TRUE), y = diff), colour = "#CE2931", fill = "#CE2931") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(x = "ISO3", y = "Diff. LTBI vs TBI)") +
  theme_minimal() + 
  theme(axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, angle = 90))  

# Self-clearance plots
MTBscp <- MTBsc %>% 
  mutate(year = time + 1950) %>% 
  left_join(POP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5))))) %>% 
  select(iso3, year, starts_with("I")) %>% 
  filter(iso3 %in% c("CHN","IND","JPN")) %>% 
  pivot_longer(cols = -c(iso3, year), names_to = "var", values_to = "val") %>% 
  mutate(var = substr(var, 1, nchar(var) - 1)) %>% 
  group_by(iso3, year, var) %>% 
  summarise(val = sum(val)/1000000) %>% 
  filter(year == 2019)

MTBnoscp <- MTBnosc %>% 
  mutate(year = time + 1950) %>% 
  left_join(POP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5))))) %>% 
  select(iso3, year, starts_with("I")) %>% 
  filter(iso3 %in% c("CHN","IND","JPN")) %>% 
  pivot_longer(cols = -c(iso3, year), names_to = "var", values_to = "val") %>% 
  mutate(var = substr(var, 1, nchar(var) - 1)) %>% 
  group_by(iso3, year, var) %>% 
  summarise(val = sum(val)/1000000) %>% 
  filter(year == 2019)

pMTBsc <- MTBscp %>% 
  mutate(type = "self-clearance")

pMTBnosc <- MTBnoscp %>% 
  mutate(type = "lifelong")

pMTB <- pMTBsc %>% 
  rbind(pMTBnosc) %>% 
  group_by(iso3, type) %>% 
  summarise(val = sum(val)) %>% 
  group_by(iso3) %>% 
  mutate(prop = val/sum(val)) %>% 
  filter(type == "self-clearance")

labels = c("I0004" = "00-04", "I0509" = "05-09", "I1014" = "10-14", "I1519" = "15-19", "I2024" = "20-24",
           "I2529" = "25-29", "I3034" = "30-34", "I3539" = "35-39", "I4044" = "40-44", "I4549" = "45-49",
           "I5054" = "50-54", "I5559" = "55-59", "I6064" = "60-64", "I6569" = "65-69", "I7074" = "70-74",
           "I7579" = "75-79", "I8000" = "80+")

JPN <- ggplot() +
  geom_col(data = filter(MTBnoscp, iso3 == "JPN"), aes(x = var, y = val), colour = "#CE2931", fill = "#FFFFFF") +
  geom_col(data = filter(MTBscp, iso3 == "JPN"), aes(x = var, y = val), colour = "#CE2931", fill = "#CE2931") +
  scale_x_discrete(labels = labels) +
  labs(title = "Japan", x = "Age group", y = "Number infected (millions)") +
  theme_minimal() + 
  theme(axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, angle = 90))

CHN <- ggplot() +
  geom_col(data = filter(MTBnoscp, iso3 == "CHN"), aes(x = var, y = val), colour = "#CE2931", fill = "#FFFFFF") +
  geom_col(data = filter(MTBscp, iso3 == "CHN"), aes(x = var, y = val), colour = "#CE2931", fill = "#CE2931") +
  scale_x_discrete(labels = labels) +
  labs(title = "China", x = "Age group", y = "Number infected (millions)") +
  theme_minimal() + 
  theme(axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, angle = 90))

IND <- ggplot() +
  geom_col(data = filter(MTBnoscp, iso3 == "IND"), aes(x = var, y = val), colour = "#CE2931", fill = "#FFFFFF") +
  geom_col(data = filter(MTBscp, iso3 == "IND"), aes(x = var, y = val), colour = "#CE2931", fill = "#CE2931") +
  scale_x_discrete(labels = labels) +
  labs(title = "India", x = "Age group", y = "Number infected (millions)") +
  theme_minimal() + 
  theme(axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, angle = 90))

rMTB <- ggplot() +
  geom_col(data = pMTB, aes(x = iso3, y = prop), colour = "#CE2931", fill = "#CE2931") +
  scale_y_continuous(limit = c(0,1), labels = scales::percent_format(scale = 100)) +
  labs(title = "Relative to lifelong infection", x = "Country", y = "Percentage") +
  theme_minimal()

MTBplots <- (IND | CHN) / (JPN | rMTB)

tiff(here("figures", "sc.tiff"),  width = 10, height = 8, units = 'in', res = 100)
print(MTBplots)
dev.off()
  