## Analysis code for Schwalb et al. 2023
## Distributed under CC BY 4.0
## RScript: LTBIdispar.R

# Packages ==========
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse

# 1. Load data ==========
mtb <- import(here("data","mtb","mMtb_WHO_norev_nomix_pop_nosc.Rdata")) # ODE results
ari <- import(here("data","ari","mARI_WHO_norev_nomix_pop.Rdata")) # ARI history
POP <- import(here("data","sources","pop","WPP_Pop_1950-2100.csv")) # Current WPP
POP_PM <- import(here("data","sources","pop","POP2014.Rdata")) # PMED WPP
PMED <- import(here("data","sources","others","PMED.1002152.csv")) # PMED results

# 2. Approaches ==========
# 2.1 Numerical approach
num <- mtb %>% # ODE results
  filter(time == 80) %>% # Change time/year to 2014
  pivot_longer(cols = -iso3, names_to = "var", values_to = "val") %>% # Pivot to longer
  filter(startsWith(as.character(var), 'S')) %>% # Keep estimates of susceptibles
  arrange(iso3) %>% # Arrange for neatness
  mutate(acat = case_when( # Fix age categories
    var == 'S0004' ~ '[00,05)', var == 'S0509' ~ '[05,10)', var == 'S1014' ~ '[10,15)',
    var == 'S1519' ~ '[15,20)', var == 'S2024' ~ '[20,25)', var == 'S2529' ~ '[25,30)',
    var == 'S3034' ~ '[30,35)', var == 'S3539' ~ '[35,40)', var == 'S4044' ~ '[40,45)',
    var == 'S4549' ~ '[45,50)', var == 'S5054' ~ '[50,55)', var == 'S5559' ~ '[55,60)',
    var == 'S6064' ~ '[60,65)', var == 'S6569' ~ '[65,70)', var == 'S7074' ~ '[70,75)',
    var == 'S7579' ~ '[75,80)', var == 'S8000' ~ '[80,90]')) %>% 
  mutate(ode = 1 - val) # Infected = 1 - Susceptible
rm(mtb)

# 2.2 Analytical approach
ana <- ari %>% # ARI histories
  filter(agegp == '00-04') %>% # Filter by any age group (Same ARI)
  arrange(iso3, desc(year)) %>% # Arrange by ISO3 and order year
  select(iso3, year, ari) %>% # Select variables
  mutate(age = 2014 - year) %>% # Create age cohort based on 2014
  filter(age >= 0) %>% # Filter out negative age
  group_by(iso3) %>% # Group by ISO3
  mutate(cari = cumsum(ari)) %>% # Estimate cumulative ARIs
  mutate(ltbi = 1 - exp(-cari)) %>% # Estimate LTBI prevalence
  mutate(acat = cut(age, breaks = c(0, seq(5,80,5), 90), include.lowest = TRUE, right = FALSE)) %>% # Create age group categories
  mutate(acat = case_when(acat == '[0,5)' ~ '[00,05)', acat == '[5,10)' ~ '[05,10)', TRUE ~ acat)) %>% # Edit categories
  group_by(iso3, acat) %>% # Group by ISO3 and age groups
  summarize(ltbi = mean(ltbi)) # Obtain age group mean
rm(ari)

# Merge approaches
df <- merge(num, ana, by = c("iso3","acat"), all = TRUE) %>% 
  select(-var, -val) %>% 
  mutate(diff = ode - ltbi) %>% 
  mutate(diff = format(diff, scientific = FALSE))
rm(num, ana)

# 3. PMED Results
PMED <- PMED %>% 
  rename(ltbi = `All LTBI`) %>% 
  select(iso3, ltbi) %>% 
  separate(ltbi, into = c("val", "ci"), sep = "\\[") %>% 
  separate(ci, into = c("lo", "hi"), sep = "\\-") %>% 
  mutate(val = trimws(val), lo = trimws(lo), hi = trimws(hi)) %>% 
  mutate(val = str_remove_all(val, ","), lo = str_remove_all(lo, ","), hi = str_remove_all(hi, ",")) %>% 
  mutate(hi = str_remove_all(hi, "]")) %>% 
  mutate(val = as.numeric(val), lo = as.numeric(lo), hi = as.numeric(hi))
gPMED <- sum(PMED$val)

# 4. Population ==========
iso <- unique(df$iso3)

# 4.1 Current WPP (POP) - Weight by population
POP <- POP %>% 
  filter(Time == 2014) %>% 
  filter(ISO3_code %in% iso) %>% 
  select(ISO3_code, AgeGrp, PopTotal) %>% 
  mutate(acat = case_when(AgeGrp %in% c('80-84','85-89','90-94','95-99','100+') ~ '80+',
                          AgeGrp == '0-4' ~ '00-04', AgeGrp == '5-9' ~ '05-09', TRUE ~ AgeGrp)) %>%
  group_by(ISO3_code, acat) %>% 
  summarise(pop = sum(PopTotal)) %>% 
  mutate(acat = case_when(acat == '00-04' ~ '[00,05)', acat == '05-09' ~ '[05,10)', acat == '10-14' ~ '[10,15)',
                          acat == '15-19' ~ '[15,20)', acat == '20-24' ~ '[20,25)', acat == '25-29' ~ '[25,30)',
                          acat == '30-34' ~ '[30,35)', acat == '35-39' ~ '[35,40)', acat == '40-44' ~ '[40,45)',
                          acat == '45-49' ~ '[45,50)', acat == '50-54' ~ '[50,55)', acat == '55-59' ~ '[55,60)',
                          acat == '60-64' ~ '[60,65)', acat == '65-69' ~ '[65,70)', acat == '70-74' ~ '[70,75)',
                          acat == '75-79' ~ '[75,80)', acat == '80+' ~ '[80,90]')) %>% 
  rename(iso3 = ISO3_code) %>% 
  mutate(pop = pop*1e3)

gPOP <- sum(POP$pop) # 7251345637

dfPOP <- df %>% 
  left_join(POP, by = c("iso3","acat")) %>% 
  group_by(iso3) %>%
  summarise(wavg_ode = sum(ode*pop)/sum(pop),
            wavg_ltbi = sum(ltbi*pop)/sum(pop)) %>% 
  mutate(pdiff = round((1-(wavg_ode/wavg_ltbi)),4),
         adiff = round((wavg_ode-wavg_ltbi),4))

gdfPOP <- df %>% 
  left_join(POP, by = c("iso3","acat")) %>% 
  mutate(odepop = ode*pop, ltbipop = ltbi*pop) %>% 
  group_by(iso3, acat) %>%
  summarise(odepop = sum(odepop), ltbipop = sum(ltbipop)) %>% 
  group_by(iso3) %>%
  summarise(odepop = sum(odepop), ltbipop = sum(ltbipop))

gODE_POP <- round(sum(gdfPOP$odepop), 0)
prevODE_POP <- round((gODE_POP/gPOP)*1e2, 1)

gLTBI_POP <- round(sum(gdfPOP$ltbipop), 0)
prevLTBI_POP <- round((gLTBI_POP/gPOP)*1e2, 1)

# 4.2 PMED WPP (POP) - Weight by population
# Weight by population (PMED WPP)
POP_PM <- POP_PM %>% 
  filter(iso3 %in% iso) %>% 
  rename(acat = age, pop = value) %>% 
  select(iso3, acat, pop) %>% 
  mutate(pop = pop*1e3) %>% 
  mutate(acat = case_when(acat == '0-4' ~ '[00,05)', acat == '5-9' ~ '[05,10)', acat == '10-14' ~ '[10,15)',
                          acat == '15-19' ~ '[15,20)', acat == '20-24' ~ '[20,25)', acat == '25-29' ~ '[25,30)',
                          acat == '30-34' ~ '[30,35)', acat == '35-39' ~ '[35,40)', acat == '40-44' ~ '[40,45)',
                          acat == '45-49' ~ '[45,50)', acat == '50-54' ~ '[50,55)', acat == '55-59' ~ '[55,60)',
                          acat == '60-64' ~ '[60,65)', acat == '65-69' ~ '[65,70)', acat == '70-74' ~ '[70,75)',
                          acat == '75-79' ~ '[75,80)', acat == '80-' ~ '[80,90]'))

gPOP_PM <- sum(POP_PM$pop) # 7224311000

dfPOP_PM <- df %>% 
  left_join(POP_PM, by = c("iso3","acat")) %>% 
  group_by(iso3) %>%
  summarise(wavg_ode = sum(ode*pop)/sum(pop),
            wavg_ltbi = sum(ltbi*pop)/sum(pop)) %>% 
  mutate(pdiff = round((1-(wavg_ode/wavg_ltbi)),4),
         adiff = round((wavg_ode-wavg_ltbi),4))

gdfPOP_PM <- df %>% 
  left_join(POP_PM, by = c("iso3","acat")) %>% 
  mutate(odepop = ode*pop, ltbipop = ltbi*pop) %>% 
  group_by(iso3, acat) %>%
  summarise(odepop = sum(odepop), ltbipop = sum(ltbipop)) %>% 
  group_by(iso3) %>%
  summarise(odepop = sum(odepop), ltbipop = sum(ltbipop))

gODE_POP_PM <- round(sum(gdfPOP_PM$odepop), 0)
prevODE_POP_PM <- round((gODE_POP_PM/gPOP_PM)*1e2, 1)

gLTBI_POP_PM <- round(sum(gdfPOP_PM$ltbipop), 0)
prevLTBI_POP_PM <- round((gLTBI_POP_PM/gPOP_PM)*1e2, 1)

# 5. Plots ==========
# 5.1 Quick comparison
tiff(here("plots", "LTBIdiscrep.tiff"), height = 1000, width = 1400, res = 300)
ggplot() +
  geom_point(aes(x = df$ode, y = df$ltbi), size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Numerical approach (ODE)", y = "Analytical approach (Cumulative ARI)") +
  theme_bw()
dev.off()

# 5.2 Quick comparison per country
pdf(here("plots","LTBIdiscrep_iso3.pdf"), height = 6, width = 10)
for (i in iso) {
  subset <- subset(df, iso3 == i)
  p <- ggplot(subset) +
    geom_point(aes(x = ode, y = ltbi), size = 1) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
    labs(x = "Numerical approach (ODE)", y = "Analytical approach (Cumulative ARI)") +
    ggtitle(paste("ISO3: ", i)) +
    theme_bw()
  print(p)
}
dev.off()
rm(i,p,subset)

# 5.3 Quick comparison for top 30 countries
top30 <- c("IND","CHN","IDN","ZAF","PAK","PHL","NGA","BGD","COD","MMR",
           "ETH","KEN","VNM","TZA","PRK","THA","RUS","AGO","MOZ","BRA",
           "NPL","UGA","ZMB","AFG","KHM","MDG","CMR","GHA","KOR","UKR")

pdf(here("plots","LTBIdiscrep_top30.pdf"), height = 6, width = 10)
for (i in top30) {
  subset <- subset(df, iso3 == i)
  p <- ggplot(subset) +
    geom_point(aes(x = ode, y = ltbi), size = 1) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
    labs(x = "Numerical approach (ODE)", y = "Analytical approach (Cumulative ARI)") +
    ggtitle(paste("ISO3: ", i)) +
    theme_bw()
  print(p)
}
dev.off()
rm(i,p,subset)
