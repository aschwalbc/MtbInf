## Analysis code for Schwalb et al. 2023
## Distributed under CC BY 4.0
## RScript: LTBIdispar.R

# Packages ==========
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
options(scipen = 999) # Remove scientific notation

# 1. Load data ==========
ari <- import(here("data","ari","mARI_WHO_norev_nomix_pop.Rdata")) # ARI history
ode <- import(here("data","mtb","mMtb_WHO_norev_nomix_pop_nosc.Rdata")) # ODE results
odi <- import(here("data","mtb","odingmtb.Rdata")) # ODIN ODE
# odi <- import(here("data","mtb","odingmtb_s.Rdata")) # ODIN ODE (Smaller time steps)
pop <- import(here("data","sources","pop","WPP_Pop_1950-2100.csv")) # Current WPP
poppm <- import(here("data","sources","pop","POP2014.Rdata")) # PMED WPP
pmed <- import(here("supp mat","Houben & Dodd - Supp Material.csv"), skip = 1) # PMED results

# 2. Approaches ==========
# 2.1 Analytical approach
ari <- ari %>% # ARI histories
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

# 2.2 Numerical approach
ode <- ode %>% # ODE results
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
  mutate(ode = 1 - val) %>% # Infected = 1 - Susceptible
  select(iso3, acat, ode) # Select variables

odi <- odi %>% # ODIN ODE results
  rename(odin = ltbi.prev, agegp = ltbi.agegp) %>% # Rename variables
  mutate(acat = case_when( # Fix age categories
    agegp == '00-04' ~ '[00,05)', agegp == '05-09' ~ '[05,10)', agegp == '10-14' ~ '[10,15)',
    agegp == '15-19' ~ '[15,20)', agegp == '20-24' ~ '[20,25)', agegp == '25-29' ~ '[25,30)',
    agegp == '30-34' ~ '[30,35)', agegp == '35-39' ~ '[35,40)', agegp == '40-44' ~ '[40,45)',
    agegp == '45-49' ~ '[45,50)', agegp == '50-54' ~ '[50,55)', agegp == '55-59' ~ '[55,60)',
    agegp == '60-64' ~ '[60,65)', agegp == '65-69' ~ '[65,70)', agegp == '70-74' ~ '[70,75)',
    agegp == '75-79' ~ '[75,80)', agegp == '80+' ~ '[80,90]')) %>% 
  select(iso3, acat, odin)

# Merge approaches
df <- merge(ari, ode, by = c("iso3","acat"), all = TRUE) %>% # Merge with ODE results
  merge(odi, by = c("iso3","acat"), all = TRUE) %>% # Merge with ODIN ODE results
  mutate(difltode = ode - ltbi, difltodi = odin - ltbi, difodeodi = ode - odin,
         pdifltode = 1 - (ode/ltbi), pdifltodi = 1 - (odin/ltbi), pdifodeodi = 1 - (ode/odin)) %>% # Calculate differences
  mutate(difltode = round(difltode, 5), difltodi = round(difltodi, 5), difodeodi = round(difodeodi, 5),
         pdifltode = round(pdifltode, 5), pdifltodi = round(pdifltodi, 5), pdifodeodi = round(pdifodeodi, 5)) # Remove scientific notation and round
rm(ari, ode, odi)

# 3. PMED Results
pmed <- pmed %>% 
  rename(ltbi = `All LTBI`) %>% 
  select(iso3, ltbi) %>% 
  separate(ltbi, into = c("val", "ci"), sep = "\\[") %>% 
  separate(ci, into = c("lo", "hi"), sep = "\\-") %>% 
  mutate(val = trimws(val), lo = trimws(lo), hi = trimws(hi)) %>% 
  mutate(val = str_remove_all(val, ","), lo = str_remove_all(lo, ","), hi = str_remove_all(hi, ",")) %>% 
  mutate(hi = str_remove_all(hi, "]")) %>% 
  mutate(val = as.integer(val), lo = as.integer(lo), hi = as.integer(hi))

# 4. Population ==========
iso <- unique(df$iso3)

# 4.1 Current WPP (POP) - Weight by population
pop <- pop %>% 
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

dfpop <- df %>% 
  left_join(pop, by = c("iso3","acat")) %>% 
  group_by(iso3) %>%
  summarise(wavg_ode = sum(ode*pop)/sum(pop),
            wavg_odin = sum(odin*pop)/sum(pop),
            wavg_ltbi = sum(ltbi*pop)/sum(pop)) %>% 
  mutate(pdiffode = round((1-(wavg_ode/wavg_ltbi)),4),
         pdiffodin = round((1-(wavg_odin/wavg_ltbi)),4),
         adiffode = round((wavg_ode-wavg_ltbi),4),
         adiffodin = round((wavg_odin-wavg_ltbi),4))
summary(dfpop$pdiffode)
summary(dfpop$adiffode)
summary(dfpop$pdiffodin)
summary(dfpop$adiffodin)

# 4.2 PMED WPP (POP) - Weight by population
# Weight by population (PMED WPP)
poppm <- poppm %>% 
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

dfpoppm <- df %>% 
  left_join(poppm, by = c("iso3","acat")) %>% 
  group_by(iso3) %>%
  summarise(wavg_ode = sum(ode*pop)/sum(pop),
            wavg_odin = sum(odin*pop)/sum(pop),
            wavg_ltbi = sum(ltbi*pop)/sum(pop)) %>% 
  mutate(pdiffode = round((1-(wavg_ode/wavg_ltbi)),4),
         pdiffodin = round((1-(wavg_odin/wavg_ltbi)),4),
         adiffode = round((wavg_ode-wavg_ltbi),4),
         adiffodin = round((wavg_odin-wavg_ltbi),4))

# 5. Plots ==========
# 5.1 Quick comparison (ODE v Analytical)
tiff(here("plots", "LTBIdiscrep_ODE.tiff"), height = 1000, width = 1400, res = 300)
ggplot() +
  geom_point(aes(x = df$ode, y = df$ltbi), size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_abline(intercept = 0.10, slope = 1, color = "red", linetype = 'dashed') +
  geom_abline(intercept = -0.10, slope = 1, color = "red", linetype = 'dashed') +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  labs(x = "Numerical approach (ODE)", y = "Analytical approach (ARI)") +
  theme_bw()
dev.off()

# 5.2 Quick comparison (ODIN ODE v Analytical)
tiff(here("plots", "LTBIdiscrep_ODIN.tiff"), height = 1000, width = 1400, res = 300)
ggplot() +
  geom_point(aes(x = df$odin, y = df$ltbi), size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_abline(intercept = 0.10, slope = 1, color = "red", linetype = 'dashed') +
  geom_abline(intercept = -0.10, slope = 1, color = "red", linetype = 'dashed') +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  labs(x = "Numerical approach (ODIN)", y = "Analytical approach (ARI)") +
  theme_bw()
dev.off()

# 5.3 Quick comparison (ODE v ODIN ODE)
tiff(here("plots", "LTBIdiscrep_ODEODIN.tiff"), height = 1000, width = 1400, res = 300)
ggplot() +
  geom_point(aes(x = df$ode, y = df$odin), size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_abline(intercept = 0.10, slope = 1, color = "red", linetype = 'dashed') +
  geom_abline(intercept = -0.10, slope = 1, color = "red", linetype = 'dashed') +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  labs(x = "Numerical approach (ODE)", y = "Numerical approach (ODIN)") +
  theme_bw()
dev.off()

sub_df <- df %>% # Subset dataframe
  mutate(ablineodeabv = ode + 0.10, ablineodeund = ode - 0.10, ablineodinabv = odin + 0.10) %>% # Slope with intercept -0.1
  mutate(outboundodeabv = as.integer(ltbi > ablineodeabv), outboundodinabv = as.integer(ltbi > ablineodinabv)) %>% # Evaluate if it is above intercept
  mutate(outboundodeund = as.integer(odin < ablineodeund))

sum(as.numeric(sub_df$outboundodeabv)) # Out of bounds
unique(sub_df$iso3[sub_df$outboundodeabv == TRUE])
sum(as.numeric(sub_df$outboundodinabv)) # Out of bounds
unique(sub_df$iso3[sub_df$outboundodinabv == TRUE])
sum(as.numeric(sub_df$outboundodeund)) # Out of bounds
unique(sub_df$iso3[sub_df$outboundodeund == TRUE])

# 5.4 Quick comparison per country
pdf(here("plots","LTBIdiscrep_iso3.pdf"), height = 6, width = 10)
for (i in iso) {
  subset <- df %>% 
    filter(iso3 == i) %>% 
    select(iso3, acat, ltbi, ode, odin) %>%
    pivot_longer(cols = c(ode, odin), names_to = "var", values_to = "val")
  
  p <- ggplot(subset, aes(x = val, y = ltbi)) +
    facet_wrap(~var,) +
    geom_point(size = 1) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    geom_abline(intercept = 0.10, slope = 1, color = "red", linetype = 'dashed') +
    geom_abline(intercept = -0.10, slope = 1, color = "red", linetype = 'dashed') +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = "Numerical approach", y = "Analytical approach (Cumulative ARI)") +
    ggtitle(paste("ISO3: ", i)) +
    theme_bw()
  
  print(p)
}

dev.off()
rm(i,p,subset)

pdf(here("plots","LTBIdiscrep_iso3_bar.pdf"), height = 10, width = 22)
for (i in iso) {
  subset <- df %>% 
    filter(iso3 == i) %>% 
    select(iso3, acat, ltbi, ode, odin) %>%
    pivot_longer(cols = c(ltbi, ode, odin), names_to = "var", values_to = "val") %>% 
    mutate(var = factor(var, levels = c("ltbi", "ode", "odin")))
  
  p <- ggplot(subset, aes(x = acat, y = val, fill = var)) +
    facet_wrap(~var,) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Numerical approach", y = "Analytical approach (Cumulative ARI)") +
    ggtitle(paste("ISO3: ", i)) +
    theme_bw()
  
  print(p)
}

dev.off()
rm(i,p,subset)

# 5.4 Quick comparison for top 30 countries
top30 <- c("IND","CHN","IDN","ZAF","PAK","PHL","NGA","BGD","COD","MMR",
           "ETH","KEN","VNM","TZA","PRK","THA","RUS","AGO","MOZ","BRA",
           "NPL","UGA","ZMB","AFG","KHM","MDG","CMR","GHA","KOR","UKR")

pdf(here("plots","LTBIdiscrep_top30.pdf"), height = 6, width = 10)
for (i in top30) {
  subset <- df %>% 
    filter(iso3 == i) %>% 
    select(iso3, acat, ltbi, ode, odin) %>%
    pivot_longer(cols = c(ode, odin), names_to = "var", values_to = "val")
  
  p <- ggplot(subset, aes(x = val, y = ltbi)) +
    facet_wrap(~var,) +
    geom_point(size = 1) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    geom_abline(intercept = 0.10, slope = 1, color = "red", linetype = 'dashed') +
    geom_abline(intercept = -0.10, slope = 1, color = "red", linetype = 'dashed') +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = "Numerical approach", y = "Analytical approach (Cumulative ARI)") +
    ggtitle(paste("ISO3: ", i)) +
    theme_bw()
  
  print(p)
}

dev.off()
rm(i,p,subset)

pdf(here("plots","LTBIdiscrep_top30_bar.pdf"), height = 10, width = 22)
for (i in top30) {
  subset <- df %>% 
    filter(iso3 == i) %>% 
    select(iso3, acat, ltbi, ode, odin) %>%
    pivot_longer(cols = c(ltbi, ode, odin), names_to = "var", values_to = "val") %>% 
    mutate(var = factor(var, levels = c("ltbi", "ode", "odin")))
  
  p <- ggplot(subset, aes(x = acat, y = val, fill = var)) +
    facet_wrap(~var,) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Numerical approach", y = "Analytical approach (Cumulative ARI)") +
    ggtitle(paste("ISO3: ", i)) +
    theme_bw()
  
  print(p)
}

dev.off()
rm(i,p,subset)

# 5.5 Disparity in numbers
dispdf <- df %>% 
  group_by(acat) %>% 
  summarise(difltode = round(mean(difltode)*1e2,2), difltodi = round(mean(difltodi)*1e2,2), difodeodi = round(mean(difodeodi)*1e2,2),
            pdifltode = round(mean(pdifltode)*1e2,2), pdifltodi = round(mean(pdifltodi)*1e2,2), pdifodeodi = round(mean(pdifodeodi)*1e2,2))
