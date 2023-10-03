# LTBI disparity
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
library(data.table) # Faster than data.frame

mtb <- import(here("data","mtb","Mtb_WHO_norev_nomix_pop_nosc.Rdata"))
ari <- import(here("data","ari","ARI_WHO_norev_nomix_pop.Rdata"))

# Numerical approach
num <- mtb %>%
  filter(time == 80) %>% # Change time/year to
  melt(id.vars = 'iso3') %>%
  filter(startsWith(as.character(variable), 'S')) %>%
  arrange(iso3) %>% 
  mutate(acat = case_when(
    variable == 'S0004' ~ '[00,05)', variable == 'S0509' ~ '[05,10)', variable == 'S1014' ~ '[10,15)',
    variable == 'S1519' ~ '[15,20)', variable == 'S2024' ~ '[20,25)', variable == 'S2529' ~ '[25,30)',
    variable == 'S3034' ~ '[30,35)', variable == 'S3539' ~ '[35,40)', variable == 'S4044' ~ '[40,45)',
    variable == 'S4549' ~ '[45,50)', variable == 'S5054' ~ '[50,55)', variable == 'S5559' ~ '[55,60)',
    variable == 'S6064' ~ '[60,65)', variable == 'S6569' ~ '[65,70)', variable == 'S7074' ~ '[70,75)',
    variable == 'S7579' ~ '[75,80)', variable == 'S8000' ~ '[80,90]')) %>% 
  mutate(ode = 1 - value)

# Analytical approach
ana <- ari %>%
  filter(acat == '45+' & agegp == '45-49') %>%
  arrange(iso3, desc(year)) %>%
  select(iso3, year, ari) %>%
  mutate(age = 2014 - year) %>%
  filter(age >= 0) %>%
  group_by(iso3) %>%
  mutate(cari = cumsum(ari)) %>%
  mutate(ltbi = 1 - exp(-cari)) %>%
  mutate(acat = cut(age, breaks = c(0, seq(5,80,5), 90), include.lowest = TRUE, right = FALSE)) %>%
  mutate(acat = case_when(acat == '[0,5)' ~ '[00,05)', acat == '[5,10)' ~ '[05,10)', TRUE ~ acat)) %>% 
  filter(!is.na(acat)) %>%
  group_by(iso3, acat) %>%
  summarize(ltbi = mean(ltbi))

# Compare approaches
plot(num$ode, ana$ltbi)
abline(a=0,b=1,col=2)

numana <- merge(num, ana, by = c("iso3","acat"), all = TRUE) %>% 
  select(-variable, -value)

iso <- unique(numana$iso3)

pdf(here("plots","LTBIdiscrep.pdf"), height = 6, width = 10) # Produce plot per country

for (i in iso) {
  # Subset the combined data for the specific 'iso3' value
  subset_data <- subset(numana, iso3 == i)
  
  # Create a scatter plot for the subset
  p <- ggplot(subset_data, aes(x = ode, y = ltbi)) +
    geom_point(aes(color = i)) +
    geom_abline(intercept = 0, slope = 1, color = "black") +
    labs(x = "ode", y = "ltbi") +
    ggtitle(paste("iso3 =", i)) +
    xlim(0, 1) + 
    ylim(0, 1) + 
    theme_minimal()
  
  # Save the plot as a separate PDF file
  print(p)
}
dev.off()
rm(i,p,subset_data)

# Weight by population (Current WPP)
POP <- import(here("data","sources","pop","WPP_Pop_1950-2100.csv"))
POP <- as.data.table(POP) %>% 
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
  rename(iso3 = ISO3_code)

numanapop <- numana %>% 
  left_join(POP, by = c("iso3","acat")) %>% 
  group_by(iso3) %>%
  summarise(
    wavg_ode = sum(ode*pop)/sum(pop),
    wavg_ltbi = sum(ltbi*pop)/sum(pop)) %>% 
  mutate(pdiff = round(abs(1-(wavg_ode/wavg_ltbi)),4))

# Weight by population (PMED WPP)
WPP_PM <- import(here("data","sources","pop","POP2014.Rdata"))
WPP_PM <- as.data.table(WPP_PM) %>% 
  filter(iso3 %in% iso) %>% 
  rename(acat = age, pop = value) %>% 
  select(iso3, acat, pop) %>% 
  mutate(acat = case_when(acat == '0-4' ~ '[00,05)', acat == '5-9' ~ '[05,10)', acat == '10-14' ~ '[10,15)',
                          acat == '15-19' ~ '[15,20)', acat == '20-24' ~ '[20,25)', acat == '25-29' ~ '[25,30)',
                          acat == '30-34' ~ '[30,35)', acat == '35-39' ~ '[35,40)', acat == '40-44' ~ '[40,45)',
                          acat == '45-49' ~ '[45,50)', acat == '50-54' ~ '[50,55)', acat == '55-59' ~ '[55,60)',
                          acat == '60-64' ~ '[60,65)', acat == '65-69' ~ '[65,70)', acat == '70-74' ~ '[70,75)',
                          acat == '75-79' ~ '[75,80)', acat == '80-' ~ '[80,90]'))
  
numanapoppmed <- numana %>% 
  left_join(WPP_PM, by = c("iso3","acat")) %>% 
  group_by(iso3) %>%
  summarise(
    wavg_ode = sum(ode*pop)/sum(pop),
    wavg_ltbi = sum(ltbi*pop)/sum(pop)) %>% 
  mutate(pdiff = round(abs(1-(wavg_ode/wavg_ltbi)),4))

# 80yos
numana80 <- numana %>% 
  filter(acat == '[80,90]') %>% 
  mutate(pdiff = round(abs(1-(ode/ltbi)),2))
