## Analysis code for Schwalb et al. 2024
## Distributed under CC BY 4.0
## RScript 06: Mtbburden.R (Full run)

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

# 2. WHO region data ==========
WHO <- as.data.table(import(here("data","sources","who","WHOest_2000-2022.csv")))

WHO <- WHO %>% 
  select(iso3, reg = g_whoregion) %>% 
  distinct()

# 3. Mtb estimates ==========
# 3.1 Individual runs
files <- list.files(here("data", "mtb", "isos_y20"), full.names = TRUE)
# files <- list.files(here("data", "mtb", "isos_y35"), full.names = TRUE)
# files <- list.files(here("data", "mtb", "isos_y50"), full.names = TRUE)

df <- list()
pb <- progress_bar$new(format = "[:bar] :percent :eta", total = length(files))
for (i in files) {
  env <- new.env()
  load(i, envir = env)
  
  objs <- mget(ls(envir = env), envir = env)
  
  for (obj in objs) {
    if (is.data.frame(obj) || class(obj) == "data.table") {
      df[[length(df) + 1]] <- obj
    }
  }
  pb$tick()
}

MTBdf <- rbindlist(df, use.names = TRUE, fill = TRUE)
rm(env, obj, objs, files, i, pb)

# 3.2 Data curation
MTB <- MTBdf %>% 
  mutate(year = time + 1950) %>% 
  left_join(POP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))))

MTBglb <- MTB %>% 
  left_join(WHO, by = c('iso3')) %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-d]?$")))) %>% 
  mutate(rIt = rowSums(across(matches("^I\\d{4}[a-b]$")))) %>%
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  select(iso3, year, reg, rep, St, It, rIt, Nt) %>% 
  group_by(year, rep) %>% 
  summarise(St = sum(St), It = sum(It), rIt = sum(rIt), Nt = sum(Nt)) %>% 
  mutate(pI = It / Nt, prI = rIt / Nt, rI = prI / pI) %>% 
  pivot_longer(cols = -c(year, rep), names_to = "var", values_to = "values") %>%
  group_by(year, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))

MTBreg <- MTB %>% 
  left_join(WHO, by = c('iso3')) %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-d]?$")))) %>% 
  mutate(rIt = rowSums(across(matches("^I\\d{4}[a-b]$")))) %>%
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  select(iso3, year, reg, rep, St, It, rIt, Nt) %>% 
  group_by(year, reg, rep) %>% 
  summarise(St = sum(St), It = sum(It), rIt = sum(rIt), Nt = sum(Nt)) %>% 
  mutate(pI = It / Nt, prI = rIt / Nt, rI = prI / pI) %>% 
  group_by(year, rep) %>% 
  mutate(regrIt = rIt / sum(rIt)) %>% 
  ungroup() %>% 
  pivot_longer(cols = -c(year, reg, rep), names_to = "var", values_to = "values") %>%
  group_by(year, reg, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))

MTBiso <- MTB %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-d]?$")))) %>% 
  mutate(rIt = rowSums(across(matches("^I\\d{4}[a-b]$")))) %>%
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  left_join(WHO, by = c('iso3')) %>% 
  select(iso3, year, reg, rep, St, It, rIt, Nt) %>% 
  mutate(pI = It / Nt, prI = rIt / Nt, rI = prI / pI) %>% 
  pivot_longer(cols = -c(iso3, year, reg, rep), names_to = "var", values_to = "values") %>%
  group_by(iso3, year, reg, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))

MTBage_pct <- MTB %>% 
  select(-starts_with('S')) %>% 
  left_join(WHO, by = c('iso3')) %>% 
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
  select(iso3, year, reg, rep, starts_with('pI'), starts_with('prI')) %>% 
  pivot_longer(cols = starts_with("pI") | starts_with("prI"), names_to = c("var", "agegp"),
               names_pattern = "^([a-z]+I)(\\d{4})$", values_to = "values") %>%
  mutate(agegp = str_replace(agegp, "(\\d{2})(\\d{2})", "\\1-\\2"),
         var = str_replace(var, "([a-z]+)I", "\\1I")) %>% 
  group_by(iso3, year, reg, agegp, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))
  
MTBage_num <- MTB %>% 
  select(-starts_with('N')) %>% 
  left_join(WHO, by = c('iso3')) %>% 
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
  select(iso3, year, reg, rep, starts_with('dI'), starts_with('rI'), starts_with('S')) %>% 
  pivot_longer(cols = -c(iso3, year, reg, rep), names_to = c("var", "agegp"),
               names_pattern = "^(rI|dI|S)(\\d{4})$", values_to = "values") %>%
  mutate(agegp = str_replace(agegp, "(\\d{2})(\\d{2})", "\\1-\\2"),
         var = case_when(str_detect(var, "^dI") ~ "dI", str_detect(var, "^rI") ~ "rI",
                         str_detect(var, "^S") ~ "S", TRUE ~ as.character(var))) %>% 
  mutate(var = factor(var, levels = c('S', 'dI', 'rI'))) %>% 
  group_by(iso3, year, reg, agegp, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))

MTBglbkid <- MTB %>% 
  select(-starts_with('N')) %>% 
  left_join(WHO, by = c('iso3')) %>% 
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
  select(iso3, year, reg, rep, starts_with('dI'), starts_with('rI'), starts_with('S')) %>% 
  pivot_longer(cols = -c(iso3, year, reg, rep), names_to = c("var", "agegp"),
               names_pattern = "^(rI|dI|S)(\\d{4})$", values_to = "values") %>%
  mutate(agegp = str_replace(agegp, "(\\d{2})(\\d{2})", "\\1-\\2"),
         var = case_when(str_detect(var, "^dI") ~ "dI", str_detect(var, "^rI") ~ "rI",
                         str_detect(var, "^S") ~ "S", TRUE ~ as.character(var))) %>% 
  mutate(acat = case_when(agegp %in% c('00-04', '05-09', '10-14') ~ 'child', TRUE ~ 'adult')) %>% 
  filter(var == 'rI') %>% 
  group_by(year, rep, acat) %>% 
  summarise(rI = sum(values)) %>% 
  mutate(values = rI / sum(rI)) %>% 
  ungroup() %>% 
  group_by(year, acat) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))
  
iso <- unique(MTB$iso3)

pdf(here("plots","06_mtbburden","Inf_iso.pdf"), height = 6, width = 10)
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
    scale_x_continuous(expand=c(0, 0), breaks = seq(1960, 2050, 20)) +
    scale_y_continuous(labels = scales::label_percent(), expand=c(0, 0), breaks = seq(0, 1, 0.2)) +
    coord_cartesian(ylim = c(0, 0.5), xlim = c(1960, 2022)) +
    labs(title = i, x = 'Year', y = 'Proportion infected (%)', colour = 'Type', fill = 'Type') +
    theme_bw() + 
    theme(legend.position = 'bottom')
  print(p)
}
dev.off()

pdf(here("plots","06_mtbburden","Inf_agegp.pdf"), height = 6, width = 10)
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

png(here("plots","06_mtbburden","RecInf.png"), width = 8, height = 5, units = 'in', res = 150)
ggplot() +
  geom_line(filter(MTBglb, var == 'rI'), mapping = aes(x = year, y = val, colour = 'Recent'), colour = '#FF5733') +
  geom_ribbon(filter(MTBglb, var == 'rI'), mapping = aes(x = year, ymin = lo, ymax = hi, fill = 'Recent'), fill = '#FF5733', alpha = 0.2) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(1960, 2050, 20)) +
  scale_y_continuous(labels = scales::label_percent(), expand=c(0, 0), breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 0.6), xlim = c(1960, 2022)) +
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
  #filter(!type %in% c('Dependency','Indeterminate','Disputed')) %>% 
  left_join(MTB2022, by = c("iso_a3_eh" = "iso3"))


colours <- c('0.0 - 0.25%' = "#fee5d9", '0.25 - 1.0%' = "#fcae91", '1.0 - 2.0%' = "#fb6a4a",
             '2.0 - 5.0%' = "#a50f15", '5.0 - 10.0%' = "#67000d")

png(here("plots","06_mtbburden","MtbMap.png"), width = 10, height = 5, units = 'in', res = 300)
ggplot(data = world) +
  geom_sf(aes(fill = prev)) +
  labs(title = expression("Global burden of recent viable " * italic("Mycobacterium tuberculosis") * " infection")) +
  scale_fill_manual(values = colours, na.value = "#F7F7F7", guide = guide_legend(title = "Prevalence")) +
  theme_void() +
  theme(legend.position = c(0.10, 0.25),
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
  coord_sf(crs = "+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dev.off()
