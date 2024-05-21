rm(list=ls())

suppressPackageStartupMessages({
   library(here)
   library(tidyverse)
   library(data.table)
   library(rio)
   library(stats)
   library(fst)
   options(dplyr.summarise.inform = FALSE)
})

# Define scenario
scenario <- "y20"
cat(paste(Sys.time(), "- Running scenario:", scenario))

# Population data 
POP <- import(here("proc", "WPP_Pop_1950-2100.csv")) %>% 
  select(iso3 = ISO3_code, year = Time, ageWPP = AgeGrp, pop = PopTotal) %>%
  mutate(iso3 = na_if(iso3, "")) %>% 
  filter(!is.na(iso3)) %>%
  filter(year <= 2022) %>%
  mutate(ageWPP = case_when(ageWPP == '0-4' ~ 'N0004', ageWPP == '5-9' ~ 'N0509', ageWPP == '10-14' ~ 'N1014', ageWPP == '15-19' ~ 'N1519',
                            ageWPP == '20-24' ~ 'N2024', ageWPP == '25-29' ~ 'N2529', ageWPP == '30-34' ~ 'N3034', ageWPP == '35-39' ~ 'N3539', ageWPP == '40-44' ~ 'N4044',
                            ageWPP == '45-49' ~ 'N4549', ageWPP == '50-54' ~ 'N5054', ageWPP == '55-59' ~ 'N5559', ageWPP == '60-64' ~ 'N6064', ageWPP == '65-69' ~ 'N6569', 
                            ageWPP == '70-74' ~ 'N7074', ageWPP == '75-79' ~ 'N7579', ageWPP %in% c('80-84','85-89','90-94','95-99','100+') ~ 'N8000')) %>%
  group_by(iso3, year, ageWPP) %>% 
  summarise(pop = sum(pop) * 1e3) %>% 
  pivot_wider(id_cols = c("iso3", "year"), names_from = ageWPP, values_from = pop)

# WHO region data
WHO <- as.data.table(import(here("proc", "WHOest_2000-2022.csv"))) %>% 
  select(iso3, reg = g_whoregion) %>% 
  distinct()

# Get all file names
isos <- list.files(path = here(paste0("mtb_", scenario)), pattern = "\\.fst$", full.names = TRUE)

# Initialize an empty list to store the data tables
mtbs <- list()

# Load data
for(i in seq_along(isos)) {
  mtbs[[i]] <- as.data.table(read.fst(isos[i]))
}
rm(i, isos)
mtbs <- rbindlist(mtbs)

MTB <- mtbs %>% 
  mutate(year = time + 1950) %>% 
  left_join(POP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))))
rm(mtbs)

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
export(MTBglb, here("outputs", paste0("mtb_", scenario), "MTBglb.Rdata"))
cat(paste(Sys.time(), "- Completed: Global estimates\n"))

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
export(MTBreg, here("outputs", paste0("mtb_", scenario), "MTBreg.Rdata"))
cat(paste(Sys.time(), "- Completed: Regional estimates\n"))

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
export(MTBiso, here("outputs", paste0("mtb_", scenario), "MTBiso.Rdata"))
cat(paste(Sys.time(), "- Completed: Country estimates\n"))

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
export(MTBage_pct, here("outputs", paste0("mtb_", scenario), "MTBage_pct.Rdata"))
cat(paste(Sys.time(), "- Completed: Country estimates per age group (proportion)\n"))

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
export(MTBage_num, here("outputs", paste0("mtb_", scenario), "MTBage_num.Rdata"))
cat(paste(Sys.time(), "- Completed: Country estimates per age group (absolute number)\n"))

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
export(MTBglbkid, here("outputs", paste0("mtb_", scenario), "MTBglbkid.Rdata"))
cat(paste(Sys.time(), "- Completed: Global estimates in children\n"))
