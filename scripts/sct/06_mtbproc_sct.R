## Mtb-Inf: Scotland

# Packages ==========
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

# Population data 
POP <- import(here("sources", "sct", "ONSpop_1961-2023.csv")) %>%
  mutate(iso3 = 'GBR') %>% 
  select(iso3, year = Year, ageWPP = Age_band, pop = Population) %>%
  mutate(ageWPP = case_when(ageWPP == '0-4' ~ '00-04', ageWPP == '5-9' ~ '05-09', TRUE ~ ageWPP)) %>% 
  arrange(year) %>% 
  filter(year <= 2023) %>% 
  mutate(ageWPP = case_when(ageWPP == '00-04' ~ 'N0004', ageWPP == '05-09' ~ 'N0509', ageWPP == '10-14' ~ 'N1014', ageWPP == '15-19' ~ 'N1519',
                            ageWPP == '20-24' ~ 'N2024', ageWPP == '25-29' ~ 'N2529', ageWPP == '30-34' ~ 'N3034', ageWPP == '35-39' ~ 'N3539', ageWPP == '40-44' ~ 'N4044',
                            ageWPP == '45-49' ~ 'N4549', ageWPP == '50-54' ~ 'N5054', ageWPP == '55-59' ~ 'N5559', ageWPP == '60-64' ~ 'N6064', ageWPP == '65-69' ~ 'N6569', 
                            ageWPP == '70-74' ~ 'N7074', ageWPP == '75-79' ~ 'N7579', ageWPP == '80+' ~ 'N8000')) %>%
  pivot_wider(id_cols = c("iso3", "year"), names_from = ageWPP, values_from = pop)

# Load data
mtbs <- as.data.table(import(here("data", "mtb", paste0("mMtb_rev_mix_pop_sc", scenario, "_sct.Rdata") )))

MTB <- mtbs %>% 
  mutate(year = time + 1961) %>% 
  left_join(POP, by = c("iso3", "year")) %>% 
  mutate(across(matches("^S\\d{4}$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5)))),
         across(matches("^I\\d{4}[a-z]?$"), ~ . * get(paste0("N", substr(cur_column(), 2, 5))))) %>% 
  mutate(reg = 'EUR')
rm(mtbs)

# Country-level estimates ====
MTBiso <- MTB %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-d]?$")))) %>% 
  mutate(rIt = rowSums(across(matches("^I\\d{4}[a-b]$")))) %>%
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  select(iso3, year, reg, St, It, rIt, Nt) %>% 
  mutate(pI = It / Nt, prI = rIt / Nt, rec = prI / pI) %>% 
  pivot_longer(cols = -c(iso3, year, reg), names_to = "var", values_to = "values") %>%
  group_by(iso3, reg, year, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))
export(MTBiso, here("outputs", paste0("sct_", scenario), "MTBiso.Rdata"))
rm(MTBiso)

MTBiso_agepct <- MTB %>% 
  select(-starts_with('S')) %>% 
  group_by(iso3, reg, year) %>% 
  summarise(across(-all_of(c('time')), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(pI0004 = rowSums(select(., matches("^I0004[az]?") & where(is.numeric)), na.rm = TRUE) / N0004,
         pI0509 = rowSums(select(., matches("^I0509[az]?") & where(is.numeric)), na.rm = TRUE) / N0509,
         pI1014 = rowSums(select(., matches("^I1014[az]?") & where(is.numeric)), na.rm = TRUE) / N1014,
         pI1519 = rowSums(select(., matches("^I1519[az]?") & where(is.numeric)), na.rm = TRUE) / N1519,
         pI2024 = rowSums(select(., matches("^I2024[az]?") & where(is.numeric)), na.rm = TRUE) / N2024,
         pI2529 = rowSums(select(., matches("^I2529[az]?") & where(is.numeric)), na.rm = TRUE) / N2529,
         pI3034 = rowSums(select(., matches("^I3034[az]?") & where(is.numeric)), na.rm = TRUE) / N3034,
         pI3539 = rowSums(select(., matches("^I3539[az]?") & where(is.numeric)), na.rm = TRUE) / N3539,
         pI4044 = rowSums(select(., matches("^I4044[az]?") & where(is.numeric)), na.rm = TRUE) / N4044,
         pI4549 = rowSums(select(., matches("^I4549[az]?") & where(is.numeric)), na.rm = TRUE) / N4549,
         pI5054 = rowSums(select(., matches("^I5054[az]?") & where(is.numeric)), na.rm = TRUE) / N5054,
         pI5559 = rowSums(select(., matches("^I5559[az]?") & where(is.numeric)), na.rm = TRUE) / N5559,
         pI6064 = rowSums(select(., matches("^I6064[az]?") & where(is.numeric)), na.rm = TRUE) / N6064,
         pI6569 = rowSums(select(., matches("^I6569[az]?") & where(is.numeric)), na.rm = TRUE) / N6569,
         pI7074 = rowSums(select(., matches("^I7074[az]?") & where(is.numeric)), na.rm = TRUE) / N7074,
         pI7579 = rowSums(select(., matches("^I7579[az]?") & where(is.numeric)), na.rm = TRUE) / N7579,
         pI8000 = rowSums(select(., matches("^I8000[az]?") & where(is.numeric)), na.rm = TRUE) / N8000) %>% 
  mutate(prI0004 = rowSums(select(., matches("^I0004[ab]$") & where(is.numeric)), na.rm = TRUE) / N0004,
         prI0509 = rowSums(select(., matches("^I0509[ab]$") & where(is.numeric)), na.rm = TRUE) / N0509,
         prI1014 = rowSums(select(., matches("^I1014[ab]$") & where(is.numeric)), na.rm = TRUE) / N1014,
         prI1519 = rowSums(select(., matches("^I1519[ab]$") & where(is.numeric)), na.rm = TRUE) / N1519,
         prI2024 = rowSums(select(., matches("^I2024[ab]$") & where(is.numeric)), na.rm = TRUE) / N2024,
         prI2529 = rowSums(select(., matches("^I2529[ab]$") & where(is.numeric)), na.rm = TRUE) / N2529,
         prI3034 = rowSums(select(., matches("^I3034[ab]$") & where(is.numeric)), na.rm = TRUE) / N3034,
         prI3539 = rowSums(select(., matches("^I3539[ab]$") & where(is.numeric)), na.rm = TRUE) / N3539,
         prI4044 = rowSums(select(., matches("^I4044[ab]$") & where(is.numeric)), na.rm = TRUE) / N4044,
         prI4549 = rowSums(select(., matches("^I4549[ab]$") & where(is.numeric)), na.rm = TRUE) / N4549,
         prI5054 = rowSums(select(., matches("^I5054[ab]$") & where(is.numeric)), na.rm = TRUE) / N5054,
         prI5559 = rowSums(select(., matches("^I5559[ab]$") & where(is.numeric)), na.rm = TRUE) / N5559,
         prI6064 = rowSums(select(., matches("^I6064[ab]$") & where(is.numeric)), na.rm = TRUE) / N6064,
         prI6569 = rowSums(select(., matches("^I6569[ab]$") & where(is.numeric)), na.rm = TRUE) / N6569,
         prI7074 = rowSums(select(., matches("^I7074[ab]$") & where(is.numeric)), na.rm = TRUE) / N7074,
         prI7579 = rowSums(select(., matches("^I7579[ab]$") & where(is.numeric)), na.rm = TRUE) / N7579,
         prI8000 = rowSums(select(., matches("^I8000[ab]$") & where(is.numeric)), na.rm = TRUE) / N8000) %>% 
  select(iso3, reg, year, starts_with('pI'), starts_with('prI')) %>% 
  mutate(rec0004 = prI0004 / pI0004, rec0509 = prI0509 / pI0509, rec1014 = prI1014 / pI1014,
         rec1519 = prI1519 / pI1519, rec2024 = prI2024 / pI2024, rec2529 = prI2529 / pI2529,
         rec3034 = prI3034 / pI3034, rec3539 = prI3539 / pI3539, rec4044 = prI4044 / pI4044,
         rec4549 = prI4549 / pI4549, rec5054 = prI5054 / pI5054, rec5559 = prI5559 / pI5559,
         rec6064 = prI6064 / pI6064, rec6569 = prI6569 / pI6569, rec7074 = prI7074 / pI7074,
         rec7579 = prI7579 / pI7579, rec8000 = prI8000 / pI8000) %>% 
  pivot_longer(cols = -c(iso3, reg, year), names_to = c("var", "agegp"), 
               names_pattern = "^(pI|prI|rec)(\\d{4})$", values_to = "values") %>% 
  mutate(agegp = str_replace(agegp, "(\\d{2})(\\d{2})", "\\1-\\2")) %>%
  group_by(iso3, reg, year, agegp, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))
export(MTBiso_agepct, here("outputs", paste0("sct_", scenario), "MTBiso_agepct.Rdata"))
rm(MTBiso_agepct)

MTBiso_agenum <- MTB %>% 
  select(-starts_with('N')) %>% 
  group_by(iso3, reg, year) %>% 
  summarise(across(-all_of(c('time')), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
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
  mutate(tI0004 = dI0004 + rI0004, tI0509 = dI0509 + rI0509, tI1014 = dI1014 + rI1014,
         tI1519 = dI1519 + rI1519, tI2024 = dI2024 + rI2024, tI2529 = dI2529 + rI2529,
         tI3034 = dI3034 + rI3034, tI3539 = dI3539 + rI3539, tI4044 = dI4044 + rI4044,
         tI4549 = dI4549 + rI4549, tI5054 = dI5054 + rI5054, tI5559 = dI5559 + rI5559,
         tI6064 = dI6064 + rI6064, tI6569 = dI6569 + rI6569, tI7074 = dI7074 + rI7074,
         tI7579 = dI7579 + rI7579, tI8000 = dI8000 + rI8000) %>% 
  select(iso3, reg, year, starts_with('tI'), starts_with('dI'), starts_with('rI'), starts_with('S')) %>% 
  pivot_longer(cols = -c(iso3, reg, year), names_to = c("var", "agegp"),
               names_pattern = "^(rI|dI|tI|S)(\\d{4})$", values_to = "values") %>% 
  mutate(agegp = str_replace(agegp, "(\\d{2})(\\d{2})", "\\1-\\2"),
         var = case_when(str_detect(var, "^dI") ~ "dI", str_detect(var, "^rI") ~ "rI",
                         str_detect(var, "^tI") ~ "tI", str_detect(var, "^S") ~ "S", TRUE ~ as.character(var))) %>% 
  mutate(var = factor(var, levels = c('S', 'rI', 'dI', 'tI'))) %>%
  group_by(iso3, reg, year, agegp, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))
export(MTBiso_agenum, here("outputs", paste0("sct_", scenario), "MTBiso_agenum.Rdata"))
rm(MTBiso_agenum)

MTBiso_kidpct <- MTB %>% 
  select(-starts_with('S')) %>% 
  group_by(iso3, reg, year) %>% 
  summarise(across(-all_of(c('time')), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(tI0004 = rowSums(select(., matches("^I0004[az]?") & where(is.numeric)), na.rm = TRUE),
         tI0509 = rowSums(select(., matches("^I0509[az]?") & where(is.numeric)), na.rm = TRUE),
         tI1014 = rowSums(select(., matches("^I1014[az]?") & where(is.numeric)), na.rm = TRUE),
         tI1519 = rowSums(select(., matches("^I1519[az]?") & where(is.numeric)), na.rm = TRUE),
         tI2024 = rowSums(select(., matches("^I2024[az]?") & where(is.numeric)), na.rm = TRUE),
         tI2529 = rowSums(select(., matches("^I2529[az]?") & where(is.numeric)), na.rm = TRUE),
         tI3034 = rowSums(select(., matches("^I3034[az]?") & where(is.numeric)), na.rm = TRUE),
         tI3539 = rowSums(select(., matches("^I3539[az]?") & where(is.numeric)), na.rm = TRUE),
         tI4044 = rowSums(select(., matches("^I4044[az]?") & where(is.numeric)), na.rm = TRUE),
         tI4549 = rowSums(select(., matches("^I4549[az]?") & where(is.numeric)), na.rm = TRUE),
         tI5054 = rowSums(select(., matches("^I5054[az]?") & where(is.numeric)), na.rm = TRUE),
         tI5559 = rowSums(select(., matches("^I5559[az]?") & where(is.numeric)), na.rm = TRUE),
         tI6064 = rowSums(select(., matches("^I6064[az]?") & where(is.numeric)), na.rm = TRUE),
         tI6569 = rowSums(select(., matches("^I6569[az]?") & where(is.numeric)), na.rm = TRUE),
         tI7074 = rowSums(select(., matches("^I7074[az]?") & where(is.numeric)), na.rm = TRUE),
         tI7579 = rowSums(select(., matches("^I7579[az]?") & where(is.numeric)), na.rm = TRUE),
         tI8000 = rowSums(select(., matches("^I8000[az]?") & where(is.numeric)), na.rm = TRUE)) %>% 
  mutate(rI0004 = rowSums(select(., matches("^I0004[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI0509 = rowSums(select(., matches("^I0509[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI1014 = rowSums(select(., matches("^I1014[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI1519 = rowSums(select(., matches("^I1519[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI2024 = rowSums(select(., matches("^I2024[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI2529 = rowSums(select(., matches("^I2529[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI3034 = rowSums(select(., matches("^I3034[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI3539 = rowSums(select(., matches("^I3539[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI4044 = rowSums(select(., matches("^I4044[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI4549 = rowSums(select(., matches("^I4549[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI5054 = rowSums(select(., matches("^I5054[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI5559 = rowSums(select(., matches("^I5559[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI6064 = rowSums(select(., matches("^I6064[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI6569 = rowSums(select(., matches("^I6569[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI7074 = rowSums(select(., matches("^I7074[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI7579 = rowSums(select(., matches("^I7579[ab]$") & where(is.numeric)), na.rm = TRUE),
         rI8000 = rowSums(select(., matches("^I8000[ab]$") & where(is.numeric)), na.rm = TRUE)) %>% 
  select(iso3, reg, year, starts_with('tI'), starts_with('rI'), starts_with('N')) %>% 
  mutate(tI0014 = tI0004 + tI0509 + tI1014, tI1500 = tI1519 + tI2024 + tI2529 + tI3034 + tI3539 + tI4044 + tI4549 + tI5054 + tI5559 + tI6064 + tI6569 + tI7074 + tI7579 + tI8000,
         rI0014 = rI0004 + rI0509 + rI1014, rI1500 = rI1519 + rI2024 + rI2529 + rI3034 + rI3539 + rI4044 + rI4549 + rI5054 + rI5559 + rI6064 + rI6569 + rI7074 + rI7579 + rI8000,
         N0014 = N0004 + N0509 + N1014, N1500 = N1519 + N2024 + N2529 + N3034 + N3539 + N4044 + N4549 + N5054 + N5559 + N6064 + N6569 + N7074 + N7579 + N8000) %>% 
  select(iso3, reg, year, tI0014, tI1500, rI0014, rI1500, N0014, N1500) %>% 
  mutate(pI0014 = tI0014 / (N0014 + N1500), pI1500 = tI1500 / (N0014 + N1500),
         prI0014 = rI0014 / (N0014 + N1500), prI1500 = rI1500 / (N0014 + N1500),
         rec0014 = prI0014 / pI0014, rec1500 = prI1500 / pI1500) %>% 
  select(iso3, reg, year, starts_with('pI'), starts_with('prI'), starts_with('rec')) %>% 
  mutate(ppI0014 = pI0014 / (pI0014 + pI1500), ppI1500 = pI1500 / (pI0014 + pI1500),
         pprI0014 = prI0014 / (prI0014 + prI1500), pprI1500 = prI1500 / (prI0014 + prI1500)) %>% 
  pivot_longer(cols = -c(iso3, reg, year), names_to = c("var", "agegp"),
               names_pattern = "^(pI|prI|rec|ppI|pprI)(\\d{4})$", values_to = "values") %>% 
  mutate(agegp = str_replace(agegp, "(\\d{2})(\\d{2})", "\\1-\\2")) %>% 
  group_by(iso3, reg, year, agegp, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))
export(MTBiso_kidpct, here("outputs", paste0("sct_", scenario), "MTBiso_kidpct.Rdata"))
rm(MTBiso_kidpct)

MTBiso_kidnum <- MTB %>% 
  select(-starts_with('N')) %>% 
  group_by(iso3, reg, year) %>% 
  summarise(across(-all_of(c('time')), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
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
  mutate(tI0004 = dI0004 + rI0004, tI0509 = dI0509 + rI0509, tI1014 = dI1014 + rI1014,
         tI1519 = dI1519 + rI1519, tI2024 = dI2024 + rI2024, tI2529 = dI2529 + rI2529,
         tI3034 = dI3034 + rI3034, tI3539 = dI3539 + rI3539, tI4044 = dI4044 + rI4044,
         tI4549 = dI4549 + rI4549, tI5054 = dI5054 + rI5054, tI5559 = dI5559 + rI5559,
         tI6064 = dI6064 + rI6064, tI6569 = dI6569 + rI6569, tI7074 = dI7074 + rI7074,
         tI7579 = dI7579 + rI7579, tI8000 = dI8000 + rI8000) %>% 
  select(iso3, reg, year, starts_with('tI'), starts_with('dI'), starts_with('rI'), starts_with('S')) %>% 
  mutate(tI0014 = tI0004 + tI0509 + tI1014, tI1500 = tI1519 + tI2024 + tI2529 + tI3034 + tI3539 + tI4044 + tI4549 + tI5054 + tI5559 + tI6064 + tI6569 + tI7074 + tI7579 + tI8000,
         dI0014 = dI0004 + dI0509 + dI1014, dI1500 = dI1519 + dI2024 + dI2529 + dI3034 + dI3539 + dI4044 + dI4549 + dI5054 + dI5559 + dI6064 + dI6569 + dI7074 + dI7579 + dI8000,
         rI0014 = rI0004 + rI0509 + rI1014, rI1500 = rI1519 + rI2024 + rI2529 + rI3034 + rI3539 + rI4044 + rI4549 + rI5054 + rI5559 + rI6064 + rI6569 + rI7074 + rI7579 + rI8000,
         S0014 = S0004 + S0509 + S1014, S1500 = S1519 + S2024 + S2529 + S3034 + S3539 + S4044 + S4549 + S5054 + S5559 + S6064 + S6569 + S7074 + S7579 + S8000) %>% 
  select(iso3, reg, year, tI0014, tI1500, dI0014, dI1500, rI0014, rI1500, S0014, S1500) %>% 
  pivot_longer(cols = -c(iso3, reg, year), names_to = c("var", "agegp"),
               names_pattern = "^(rI|dI|tI|S)(\\d{4})$", values_to = "values") %>% 
  mutate(agegp = str_replace(agegp, "(\\d{2})(\\d{2})", "\\1-\\2"),
         var = case_when(str_detect(var, "^dI") ~ "dI", str_detect(var, "^rI") ~ "rI",
                         str_detect(var, "^tI") ~ "tI", str_detect(var, "^S") ~ "S", TRUE ~ as.character(var))) %>% 
  mutate(var = factor(var, levels = c('S', 'rI', 'dI', 'tI'))) %>%
  group_by(iso3, reg, year, agegp, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))
export(MTBiso_kidnum, here("outputs", paste0("sct_", scenario), "MTBiso_kidnum.Rdata"))
rm(MTBiso_kidnum)
