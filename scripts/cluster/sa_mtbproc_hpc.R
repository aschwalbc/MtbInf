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
scenario <- "sa_corr"
cat(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Running scenario:", scenario, "\n"))

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

# Global estimates ====
MTBglb <- MTB %>% 
  left_join(WHO, by = c('iso3')) %>% 
  mutate(St = rowSums(across(matches("^S\\d{4}$")))) %>% 
  mutate(It = rowSums(across(matches("^I\\d{4}[a-d]?$")))) %>% 
  mutate(rIt = rowSums(across(matches("^I\\d{4}[a-b]$")))) %>%
  mutate(Nt = rowSums(across(matches("^N\\d{4}$")))) %>% 
  select(iso3, year, reg, rep, St, It, rIt, Nt) %>% 
  group_by(year, rep) %>% 
  summarise(St = sum(St), It = sum(It), rIt = sum(rIt), Nt = sum(Nt)) %>% 
  mutate(pI = It / Nt, prI = rIt / Nt, rec = prI / pI) %>% 
  pivot_longer(cols = -c(year, rep), names_to = "var", values_to = "values") %>%
  group_by(year, var) %>%
  summarise(val = median(values, na.rm = TRUE), 
            lo = quantile(values, 0.025, na.rm = TRUE), 
            hi = quantile(values, 0.975, na.rm = TRUE))
export(MTBglb, here("outputs", paste0("mtb_", scenario), "MTBglb.Rdata"))
cat(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Completed: Global estimates\n"))
rm(MTBglb)
