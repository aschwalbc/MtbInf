## Analysis code for Schwalb et al. 2023
## Adapted from Houben & Dodd 2016
## Distributed under CC BY 4.0
## RScript 01: DataPrep.R

# Packages ==========
library(data.table) # Faster than data.frame, allows use of j operator (:=)
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
library(ggplot2) # To build comparative plots
library(scales) # Scale functions for visualisation
library(patchwork) # Plot composition

# 0. Reversion parameters ==========
# 0.1 Reversion underestimation
rev <- 3.5 # True ARI = 2-5x higher, 3.5 for average
revE <- 1.5 # Increasing variance estimates by 50%

# 1. WHO TB estimates ==========
# 1.1 Load data
WHOn <- as.data.table(import(here("data","sources","who","WHOest_2000-2021.csv"))) # 'New' WHO data (2000-2021)
WHOo <- as.data.table(import(here("data","sources","who","WHOest_1990-2014.csv"))) # 'Old' WHO data (1990-2014)

# 1.2 Data curation
WHOn <- WHOn[,c("iso3","year", # ISO 3-character country/territory code and year
              "e_inc_num","e_inc_num_lo","e_inc_num_hi", # Estimated number of incident cases (all forms)[high and low bound]
              "c_cdr","c_cdr_lo","c_cdr_hi", # Case detection rate/TB treatment coverage (all forms)[high and low bound]
              "e_tbhiv_prct","e_tbhiv_prct_lo","e_tbhiv_prct_hi")] # Estimated HIV in incident TB (percent)[high and low bound]

WHOo <- WHOo[,c("iso3","year", # ISO 3-character country/territory code and year
                "e_prev_100k","e_prev_100k_lo","e_prev_100k_hi", # Estimated number of prevalent cases (all forms)[high and low bound]
                "e_inc_num","e_inc_num_lo","e_inc_num_hi", # Estimated number of incident cases (all forms)[high and low bound]
                "c_cdr","c_cdr_lo","c_cdr_hi", # Case detection rate/TB treatment coverage (all forms)[high and low bound]
                "e_tbhiv_prct","e_tbhiv_prct_lo","e_tbhiv_prct_hi")] # Estimated HIV in incident TB (percent)[high and low bound]

# 1.3 Country mergers
# a. Serbia and Montenegro
tmp <- WHOn[iso3=="SCG"] # ISO-3 SCG: Serbia and Montenegro (1990-2004)
tmp$iso3 <- "SRB" # Change to ISO-3 SRB: Serbia
WHOn <- rbind(WHOn,tmp) # Add SRB (1990-2004) to WHO database
tmp$iso3 <- "MNE" # Change to ISO-3 MNE: Montenegro
WHOn <- rbind(WHOn,tmp) # Add MNE (1990-2004) to WHO database
WHOn <- WHOn[iso3!="SCG",] # Drop SCG (1990-2004)
rm(tmp) # Clean objects

tmp <- WHOo[iso3=="SCG"] # ISO-3 SCG: Serbia and Montenegro (1990-2004)
tmp$iso3 <- "SRB" # Change to ISO-3 SRB: Serbia
WHOo <- rbind(WHOo,tmp) # Add SRB (1990-2004) to WHO database
tmp$iso3 <- "MNE" # Change to ISO-3 MNE: Montenegro
WHOo <- rbind(WHOo,tmp) # Add MNE (1990-2004) to WHO database
WHOo <- WHOo[iso3!="SCG",] # Drop SCG (1990-2004)
rm(tmp) # Clean objects

# b. Sudan and South Sudan
tmp <- copy(WHOn[iso3=="SDN"]) # ISO-3 SDN: Sudan
tmp <- tmp[year<=2010] # Select years before split
tmp$iso3 <- "SSD" # Change to ISO-3 SRB: South Sudan
WHOn <- rbind(WHOn,tmp) # Add SSD (<=2010) to WHO database 
rm(tmp) # Clean objects

tmp <- copy(WHOo[iso3=="SDN"]) # ISO-3 SDN: Sudan
tmp <- tmp[year<=2010] # Select years before split
tmp$iso3 <- "SSD" # Change to ISO-3 SRB: South Sudan
WHOo <- rbind(WHOo,tmp) # Add SSD (<=2010) to WHO database 
rm(tmp) # Clean objects

# 1.4 Finalise dataframe
WHOn <- WHOn[order(iso3,year)] # Order per country and year
WHOn$iso3 <- factor(WHOn$iso3) # Re-factor country

WHOo <- WHOo[order(iso3,year)] # Order per country and year
WHOo$iso3 <- factor(WHOo$iso3) # Re-factor country

# 1.5 Data comparison
# a. WHO global incidence
globincWHOn <- WHOn %>% # Global incidence 'new' WHO
  select(year,e_inc_num) %>% # Select year and incidence
  group_by(year) %>% # Group by year
  summarise(e_inc_num = sum(e_inc_num)) %>% # Total incidence per year
  mutate(source = "WHO (2000-2021)") # Add source variable
write_csv(globincWHOn, here("data","global","Global TB incidence WHO (2000-2022).csv"), append = FALSE)
  
globincWHOo <- WHOo %>% # Global incidence 'old' WHO
  select(year,e_inc_num) %>% # Select year and incidence
  group_by(year) %>% # Group by year
  summarise(e_inc_num = sum(e_inc_num)) %>% # Total incidence per year
  mutate(source = "WHO (1990-2014)") # Add source variable
write_csv(globincWHOo, here("data","global","Global TB incidence WHO (1990-2014).csv"), append = FALSE)

globincWHO <- bind_rows(globincWHOn,globincWHOo) # Merge both WHO data frames
rm(globincWHOn, globincWHOo)

# b. WHO global prevalence
globprevWHOo <- WHOo %>% # Global incidence 'old' WHO
  select(year,e_prev_100k) %>% # Select year and prevalence rate
  group_by(year) %>% # Group by year
  summarise(e_prev_100k = sum(e_prev_100k)) %>% # Total prevalence rate per year
  mutate(source = "WHO (1990-2014)") # Add source variable
write_csv(globprevWHOo, here("data","global","Global TB prevalence WHO (1990-2014).csv"), append = FALSE)

# 2. IHME TB prevalence estimates ==========
# 2.1 Load data
years <- 1990:2019 # Temporary vector for range of years of IHME data
IHME <- list() # Empty list defined

for (i in 1:length(years)) { # For loop to import IHME data
  IHME[[i]] <- as.data.table(import(here("data","sources","ihme",paste0(years[i],".csv")))) 
}

IHME <- do.call("rbind", IHME) # Bind lists into one data table
rm(i,years) # Clean objects

# 2.2 Data curation
IHMEkey <- as.data.table(import(here("data","sources","ihme","IHMEkey.csv"))) # For matching ISO codes to countries 
IHME <- merge(IHME,IHMEkey,by = "location_name") # Merge for ISO codes
rm(IHMEkey) # Clean objects

globincIHME <- IHME %>% # Extract TB incidence data from IHME
  filter(measure_name == "Incidence" & metric_name == "Number" & cause_name == "Tuberculosis") %>%
  select(year,val) %>% # Select variables
  rename(e_inc_num = val) %>% # Rename
  group_by(year) %>% # Group by year
  summarise(e_inc_num = sum(e_inc_num)) %>% # Total incidence per year
  mutate(source = "IHME (1990-2019)") # Add source variable
write_csv(globincIHME, here("data","global","Global TB incidence IHME (1990-2019).csv"), append = FALSE)

globprevIHME <- IHME %>% # Extract TB prevalence data from IHME
  filter(measure_name == "Prevalence" & metric_name == "Rate") %>%
  filter(cause_id == 934 | cause_id == 946 | cause_id == 947) %>%
  select(year,val) %>% # Select variables
  rename(e_prev_100k = val) %>% # Rename
  group_by(year) %>% # Group by year
  summarise(e_prev_100k = sum(e_prev_100k)) %>% # Total prevalence per year
  mutate(source = "IHME (1990-2019)") # Add source variable
write_csv(globprevIHME, here("data","global","Global TB prevalence IHME (1990-2019).csv"), append = FALSE)

WHOo_mod <- WHOo %>% # Extract TB data from WHO (<2000)
  filter(year<2000) %>% # Filter estimates from <2000
  within(rm(e_prev_100k,e_prev_100k_lo,e_prev_100k_hi)) # Remove prevalence estimates

IHME <- IHME %>% # Extract TB prevalence data from IHME
  filter(measure_name == "Prevalence" & metric_name == "Rate") %>%
  filter(cause_id == 934 | cause_id == 946 | cause_id == 947) %>%
  select(iso3,year,val,lower,upper) %>% # Select variables
  group_by(iso3,year) %>% # Group by country and year
  summarise(val = sum(val), lower = sum(lower), upper = sum(upper)) %>% # Total prevalence per year
  rename(e_prev_100k = val, e_prev_100k_lo = lower, e_prev_100k_hi = upper) %>% # Rename
  full_join(WHOn) %>% # Merge with 'new' WHO data
  full_join(WHOo_mod, by = c("iso3","year")) %>% # Merge with modified 'old' WHO data
  arrange(iso3,year) %>% # Order by ISO3 and year
  as.data.frame() # Set as data frame to run

var <- c("e_inc_num","e_inc_num_lo","e_inc_num_hi",
         "c_cdr","c_cdr_lo","c_cdr_hi",
         "e_tbhiv_prct","e_tbhiv_prct_lo","e_tbhiv_prct_hi")

for(i in 1:length(var)) {
  varlist <- paste0(var[i],".[x|y]")
  index <- grep(pattern = varlist, x = colnames(IHME))
  na <- which(is.na(IHME[,index[1]]))
  IHME[na,index[1]] <- IHME[na,index[2]]
}
rm(i,index,na,varlist,WHOn,WHOo_mod)

IHME <- IHME %>%
  select(iso3,year,e_prev_100k,e_prev_100k_lo,e_prev_100k_hi,(paste0(var,".x"))) %>%
  arrange(iso3,year) %>% # Order by ISO3 and year
  as.data.table() # Set as data table

for (col in 1:ncol(IHME)){
  colnames(IHME)[col] <-  sub(".x","", colnames(IHME)[col])
}
rm(col,var)

IHMEna <- IHME[(IHME$year != 2020 & IHME$year != 2021) & is.na(IHME$e_prev_100k),] # Missing TB prevalence (excluding 2020-2021)
rmiso3 <- unique(IHMEna$iso3) # 14 countries: ABW AIA ANT CUW CYM HKG MAC MSR NCL PYF SXM TCA VGB WLF

IHME <- IHME %>%
  filter(!iso3 %in% rmiso3) %>% # Remove countries with missing prevalence data
  filter(!year %in% c(2020,2021)) # Remove 2020 and 2021 rows
rm(IHMEna,rmiso3)

write_csv(IHME, here("data","global","Global TB prevalence IHME per country (1990-2019).csv"), append = FALSE)

# 2.3 Data comparison
# a. TB incidence (WHO vs IHME)
globTBinc <- bind_rows(globincWHO,globincIHME) # Merge WHO and IHME data frames
rm(globincWHO, globincIHME) # Clean objects

globTBincplot <- ggplot() + # Incidence comparison between WHO and IHME sources
  geom_line(data=globTBinc, aes(x=year, y=e_inc_num, color = source), linewidth = 1) + 
  scale_x_continuous(name = "Year",breaks_extended(n=10)) +
  scale_y_continuous(name = "Total TB incidence", labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(title = "Global TB incidence by WHO and IHME") +
  theme(legend.position = "bottom")
jpeg(here("plots","checks","Incidence.jpeg"), height = 500, width = 700, quality = 100) # Produce comparative plot
print(globTBincplot)
dev.off()
rm(globTBinc,globTBincplot) # Clean objects

# b. TB prevalence (WHO vs IHME)
globTBprev <- bind_rows(globprevWHOo,globprevIHME) # Merge WHO and IHME data frames
rm(globprevWHOo,globprevIHME) # Clean objects

globTBprevplot <- ggplot() + # Prevalence comparison between WHO and IHME sources
  geom_line(data=globTBprev, aes(x=year, y=e_prev_100k, color = source), linewidth = 1) + 
  scale_x_continuous(name = "Year", breaks_extended(n=10)) +
  scale_y_continuous(name = "Total TB prevalence") +
  labs(title = "Global TB prevalence by WHO and IHME") +
  theme(legend.position = "bottom")
jpeg(here("plots","checks","Prevalence.jpeg"), height = 500, width = 700, quality = 100) # Produce comparative plot
print(globTBprevplot)
dev.off()
rm(globTBprev,globTBprevplot) # Clean objects

# c. TB prevalence (WHO vs Prevalence surveys)
TBPrevSurvey <- as.data.table(import(here("data","sources","surveys","TBPrevSurv.csv"))) # Nationally representative TB prevalence surveys
countries <- sort(unique(TBPrevSurvey$iso3)) # 32 countries

pdf(here("plots","checks","WHO - TB Prev Surveys.pdf"), height = 11, width = 7) # Produce comparative plot per country
for(i in 1:length(countries)){
  dbprev <- WHOo %>%
    filter(iso3 == countries[i]) # Filter WHOo by survey countries
  dbsurv <- TBPrevSurvey %>%
    filter(iso3 == countries[i]) # Filter Surveys by survey countries
  p <- ggplot() + # Create plots
    geom_line(data = dbprev, aes(x = year, y = e_prev_100k), colour = "#D95F02", size = 0.8) +
    geom_ribbon(data = dbprev, aes(x = year, ymin = e_prev_100k_lo, ymax = e_prev_100k_hi), fill="#D95F02", size = 1, alpha = 0.25) +
    #geom_point(data = dbsurv, aes(x = year, y = sm_prev_100k), shape=16, colour = "#7570B3") +
    #geom_errorbar(data = dbsurv, aes(x = year, ymin = sm_prev_100k_lo, ymax = sm_prev_100k_hi), size = 0.5, alpha = 0.8, width = 1, colour = "#7570B3") +
    geom_point(data = dbsurv, aes(x = year, y = bac_prev_100k), shape=16, colour = "#1B9E77") +
    geom_errorbar(data = dbsurv, aes(x = year, ymin = bac_prev_100k_lo, ymax = bac_prev_100k_hi), size = 0.5, alpha = 0.8, width = 1, colour = "#1B9E77") +
    scale_x_continuous(name = "Year", breaks_extended(n=10)) +
    scale_y_continuous(name = "Prevalence per 100,000") +
    labs(title = countries[i])
  print(p)
}
dev.off()
rm(WHOo)

# d. TB prevalence (IHME vs Prevalence surveys)
pdf(here("plots","checks","IHME - TB Prev Surveys.pdf"), height = 11, width = 7) # Produce comparative plot per country
for(i in 1:length(countries)){
  dbprev <- IHME %>%
    filter(iso3 == countries[i]) # Filter IHME by survey countries
  dbsurv <- TBPrevSurvey %>%
    filter(iso3 == countries[i]) # Filter Surveys by survey countries
  p <- ggplot() + # Create plots
    geom_line(data = dbprev, aes(x = year, y = e_prev_100k), colour = "#D95F02", size = 0.8) +
    geom_ribbon(data = dbprev, aes(x = year, ymin = e_prev_100k_lo, ymax = e_prev_100k_hi), fill="#D95F02", size = 1, alpha = 0.25) +
    #geom_point(data = dbsurv, aes(x = year, y = sm_prev_100k), shape=16, colour = "#7570B3") +
    #geom_errorbar(data = dbsurv, aes(x = year, ymin = sm_prev_100k_lo, ymax = sm_prev_100k_hi), size = 0.5, alpha = 0.8, width = 1, colour = "#7570B3") +
    geom_point(data = dbsurv, aes(x = year, y = bac_prev_100k), shape=16, colour = "#1B9E77") +
    geom_errorbar(data = dbsurv, aes(x = year, ymin = bac_prev_100k_lo, ymax = bac_prev_100k_hi), size = 0.5, alpha = 0.8, width = 1, colour = "#1B9E77") +
    scale_x_continuous(name = "Year", breaks_extended(n=10)) +
    scale_y_continuous(name = "Prevalence per 100,000") +
    labs(title = countries[i])
  print(p)
}
dev.off()
rm(dbprev,dbsurv,p,countries,i,TBPrevSurvey)

# 3. Cauthen et al. ARI estimates ==========
# 3.1 Load data
CAU <- as.data.table(import(here("data","sources","surveys","ARI_Cauthen.csv"))) # Cauthen et al. ARI estimates 

# 3.2 Data curation
CAU$ari <- 1e-2*CAU$ARI # Convert percentage to decimals

AGEstart <- unlist(lapply(X=as.character(CAU$AGE),FUN=function(x)regexpr('\\(',x)[[1]])) # Counts characters up to start of mean age
AGEend <- unlist(lapply(X=as.character(CAU$AGE),FUN=function(x)regexpr('\\)',x)[[1]])) # Counts characters up to end of mean age
CAU$age <- as.numeric(substr(as.character(CAU$AGE),start=AGEstart+1,stop=AGEend-1)) # Extracts mean age as numeric
rm(AGEstart, AGEend) # Clean objects

CAU$var <- (CAU$ari)/(CAU$age*CAU$N) # Quantify variance
CAU$E <- sqrt(CAU$var)/CAU$ari # Quantify measurement variance

CAUrev <- copy(CAU) # Cauthen et al. reversion-adjusted ARI estimates 
CAUrev$ari <- CAUrev$ari*rev # Adjust for reversion
CAUrev$E <- CAUrev$E*revE # Adjust variance

CAU$lari <- log(CAU$ari) # Computes ARI logarithms
CAUrev$lari <- log(CAUrev$ari) # Computes reversion-adjusted ARI logarithms

CAU$type <- "TST survey" # Adds type
CAUrev$type <- "TST survey" # Adds type

# 3.3 Finalise dataframe
CAU <- CAU[,c("iso3","year","ari","E","lari","type")] # Select variables
CAU <- na.omit(CAU) # Clear missing values
CAU <- CAU[order(iso3,year)] # Order per country and year
CAU$iso3 <- factor(CAU$iso3) # Re-factor country

CAUrev <- CAUrev[,c("iso3","year","ari","E","lari","type")] # Select variables
CAUrev <- na.omit(CAUrev) # Clear missing values
CAUrev <- CAUrev[order(iso3,year)] # Order per country and year
CAUrev$iso3 <- factor(CAUrev$iso3) # Re-factor country

# 4. Systematic review of ARI estimates ==========
# 4.1 Load data
REV <- as.data.table(import(here("data","sources","surveys","ARI_SystRev.csv"))) # ARI estimates from SR

# 4.2 Data curation
REV$ari <- 1e-2*REV$ARI # Convert percentage to decimals

AGEstart <- unlist(lapply(X=as.character(REV$AGE),FUN=function(x)regexpr('\\(',x)[[1]])) # Counts characters up to start of mean age
AGEend <- unlist(lapply(X=as.character(REV$AGE),FUN=function(x)regexpr('\\)',x)[[1]])) # Counts characters up to end of mean age
REV$age <- as.numeric(substr(as.character(REV$AGE),start=AGEstart+1,stop=AGEend-1)) # Extracts mean age as numeric
rm(AGEstart, AGEend) # Clean objects
REV <- filter(REV,!REV$iso3 == "HKG") # Filter out HKG (no country estimate in IHME)

REV$var <- (REV$ari)/(REV$age*REV$N) # Quantify variance
REV$var[!is.na(REV$ARI_hi)] <- (1.96e-2*(REV$ARI_hi-REV$ARI_lo)[!is.na(REV$ARI_hi)])^2 # Quantify variance
REV$E <- sqrt(REV$var)/REV$ari # Quantify measurement variance

REVrev <- copy(REV) # Cauthen et al. reversion-adjusted ARI estimates 
REVrev$ari <- REVrev$ari*rev # Adjust for reversion
REVrev$E <- REVrev$E*revE # Adjust variance

REV$lari <- log(REV$ari) # Computes ARI logarithms
REVrev$lari <- log(REVrev$ari) # Computes reversion-adjusted ARI logarithms

REV$type <- "TST survey" # Adds type
REVrev$type <- "TST survey" # Adds type

# 4.3 Finalise dataframe
REV <- REV[,c("iso3","year","ari","E","lari","type")] # Select variables
REV <- na.omit(REV) # Clear missing values
REV <- REV[order(iso3,year)] # Order per country and year
REV$iso3 <- factor(REV$iso3) # Re-factor country

REVrev <- REVrev[,c("iso3","year","ari","E","lari","type")] # Select variables
REVrev <- na.omit(REVrev) # Clear missing values
REVrev <- REVrev[order(iso3,year)] # Order per country and year
REVrev$iso3 <- factor(REVrev$iso3) # Re-factor country

# 4.4 Data exploration 
tmp <- rbind(REV,CAU) # 141 country-years
unique(tmp$iso3) # 43 unique countries
rm(tmp)

# 5. HIV in TB ==========
# 5.1 Data curation
HIV <- IHME[,list(iso3,year,
                  cdr = c_cdr/100, # Case detection rate/TB treatment coverage (percent)
                  vcdr = (c_cdr_hi-c_cdr_lo)^2/(100*3.92)^2, # Variance: Case detection rate/TB treatment coverage 
                  p = e_tbhiv_prct/100, # Estimated HIV in incident TB (percent)
                  vp = (e_tbhiv_prct_hi-e_tbhiv_prct_lo)^2/(100*3.92)^2)] # Variance: Estimated HIV in incident TB (percent)

HIV <- HIV[!is.na(p),] # Drop NA from HIV in incident TB percentage
HIV <- HIV[p>1e-2,] # Drop HIV in incident TB percentage below 1%

HIV[is.na(cdr),] # Check: 99 country-years without CDR information
noCDR <- unique(HIV[is.na(cdr),iso3]) # 39 unique countries without CDR information
for(tmp in as.character(noCDR)){ # For-loop to clean missing CDR
  HIV$cdr[HIV$iso3==tmp & is.na(HIV$cdr)] <- min(HIV$cdr[HIV$iso3==tmp & !is.na(HIV$cdr)]) # Use min CDR
  HIV$vcdr[HIV$iso3==tmp & is.na(HIV$vcdr)] <- max(HIV$vcdr[HIV$iso3==tmp & !is.na(HIV$vcdr)]) # Use max vCDR
}
HIV[is.na(cdr),] # Check: No country-years without CDR information
rm(tmp,noCDR) # Clean objects

HIV[cdr>1]$vcdr <- 0 # Sets null variance for CDR >100%
HIV[cdr>1]$cdr <- 1 # Sets maximum CDR as 100%

HIV[is.na(vp)]$vp <- 0 # Sets null variance for 100% HIV prevalence

HIV2 <- copy(HIV) # Copy for sensitivity analysis below

# 4.2 HIV influence on smear-positivity
# Duration of TB disease by HIV and notification status (T):
# T1n~U[0.2,2] = HIV negative, notified
# T1u~U[1,4] = HIV negative, un-notified
# T2n~U[0.01,1] = HIV positive, notified
# T2u~U[0.01,0.22] = HIV positive, un-notified
# T1 = CDR*T1n + (1-CDR)*T1u 
# T2 = CDR*T2n + (1-CDR)*T2u
# var(T) ~= (Tn-Tu)^2.var(CDR) + CDR^2.var(Tn) + (1-CDR)^2.var(Tu)
HIV[,T1:=cdr*(2+.2)/2 + (1-cdr)*(4+1)/2]
HIV[,T2:=cdr*(1+.01)/2 + (1-cdr)*(.2+.01)/2]
HIV[,vT1:=vcdr*.25*(2+.2-4-1)^2 + cdr^2*(2-.2)^2/12 + (1-cdr)^2*(4-1)^2/12]
HIV[,vT2:=vcdr*.25*(1+.01-.2-.01)^2 + cdr^2*(1-.01)^2/12 + (1-cdr)^2*(.2-.01)^2/12]

# Smear-positivity of TB in people living with HIV (Factor f):
# f~U[0.3,0.4]/U[0.4,0.5]
# Factor f (mean) = ((0.4+0.3)/2)/((0.5+0.4)/2)
mf <- 0.780
# Factor f (variance) = ((1/12)*(0.4-0.3)^2)/((1/12)*(0.5-0.4)^2)
vf <- 0.00666 

# Smear-positivity adjustment factor (S):
# p = WHO estimate of proportion of incident TB that is HIV-associated 
# S = (p*T2*f + (1-p)*T1) / (p*T2 + (1-p)*T1) = A / B
HIV[,A:=(p*T2*mf + (1-p)*T1)] # S formula - numerator
HIV[,B:=(p*T2 + (1-p)*T1)] # S formula - denominator
HIV[,S:= A/B] # S - Factor reduction

# Variance of logarithm S formula (vlS):
# var(p)*(p*(1-p)*(1-f)*T1 / (AB))^2 +
# (T1^2*var(T2) + T2^2*var(T1))*(p*(1-p)*(1-f) / (AB) )^2 +
# var(f)*(p*T2 / A)^2 
HIV[,vlS:= vp*(p*(1-p)*(1-mf)*T1/(A*B))^2 +
     (T1^2*vT2+T2^2*vT1)*(p*(1-p)*(1-mf)/(A*B))^2 +
     vf*(p*T2/A)^2]

# 4.3 Finalise dataframe
HIV3 <- copy(HIV) # Copy for sensitivity analysis below
HIV <- HIV[,list(iso3,year,vlS,S)] # Select variables
rm(mf,vf) # Clean objects

# 5. Childhood TB ==========
# 5.1 Load data
KID <- as.data.table(import(here("data","sources","others","TBinc_kids.csv"))) # 2014
pKID <- as.data.table(import(here("data","sources","others","TBinc_kids_prop.csv"))) # Dodd et al. Lancet GH 2014

# 5.2 Data curation
KID <- merge(IHME[year==2014,list(iso3,year,e_inc_num,e_inc_num_lo,e_inc_num_hi)],KID,by='iso3') # Merge with 2014 WHO incidence
KID[,pk:=inc.num/e_inc_num] # Proportion of TB incidence in kids
KID[,vpk:=pk^2*(inc.num.sd^2/inc.num^2 + ((e_inc_num_hi-e_inc_num_lo)/3.92)^2/e_inc_num^2)] # Variance: Proportion of TB incidence in kids
KID[pk>1,]$pk <- 0.1 # Correct extreme values
KID[vpk>1e-2,]$vpk <- 1e-2 # Correct extreme values
KID <- filter(KID,!is.nan(vpk)) # Remove countries with NaN

pKID[,pu5:=a/(a+b)] # Proportion of kids under 5 years of age
pKID[,vpu5:=(1-pu5)*pu5/(a+b+1)] # Proportion of kids under 5 years of age (Variance)
KID <- merge(KID[,list(iso3,pk,vpk)],pKID[,list(iso3,pu5,vpu5)],by='iso3') # Merge with 2014 WHO incidence for adult and children
rm(pKID) # Clean objects

# 5.3 Smear-positivity by age (Kunkel et al. BMC ID 2016)
# Children aged 0-4 (YK) = 0.5% (0.0 - 1.9)
# Children aged 5-14 (OK) = 14.0% (8.9 - 19.4)
# Adults (A) = 52.0% (40.0 - 64.0)
YK <- .5e-2
OK <- 14e-2
A <- 0.52
vYK <- (1.9*1e-2/3.92)^2
vOK <- ((19.4-8.9)*1e-2/3.92)^2
vA <- ((0.64-0.4)/3.92)^2
KID[,mFr:= pk*pu5*YK + pk*(1-pu5)*OK + (1-pk)*A] # Fraction smear-positivity (Mean)
KID[,vFr:= mFr^2*(vpk*(pu5*YK+(1-pu5)*OK-A)^2 + vpu5*(pk*YK-pk*OK)^2 +
                     vYK*(pk*pu5)^2 + vOK*(pk*(1-pu5))^2 + vA*(1-pk)^2)] # Fraction smear-positivity (Variance)

# 5.4 Finalise dataframe
KID <- merge(HIV[,list(iso3,year,S,vlS)],KID[,list(iso3,mFr,vFr)],by='iso3',all=TRUE) # Merge with HIV data
rm(HIV) # Clean objects
KID[is.na(year),]$year <- 2014 # Tidying data
rm(YK,vYK,OK,vOK,A,vA) # Clean objects

# 6. Styblo ratio ==========
#LogNormal(1.678, 0.371)
mu <- 1.678
sig <- 0.371
stb <- exp(mu + .5*sig^2)
vstb <- (exp(sig^2)-1)*exp(2*mu+sig^2)
rm(mu,sig) # Clean objects

# 7. Final dataframe preparation  ==========
GTB <- copy(IHME)
GTB <- merge(IHME,KID,by=c('iso3','year'),all = TRUE) # Merge data sets
rm(IHME,KID) # Clean objects

# Missing fraction smear-positivity (mFr)

# Missing from merge
for(tmp in miss[V1==FALSE,as.character(iso3)]){ 
  if(GTB[iso3==tmp,any(is.na(mFr))]){ 
    vlu <- mean(GTB[iso3==tmp,mFr],na.rm=TRUE) # Average mFr for country
    GTB[iso3==tmp,]$mFr <- vlu # Set average value for mFr
    vlu <- mean(GTB[iso3==tmp,vFr],na.rm=TRUE) # Average vFr for country
    GTB[iso3==tmp,]$vFr <- vlu # Set average value for vFr
  }
}
rm(tmp,vlu) # Clean objects

# Countries with no mFr data
miss <- GTB[,all(is.na(mFr)),by=iso3] # Countries with at least one missing mFr = FALSE

mmFr <- GTB[,median(mFr,na.rm=TRUE)] # Calculate overall median mFr
mvFr <- GTB[,median(vFr,na.rm=TRUE)] # Calculate overall median vFr

for(tmp in miss[V1==TRUE,as.character(iso3)]){
  GTB[iso3==tmp,]$mFr <- mmFr # Set overall median mFr
  GTB[iso3==tmp,]$vFr <- mvFr # Set overall median vFr
}
rm(tmp,miss,mmFr,mvFr) # Clean objects

# Missing HIV information
GTB[is.na(S),]$vlS <- 0 # Set vlS in countries without HIV
GTB[is.na(S),]$S <- 1 # Set S in countries without HIV

# 8. Calculations  ==========
GTBrev <- copy(GTB) # Reversion-adjusted ARI histories

GTB[,ari:= e_prev_100k * 1e-5 * mFr * S * stb] # ARI estimation with mFr, smear-positivity adjustment and Styblo factor
GTB[,lari:= log(ari)] # Computes ARI logarithms
GTB[,E1:= (e_prev_100k_hi-e_prev_100k_lo)/(3.92*e_prev_100k)] # SD over mean prevalence
GTB[,E:= sqrt(E1^2+vstb/stb^2 + vlS + vFr/mFr^2)] # Quantify measurement variance
GTB <- as.data.frame(GTB[,list(iso3,year,ari,E,lari)]) # Select variables
GTB$type <- 'Prevalence estimate' # Adds type

GTBrev[,ari:= e_prev_100k * 1e-5 * mFr * S * stb * rev] # ARI estimation with mFr, smear-positivity adjustment and Styblo factor
GTBrev[,lari:= log(ari)] # Computes ARI logarithms
GTBrev[,E1:= (e_prev_100k_hi-e_prev_100k_lo)/(3.92*e_prev_100k)] # SD over mean prevalence
GTBrev[,E:= sqrt(E1^2+vstb/stb^2 + vlS + vFr/mFr^2) * revE] # Quantify measurement variance
GTBrev <- as.data.frame(GTBrev[,list(iso3,year,ari,E,lari)]) # Select variables
GTBrev$type <- 'Prevalence estimate' # Adds type

rm(stb,vstb,rev,revE) # Clean objects

ARI <- rbind(GTB,CAU,REV) # Combine ARI from WHO estimates, Cauthen et al., and review
ARIrev <- rbind(GTBrev,CAUrev,REVrev) # Combine ARI from WHO estimates, Cauthen et al., and review
rm(GTB,GTBrev,CAU,CAUrev,REV,REVrev) # Clean objects
export(ARI,here("data","ari","ARI_IHME_norev.Rdata")) # Save data frame
export(ARIrev,here("data","ari","ARI_IHME_rev.Rdata")) # Save data frame

# 9. Sensitivity analysis ========== 
# HIV influence on smear-positivity
HIV2[,cdr2:=1.0] # 100% CDR in people living with HIV
HIV2[,T1:=cdr*(2+.2)/2+(1-cdr)*(4+1)/2]
HIV2[,T2:=cdr2*(1+.01)/2+(1-cdr2)*(.2+.01)/2]
HIV2[,vT1:=vcdr*.25*(2+.2-4-1)^2 + cdr^2*(2-.2)^2/12 + (1-cdr)^2*(4-1)^2/12]
HIV2[,vT2:=vcdr*.25*(1+.01-.2-.01)^2 + cdr2^2*(1-.01)^2/12 + (1-cdr2)^2*(.2-.01)^2/12]

# Smear-positivity of TB in people living with HIV (Factor f):
mf <- 0.780 # Factor f (mean)
vf <- 0.00666 # Factor f (variance)

# Smear-positivity adjustment factor (S):
HIV2[,A:=(p*T2*mf + (1-p)*T1)] # S formula - numerator
HIV2[,B:=(p*T2 + (1-p)*T1)] # S formula - denominator
HIV2[,S:= A/B] # S - Factor reduction

# Variance of logarithm S formula (vlS):
HIV2[,vlS:= vp*(p*(1-p)*(1-mf)*T1/(A*B))^2 +
      (T1^2*vT2+T2^2*vT1)*(p*(1-p)*(1-mf)/(A*B))^2 +
      vf*(p*T2/A)^2]

rm(mf, vf) # Clean objects

# Proportional change in S
summary(1e2*(1-HIV2$S/HIV3$S))
# 0.14% (IQR:0.04-0.55)
rm(HIV2,HIV3) # Clean objects