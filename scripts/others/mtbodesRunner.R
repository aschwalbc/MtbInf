## Analysis code for Schwalb et al. 2023
## Created by P Dodd
## Distributed under CC BY 4.0
## RScript: mtbodesRunner.R

# Packages ==========
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
library(ggplot2) # To build comparative plots
library(data.table) # Faster than data.frame
library(odin) # Solver for ordinary differential equations
library(tictoc) # Track running time

# 1. Load data ==========
ari <- as.data.table(import(here("data", "ari", "mARI_WHO_norev_nomix_pop.Rdata")))

# 2. Parameters ==========
# 2.1 Self-clearance rate
gamma_a <- 0.0 # Infection year Ja
gamma_b <- 0.0 # Infection year Jb
gamma_c <- 0.0 # Infection year Jc
gamma_d <- 0.0 # Infection year Jd

# 2.2 Infection year transition
kappa_ab <- 1 # Infection year Ja - Jb
kappa_bc <- 1 # Infection year Jb - Jc
kappa_cd <- 1 # Infection year Jc - Jd **NEEDS TO BE CHECKED**

# 2.3 Times
times <- seq(from = 0, to = 116, by = 0.1)
years <- times + 1934 # Start year: 1934
data_times <- seq(from = 0, to = 116, by = 1) 
data_years <- data_times + 1934 # Start year: 1934

# 2.4 ISOs
iso <- unique(ari$iso3)
iso <- 'JPN'

# 4. Model ==========
# 4.1 Compile model
fn <- here("scripts", "others", "mtbodes.R")
mod <- odin(fn)

# 4.2 Parameter object
parms <- list(time_data = data_years, 
              frac_data = as.matrix(dcast(data = ari[iso3 == iso & year %in% years, .(year, agegp, fpop)],
                                          year ~ agegp, value.var = 'fpop'))[,-1],
              theta_data = ari[iso3 == iso & year %in% years & agegp == '00-04', birthrate],
              lambda_data = as.matrix(dcast(data = ari[iso3 == iso & year %in% years, .(year, agegp, ari)],
                                            year ~ agegp, value.var = 'ari'))[,-1],
              gamma = c(gamma_a, gamma_b, gamma_c, gamma_d),
              kappa = c(kappa_ab, kappa_bc, kappa_cd, 0))

# 4.3 Generate model
md <- mod$new(user = parms)

# 4.4 Run model
out <- md$run(t = years)

# 5. Reformatting functions ==========
# 5.1 Extract age group or infection year (when "[]")
getAJ <- function(x) as.numeric(gsub("\\]", "", gsub("^.+\\[", "", x))) 

# 5.2 Extract age group (A) (when "[A,J]")
getAno <- function(x) {
  a <- str_extract(x, "\\[(.*?),") # Capture a sequence of characters from "[" to ","
  a <- gsub("\\[", "", a) # Removes the "["
  a <- gsub(",", "", a) # Removes the ","
  as.integer(a) # Convert to integer
}

# 5.3 Extract infection year (J) (when "[A,J]")
getJno <- function(x) {
  a <- str_extract(x, ",(.*?)\\]") # Capture a sequence of characters from "," to "]"
  a <- gsub("\\]", "", a) # Removes the "]"
  a <- gsub(",", "", a) # Removes the ","
  as.integer(a) # Convert to integer
}

# 6. Data curation ==========
# 6.1 Reshaping for analysis
mtb <- as.data.frame(out) # Set as data frame
mtb <- pivot_longer(mtb, cols = -t, names_to = "var", values_to = "val") # Pivot longer
mtb <- as.data.table(mtb) # Set as data table

mtb[,J:= getJno(var)] # Add infection year variable
mtb[,A:= getAno(var)] # Add age group variable
mtb[is.na(A), A:= getAJ(var)] # Add age group variable for susceptible

agz <- ari[,unique(agegp)] # Isolate age groups
mtb[,agegp:= agz[A]] # Add age groups
mtb <- merge(ari[iso3 == iso, .(t = year, pop, agegp)], mtb, by = c('t','agegp')) # Merge population

# 6.2 Evaluate per year
yr <- 2014 # Set focus year
mtbi <- mtb[t == yr & !is.na(J), .(prev = sum(val)), by = agegp] # Focus on infected
mtbi <- merge(ari[iso3 == iso & year == yr, .(pop, agegp)], mtbi, by = c('agegp')) # Add population
mtbi[, .(ltbi = 1e2 * weighted.mean(x = prev, w = pop))]

# 7. Streamlined function ==========
getLTBI <- function(iso, yr){
  
  # Parameter object
  parms <- list(time_data = data_years, 
                frac_data = as.matrix(dcast(data = ari[iso3 == iso & year %in% years, .(year, agegp, fpop)], year ~ agegp, value.var = 'fpop'))[,-1],
                theta_data = ari[iso3 == iso & year %in% years & agegp == '00-04', birthrate],
                lambda_data = as.matrix(dcast(data = ari[iso3 == iso & year %in% years, .(year, agegp, ari)], year ~ agegp, value.var = 'ari'))[,-1],
                gamma = c(gamma_a, gamma_b, gamma_c, gamma_d),
                kappa = c(kappa_ab,kappa_bc,kappa_cd,0))

  # Generate model
  md <- mod$new(user = parms)

  # Run model
  out <- md$run(t = years)

  mtb <- as.data.frame(out)
  mtb <- pivot_longer(mtb, cols = -t, names_to = "var", values_to = "val")
  mtb <- as.data.table(mtb)

  mtb[,J:= getJno(var)]
  mtb[,A:= getAno(var)]
  mtb[is.na(A), A:= getAJ(var)]

  agz <- ari[,unique(agegp)]
  mtb[,agegp:= agz[A]]
  mtb <- merge(ari[iso3 == iso, .(t = year, pop, agegp)], mtb, by = c('t','agegp'))

  # Calculate weighted mean
  mtbi <- mtb[t == yr & !is.na(J), .(prev = sum(val)), by = agegp]
  mtbi <- merge(ari[iso3 == iso & year == yr, .(pop, agegp)], mtbi, by = c('agegp'))
  mtbi[, .(prev, agegp)]
  # mtbi[, .(ltbi = 1e2 * weighted.mean(x = prev, w = pop))]
}

# Test for ISO3 and year
getLTBI('IND', 2014)

# Run through all ISO3
tic()
gmtb <- list()
for (iso in ari[,unique(iso3)]) {
  print(iso)
  gmtb[[iso]] <- data.table(iso3 = iso, ltbi = getLTBI(iso, 2014))
}
gmtb <- rbindlist(gmtb)
toc()

export(gmtb, here("data", "mtb", "odingmtb_s.Rdata"))
