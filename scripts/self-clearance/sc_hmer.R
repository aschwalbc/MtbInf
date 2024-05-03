## Analysis code for Schwalb et al. 2024
## Distributed under CC BY 4.0
## RScript: SC_hmer.R

# Packages ==========
library(rio)
library(here)
library(tidyverse)
library(hmer)
library(lhs)
library(deSolve)
library(reshape2)
library(progress)

# 1. Parameters ==========
# 1.1 Baseline parameters
parms = c(
  gamma_a = 1.26, # Self-clearance Year 1
  gamma_b = 1.26, # Self-clearance Year 2
  gamma_c = 0.50) # Self-clearance Year 3-9

# 1.2 Parameter ranges
ranges = list(
  gamma_a = c(0,2), # Self-clearance Year 1
  gamma_b = c(0,2), # Self-clearance Year 2
  gamma_c = c(0,2)) # Self-clearance Year 3-9

# 2. Model ==========
sc_model <- function(parms, end_time = 10) {
  
  des <- function(time, state, parms) {
    I <- state["I"]
    S <- state["S"]
    
    # Determine gamma based on the current time
    if (time >= 0 && time < 1) {
      gamma <- parms["gamma_a"]
    } else if (time >= 1 && time < 2) {
      gamma <- parms["gamma_b"]
    } else {
      gamma <- parms["gamma_c"]
    } 
    
    dI <- -gamma * I
    dS <- gamma * I
    
    return(list(c(dI, dS), pSC = S/1e5))
  }
  
  state <- c(I = 1e5, S = 0)
  times <- seq(from = 0, to = end_time, by = 1)
  out <- deSolve::ode(y = state, times = times, func = des, parms = parms)
  
  df_out <- as.data.frame(out)
  
  if("pSC.S" %in% names(df_out)) {
    names(df_out)[names(df_out) == 'pSC.S'] <- 'pSC'
  }
  
  return(df_out)
}

head(as.data.frame(sc_model(parms))) # Test

# 3. HMER ==========
# 3.1 HMER-format results
hmer_res <- function(params, times, outputs) {
  t_max = max(times) # Define max times
  all_res = sc_model(params, end_time = t_max) # Run model until max times 
  actual_res = all_res[all_res[,'time'] %in% times, c('time', 'pSC')] # Subset desired time and outputs
  shaped = reshape2::melt(actual_res[,'pSC']) # Switch to long format
  return(setNames(shaped$value, paste0('pSC', actual_res[,'time'], sep = ""))) # Set for HMER-format
}

# 3.2 Fitting targets
targets <- list(
  pSC1 = c(0.801, 0.817),
  pSC2 = c(0.914, 0.925),
  pSC10 = c(0.969, 0.975))

targetsdb <- as.data.frame(t(as.data.frame(targets))) # Create dataframe with fitting targets
targetsdb$var <- substr(rownames(targetsdb),1,3) # Create variable classification
targetsdb$time <- as.numeric(substr(rownames(targetsdb),4,7)) # Create time variable
colnames(targetsdb) <- c("lo","hi","var","time") # Rename columns

# 3.3 Extract fits function
target_subset <- function(wave = NULL, sens = 0, targets = targetsdb, parms = parms){
  # Prepare dataframes
  wave <- as.data.frame(wave)
  target <- rownames(targets)
  listdf <- list()
  
  # Perform target range check
  for (i in 1:length(target)){
    targ <- target[i]
    targval <- wave[, grep(colnames(wave), pattern = targ)]
    targlim <- targets[grep(rownames(targets), pattern = targ), c('lo','hi')]
    check <- targval >= targlim$lo & targval <= targlim$hi
    df <- data.frame("check" = check)
    colnames(df) <- targ
    listdf[[i]] <- df
  }
  targetchecks <- as.data.frame(do.call("cbind",listdf))
  targetchecks <- as.data.frame(rowSums(targetchecks))
  colnames(targetchecks)[length(targetchecks)] <- "check"
  wave_check <- cbind(wave,targetchecks)
  wave_check <- wave_check[wave_check$check >= (length(target)-sens),]
  wave_check <- wave_check[,1:length(wave_check)-1]
  return(wave_check)
}

# 3.3 HMER initial run
wave_res <- list() # Empty list for wave results
wave <- list() # Empty list for data used to train and validate per wave
simulator <- list() # Empty list for simulator plots
wave_check <- list() # Empty list for wave checks
wave_train <- list() # Empty list for wave training
wave_val <- list() # Empty list for wave validation
ems <- list() # Empty list for emulators per wave
activeparms <- list() # Empty list for active parameters plots
invalid_pts <- list() # Empty list for invalid parameter sets
invalid_diag <- list() # Empty list for invalid parameter sets after diagnostics
invalid_bad <- list() # Empty list for invalid parameter sets after diagnostics and removal of bad emulators
non_imp_pts <- list() # Empty list for non-implausible points generated per wave

ini_LHS_train <- lhs::maximinLHS(10*length(ranges), length(ranges))
ini_LHS_val <- lhs::maximinLHS(10*length(ranges), length(ranges))
ini_LHS <- rbind(ini_LHS_train, ini_LHS_val)
ini_pts <- setNames(data.frame(t(apply(ini_LHS, 1, function(x) x*unlist(lapply(ranges, function(x) x[2]-x[1])) + unlist(lapply(ranges, function(x) x[1]))))), names(ranges)) # Set random sets to create points
rm(ini_LHS_train, ini_LHS_val, ini_LHS) # Clean objects

pb <- progress_bar$new(format = "[:bar] :percent :eta", total = nrow(ini_pts))
tmp <- list()
for (i in seq_len(nrow(ini_pts))) {
  res <- t(apply(ini_pts[i,], 1, hmer_res, c(1, 2, 10), 'pSC'))
  tmp[[i]] <- data.frame(res)[, names(targets)]
  pb$tick()  # Advance the progress bar
}
wave_res[[1]] <- do.call(rbind, tmp)
wave[[1]] <- cbind(ini_pts, wave_res[[1]]) # Bind run points and results
rm(pb, tmp, res, i) # Clean objects
simulator[[1]] <- simulator_plot(wave_res, targets, normalize = TRUE, byhit = TRUE)
print(simulator[[1]])
wave_check[[1]] <- target_subset(wave[[1]])

sample <- sample(1:nrow(wave[[1]]), round(length(wave[[1]][,1])/2))
wave_train[[1]] <- wave[[1]][sample,] 
wave_val[[1]] <- wave[[1]][-sample,]
rm(sample)
ems[[1]] <- emulator_from_data(wave_train[[1]], names(targets), ranges)
activeparms[[1]] <- plot_actives(ems[[1]])

invalid_pts[[1]] <- validation_diagnostics(ems[[1]], validation = wave_val[[1]], targets = targets, plt = FALSE)

for (j in 1:length(ems[[1]])) {
  misclass <- nrow(classification_diag(ems[[1]][[j]], targets, wave_val[[1]], plt = FALSE))
  while(misclass > 0) {
    ems[[1]][[j]] <- ems[[1]][[j]]$mult_sigma(1.1)
    misclass <- nrow(classification_diag(ems[[1]][[j]], targets, wave_val[[1]], plt = FALSE))
  }
}

invalid_diag[[1]] <- validation_diagnostics(ems[[1]], validation = wave_val[[1]], targets = targets, plt = FALSE)

bad.ems <- c()
for (j in 1:length(ems[[1]])) {
  bad.model <- nrow(comparison_diag(ems[[1]][[j]], targets, wave_val[[1]], plt = FALSE))
  if (bad.model > floor(nrow(wave_val[[1]])/10)) {
    bad.ems <- c(bad.ems, j)
  }
}
ems[[1]] <- ems[[1]][!seq_along(ems[[1]]) %in% bad.ems]

invalid_bad[[1]] <- validation_diagnostics(ems[[1]], validation = wave_val[[1]], targets = targets, plt = FALSE)

non_imp_pts[[1]] <- generate_new_design(ems[[1]], (10*length(ranges))*2, targets, verbose = TRUE) # Generate new points

# 3.4 HMER loop runs
for(i in 1001:2000) {
  w <- i # Update wave run
  
  pb <- progress_bar$new(format = "[:bar] :percent :eta", total = nrow(non_imp_pts[[w-1]]))
  tmp <- list()
  for (i in seq_len(nrow(non_imp_pts[[w-1]]))) {
    res <- t(apply(non_imp_pts[[w-1]][i,], 1, hmer_res, c(1, 2, 10), 'pSC'))
    tmp[[i]] <- data.frame(res)[, names(targets)]
    pb$tick()  # Advance the progress bar
  }
  wave_res[[w]] <- do.call(rbind, tmp)
  wave[[w]] <- cbind(non_imp_pts[[w-1]], wave_res[[w]])
  rm(pb, tmp, res, i) # Clean objects
  simulator[[w]] <- simulator_plot(wave_res[w], targets, normalize = TRUE, byhit = TRUE)
  print(simulator[[w]])
  wave_check[[w]] <- target_subset(wave[[w]])
  
  sample <- sample(1:nrow(wave[[w]]), round(length(wave[[w]][,1])/2))
  wave_train[[w]] <- wave[[w]][sample,] 
  wave_val[[w]] <- wave[[w]][-sample,]
  rm(sample)
  ems[[w]] <- emulator_from_data(wave_train[[w]], names(targets), ranges, check.ranges = TRUE)
  activeparms[[w]] <- plot_actives(ems[[w]])

  invalid_pts[[w]] <- validation_diagnostics(ems[[w]], validation = wave_val[[w]], targets = targets, plt = FALSE)
  
  for (j in 1:length(ems[[w]])) {
    misclass <- nrow(classification_diag(ems[[w]][[j]], targets, wave_val[[w]], plt = FALSE))
    while(misclass > 0) {
      ems[[w]][[j]] <- ems[[w]][[j]]$mult_sigma(1.1)
      misclass <- nrow(classification_diag(ems[[w]][[j]], targets, wave_val[[w]], plt = FALSE))
    }
  }
  
  invalid_diag[[w]] <- validation_diagnostics(ems[[w]], validation = wave_val[[w]], targets = targets, plt = FALSE)
  
  bad.ems <- c()
  for (j in 1:length(ems[[w]])) {
    bad.model <- nrow(comparison_diag(ems[[w]][[j]], targets, wave_val[[w]], plt = FALSE))
    if (bad.model > floor(nrow(wave_val[[w]])/10)) {
      bad.ems <- c(bad.ems, j)
    }
  }
  ems[[w]] <- ems[[w]][!seq_along(ems[[w]]) %in% bad.ems]
  
  invalid_bad[[w]] <- validation_diagnostics(ems[[w]], validation = wave_val[[w]], targets = targets, plt = FALSE)
  
  non_imp_pts[[w]] <- generate_new_design(c(ems[1:w]), (10*length(ranges))*2, targets, verbose = TRUE) # Generate new points
}

# 4. Calibration points ==========
plaus_pts <- as.data.frame(do.call("rbind", wave_check))[1:length(parms)]
plaus_pts <- sample_n(as.data.frame(do.call("rbind", wave_check))[1:length(parms)], size = 200000)

# export(plaus_pts, here("scripts", "others", "parms_y20.Rdata"))
# export(plaus_pts, here("scripts", "others", "parms_y35.Rdata"))
export(plaus_pts, here("scripts", "others", "parms_y50.Rdata"))

quants <- c(0.025,0.5,0.975) # Set quantiles
parameters <- apply(plaus_pts, 2, quantile, probs = quants, na.rm = TRUE) # Set parameter quantiles
t_parameters <- data.table::transpose(as.data.frame(parameters)) # Transpose parameters
colnames(t_parameters) = rownames(parameters) # Set column names
rownames(t_parameters) = colnames(parameters) # Set row names
parameters <- t_parameters # Rename parameters
rm(t_parameters) # Clean objects

parameters$parameter <- c("gamma_a", "gamma_b", "gamma_c", "gamma_d")

parameters[,c(1,2,3)] <- round(parameters[,c(1,2,3)], 3) # Round to 3 decimal places
table <- data.frame(parameter = parameters$parameter, low  = parameters$`2.5%`, med = parameters$`50%`, hig = parameters$`97.5%`) # Output table

# export(table, here("scripts", "others", "sc_rates_y20.csv")) # Save data frame
# export(table, here("scripts", "others", "sc_rates_y35.csv")) # Save data frame
export(table, here("scripts", "others", "sc_rates_y50.csv")) # Save data frame

rm(list = ls())

# 5. SC run ==========
# 5.1 Model
sc_model <- function(parms, end_time = 50) {
  
  des <- function(time, state, parms) {
    I <- state["I"]
    S <- state["S"]
    
    # Determine gamma based on the current time
    if (time >= 0 && time < 1) {
      gamma <- parms[, 'gamma_a']
    } else if (time >= 1 && time < 2) {
      gamma <- parms[, 'gamma_b']
    } else if (time >= 2 && time < 9) {
      gamma <- parms[, 'gamma_c']
    } else {
      gamma <- parms[, 'gamma_d']
    }
    
    dI <- -gamma * I
    dS <- gamma * I
    
    return(list(c(dI, dS), pSC = S/1e5))
  }
  
  state <- c(I = 1e5, S = 0)
  times <- seq(from = 0, to = end_time, by = 1)
  out <- deSolve::ode(y = state, times = times, func = des, parms = parms,
                      method = 'lsoda', rtol = 1e-6, atol = 1e-6)
  
  df_out <- as.data.frame(out)
  
  if("pSC.S" %in% names(df_out)) {
    names(df_out)[names(df_out) == 'pSC.S'] <- 'pSC'
  }
  
  return(df_out)
}

# 5.2  Model loops
# fits <- import(here("scripts", "others", "parms_y20.Rdata"))
# fits <- import(here("scripts", "others", "parms_y35.Rdata"))
fits <- import(here("scripts", "others", "parms_y50.Rdata"))

loop <- list()

pb <- progress_bar$new(format = "[:bar] :percent :eta", total = nrow(fits))
for(i in 1:nrow(fits)) {
  parms <- fits[i,]
  
  out <- as.data.frame(sc_model(parms))
  loop[[i]] <- out
  pb$tick()
}

run <- do.call("rbind", loop) %>% 
  pivot_longer(cols = -time, names_to = "var", values_to = "values") %>% 
  group_by(time, var) %>% 
  summarise(val = median(values, na.rm = TRUE),
            lo = quantile(values, 0.025, na.rm = TRUE),
            hi = quantile(values, 0.975, na.rm = TRUE)) 

# export(run, here("scripts", "others", "runs_y20.Rdata"))
# export(run, here("scripts", "others", "runs_y35.Rdata"))
export(run, here("scripts", "others", "runs_y50.Rdata"))

rm(list = ls())

# 5.3 Plot
# run <- import(here("scripts", "others", "runs_y20.Rdata"))
# run <- import(here("scripts", "others", "runs_y35.Rdata"))
run <- import(here("scripts", "others", "runs_y50.Rdata"))

# tiff(here("scripts", "others", "cleared_y20.tiff"), width = 15, height = 3, units = 'in', res = 150)
# tiff(here("scripts", "others", "cleared_y35.tiff"), width = 15, height = 3, units = 'in', res = 150)
tiff(here("scripts", "others", "cleared_y50.tiff"), width = 15, height = 3, units = 'in', res = 150)
ggplot(filter(run, var == 'pSC')) +
  geom_errorbar(data = targetsdb, aes(x = time, ymin = lo, ymax = hi), colour = '#000000', width = 0.5) + 
  geom_line(aes(x = time, y = val), colour = '#CE2931') +
  geom_ribbon(aes(x= time, ymin = lo, ymax = hi), fill = '#CE2931', alpha = 0.2) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.005, 0.005)) +
  scale_x_continuous(expand = c(0.005, 0.005)) +
  coord_cartesian(ylim = c(0.75, 1)) +
  labs(x = 'Years', y = 'Proportion self-cleared/recovered') +
  theme_minimal()
dev.off()

