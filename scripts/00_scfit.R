## Analysis code for Mtb Inf Burden
## Authors: P Dodd and A Schwalb
## RScript 00: SCfit.R

# Packages ==========
library(here)
library(rio)
library(progress)
library(tidyverse)
library(data.table)
library(rstan)
library(expm)

# 1. Calibration ====
# 1.1 Compile model
mod <- stan_model(here("data", "sc", "expm.stan"))

# 1.2 Calibration targets
# 1.2.1 Target midpoints:
m1 <- c(0.801 + 0.817)/2
m2 <- c(0.914 + 0.925)/2
m10 <- c(0.969 + 0.975)/2
m10plus <- c(0.985 + 0.995)/2

# 1.2.2 Target SDs
s1 <- - c(0.801 - 0.817)/3.92
s2 <- - c(0.914 - 0.925)/3.92
s10 <- - c(0.969 - 0.975)/3.92
s10plus <- - c(0.985 - 0.995)/3.92

# 1.2.3 Group data
D <- list(t1 = 1,t2 = 2,t3 = 10, t4 = 50, # CHANGE
          m1 = m1, m2 = m2, m3 = m10, m4 = m10plus,
          s1 = s1, s2 = s2, s3 = s10, s4 = s10plus)

# 1.3 Sampling
C <- sampling(mod, data = D, iter = 40000) # Adjust iterations to get n_eff > 10K
print(C) # Check
export(C, here("data", "sc", "mcmc_y20.rds"))
export(C, here("data", "sc", "mcmc_y50.rds"))

# 1.3.1 Extract parameters
S <- extract(C, pars = c("gamma_a", "gamma_b", "gamma_c", "gamma_d"))
fits <- as.data.table(do.call("cbind", S))
export(fits, here("data", "sc", "parms_y20.Rdata"))
export(fits, here("data", "sc", "parms_y50.Rdata"))

parms <- apply(fits, 2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
t_parms <- data.table::transpose(as.data.frame(parms))
colnames(t_parms) = rownames(parms)
rownames(t_parms) = colnames(parms)
parms <- t_parms 
parms$parms <- c("gamma_a", "gamma_b", "gamma_c", "gamma_d")
parms[,c(1,2,3)] <- round(parms[,c(1,2,3)], 3)
parms <- data.frame(parms = parms$parms, val = parms$`50%`, lo  = parms$`2.5%`, hi = parms$`97.5%`)
export(parms, here("data", "sc", "sc_rates_y20.csv"))
export(parms, here("data", "sc", "sc_rates_y50.csv"))

# 2. Fit checks ====
# 2.1 Parameters
kappa_ab <- 1 # Transition Y1 -> Y2
kappa_bc <- 1 # Transition Y2 -> Y3-9
kappa_cd <- 1/8 # Transition Y3-9 -> Y10+

# 2.2 Matrix exponents approach
get.targets <- function(gamma_a, gamma_b, gamma_c, gamma_d,
                        kappa_ab, kappa_bc, kappa_cd, Y10plus = 20) { # CHANGE
  state0 <- c(S = 0, Ia = 1, Ib = 0, Ic = 0, Id = 0)
  R <- matrix(
    c(0, gamma_a, gamma_b, gamma_c, gamma_d,    # S
      0, - gamma_a - kappa_ab, 0, 0, 0,         # Ia
      0, kappa_ab, - gamma_b - kappa_bc, 0, 0,  # Ib
      0, 0, kappa_bc, - gamma_c - kappa_cd, 0,  # Ic
      0, 0, 0, kappa_cd, - gamma_d),            # Id
    5, 5, byrow = TRUE)
  c((expm(R * 1) %*% state0)[1],
    (expm(R * 2) %*% state0)[1],
    (expm(R * 10) %*% state0)[1],
    (expm(R * Y10plus) %*% state0)[1])
}

# 2.2.1 Simulation
N <- 100
A <- matrix(nrow = N, ncol = 4)
for(i in 1:100) {
  A[i, ] <- get.targets(gamma_a = S$gamma_a[i], gamma_b = S$gamma_b[i], 
                        gamma_c = S$gamma_c[i], gamma_d = S$gamma_d[i],
                        kappa_ab = kappa_ab, kappa_bc = kappa_bc, 
                        kappa_cd = kappa_cd, Y10plus = 20) # CHANGE
}

# 2.2.2 Target comparison
tgt <- c(D$m1, D$m2, D$m3, D$m4)
matplot(A)
for(i in 1:4) abline(h = tgt[i], col = i)

# 2.3 Matrix exponents approach
sc_model <- function(parms, end_time = 50) {
  
  des <- function(time, state, parms) {
    S <- state["S"]
    Ia <- state["Ia"]
    Ib <- state["Ib"]
    Ic <- state["Ic"]
    Id <- state["Id"]
    
    gamma_a <- parms[['gamma_a']]
    gamma_b <- parms[['gamma_b']]
    gamma_c <- parms[['gamma_c']]
    gamma_d <- parms[['gamma_d']]
    kappa_ab <- 1 # Transition Y1 -> Y2
    kappa_bc <- 1 # Transition Y2 -> Y3-9
    kappa_cd <- 1/8 # Transition Y3-9 -> Y10+
    
    dS <- (gamma_a * Ia) + (gamma_b * Ib) + (gamma_c * Ic) + (gamma_d * Id)
    dIa <- - (gamma_a * Ia) - (kappa_ab * Ia)
    dIb <- - (gamma_b * Ib) + (kappa_ab * Ia) - (kappa_bc * Ib)
    dIc <- - (gamma_c * Ic) + (kappa_bc * Ib) - (kappa_cd * Ic)
    dId <- - (gamma_d * Id) + (kappa_cd * Ic)
    
    return(list(c(dS, dIa, dIb, dIc, dId), pSC = S/1e5))
  }
  
  state <- c(S = 0, Ia = 1e5, Ib = 0, Ic = 0, Id = 0)
  times <- seq(from = 0, to = end_time, by = 1)
  out <- deSolve::ode(y = state, times = times, func = des, parms = parms)
  
  df_out <- as.data.frame(out)
  
  if("pSC.S" %in% names(df_out)) {
    names(df_out)[names(df_out) == 'pSC.S'] <- 'pSC'
  }
  
  return(df_out)
}

# 2.3.1  Model loops
loop <- list()

pb <- progress_bar$new(format = "[:bar] :percent :eta", total = nrow(fits))
for(i in 1:nrow(fits)) {
  parms <- fits[i,]
  
  out <- as.data.frame(sc_model(parms))
  loop[[i]] <- out
  pb$tick()
}

# 2.3.2 Output processing
run <- do.call("rbind", loop) %>% 
  pivot_longer(cols = -time, names_to = "var", values_to = "values") %>% 
  group_by(time, var) %>% 
  summarise(val = median(values, na.rm = TRUE),
            lo = quantile(values, 0.025, na.rm = TRUE),
            hi = quantile(values, 0.975, na.rm = TRUE)) %>% 
  mutate(val = ifelse(val < 0, 0, val),
         lo = ifelse(lo < 0, 0, lo),
         hi = ifelse(hi < 0, 0, hi))

export(run, here("data", "sc", "runs_y20.Rdata"))
export(run, here("data", "sc", "runs_y50.Rdata"))

# 3. Plots ====
# 3.1 Targets
targets <- data.frame(time = c(1, 2, 10, 20), # CHANGE
                      var = c('pSC', 'pSC', 'pSC', 'pSC'),
                      lo = c(0.801, 0.914, 0.969, 0.985),
                      hi = c(0.817, 0.925, 0.975, 0.995))

# 3.2 Fit plot
png(here("plots", "00_fit_y20.png"), width = 8, height = 6, units = 'in', res = 1000)
png(here("plots", "00_fit_y50.png"), width = 8, height = 6, units = 'in', res = 1000)
ggplot(filter(run, var == 'pSC')) +
  geom_errorbar(data = targets, aes(x = time, ymin = lo, ymax = hi), colour = '#000000', width = 0.5) + 
  geom_line(aes(x = time, y = val), colour = '#CE2931') +
  geom_ribbon(aes(x = time, ymin = lo, ymax = hi), fill = '#CE2931', alpha = 0.2) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.75, 1), xlim = c(0, 51)) +
  labs(x = 'Years', y = 'Percentage self-cleared or recovered') +
  theme_bw() + 
  theme(text = element_text(family = "Open Sans"))
dev.off()

# 3.3 Proportion self-cleared
png(here("plots", "00_scfit_y20.png"), width = 8, height = 6, units = 'in', res = 1000)
png(here("plots", "00_scfit_y50.png"), width = 8, height = 6, units = 'in', res = 1000)
ggplot(filter(run, var != 'pSC')) +
  geom_bar(aes(x = time, y = val, fill = var), stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c('Ia' = '#710301', 'Ib' = '#C52104', 'Ic' = '#F28705', 'Id' = '#F2B807', 'S' = '#CCCCCC'),
                    labels = c('Ia' = 'Infected Y1', 'Ib' = 'Infected Y2', 'Ic' = 'Infected Y3-9', 'Id' = 'Infected Y10+', 'S' = 'Not infected')) +
  coord_cartesian(ylim = c(0.80, 1)) +
  labs(y = 'Proportion of cohort', x = 'Years', fill = 'Compartment') +
  theme_bw() +
  theme(legend.position = 'bottom', text = element_text(family = "Open Sans"))
dev.off()

# 3.4 Scenarios 
y20 <- import(here("data", "sc", "runs_y20.Rdata")) %>% 
  mutate(scen = 'y20')

y50 <- import(here("data", "sc", "runs_y50.Rdata")) %>% 
  mutate(scen = 'y50')

y20t <- data.frame(time = c(1, 2, 10, 20), 
                   var = c('pSC', 'pSC', 'pSC', 'pSC'),
                   lo = c(0.801, 0.914, 0.969, 0.985), 
                   hi = c(0.817, 0.925, 0.975, 0.995)) %>% 
  mutate(scen = 'y20')

y50t <- data.frame(time = c(1, 2, 10, 50),
                   var = c('pSC', 'pSC', 'pSC', 'pSC'),
                   lo = c(0.801, 0.914, 0.969, 0.985),
                   hi = c(0.817, 0.925, 0.975, 0.995)) %>% 
  mutate(scen = 'y50')

run <- rbind(y20, y50)
targets <- rbind(y20t, y50t)

label <- c("y20" = "High self-clearance scenario", "y50" = "Low self-clearance scenario")

png(here("plots", "00_fit.png"), width = 8, height = 6, units = 'in', res = 1000)
ggplot(filter(run, var == 'pSC')) +
  facet_wrap(~scen, nrow = 2, labeller = labeller(scen = label)) +
  geom_errorbar(data = targets, aes(x = time, ymin = lo, ymax = hi), colour = '#000000', width = 0.5) + 
  geom_line(aes(x = time, y = val), colour = '#CE2931') +
  geom_ribbon(aes(x = time, ymin = lo, ymax = hi), fill = '#CE2931', alpha = 0.2) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.75, 1)) +
  labs(x = 'Years', y = 'Percentage self-cleared or recovered') +
  theme_bw() +
  theme(text = element_text(family = "Open Sans"))
dev.off()

