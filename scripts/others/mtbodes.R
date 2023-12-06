## Analysis code for Schwalb et al. 2023
## Created by P Dodd
## Distributed under CC BY 4.0
## RScript: mtbodes.R

# 1. Equations (Odin) ==========

# 1.1 Susceptible (dS(a)/dt)

# Block A: + krondelta_a1 * theta * 1/frac(a) * [1 - S(a)] 
# Kronecker delta * birth rate * fraction of the population in age group * infected population of age group

# Block B: + (1-krondelta_a1) * alpha * [S(a-1) - S(a)] * frac(a-1)/frac(a)
# Kronecker delta * age transitions * difference of susceptible population between age groups * population fraction ratio

# Block C: + sum_j [gamma^j(a) * I^j(a)] 
# Sum of self-clearance from all infected compartments

# Block D: - lambda(a) * S(a)
# Infection per ARI * all susceptible population 

# 1.2 Infected (dI^j(a)/dt)

# Block E: + krondelta_1j * lambda(a) * S(a) 
# Kronecker delta * infection per ARI * all susceptible population

# Block F: - kappa^j * I^j(a)
# Transition out of year of infection (no transition if j = 3) * infected population in year of infection

# Block G: + (1-krondelta_1j) * kappa^(j-1) * I^(j-1)(a)
# Kronecker delta * transition into year of infection * infected population in previous year of infection

# Block H: - gamma^j * I^j(a)
# Self-clearance for infection year * infected population in year of infection

# Block I: + (1-krondelta_1a) * alpha * [I^j(a-1) - I^j(a)] * frac(a-1)/frac(a)
# Kronecker delta * age transition * difference of infected population between age groups * population fraction ratio

# Block J: - krondelta_1a * I^j(a) * theta * 1/frac(a)
# Kronecker delta * infected population in year of infection * birth rate * fraction of the population in age group

# 2. Parameters ==========
# 2.1 Fixed
Na <- 17 # Number of age groups (5y age groups, up to 80+)
Nj <- 4 # Number of infection years (1, 2, 3-9, 10+)
alpha <- 1/5 # Age transition 

# 2.2 Interpolation inputs
time_data[] <- user() # Time data
frac_data[,] <- user() # Population fraction
theta_data[] <- user() # Birth rate
lambda_data[,] <- user() # Annual risk of infection

# 2.3 Others
gamma[] <- user() # Self-clearance
kappa[] <- user() # Infected year transition

## -------- dynamics -------
## deriv(S[]) <- ageDelt[i] * (b/F[i]) * (1-S[i]) +
##   alph * (1-ageDelt[i]) * (S[i-1]-S[i]) * (F[i-1]/F[i]) +
##   sum(rhoI[i,]) - lam[i] * S[i]
deriv(S[1]) <-  (b/F[1]) * (1-S[1]) + sum(rhoI[1,]) - lam[1] * S[1]
deriv(S[2:Na]) <- alph * (S[i-1]-S[i]) * (F[i-1]/F[i]) + sum(rhoI[i,]) - lam[i] * S[i]


## deriv(I[,]) <- lam[i] * S[i] * JDelt[j] - g[j] * I[i,j] + g[j-1] * I[i,j-1] * (1-DeltJ[j])-
##   rhoI[i,j] + alph * (1-deltAge[i]) * (I[i-1,j]-I[i,j]) * (F[i-1]/F[i])-
##   I[i,j] * ageDelt[i] * (b/F[i])
deriv(I[1,1]) <- lam[1] * S[1] - g[1] * I[1,1] - rhoI[1,1] - I[1,1]  * (b/F[1])
deriv(I[1,2:Nj]) <-  - g[j] * I[1,j] + g[j-1] * I[1,j-1] - rhoI[1,j] - I[1,j] * (b/F[1])
deriv(I[2:Na,1]) <- lam[i]*S[i]  - g[1] * I[i,1] - rhoI[i,1] + alph *  (I[i-1,1]-I[i,1]) * (F[i-1]/F[i])
deriv(I[2:Na,2:Nj]) <- -g[j]*I[i,j] + g[j-1]*I[i,j-1] - rhoI[i,j] + alph*(I[i-1,j]-I[i,j])*(F[i-1]/F[i])


## -------- definitions
rhoI[,] <- rho[j] * I[i,j]
Sfrac[] <- exp( -lam_data[1,i] * (5*i-2.5) )
ST <- sum(Sfrac)

## -------- initial state -------
initial(S[]) <- Sfrac[i]/ST
initial(I[,]) <- (1-Sfrac[i]/ST)/Nj

## -------- interpolation -------
F[] <- interpolate(time_data,F_data,'linear')
lam[] <- interpolate(time_data,lam_data,'linear')
b <- interpolate(time_data,b_data,'linear')

## -------- dimensions -------
dim(I) <- c(Na,Nj)
dim(S) <- Na
dim(F) <- Na
dim(lam) <- Na

dim(time_data) <- user()
len_time_data <- length(time_data)
dim(F_data) <- c(len_time_data,Na)
dim(lam_data) <- c(len_time_data,Na)
dim(b_data) <- len_time_data

dim(g) <- Nj
dim(rho) <- Nj

dim(rhoI) <- c(Na,Nj)
dim(Sfrac) <- c(Na)
