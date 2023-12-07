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

# 3. Dynamics ========== 
# Using i for age groups and j for infected years

# 3.1 Susceptible
deriv(S[1]) <- (theta/frac[1]) * (1 - S[1]) + sum(gammaI[1,]) - lambda[1] * S[1] # 00-04 age group
deriv(S[2:Na]) <- alpha * (S[i-1] - S[i]) * (frac[i-1]/frac[i]) + sum(gammaI[i,]) - lambda[i] * S[i] # All other age groups

# 3.2 Infected
deriv(I[1,1]) <- lambda[1] * S[1] - kappa[1] * I[1,1] - gammaI[1,1] - I[1,1]  * (theta/frac[1]) # 00-04 age group - Infection year 1
deriv(I[1,2:Nj]) <- - kappa[j] * I[1,j] + kappa[j-1] * I[1,j-1] - gammaI[1,j] - I[1,j] * (theta/frac[1]) # 00-04 age group - Other infection years
deriv(I[2:Na,1]) <- lambda[i] * S[i] - kappa[1] * I[i,1] - gammaI[i,1] + alpha * (I[i-1,1] - I[i,1]) * (frac[i-1]/frac[i]) # Other age groups - Infection year 1
deriv(I[2:Na,2:Nj]) <- -kappa[j] * I[i,j] + kappa[j-1] * I[i,j-1] - gammaI[i,j] + alpha * (I[i-1,j] - I[i,j]) * (frac[i-1]/frac[i]) # Other age groups - Other infection years

# 3.3 Definitions
gammaI[,] <- gamma[j] * I[i,j] # Self-clearance of infection year
Sfrac[] <- exp( -lambda_data[1,i] * (5*i-2.5) ) # Proportion susceptible
ST <- sum(Sfrac) # Sum of all susceptibles

# 3.4 Initial states 
initial(S[]) <- Sfrac[i]/ST # Susceptible
initial(I[,]) <- (1-Sfrac[i]/ST)/Nj # Infected

# 3.5 Interpolation
frac[] <- interpolate(time_data, frac_data, 'linear') # Population fraction
lambda[] <- interpolate(time_data, lambda_data, 'linear') # Annual risk of infection
theta <- interpolate(time_data, theta_data, 'linear') # Birth rate

# 3.6 Dimensions
dim(I) <- c(Na, Nj) # Infected per age group (Na) and infection year (Nj)
dim(S) <- Na # Susceptible per age group (Na)
dim(frac) <- Na # Population fraction per age group (Na)
dim(lambda) <- Na # Annual risk of infection per age group (Na)
dim(time_data) <- user() # Time data
len_time_data <- length(time_data) # Length time data
dim(frac_data) <- c(len_time_data, Na) # Population fraction interpolation
dim(lambda_data) <- c(len_time_data, Na) # Annual risk of infection interpolation
dim(theta_data) <- len_time_data # Birth rate interpolation
dim(gamma) <- Nj # Self-clearance per infection year (Nj)
dim(gammaI) <- c(Na, Nj) # Self-clearance per age group (Na) and infection year (Nj)
dim(kappa) <- Nj # Infection year transition per infection year
dim(Sfrac) <- Na # Proportion susceptible per age group
