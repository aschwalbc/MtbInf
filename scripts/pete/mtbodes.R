## odin version of Mtb equations
## ds(a)/dt = delta_a1 * b N/N(a) *(1-s) + alph * (1-delta_1a)*[s(a-1)-s(a)] * N(a-1)/N(a)
##              sum_j rho^j(a) i^j(a) - l(a) s
##
## di^j(a)/dt = l(a)s(a)delta_1j - g^j i^j(a) + g^(j-1)i^(j-1)(a) (1-delta_1j)
##               -rho^j i^j + alph * (1-delta_1a) * (i^j(a-1)-i^j(a)) * N(a-1)/N(a)
##                -i^j(a)delta_1a bN/N(a)

## use: i for age; j for j

## -------- parameters -------
## fixed
Nj <- 4
Na <- 17
alph <- 0.2
## data inputs for interpolation
time_data[] <- user()
F_data[,] <- user()
b_data[] <- user()
lam_data[,] <- user()
## other inputs
rho[] <- user()
g[] <- user()

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

