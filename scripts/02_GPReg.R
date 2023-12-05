## Analysis code for Schwalb et al. 2023
## Adapted from Houben & Dodd 2016
## Distributed under CC BY 4.0
## RScript 02: GPReg.R

# Packages ==========
library(data.table) # Faster than data.frame, allows use of j operator (:=)
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
library(MASS) # To simulate from a Multivariate Normal Distribution (mvrnorm)
library(Matrix) # Extends support of highly dense or sparse matrices

# 1. Load data ==========
ARI <- as.data.table(import(here("data","ari","ARI_IHME_norev.Rdata"))) # No reversion
ARI <- as.data.table(import(here("data","ari","ARI_IHME_rev.Rdata"))) # Reversion
ARI <- as.data.table(import(here("data","ari","ARI_PMED_norev.Rdata"))) # PMED Baseline check

# 1.1 Data curation
ARI_all <- ARI
ARI_all <- ARI_all[ARI_all$lari!=-Inf,] # Remove negative infinity
lin <- 1 # CHANGE HERE: Constant (0) or linear (1)
iso <- unique(as.character(ARI$iso3)) # List unique ISO codes

# 2. Functions ==========
# 2.1 Function 01
getKKtonly <- function(t1,t2,k=function(x,y) exp(-abs(x-y)),Wm){
  K <- outer(t1,t2,FUN=k)
  K
}                                       #make K(X,Y) matrices
# 2.2 Function 02
## function to do double-backsolve inversion given cholesky decomp
bsi <- function(CH,v) backsolve(CH,backsolve(CH, v, transpose = TRUE))

# 2.3 Function 03
## this is basically 1,t x I(country==j)
getHtonly <- function(t,n=1){             #n is highest power
  H <- Matrix(0,nrow=(n+1),ncol=length(t)) #3 for 1,t,t^2
  for(k in 0:n)
    H[1 + k,] <- t^k
  as(H,'sparseMatrix')
}

# 2.4 Function 04
tr <- function(x) sum(diag(x))

# 2.5 Function 05
## gets s_k^2, L, tscale
PRMconvert2 <- function(x) c( exp(x[1]), exp(x[2]/2))

# 2.6 Function 06
getMLnGradT <- function(x,grad=TRUE){                    # see Eqn 2.45 in R&W
  ## preliminaries
  ## -- covariances --
  a <- x[1]; b <- x[2]
  K <- outer(tdz,tdz,FUN=function(x,y) exp(a-exp(-b)*(x-y)^2))
  K <- Matrix(K)                      #kxx
  K2 <- outer(tdz,tdz,FUN=function(x,y) (x-y)^2*exp(a-exp(-b)*(x-y)^2))
  K2 <- Matrix(K2)                     #dK/da
  ## -- derived matrices (as used above) --
  ## new version
  cvy <- K + sigz
  U <- chol(cvy)
  Uy <- backsolve(U, y, transpose = TRUE)
  ky <- backsolve(U, Uy) 
  hky <- H %*% ky
  AM <- symmpart(H %*% bsi(U,t(H)))
  V <- chol(AM)
  Vy <- backsolve(V, hky, transpose = TRUE)
  ## -- marginal loglikelihood --
  LML <- -sum(Uy^2) + sum(Vy^2) #data likelihood
  LML <- LML - 2*sum(log(diag(U)))
  LML <- LML - 2*sum(log(diag(V)))
  LML <- LML/2
  ## extras for dLML
  VVy <- backsolve(V, Vy)
  ympy <- bsi(U, t(H) %*% VVy)       #K^{-1} x ...
  dLML <- NULL
  if(grad){
    ## -- gradient --
    ## --- gradient helper function ---
    dHelp <- function(dK){              #takes the local vars from up-level
      dML <- t(ky) %*% dK %*% ky + t(ympy) %*% dK %*% ympy
      dML <- dML - 2*t(ympy) %*% dK %*% ky
      tmp <- bsi(U,dK)        #K^{-1}dK
      dML <- dML - tr(tmp)
      tmp <- bsi(V,H%*%tmp)
      tmp <- bsi(U, t(H)%*%tmp)
      dML <- dML + tr(tmp)  
      return(as.numeric(dML/2))
    }
    ## --- get gradient ---
    dLML <- c( dHelp(K), dHelp(K2) )
  }
  if(LML>0){LML <- -1e3; dLML <- -1e3*rep(1,2)}
  ## return
  return(list(LML=LML,dLML=dLML))
}

# 2.7 Function 07
getPredztonly <- function(x,tdz,tez,y,Vz){
  ## from here
  usek <- function(i,j)x[1]*exp(-abs(i-j)^2/x[2]^2)
  ## H
  H <- getHtonly(tdz,n=lin)
  Hs <- getHtonly(tez,n=lin)
  ## make matrices
  kxx <- getKKtonly(tdz,tdz,k=usek)
  kxxs <- getKKtonly(tdz,tez,k=usek)
  kxsx <- t(kxxs)
  kxsxs <- getKKtonly(tez,tez,k=usek)
  sigz <- Diagonal(x=Vz)                         #noise
  covy <- kxx + sigz
  U <- chol(covy)
  ## regress
  reg <- 1                                #flag
  HKH <- H %*% bsi(U,t(H))
  V <- chol(symmpart(HKH))                       
  R <- Hs - H %*% bsi(U,kxxs)
  mn <- 0
  y <- y-mn
  ## mean/covar
  mf <- kxsx %*% bsi(U,y)          #mean prediction
  cf <- kxsxs  - kxsx %*% bsi(U,kxxs)
  ## mean stuff
  bbar <- bsi(V,(H %*% bsi(U,y)))
  mg <- mf + reg * t(R) %*% bbar
  if(nrow(V)>1)
    cg <- cf + reg * t(R) %*% bsi(V,R)
  else
    cg <- cf + reg * t(R) %*% (R/V[1,1])
  ## return
  return(list(mg=mg,cg=cg))
}

# 3. GP regression ==========
# 3.1 Country loops
#i <- 1 # Focus on country (e.g., 153 = PER)
for(i in seq(1, length(iso))) {
cn <- iso[i]
print(cn)
ARI <- filter(ARI_all, iso3 == cn)

# 3.2 Time/country vectors
fyear <- 1934
tdz <- ARI$year-fyear
tez <- 1934:2050 - fyear # Extrapolation times

## H as a global
## dcnz, tdz, sigz, y as globals
## H
H <- getHtonly(tdz,n=lin)
Hs <- getHtonly(tez,n=lin)
y <- ARI$lari
vz <- (ARI$E)^2
sigz <- Diagonal(x=vz)                         #noise

mz <- c(log(.5),2*1.5*log(2))
sz <- c(1,1)*100
LMLfun2 <- function(x) -(getMLnGradT(x,grad=FALSE)$LML
                         - sum(.5*(x-mz)^2/sz^2))
dLMLfun2 <- function(x) -(getMLnGradT(x)$dLML-(x-mz)/sz^2)
x02 <- mz
## optimize
system.time({                           #<1s
  testo2 <- optim(par=x02,fn = LMLfun2,gr = dLMLfun2)
})
pab <- testo2$par
ab <- PRMconvert2(pab)
## ab
print(ab);
## NB trade-off flexibility vs growing noise going back
xx <- ab
tot <- getPredztonly(xx,tdz,tez,y,vz)
scf <- as.numeric(sqrt(diag(tot$cg)))
erw <- data.frame(year=tez+fyear,iso3=as.character(unique(ARI$iso3)),
                  lari=as.numeric(tot$mg),
                  upper=as.numeric(tot$mg) + 1.96*scf,
                  lower=as.numeric(tot$mg) - 1.96*scf)

save(erw,file=paste0('data/tmp/',cn,'.Rdata'))

runs <- mvrnorm(n=2e2,mu=as.numeric(tot$mg),Sigma=as.matrix(symmpart(tot$cg)))
runsdf <- data.frame(year=tez+fyear,iso3=as.character(unique(ARI$iso3)),
                     lari=c(t(runs)),replicate=rep(1:nrow(runs),each=ncol(runs)))

save(runsdf,file=paste0('data/tmp/zz_',cn,'.Rdata'))

if(i == 1) {
  erw_full <- erw
  runsdf_full <- runsdf
  
  } else {
  erw_full <- rbind(erw_full, erw)
  runsdf_full <- rbind(runsdf_full, runsdf)
  }
}

# Save output
# No reversion
save(erw_full, file = here("data","gp","GP_IHME_norev.Rdata"))
save(runsdf_full,file = here("data","gp","GPruns_IHME_norev.Rdata"))

# Reversion
save(erw_full, file = here("data","gp","GP_IHME_rev.Rdata"))
save(runsdf_full,file = here("data","gp","GPruns_IHME_rev.Rdata"))

# PMED
save(erw_full, file = here("data","gp","GP_PMED.Rdata"))
save(runsdf_full,file = here("data","gp","GPruns_PMED.Rdata"))

rm(list = ls())
