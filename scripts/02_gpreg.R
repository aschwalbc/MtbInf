## Analysis code for Mtb Inf Burden
## Authors: P Dodd
## RScript 02: GPReg.R

# Packages ==========
library(data.table)
library(rio)
library(here)
library(tidyverse)
library(ggplot2)
library(MASS)
library(Matrix)

# 1. Data ==========
ARI <- as.data.table(import(here("data", "ari", "ARI_rev.Rdata")))

# ARI dataset (Houben and Dodd PMED 2016)
# ARI <- as.data.table(import(here("data", "ari", "ARI_PMED.Rdata"))) %>% 
#   filter(iso3 %in% c("AGO", "BGD", "BRA", "CAF", "CHN", "COD", "COG", "PRK", "ETH", "GAB",
#                      "IND", "IDN", "KEN", "LSO", "LBR", "MNG", "MOZ", "MMR", "NAM", "NGA", 
#                      "PAK", "PNG", "PHL", "SLE", "ZAF", "THA", "UGA", "TZA", "VNM", "ZMB"))

iso <- unique(as.character(ARI$iso3)) # List unique ISO codes
interp <- 1 # CHANGE HERE: Constant (0) or linear (1)

# 2. Functions ==========
# 2.1 Function 01
getKKtonly <- function(t1, t2, k = function(x,y) exp(-abs(x - y)), Wm) {
  K <- outer(t1, t2, FUN = k)
  K
}

# 2.2 Function 02
bsi <- function(CH, v) backsolve(CH, backsolve(CH, v, transpose = TRUE))

# 2.3 Function 03
getHtonly <- function(t, n = 1) {            
  H <- Matrix(0, nrow = (n + 1), ncol = length(t))
  for(k in 0:n)
    H[1 + k,] <- t^k
  as(H, 'sparseMatrix')
}

# 2.4 Function 04
tr <- function(x) sum(diag(x))

# 2.5 Function 05
PRMconvert2 <- function(x) c(exp(x[1]), exp(x[2] / 2))

# 2.6 Function 06
getMLnGradT <- function(x, grad = TRUE) {
  a <- x[1]; b <- x[2]
  K <- outer(tdz, tdz, FUN = function(x,y) exp(a - exp(-b) * (x - y)^2))
  K <- Matrix(K)
  K2 <- outer(tdz, tdz, FUN = function(x,y) (x - y)^2 * exp(a - exp(-b) * (x - y)^2))
  K2 <- Matrix(K2)

  cvy <- K + sigz
  U <- chol(cvy)
  Uy <- backsolve(U, y, transpose = TRUE)
  ky <- backsolve(U, Uy) 
  hky <- H %*% ky
  AM <- symmpart(H %*% bsi(U, t(H)))
  V <- chol(AM)
  Vy <- backsolve(V, hky, transpose = TRUE)

  LML <- -sum(Uy^2) + sum(Vy^2)
  LML <- LML - 2 * sum(log(diag(U)))
  LML <- LML - 2 * sum(log(diag(V)))
  LML <- LML/2

  VVy <- backsolve(V, Vy)
  ympy <- bsi(U, t(H) %*% VVy) 
  dLML <- NULL
  
  if(grad){
    dHelp <- function(dK) {
      dML <- t(ky) %*% dK %*% ky + t(ympy) %*% dK %*% ympy
      dML <- dML - 2 * t(ympy) %*% dK %*% ky
      tmp <- bsi(U, dK)
      dML <- dML - tr(tmp)
      tmp <- bsi(V, H %*% tmp)
      tmp <- bsi(U, t(H) %*% tmp)
      dML <- dML + tr(tmp)  
      return(as.numeric(dML / 2))
    }
    dLML <- c(dHelp(K), dHelp(K2))
  }
  
  if(LML > 0) {
    LML <- -1e3; dLML <- -1e3 * rep(1, 2)
  }
  
  return(list(LML = LML, dLML = dLML))
}

# 2.7 Function 07
getPredztonly <- function(x, tdz, tez, y, Vz) {
  usek <- function(i,j) x[1] * exp(-abs(i - j)^2 / x[2]^2)

  H <- getHtonly(tdz, n=interp)
  Hs <- getHtonly(tez, n=interp)

  kxx <- getKKtonly(tdz, tdz, k = usek)
  kxxs <- getKKtonly(tdz, tez, k = usek)
  kxsx <- t(kxxs)
  kxsxs <- getKKtonly(tez, tez, k = usek)
  sigz <- Diagonal(x = Vz)
  covy <- kxx + sigz
  U <- chol(covy)

  reg <- 1 
  HKH <- H %*% bsi(U, t(H))
  V <- chol(symmpart(HKH))                       
  R <- Hs - H %*% bsi(U, kxxs)
  mn <- 0
  y <- y - mn

  mf <- kxsx %*% bsi(U, y)
  cf <- kxsxs - kxsx %*% bsi(U, kxxs)

  bbar <- bsi(V, (H %*% bsi(U, y)))
  mg <- mf + reg * t(R) %*% bbar
  if(nrow(V) > 1)
    cg <- cf + reg * t(R) %*% bsi(V, R)
  else
    cg <- cf + reg * t(R) %*% (R / V[1,1])

  return(list(mg = mg,cg = cg))
}

# 3. GP regression ==========
for(i in seq(1, length(iso))) {
  cn <- iso[i]
  print(cn)
  db <- filter(ARI, iso3 == cn)
  
  fyear <- 1950
  tdz <- db$year - fyear
  tez <- 1950:2022 - fyear
  
  H <- getHtonly(tdz, n = interp)
  Hs <- getHtonly(tez, n = interp)
  y <- db$lari
  vz <- (db$E)^2
  sigz <- Diagonal(x = vz)
  
  mz <- c(log(.5), 2 * 1.5 * log(2))
  sz <- c(1,1) * 100
  LMLfun2 <- function(x) -(getMLnGradT(x, grad = FALSE)$LML
                           - sum(.5 * (x - mz)^2 / sz^2))
  dLMLfun2 <- function(x) -(getMLnGradT(x)$dLML - (x - mz) / sz^2)
  x02 <- mz
  system.time({  
    testo2 <- optim(par = x02, fn = LMLfun2, gr = dLMLfun2)
  })
  pab <- testo2$par
  ab <- PRMconvert2(pab)
  print(ab)
  
  xx <- ab
  tot <- getPredztonly(xx, tdz, tez, y, vz)
  scf <- as.numeric(sqrt(diag(tot$cg)))
  erw <- data.frame(year = tez + fyear, iso3 = as.character(unique(db$iso3)),
                    lari = as.numeric(tot$mg),
                    upper = as.numeric(tot$mg) + 1.96 * scf,
                    lower = as.numeric(tot$mg) - 1.96 * scf)
  
  save(erw, file = paste0('data/gp/iso/',cn,'.Rdata'))
  
  runs <- mvrnorm(n = 1e3, mu = as.numeric(tot$mg), Sigma = as.matrix(symmpart(tot$cg)))
  runsdf <- data.frame(year = tez+fyear, iso3 = as.character(unique(db$iso3)),
                       lari = c(t(runs)), rep = rep(1:nrow(runs), each = ncol(runs)))
  
  save(runsdf,file=paste0('data/gp/iso/zz_',cn,'.Rdata'))

  if(i == 1) {
    erw_full <- erw
    runsdf_full <- runsdf
    
  } else {
    erw_full <- rbind(erw_full, erw)
    runsdf_full <- rbind(runsdf_full, runsdf)
  }
}

# Reversion: Save output
save(erw_full, file = here("data", "gp", "GP_rev.Rdata"))
save(runsdf_full,file = here("data", "gp", "GPruns_rev.Rdata"))

rm(list = ls())
detach(package:MASS, unload = TRUE) # Detach due to issues with tidyverse "select" 

# 4. GP plots ==========
ARIhist <- as.data.table(import(here("data", "gp", "GP_rev.Rdata")))
ARI <- as.data.table(import(here("data", "ari", "ARI_rev.Rdata")))

iso <- sort(unique(as.character(ARI$iso3)))

pdf(here("plots", "02_gplin_logari.pdf"), height = 5, width = 8)
for(i in 1:length(iso)){
  print(iso[i])
  ari <- ARI %>%
    filter(iso3 == iso[i])
  hist <- ARIhist %>%
    filter(iso3 == iso[i])
  p <- ggplot() +
    geom_line(hist, mapping = aes(x = year, y = lari), colour = "#FDB827") +
    geom_ribbon(hist, mapping = aes(x = year, ymin = lower, ymax = upper), fill = "#FDB827", alpha = 0.2) +
    geom_point(ari, mapping = aes(x = year, y = lari, colour = type, shape = type), show.legend = TRUE) +
    scale_x_continuous(expand=c(0.01, 0.01), breaks = seq(1950, 2025, 10)) +
    scale_y_continuous(expand=c(0.01, 0.01), breaks = seq(-10, 0, 2.5)) +
    scale_colour_manual(values = c("prev" = "#CE1126", "surv" = "#003884"),
                        labels = c("prev" = "TB prevalence estimates", "surv" = "Immunoreactivity surveys")) +
    scale_shape_manual(values = c("prev" = 16, "surv" = 17),
                       labels = c("prev" = "TB prevalence estimates", "surv" = "Immunoreactivity surveys")) +
    coord_cartesian(ylim = c(-10, 0), xlim = c(1950, 2022)) +
    labs(title = iso[i], x = 'Year', y = 'Annual risk of infection (log)', colour = 'Source', shape = 'Source') +
    theme_bw() + 
    theme(legend.position = 'bottom')
  print(p)
}
dev.off()

pdf(here("plots", "02_gplin_ari.pdf"), height = 6, width = 10)
for(i in 1:length(iso)){
  print(iso[i])
  ari <- ARI %>%
    filter(iso3 == iso[i])
  hist <- ARIhist %>%
    filter(iso3 == iso[i])
  p <- ggplot() +
    geom_line(hist, mapping = aes(x = year, y = exp(lari)), colour = "#FDB827") +
    geom_ribbon(hist, mapping = aes(x = year, ymin = exp(lower), ymax = exp(upper)), fill = "#FDB827", alpha = 0.2) +
    geom_point(ari, mapping = aes(x = year, y = ari, colour = type, shape = type), show.legend = TRUE) +
    scale_x_continuous(expand=c(0.01, 0.01), breaks = seq(1950, 2025, 25)) +
    scale_y_continuous(expand=c(0.01, 0.01), labels = scales::label_percent(), breaks = seq(0, 0.15, 0.05)) +
    scale_colour_manual(values = c("prev" = "#CE1126", "surv" = "#003884"),
                        labels = c("prev" = "TB prevalence estimates", "surv" = "Immunoreactivity surveys")) +
    scale_shape_manual(values = c("prev" = 16, "surv" = 17),
                       labels = c("prev" = "TB prevalence estimates", "surv" = "Immunoreactivity surveys")) +
    coord_cartesian(ylim = c(0, 0.15), xlim = c(1950, 2022)) +
    labs(title = iso[i], x = 'Year', y = 'Annual risk of infection', colour = 'Type', shape = 'Type') +
    theme_bw() + 
    theme(legend.position = 'bottom')
  print(p)
}
dev.off()

rm(list = ls())
