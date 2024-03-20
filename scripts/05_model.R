## Analysis code for Schwalb et al. 2024
## Distributed under CC BY 4.0
## RScript 05: Model.R

# Packages ==========
library(rio)
library(here)
library(tidyverse)
library(deSolve)
library(tictoc)
library(data.table)

# 1. Age-specific parameters ==========
ARI <- import(here("data","ari","mARI_rev_mix_pop.Rdata"))

agp_0004 <- ARI %>% 
  filter(ageWPP == '00-04') %>% 
  mutate(lambda_0014 = ari) %>%
  rename(fp_0004 = fpop, theta = birthrate) %>% 
  select(iso3,year,lambda_0014,theta,fp_0004)

agp_0509 <- ARI %>% 
  filter(ageWPP == '05-09') %>% 
  mutate(lambda_0014 = ari) %>%
  rename(fp_0509 = fpop) %>% 
  select(iso3,year,lambda_0014,fp_0509)

agp_1014 <- ARI %>% 
  filter(ageWPP == '10-14') %>% 
  mutate(lambda_0014 = ari) %>%
  rename(fp_1014 = fpop) %>% 
  select(iso3,year,lambda_0014,fp_1014)

agp_1519 <- ARI %>% 
  filter(ageWPP == '15-19') %>% 
  mutate(lambda_1544 = ari) %>%
  rename(fp_1519 = fpop) %>% 
  select(iso3,year,lambda_1544,fp_1519)

agp_2024 <- ARI %>% 
  filter(ageWPP == '20-24') %>% 
  mutate(lambda_1544 = ari) %>%
  rename(fp_2024 = fpop) %>% 
  select(iso3,year,lambda_1544,fp_2024)

agp_2529 <- ARI %>% 
  filter(ageWPP == '25-29') %>% 
  mutate(lambda_1544 = ari) %>%
  rename(fp_2529 = fpop) %>% 
  select(iso3,year,lambda_1544,fp_2529)

agp_3034 <- ARI %>% 
  filter(ageWPP == '30-34') %>% 
  mutate(lambda_1544 = ari) %>%
  rename(fp_3034 = fpop) %>% 
  select(iso3,year,lambda_1544,fp_3034)

agp_3539 <- ARI %>% 
  filter(ageWPP == '35-39') %>% 
  mutate(lambda_1544 = ari) %>%
  rename(fp_3539 = fpop) %>% 
  select(iso3,year,lambda_1544,fp_3539)

agp_4044 <- ARI %>% 
  filter(ageWPP == '40-44') %>% 
  mutate(lambda_1544 = ari) %>%
  rename(fp_4044 = fpop) %>% 
  select(iso3,year,lambda_1544,fp_4044)

agp_4549 <- ARI %>% 
  filter(ageWPP == '45-49') %>% 
  mutate(lambda_4500 = ari) %>%
  rename(fp_4549 = fpop) %>% 
  select(iso3,year,lambda_4500,fp_4549)

agp_5054 <- ARI %>% 
  filter(ageWPP == '50-54') %>% 
  mutate(lambda_4500 = ari) %>%
  rename(fp_5054 = fpop) %>% 
  select(iso3,year,lambda_4500,fp_5054)

agp_5559 <- ARI %>% 
  filter(ageWPP == '55-59') %>% 
  mutate(lambda_4500 = ari) %>%
  rename(fp_5559 = fpop) %>% 
  select(iso3,year,lambda_4500,fp_5559)

agp_6064 <- ARI %>% 
  filter(ageWPP == '60-64') %>% 
  mutate(lambda_4500 = ari) %>%
  rename(fp_6064 = fpop) %>% 
  select(iso3,year,lambda_4500,fp_6064)

agp_6569 <- ARI %>% 
  filter(ageWPP == '65-69') %>% 
  mutate(lambda_4500 = ari) %>%
  rename(fp_6569 = fpop) %>% 
  select(iso3,year,lambda_4500,fp_6569)

agp_7074 <- ARI %>% 
  filter(ageWPP == '70-74') %>% 
  mutate(lambda_4500 = ari) %>%
  rename(fp_7074 = fpop) %>% 
  select(iso3,year,lambda_4500,fp_7074)

agp_7579 <- ARI %>% 
  filter(ageWPP == '75-79') %>% 
  mutate(lambda_4500 = ari) %>%
  rename(fp_7579 = fpop) %>% 
  select(iso3,year,lambda_4500,fp_7579)

agp_8000 <- ARI %>% 
  filter(ageWPP == '80+') %>% 
  mutate(lambda_4500 = ari) %>%
  rename(fp_8000 = fpop) %>% 
  select(iso3,year,lambda_4500,fp_8000)

parameters <- agp_0004 %>% # Create parameter dataframe
  left_join(agp_0509, by = c("iso3","year","lambda_0014")) %>% 
  left_join(agp_1014, by = c("iso3","year","lambda_0014")) %>% 
  left_join(agp_1519, by = c("iso3","year")) %>% 
  left_join(agp_2024, by = c("iso3","year","lambda_1544")) %>% 
  left_join(agp_2529, by = c("iso3","year","lambda_1544")) %>% 
  left_join(agp_3034, by = c("iso3","year","lambda_1544")) %>% 
  left_join(agp_3539, by = c("iso3","year","lambda_1544")) %>% 
  left_join(agp_4044, by = c("iso3","year","lambda_1544")) %>% 
  left_join(agp_4549, by = c("iso3","year")) %>% 
  left_join(agp_5054, by = c("iso3","year","lambda_4500")) %>% 
  left_join(agp_5559, by = c("iso3","year","lambda_4500")) %>% 
  left_join(agp_6064, by = c("iso3","year","lambda_4500")) %>% 
  left_join(agp_6569, by = c("iso3","year","lambda_4500")) %>% 
  left_join(agp_7074, by = c("iso3","year","lambda_4500")) %>% 
  left_join(agp_7579, by = c("iso3","year","lambda_4500")) %>% 
  left_join(agp_8000, by = c("iso3","year","lambda_4500")) %>% 
  mutate(time = year - 1950) %>% 
  select(iso3,time,starts_with("lambda"),starts_with("fp"),"theta")

rm(list = ls(pattern = "^agp"))

parameters <- as.data.table(parameters)

# 2. Mtb Model ==========
sis <- function(times, state, parms) {
  S0004  <- state["S0004"]; I0004a  <- state["I0004a"]; I0004b  <- state["I0004b"]; I0004c  <- state["I0004c"]; I0004d  <- state["I0004d"]
  S0509  <- state["S0509"]; I0509a  <- state["I0509a"]; I0509b  <- state["I0509b"]; I0509c  <- state["I0509c"]; I0509d  <- state["I0509d"]
  S1014  <- state["S1014"]; I1014a  <- state["I1014a"]; I1014b  <- state["I1014b"]; I1014c  <- state["I1014c"]; I1014d  <- state["I1014d"]
  S1519  <- state["S1519"]; I1519a  <- state["I1519a"]; I1519b  <- state["I1519b"]; I1519c  <- state["I1519c"]; I1519d  <- state["I1519d"]
  S2024  <- state["S2024"]; I2024a  <- state["I2024a"]; I2024b  <- state["I2024b"]; I2024c  <- state["I2024c"]; I2024d  <- state["I2024d"]
  S2529  <- state["S2529"]; I2529a  <- state["I2529a"]; I2529b  <- state["I2529b"]; I2529c  <- state["I2529c"]; I2529d  <- state["I2529d"]
  S3034  <- state["S3034"]; I3034a  <- state["I3034a"]; I3034b  <- state["I3034b"]; I3034c  <- state["I3034c"]; I3034d  <- state["I3034d"]
  S3539  <- state["S3539"]; I3539a  <- state["I3539a"]; I3539b  <- state["I3539b"]; I3539c  <- state["I3539c"]; I3539d  <- state["I3539d"]
  S4044  <- state["S4044"]; I4044a  <- state["I4044a"]; I4044b  <- state["I4044b"]; I4044c  <- state["I4044c"]; I4044d  <- state["I4044d"]
  S4549  <- state["S4549"]; I4549a  <- state["I4549a"]; I4549b  <- state["I4549b"]; I4549c  <- state["I4549c"]; I4549d  <- state["I4549d"]
  S5054  <- state["S5054"]; I5054a  <- state["I5054a"]; I5054b  <- state["I5054b"]; I5054c  <- state["I5054c"]; I5054d  <- state["I5054d"]
  S5559  <- state["S5559"]; I5559a  <- state["I5559a"]; I5559b  <- state["I5559b"]; I5559c  <- state["I5559c"]; I5559d  <- state["I5559d"]
  S6064  <- state["S6064"]; I6064a  <- state["I6064a"]; I6064b  <- state["I6064b"]; I6064c  <- state["I6064c"]; I6064d  <- state["I6064d"]
  S6569  <- state["S6569"]; I6569a  <- state["I6569a"]; I6569b  <- state["I6569b"]; I6569c  <- state["I6569c"]; I6569d  <- state["I6569d"]
  S7074  <- state["S7074"]; I7074a  <- state["I7074a"]; I7074b  <- state["I7074b"]; I7074c  <- state["I7074c"]; I7074d  <- state["I7074d"]
  S7579  <- state["S7579"]; I7579a  <- state["I7579a"]; I7579b  <- state["I7579b"]; I7579c  <- state["I7579c"]; I7579d  <- state["I7579d"]
  S8000  <- state["S8000"]; I8000a  <- state["I8000a"]; I8000b  <- state["I8000b"]; I8000c  <- state["I8000c"]; I8000d  <- state["I8000d"]
  
  gamma_a <- 1.26 # Self-clearance rate (A)
  gamma_b <- 1.26 # Self-clearance rate (B)
  gamma_c <- 0.5 # Self-clearance rate (C)
  gamma_d <- 0.0 # Self-clearance rate (D)
  alpha <- 1/5 # Age transition
  kappa_ab <- 1 # Transition between infection years (A-B)
  kappa_bc <- 1 # Transition between infection years (B-C)
  kappa_cd <- 1/8 # Transition between infection years (C-D)
  pi_a <- 0 # Reinfection (A) 
  pi_b <- 0 # Reinfection (B)
  pi_c <- 0.21 # Reinfection (C)
  pi_d <- 0.21 # Reinfection (D)
  
  par <- names(parms)
  par <- par[-c(1,2)]
  interps <- list()
  for(p in par) {
    interps[[p]] <- approxfun(parms$time, parms[[p]], method = 'linear', rule = 2)
  }
  
  dS0004  <- + ((interps[["theta"]](times)/interps[["fp_0004"]](times))*(1-S0004)) - (alpha*S0004) - (interps[["lambda_0014"]](times)*S0004) + (gamma_a*I0004a) + (gamma_b*I0004b) + (gamma_c*I0004c) + (gamma_d*I0004d) 
  dI0004a <- - ((interps[["theta"]](times)/interps[["fp_0004"]](times))*I0004a) - (alpha*I0004a) - (kappa_ab*I0004a) - (gamma_a*I0004a) + (interps[["lambda_0014"]](times)*(S0004+(pi_a*I0004a)+(pi_b*I0004b)+(pi_c*I0004c)+(pi_d*I0004d))) - (interps[["lambda_0014"]](times)*(pi_a*I0004a))
  dI0004b <- - ((interps[["theta"]](times)/interps[["fp_0004"]](times))*I0004b) - (alpha*I0004b) - (kappa_bc*I0004b) + (kappa_ab*I0004a)  - (gamma_b*I0004b) - (interps[["lambda_0014"]](times)*(pi_b*I0004b))
  dI0004c <- - ((interps[["theta"]](times)/interps[["fp_0004"]](times))*I0004c) - (alpha*I0004c) - (kappa_cd*I0004c) + (kappa_bc*I0004b)  - (gamma_c*I0004c) - (interps[["lambda_0014"]](times)*(pi_c*I0004c))
  dI0004d <- - ((interps[["theta"]](times)/interps[["fp_0004"]](times))*I0004d) - (alpha*I0004d) + (kappa_cd*I0004c) - (gamma_d*I0004d) - (interps[["lambda_0014"]](times)*(pi_d*I0004d))
  
  dS0509  <- + (alpha*S0004*(interps[["fp_0004"]](times)/interps[["fp_0509"]](times))) - (alpha*S0509) - (interps[["lambda_0014"]](times)*S0509) + (gamma_a*I0509a) + (gamma_b*I0509b) + (gamma_c*I0509c) + (gamma_d*I0509d)  
  dI0509a <- + (alpha*I0004a*(interps[["fp_0004"]](times)/interps[["fp_0509"]](times))) - (alpha*I0509a) - (kappa_ab*I0509a) - (gamma_a*I0509a) + (interps[["lambda_0014"]](times)*(S0509+(pi_a*I0509a)+(pi_b*I0509b)+(pi_c*I0509c)+(pi_d*I0509d))) - (interps[["lambda_0014"]](times)*(pi_a*I0509a))
  dI0509b <- + (alpha*I0004b*(interps[["fp_0004"]](times)/interps[["fp_0509"]](times))) - (alpha*I0509b) - (kappa_bc*I0509b) + (kappa_ab*I0509a) - (gamma_b*I0509b) - (interps[["lambda_0014"]](times)*(pi_b*I0509b))
  dI0509c <- + (alpha*I0004c*(interps[["fp_0004"]](times)/interps[["fp_0509"]](times))) - (alpha*I0509c) - (kappa_cd*I0509c) + (kappa_bc*I0509b) - (gamma_c*I0509c) - (interps[["lambda_0014"]](times)*(pi_c*I0509c))
  dI0509d <- + (alpha*I0004d*(interps[["fp_0004"]](times)/interps[["fp_0509"]](times))) - (alpha*I0509d) + (kappa_cd*I0509c) - (gamma_d*I0509d) - (interps[["lambda_0014"]](times)*(pi_d*I0509d))
  
  dS1014  <- + (alpha*S0509*(interps[["fp_0509"]](times)/interps[["fp_1014"]](times))) - (alpha*S1014) - (interps[["lambda_0014"]](times)*S1014) + (gamma_a*I1014a) + (gamma_b*I1014b) + (gamma_c*I1014c) + (gamma_d*I1014d)  
  dI1014a <- + (alpha*I0509a*(interps[["fp_0509"]](times)/interps[["fp_1014"]](times))) - (alpha*I1014a) - (kappa_ab*I1014a) - (gamma_a*I1014a) + (interps[["lambda_0014"]](times)*(S1014+(pi_a*I1014a)+(pi_b*I1014b)+(pi_c*I1014c)+(pi_d*I1014d))) - (interps[["lambda_0014"]](times)*(pi_a*I1014a))
  dI1014b <- + (alpha*I0509b*(interps[["fp_0509"]](times)/interps[["fp_1014"]](times))) - (alpha*I1014b) - (kappa_bc*I1014b) + (kappa_ab*I1014a) - (gamma_b*I1014b) - (interps[["lambda_0014"]](times)*(pi_b*I1014b))
  dI1014c <- + (alpha*I0509c*(interps[["fp_0509"]](times)/interps[["fp_1014"]](times))) - (alpha*I1014c) - (kappa_cd*I1014c) + (kappa_bc*I1014b) - (gamma_c*I1014c) - (interps[["lambda_0014"]](times)*(pi_c*I1014c))
  dI1014d <- + (alpha*I0509d*(interps[["fp_0509"]](times)/interps[["fp_1014"]](times))) - (alpha*I1014d) + (kappa_cd*I1014c) - (gamma_d*I1014d) - (interps[["lambda_0014"]](times)*(pi_d*I1014d))
  
  dS1519  <- + (alpha*S1014*(interps[["fp_1014"]](times)/interps[["fp_1519"]](times))) - (alpha*S1519) - (interps[["lambda_1544"]](times)*S1519) + (gamma_a*I1519a) + (gamma_b*I1519b) + (gamma_c*I1519c) + (gamma_d*I1519d)  
  dI1519a <- + (alpha*I1014a*(interps[["fp_1014"]](times)/interps[["fp_1519"]](times))) - (alpha*I1519a) - (kappa_ab*I1519a) - (gamma_a*I1519a) + (interps[["lambda_1544"]](times)*(S1519+(pi_a*I1519a)+(pi_b*I1519b)+(pi_c*I1519c)+(pi_d*I1519d))) - (interps[["lambda_1544"]](times)*(pi_a*I1519a))
  dI1519b <- + (alpha*I1014b*(interps[["fp_1014"]](times)/interps[["fp_1519"]](times))) - (alpha*I1519b) - (kappa_bc*I1519b) + (kappa_ab*I1519a) - (gamma_b*I1519b) - (interps[["lambda_1544"]](times)*(pi_b*I1519b))
  dI1519c <- + (alpha*I1014c*(interps[["fp_1014"]](times)/interps[["fp_1519"]](times))) - (alpha*I1519c) - (kappa_cd*I1519c) + (kappa_bc*I1519b) - (gamma_c*I1519c) - (interps[["lambda_1544"]](times)*(pi_c*I1519c))
  dI1519d <- + (alpha*I1014d*(interps[["fp_1014"]](times)/interps[["fp_1519"]](times))) - (alpha*I1519d) + (kappa_cd*I1519c) - (gamma_d*I1519d) - (interps[["lambda_1544"]](times)*(pi_d*I1519d))

  dS2024  <- + (alpha*S1519*(interps[["fp_1519"]](times)/interps[["fp_2024"]](times))) - (alpha*S2024) - (interps[["lambda_1544"]](times)*S2024) + (gamma_a*I2024a) + (gamma_b*I2024b) + (gamma_c*I2024c) + (gamma_d*I2024d)  
  dI2024a <- + (alpha*I1519a*(interps[["fp_1519"]](times)/interps[["fp_2024"]](times))) - (alpha*I2024a) - (kappa_ab*I2024a) - (gamma_a*I2024a) + (interps[["lambda_1544"]](times)*(S2024+(pi_a*I2024a)+(pi_b*I2024b)+(pi_c*I2024c)+(pi_d*I2024d))) - (interps[["lambda_1544"]](times)*(pi_a*I2024a))
  dI2024b <- + (alpha*I1519b*(interps[["fp_1519"]](times)/interps[["fp_2024"]](times))) - (alpha*I2024b) - (kappa_bc*I2024b) + (kappa_ab*I2024a) - (gamma_b*I2024b) - (interps[["lambda_1544"]](times)*(pi_b*I2024b))
  dI2024c <- + (alpha*I1519c*(interps[["fp_1519"]](times)/interps[["fp_2024"]](times))) - (alpha*I2024c) - (kappa_cd*I2024c) + (kappa_bc*I2024b) - (gamma_c*I2024c) - (interps[["lambda_1544"]](times)*(pi_c*I2024c))
  dI2024d <- + (alpha*I1519d*(interps[["fp_1519"]](times)/interps[["fp_2024"]](times))) - (alpha*I2024d) + (kappa_cd*I2024c) - (gamma_d*I2024d) - (interps[["lambda_1544"]](times)*(pi_d*I2024d))

  dS2529  <- + (alpha*S2024*(interps[["fp_2024"]](times)/interps[["fp_2529"]](times))) - (alpha*S2529) - (interps[["lambda_1544"]](times)*S2529) + (gamma_a*I2529a) + (gamma_b*I2529b) + (gamma_c*I2529c) + (gamma_d*I2529d)  
  dI2529a <- + (alpha*I2024a*(interps[["fp_2024"]](times)/interps[["fp_2529"]](times))) - (alpha*I2529a) - (kappa_ab*I2529a) - (gamma_a*I2529a) + (interps[["lambda_1544"]](times)*(S2529+(pi_a*I2529a)+(pi_b*I2529b)+(pi_c*I2529c)+(pi_d*I2529d))) - (interps[["lambda_1544"]](times)*(pi_a*I2529a))
  dI2529b <- + (alpha*I2024b*(interps[["fp_2024"]](times)/interps[["fp_2529"]](times))) - (alpha*I2529b) - (kappa_bc*I2529b) + (kappa_ab*I2529a) - (gamma_b*I2529b) - (interps[["lambda_1544"]](times)*(pi_b*I2529b))
  dI2529c <- + (alpha*I2024c*(interps[["fp_2024"]](times)/interps[["fp_2529"]](times))) - (alpha*I2529c) - (kappa_cd*I2529c) + (kappa_bc*I2529b) - (gamma_c*I2529c) - (interps[["lambda_1544"]](times)*(pi_c*I2529c))
  dI2529d <- + (alpha*I2024d*(interps[["fp_2024"]](times)/interps[["fp_2529"]](times))) - (alpha*I2529d) + (kappa_cd*I2529c) - (gamma_d*I2529d) - (interps[["lambda_1544"]](times)*(pi_d*I2529d))
  
  dS3034  <- + (alpha*S2529*(interps[["fp_2529"]](times)/interps[["fp_3034"]](times))) - (alpha*S3034) - (interps[["lambda_1544"]](times)*S3034) + (gamma_a*I3034a) + (gamma_b*I3034b) + (gamma_c*I3034c) + (gamma_d*I3034d)  
  dI3034a <- + (alpha*I2529a*(interps[["fp_2529"]](times)/interps[["fp_3034"]](times))) - (alpha*I3034a) - (kappa_ab*I3034a) - (gamma_a*I3034a) + (interps[["lambda_1544"]](times)*(S3034+(pi_a*I3034a)+(pi_b*I3034b)+(pi_c*I3034c)+(pi_d*I3034d))) - (interps[["lambda_1544"]](times)*(pi_a*I3034a))
  dI3034b <- + (alpha*I2529b*(interps[["fp_2529"]](times)/interps[["fp_3034"]](times))) - (alpha*I3034b) - (kappa_bc*I3034b) + (kappa_ab*I3034a) - (gamma_b*I3034b) - (interps[["lambda_1544"]](times)*(pi_b*I3034b))
  dI3034c <- + (alpha*I2529c*(interps[["fp_2529"]](times)/interps[["fp_3034"]](times))) - (alpha*I3034c) - (kappa_cd*I3034c) + (kappa_bc*I3034b) - (gamma_c*I3034c) - (interps[["lambda_1544"]](times)*(pi_c*I3034c))
  dI3034d <- + (alpha*I2529d*(interps[["fp_2529"]](times)/interps[["fp_3034"]](times))) - (alpha*I3034d) + (kappa_cd*I3034c) - (gamma_d*I3034d) - (interps[["lambda_1544"]](times)*(pi_d*I3034d))
  
  dS3539  <- + (alpha*S3034*(interps[["fp_3034"]](times)/interps[["fp_3539"]](times))) - (alpha*S3539) - (interps[["lambda_1544"]](times)*S3539) + (gamma_a*I3539a) + (gamma_b*I3539b) + (gamma_c*I3539c) + (gamma_d*I3539d)  
  dI3539a <- + (alpha*I3034a*(interps[["fp_3034"]](times)/interps[["fp_3539"]](times))) - (alpha*I3539a) - (kappa_ab*I3539a) - (gamma_a*I3539a) + (interps[["lambda_1544"]](times)*(S3539+(pi_a*I3539a)+(pi_b*I3539b)+(pi_c*I3539c)+(pi_d*I3539d))) - (interps[["lambda_1544"]](times)*(pi_a*I3539a))
  dI3539b <- + (alpha*I3034b*(interps[["fp_3034"]](times)/interps[["fp_3539"]](times))) - (alpha*I3539b) - (kappa_bc*I3539b) + (kappa_ab*I3539a) - (gamma_b*I3539b) - (interps[["lambda_1544"]](times)*(pi_b*I3539b))
  dI3539c <- + (alpha*I3034c*(interps[["fp_3034"]](times)/interps[["fp_3539"]](times))) - (alpha*I3539c) - (kappa_cd*I3539c) + (kappa_bc*I3539b) - (gamma_c*I3539c) - (interps[["lambda_1544"]](times)*(pi_c*I3539c))
  dI3539d <- + (alpha*I3034d*(interps[["fp_3034"]](times)/interps[["fp_3539"]](times))) - (alpha*I3539d) + (kappa_cd*I3539c) - (gamma_d*I3539d) - (interps[["lambda_1544"]](times)*(pi_d*I3539d))
  
  dS4044  <- + (alpha*S3539*(interps[["fp_3539"]](times)/interps[["fp_4044"]](times))) - (alpha*S4044) - (interps[["lambda_1544"]](times)*S4044) + (gamma_a*I4044a) + (gamma_b*I4044b) + (gamma_c*I4044c) + (gamma_d*I4044d)  
  dI4044a <- + (alpha*I3539a*(interps[["fp_3539"]](times)/interps[["fp_4044"]](times))) - (alpha*I4044a) - (kappa_ab*I4044a) - (gamma_a*I4044a) + (interps[["lambda_1544"]](times)*(S4044+(pi_a*I4044a)+(pi_b*I4044b)+(pi_c*I4044c)+(pi_d*I4044d))) - (interps[["lambda_1544"]](times)*(pi_a*I4044a))
  dI4044b <- + (alpha*I3539b*(interps[["fp_3539"]](times)/interps[["fp_4044"]](times))) - (alpha*I4044b) - (kappa_bc*I4044b) + (kappa_ab*I4044a) - (gamma_b*I4044b) - (interps[["lambda_1544"]](times)*(pi_b*I4044b))
  dI4044c <- + (alpha*I3539c*(interps[["fp_3539"]](times)/interps[["fp_4044"]](times))) - (alpha*I4044c) - (kappa_cd*I4044c) + (kappa_bc*I4044b) - (gamma_c*I4044c) - (interps[["lambda_1544"]](times)*(pi_c*I4044c))
  dI4044d <- + (alpha*I3539d*(interps[["fp_3539"]](times)/interps[["fp_4044"]](times))) - (alpha*I4044d) + (kappa_cd*I4044c) - (gamma_d*I4044d) - (interps[["lambda_1544"]](times)*(pi_d*I4044d))
  
  dS4549  <- + (alpha*S4044*(interps[["fp_4044"]](times)/interps[["fp_4549"]](times))) - (alpha*S4549) - (interps[["lambda_4500"]](times)*S4549) + (gamma_a*I4549a) + (gamma_b*I4549b) + (gamma_c*I4549c) + (gamma_d*I4549d)  
  dI4549a <- + (alpha*I4044a*(interps[["fp_4044"]](times)/interps[["fp_4549"]](times))) - (alpha*I4549a) - (kappa_ab*I4549a) - (gamma_a*I4549a) + (interps[["lambda_4500"]](times)*(S4549+(pi_a*I4549a)+(pi_b*I4549b)+(pi_c*I4549c)+(pi_d*I4549d))) - (interps[["lambda_4500"]](times)*(pi_a*I4549a))
  dI4549b <- + (alpha*I4044b*(interps[["fp_4044"]](times)/interps[["fp_4549"]](times))) - (alpha*I4549b) - (kappa_bc*I4549b) + (kappa_ab*I4549a) - (gamma_b*I4549b) - (interps[["lambda_4500"]](times)*(pi_b*I4549b))
  dI4549c <- + (alpha*I4044c*(interps[["fp_4044"]](times)/interps[["fp_4549"]](times))) - (alpha*I4549c) - (kappa_cd*I4549c) + (kappa_bc*I4549b) - (gamma_c*I4549c) - (interps[["lambda_4500"]](times)*(pi_c*I4549c))
  dI4549d <- + (alpha*I4044d*(interps[["fp_4044"]](times)/interps[["fp_4549"]](times))) - (alpha*I4549d) + (kappa_cd*I4549c) - (gamma_d*I4549d) - (interps[["lambda_4500"]](times)*(pi_d*I4549d))
  
  dS5054  <- + (alpha*S4549*(interps[["fp_4549"]](times)/interps[["fp_5054"]](times))) - (alpha*S5054) - (interps[["lambda_4500"]](times)*S5054) + (gamma_a*I5054a) + (gamma_b*I5054b) + (gamma_c*I5054c) + (gamma_d*I5054d)  
  dI5054a <- + (alpha*I4549a*(interps[["fp_4549"]](times)/interps[["fp_5054"]](times))) - (alpha*I5054a) - (kappa_ab*I5054a) - (gamma_a*I5054a) + (interps[["lambda_4500"]](times)*(S5054+(pi_a*I5054a)+(pi_b*I5054b)+(pi_c*I5054c)+(pi_d*I5054d))) - (interps[["lambda_4500"]](times)*(pi_a*I5054a))
  dI5054b <- + (alpha*I4549b*(interps[["fp_4549"]](times)/interps[["fp_5054"]](times))) - (alpha*I5054b) - (kappa_bc*I5054b) + (kappa_ab*I5054a) - (gamma_b*I5054b) - (interps[["lambda_4500"]](times)*(pi_b*I5054b))
  dI5054c <- + (alpha*I4549c*(interps[["fp_4549"]](times)/interps[["fp_5054"]](times))) - (alpha*I5054c) - (kappa_cd*I5054c) + (kappa_bc*I5054b) - (gamma_c*I5054c) - (interps[["lambda_4500"]](times)*(pi_c*I5054c))
  dI5054d <- + (alpha*I4549d*(interps[["fp_4549"]](times)/interps[["fp_5054"]](times))) - (alpha*I5054d) + (kappa_cd*I5054c) - (gamma_d*I5054d) - (interps[["lambda_4500"]](times)*(pi_d*I5054d))
  
  dS5559  <- + (alpha*S5054*(interps[["fp_5054"]](times)/interps[["fp_5559"]](times))) - (alpha*S5559) - (interps[["lambda_4500"]](times)*S5559) + (gamma_a*I5559a) + (gamma_b*I5559b) + (gamma_c*I5559c) + (gamma_d*I5559d)  
  dI5559a <- + (alpha*I5054a*(interps[["fp_5054"]](times)/interps[["fp_5559"]](times))) - (alpha*I5559a) - (kappa_ab*I5559a) - (gamma_a*I5559a) + (interps[["lambda_4500"]](times)*(S5559+(pi_a*I5559a)+(pi_b*I5559b)+(pi_c*I5559c)+(pi_d*I5559d))) - (interps[["lambda_4500"]](times)*(pi_a*I5559a))
  dI5559b <- + (alpha*I5054b*(interps[["fp_5054"]](times)/interps[["fp_5559"]](times))) - (alpha*I5559b) - (kappa_bc*I5559b) + (kappa_ab*I5559a) - (gamma_b*I5559b) - (interps[["lambda_4500"]](times)*(pi_b*I5559b))
  dI5559c <- + (alpha*I5054c*(interps[["fp_5054"]](times)/interps[["fp_5559"]](times))) - (alpha*I5559c) - (kappa_cd*I5559c) + (kappa_bc*I5559b) - (gamma_c*I5559c) - (interps[["lambda_4500"]](times)*(pi_c*I5559c))
  dI5559d <- + (alpha*I5054d*(interps[["fp_5054"]](times)/interps[["fp_5559"]](times))) - (alpha*I5559d) + (kappa_cd*I5559c) - (gamma_d*I5559d) - (interps[["lambda_4500"]](times)*(pi_d*I5559d))
  
  dS6064  <- + (alpha*S5559*(interps[["fp_5559"]](times)/interps[["fp_6064"]](times))) - (alpha*S6064) - (interps[["lambda_4500"]](times)*S6064) + (gamma_a*I6064a) + (gamma_b*I6064b) + (gamma_c*I6064c) + (gamma_d*I6064d)  
  dI6064a <- + (alpha*I5559a*(interps[["fp_5559"]](times)/interps[["fp_6064"]](times))) - (alpha*I6064a) - (kappa_ab*I6064a) - (gamma_a*I6064a) + (interps[["lambda_4500"]](times)*(S6064+(pi_a*I6064a)+(pi_b*I6064b)+(pi_c*I6064c)+(pi_d*I6064d))) - (interps[["lambda_4500"]](times)*(pi_a*I6064a))
  dI6064b <- + (alpha*I5559b*(interps[["fp_5559"]](times)/interps[["fp_6064"]](times))) - (alpha*I6064b) - (kappa_bc*I6064b) + (kappa_ab*I6064a) - (gamma_b*I6064b) - (interps[["lambda_4500"]](times)*(pi_b*I6064b))
  dI6064c <- + (alpha*I5559c*(interps[["fp_5559"]](times)/interps[["fp_6064"]](times))) - (alpha*I6064c) - (kappa_cd*I6064c) + (kappa_bc*I6064b) - (gamma_c*I6064c) - (interps[["lambda_4500"]](times)*(pi_c*I6064c))
  dI6064d <- + (alpha*I5559d*(interps[["fp_5559"]](times)/interps[["fp_6064"]](times))) - (alpha*I6064d) + (kappa_cd*I6064c) - (gamma_d*I6064d) - (interps[["lambda_4500"]](times)*(pi_d*I6064d))

  dS6569  <- + (alpha*S6064*(interps[["fp_6064"]](times)/interps[["fp_6569"]](times))) - (alpha*S6569) - (interps[["lambda_4500"]](times)*S6569) + (gamma_a*I6569a) + (gamma_b*I6569b) + (gamma_c*I6569c) + (gamma_d*I6569d)
  dI6569a <- + (alpha*I6064a*(interps[["fp_6064"]](times)/interps[["fp_6569"]](times))) - (alpha*I6569a) - (kappa_ab*I6569a) - (gamma_a*I6569a) + (interps[["lambda_4500"]](times)*(S6569+(pi_a*I6569a)+(pi_b*I6569b)+(pi_c*I6569c)+(pi_d*I6569d))) - (interps[["lambda_4500"]](times)*(pi_a*I6569a))
  dI6569b <- + (alpha*I6064b*(interps[["fp_6064"]](times)/interps[["fp_6569"]](times))) - (alpha*I6569b) - (kappa_bc*I6569b) + (kappa_ab*I6569a) - (gamma_b*I6569b) - (interps[["lambda_4500"]](times)*(pi_b*I6569b))
  dI6569c <- + (alpha*I6064c*(interps[["fp_6064"]](times)/interps[["fp_6569"]](times))) - (alpha*I6569c) - (kappa_cd*I6569c) + (kappa_bc*I6569b) - (gamma_c*I6569c) - (interps[["lambda_4500"]](times)*(pi_c*I6569c))
  dI6569d <- + (alpha*I6064d*(interps[["fp_6064"]](times)/interps[["fp_6569"]](times))) - (alpha*I6569d) + (kappa_cd*I6569c) - (gamma_d*I6569d) - (interps[["lambda_4500"]](times)*(pi_d*I6569d))
  
  dS7074  <- + (alpha*S6569*(interps[["fp_6569"]](times)/interps[["fp_7074"]](times))) - (alpha*S7074) - (interps[["lambda_4500"]](times)*S7074) + (gamma_a*I7074a) + (gamma_b*I7074b) + (gamma_c*I7074c) + (gamma_d*I7074d)
  dI7074a <- + (alpha*I6569a*(interps[["fp_6569"]](times)/interps[["fp_7074"]](times))) - (alpha*I7074a) - (kappa_ab*I7074a) - (gamma_a*I7074a) + (interps[["lambda_4500"]](times)*(S7074+(pi_a*I7074a)+(pi_b*I7074b)+(pi_c*I7074c)+(pi_d*I7074d))) - (interps[["lambda_4500"]](times)*(pi_a*I7074a))
  dI7074b <- + (alpha*I6569b*(interps[["fp_6569"]](times)/interps[["fp_7074"]](times))) - (alpha*I7074b) - (kappa_bc*I7074b) + (kappa_ab*I7074a) - (gamma_b*I7074b) - (interps[["lambda_4500"]](times)*(pi_b*I7074b))
  dI7074c <- + (alpha*I6569c*(interps[["fp_6569"]](times)/interps[["fp_7074"]](times))) - (alpha*I7074c) - (kappa_cd*I7074c) + (kappa_bc*I7074b) - (gamma_c*I7074c) - (interps[["lambda_4500"]](times)*(pi_c*I7074c))
  dI7074d <- + (alpha*I6569d*(interps[["fp_6569"]](times)/interps[["fp_7074"]](times))) - (alpha*I7074d) + (kappa_cd*I7074c) - (gamma_d*I7074d) - (interps[["lambda_4500"]](times)*(pi_d*I7074d))
  
  dS7579  <- + (alpha*S7074*(interps[["fp_7074"]](times)/interps[["fp_7579"]](times))) - (alpha*S7579) - (interps[["lambda_4500"]](times)*S7579) + (gamma_a*I7579a) + (gamma_b*I7579b) + (gamma_c*I7579c) + (gamma_d*I7579d)
  dI7579a <- + (alpha*I7074a*(interps[["fp_7074"]](times)/interps[["fp_7579"]](times))) - (alpha*I7579a) - (kappa_ab*I7579a) - (gamma_a*I7579a) + (interps[["lambda_4500"]](times)*(S7579+(pi_a*I7579a)+(pi_b*I7579b)+(pi_c*I7579c)+(pi_d*I7579d))) - (interps[["lambda_4500"]](times)*(pi_a*I7579a))
  dI7579b <- + (alpha*I7074b*(interps[["fp_7074"]](times)/interps[["fp_7579"]](times))) - (alpha*I7579b) - (kappa_bc*I7579b) + (kappa_ab*I7579a) - (gamma_b*I7579b) - (interps[["lambda_4500"]](times)*(pi_b*I7579b))
  dI7579c <- + (alpha*I7074c*(interps[["fp_7074"]](times)/interps[["fp_7579"]](times))) - (alpha*I7579c) - (kappa_cd*I7579c) + (kappa_bc*I7579b) - (gamma_c*I7579c) - (interps[["lambda_4500"]](times)*(pi_c*I7579c))
  dI7579d <- + (alpha*I7074d*(interps[["fp_7074"]](times)/interps[["fp_7579"]](times))) - (alpha*I7579d) + (kappa_cd*I7579c) - (gamma_d*I7579d) - (interps[["lambda_4500"]](times)*(pi_d*I7579d))
  
  dS8000  <- + (alpha*S7579*(interps[["fp_7579"]](times)/interps[["fp_8000"]](times))) - (interps[["lambda_4500"]](times)*S8000) + (gamma_a*I8000a) + (gamma_b*I8000b) + (gamma_c*I8000c) + (gamma_d*I8000d)
  dI8000a <- + (alpha*I7579a*(interps[["fp_7579"]](times)/interps[["fp_8000"]](times))) - (kappa_ab*I8000a) - (gamma_a*I8000a) + (interps[["lambda_4500"]](times)*(S8000+(pi_a*I8000a)+(pi_b*I8000b)+(pi_c*I8000c)+(pi_d*I8000d))) - (interps[["lambda_4500"]](times)*(pi_a*I8000a))
  dI8000b <- + (alpha*I7579b*(interps[["fp_7579"]](times)/interps[["fp_8000"]](times))) - (kappa_bc*I8000b) + (kappa_ab*I8000a) - (gamma_b*I8000b) - (interps[["lambda_4500"]](times)*(pi_b*I8000b))
  dI8000c <- + (alpha*I7579c*(interps[["fp_7579"]](times)/interps[["fp_8000"]](times))) - (kappa_cd*I8000c) + (kappa_bc*I8000b) - (gamma_c*I8000c) - (interps[["lambda_4500"]](times)*(pi_c*I8000c))
  dI8000d <- + (alpha*I7579d*(interps[["fp_7579"]](times)/interps[["fp_8000"]](times))) + (kappa_cd*I8000c) - (gamma_d*I8000d) - (interps[["lambda_4500"]](times)*(pi_d*I8000d))
  
  out <- list(c(
    dS0004,dI0004a,dI0004b,dI0004c,dI0004d,dS0509,dI0509a,dI0509b,dI0509c,dI0509d,dS1014,dI1014a,dI1014b,dI1014c,dI1014d,
    dS1519,dI1519a,dI1519b,dI1519c,dI1519d,dS2024,dI2024a,dI2024b,dI2024c,dI2024d,dS2529,dI2529a,dI2529b,dI2529c,dI2529d,
    dS3034,dI3034a,dI3034b,dI3034c,dI3034d,dS3539,dI3539a,dI3539b,dI3539c,dI3539d,dS4044,dI4044a,dI4044b,dI4044c,dI4044d,
    dS4549,dI4549a,dI4549b,dI4549c,dI4549d,dS5054,dI5054a,dI5054b,dI5054c,dI5054d,dS5559,dI5559a,dI5559b,dI5559c,dI5559d,
    dS6064,dI6064a,dI6064b,dI6064c,dI6064d,dS6569,dI6569a,dI6569b,dI6569c,dI6569d,dS7074,dI7074a,dI7074b,dI7074c,dI7074d,
    dS7579,dI7579a,dI7579b,dI7579c,dI7579d,dS8000,dI8000a,dI8000b,dI8000c,dI8000d))
  return(out)
}

# 3. Model run ==========
times <- seq(from = 0, to = 100, by = 1)
isos <- unique(ARI$iso3) # 171 countries
list_df <- list()

tic()
for (c in 1:(length(isos))){
  iso <- isos[c]
  print(iso)
  
  parms <- as.data.table(parameters) %>% filter(iso3 == iso)
  ari <- as.data.table(ARI) %>% filter(iso3 == iso)

  maxage <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,90)
  
  # Proportion viable (i.e. not self-cleared or recovered) 
  via_p1 <- 1 - 0.809 # Year 1
  via_p2 <- 1 - 0.919 # Year 2
  via_p3 <- 1 - 0.972 # Year 10
  
  state <- c(S0004 = exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*2.5)), 
             I0004a = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*2.5)))*(1/(sum(1+1+2+(maxage[1]-3)))), 
             I0004b = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*2.5)))*(1/(sum(1+1+2+(maxage[1]-3))))*via_p1, 
             I0004c = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*2.5)))*(2/(sum(1+1+2+(maxage[1]-3))))*via_p2, 
             I0004d = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*2.5)))*((maxage[1]-3)/(sum(1+1+2+(maxage[1]-3))))*via_p3, 
             S0509 = exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*7.5)), 
             I0509a = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*7.5)))*(1/(sum(1+1+2+(maxage[2]-3)))), 
             I0509b = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*7.5)))*(1/(sum(1+1+2+(maxage[2]-3))))*via_p1, 
             I0509c = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*7.5)))*(2/(sum(1+1+2+(maxage[2]-3))))*via_p2, 
             I0509d = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*7.5)))*((maxage[2]-3)/(sum(1+1+2+(maxage[2]-3))))*via_p3,
             S1014 = exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*12.5)), 
             I1014a = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*12.5)))*(1/(sum(1+1+2+(maxage[3]-3)))), 
             I1014b = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*12.5)))*(1/(sum(1+1+2+(maxage[3]-3))))*via_p1, 
             I1014c = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*12.5)))*(2/(sum(1+1+2+(maxage[3]-3))))*via_p2,
             I1014d = (1-exp((-(ari[year == 1950 & ageARI == "00-14", ari][1])*12.5)))*((maxage[3]-3)/(sum(1+1+2+(maxage[3]-3))))*via_p3, 
             S1519 = exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*17.5)), 
             I1519a = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*17.5)))*(1/(sum(1+1+2+(maxage[4]-3)))), 
             I1519b = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*17.5)))*(1/(sum(1+1+2+(maxage[4]-3))))*via_p1, 
             I1519c = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*17.5)))*(2/(sum(1+1+2+(maxage[4]-3))))*via_p2,
             I1519d = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*17.5)))*((maxage[4]-3)/(sum(1+1+2+(maxage[4]-3))))*via_p3,
             S2024 = exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*22.5)), 
             I2024a = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*22.5)))*(1/(sum(1+1+2+(maxage[5]-3)))), 
             I2024b = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*22.5)))*(1/(sum(1+1+2+(maxage[5]-3))))*via_p1, 
             I2024c = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*22.5)))*(2/(sum(1+1+2+(maxage[5]-3))))*via_p2,
             I2024d = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*22.5)))*((maxage[5]-3)/(sum(1+1+2+(maxage[5]-3))))*via_p3, 
             S2529 = exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*27.5)), 
             I2529a = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*27.5)))*(1/(sum(1+1+2+(maxage[6]-3)))), 
             I2529b = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*27.5)))*(1/(sum(1+1+2+(maxage[6]-3))))*via_p1,
             I2529c = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*27.5)))*(2/(sum(1+1+2+(maxage[6]-3))))*via_p2, 
             I2529d = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*27.5)))*((maxage[6]-3)/(sum(1+1+2+(maxage[6]-3))))*via_p3,
             S3034 = exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*32.5)), 
             I3034a = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*32.5)))*(1/(sum(1+1+2+(maxage[7]-3)))), 
             I3034b = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*32.5)))*(1/(sum(1+1+2+(maxage[7]-3))))*via_p1, 
             I3034c = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*32.5)))*(2/(sum(1+1+2+(maxage[7]-3))))*via_p2, 
             I3034d = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*32.5)))*((maxage[7]-3)/(sum(1+1+2+(maxage[7]-3))))*via_p3, 
             S3539 = exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*37.5)), 
             I3539a = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*37.5)))*(1/(sum(1+1+2+(maxage[8]-3)))), 
             I3539b = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*37.5)))*(1/(sum(1+1+2+(maxage[8]-3))))*via_p1,
             I3539c = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*37.5)))*(2/(sum(1+1+2+(maxage[8]-3))))*via_p2,
             I3539d = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*37.5)))*((maxage[8]-3)/(sum(1+1+2+(maxage[8]-3))))*via_p3,
             S4044 = exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*42.5)), 
             I4044a = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*42.5)))*(1/(sum(1+1+2+(maxage[9]-3)))), 
             I4044b = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*42.5)))*(1/(sum(1+1+2+(maxage[9]-3))))*via_p1, 
             I4044c = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*42.5)))*(2/(sum(1+1+2+(maxage[9]-3))))*via_p2,
             I4044d = (1-exp((-(ari[year == 1950 & ageARI == "15-44", ari][1])*42.5)))*((maxage[9]-3)/(sum(1+1+2+(maxage[9]-3))))*via_p3, 
             S4549 = exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*47.5)), 
             I4549a = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*47.5)))*(1/(sum(1+1+2+(maxage[10]-3)))), 
             I4549b = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*47.5)))*(1/(sum(1+1+2+(maxage[10]-3))))*via_p1,
             I4549c = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*47.5)))*(2/(sum(1+1+2+(maxage[10]-3))))*via_p2,
             I4549d = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*47.5)))*((maxage[10]-3)/(sum(1+1+2+(maxage[10]-3))))*via_p3,
             S5054 = exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*52.5)), 
             I5054a = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*52.5)))*(1/(sum(1+1+2+(maxage[11]-3)))), 
             I5054b = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*52.5)))*(1/(sum(1+1+2+(maxage[11]-3))))*via_p1, 
             I5054c = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*52.5)))*(2/(sum(1+1+2+(maxage[11]-3))))*via_p2, 
             I5054d = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*52.5)))*((maxage[11]-3)/(sum(1+1+2+(maxage[11]-3))))*via_p3, 
             S5559 = exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*57.5)), 
             I5559a = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*57.5)))*(1/(sum(1+1+2+(maxage[12]-3)))), 
             I5559b = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*57.5)))*(1/(sum(1+1+2+(maxage[12]-3))))*via_p1, 
             I5559c = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*57.5)))*(2/(sum(1+1+2+(maxage[12]-3))))*via_p2,
             I5559d = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*57.5)))*((maxage[12]-3)/(sum(1+1+2+(maxage[12]-3))))*via_p3,
             S6064 = exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*62.5)), 
             I6064a = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*62.5)))*(1/(sum(1+1+2+(maxage[13]-3)))), 
             I6064b = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*62.5)))*(1/(sum(1+1+2+(maxage[13]-3))))*via_p1,
             I6064c = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*62.5)))*(2/(sum(1+1+2+(maxage[13]-3))))*via_p2,
             I6064d = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*62.5)))*((maxage[13]-3)/(sum(1+1+2+(maxage[13]-3))))*via_p3, 
             S6569 = exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*67.5)), 
             I6569a = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*67.5)))*(1/(sum(1+1+2+(maxage[14]-3)))), 
             I6569b = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*67.5)))*(1/(sum(1+1+2+(maxage[14]-3))))*via_p1,
             I6569c = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*67.5)))*(2/(sum(1+1+2+(maxage[14]-3))))*via_p2,
             I6569d = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*67.5)))*((maxage[14]-3)/(sum(1+1+2+(maxage[14]-3))))*via_p3,
             S7074 = exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*72.5)), 
             I7074a = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*72.5)))*(1/(sum(1+1+2+(maxage[15]-3)))), 
             I7074b = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*72.5)))*(1/(sum(1+1+2+(maxage[15]-3))))*via_p1,
             I7074c = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*72.5)))*(2/(sum(1+1+2+(maxage[15]-3))))*via_p2, 
             I7074d = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*72.5)))*((maxage[15]-3)/(sum(1+1+2+(maxage[15]-3))))*via_p3,
             S7579 = exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*77.5)), 
             I7579a = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*77.5)))*(1/(sum(1+1+2+(maxage[16]-3)))), 
             I7579b = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*77.5)))*(1/(sum(1+1+2+(maxage[16]-3))))*via_p1, 
             I7579c = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*77.5)))*(2/(sum(1+1+2+(maxage[16]-3))))*via_p2, 
             I7579d = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*77.5)))*((maxage[16]-3)/(sum(1+1+2+(maxage[16]-3))))*via_p3,
             S8000 = exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*85)), 
             I8000a = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*85)))*(1/(sum(1+1+2+(maxage[17]-3)))), 
             I8000b = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*85)))*(1/(sum(1+1+2+(maxage[17]-3))))*via_p1,
             I8000c = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*85)))*(2/(sum(1+1+2+(maxage[17]-3))))*via_p2,
             I8000d = (1-exp((-(ari[year == 1950 & ageARI == "45+", ari][1])*85)))*((maxage[17]-3)/(sum(1+1+2+(maxage[17]-3))))*via_p3)
  
  output_raw <- ode(y = state, times = times, func = sis, 
                    parms = parms, method = "lsoda")
  
  df <- data.frame("iso3" = iso, cbind(output_raw))
  list_df[[c]] <- df
}

mtb <- do.call("rbind",list_df)
toc()

mtbi <- mtb %>% # Check, each age group sum should be 1
  mutate(N0004 = rowSums(select(., contains('0004')), na.rm = TRUE),
         N0509 = rowSums(select(., contains('0509')), na.rm = TRUE),
         N1014 = rowSums(select(., contains('1014')), na.rm = TRUE),
         N1519 = rowSums(select(., contains('1519')), na.rm = TRUE),
         N2024 = rowSums(select(., contains('2024')), na.rm = TRUE),      
         N2529 = rowSums(select(., contains('2529')), na.rm = TRUE),
         N3034 = rowSums(select(., contains('3034')), na.rm = TRUE),
         N3539 = rowSums(select(., contains('3539')), na.rm = TRUE),
         N4044 = rowSums(select(., contains('4044')), na.rm = TRUE),
         N4549 = rowSums(select(., contains('4549')), na.rm = TRUE),
         N5054 = rowSums(select(., contains('5054')), na.rm = TRUE),
         N5559 = rowSums(select(., contains('5559')), na.rm = TRUE),
         N6064 = rowSums(select(., contains('6064')), na.rm = TRUE),
         N6569 = rowSums(select(., contains('6569')), na.rm = TRUE),
         N7074 = rowSums(select(., contains('7074')), na.rm = TRUE),
         N7579 = rowSums(select(., contains('7579')), na.rm = TRUE),
         N8000 = rowSums(select(., contains('8000')), na.rm = TRUE))

export(mtb, here("data","mtb","mMtb_rev_mix_pop_sc.Rdata"))

         