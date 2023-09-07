## Analysis code for Schwalb et al. 2023
## Distributed under CC BY 4.0
## RScript 08: Miscellaneous.R

## Lambda function - Model.R
lambda <- function(y,lari){
  lambdaa <- exp(ari[year == y & acat == '0-14',lari])
  lambdab <- exp(ari[year == y & acat == '15-44',lari])
  lambdac <- exp(ari[year == y & acat == '45+',lari])
  return(c(lambdaa,lambdab,lambdac))
}

lambdas <- lambda(y = years[y], lari = "lari")
lambdasu <- lambda(y = years[y], lari = "upper")
lambdasl <- lambda(y = years[y], lari = "lower")

## Difference equation - Model.R
countries <- unique(ARI$iso3) # 203 countries
list_df <- list()

for (c in 1:(length(countries))){
  country <- countries[c]
  print(country)
  ari <- filter(ARI,iso3 == country)
  years <- unique(ari$year)
  Sa <- c(); Sb <- c(); Sc <- c(); St <- c()
  Ia <- c(); Ib <- c(); Ic <- c(); It <- c()
  gamma <- 0.033 # Emery et al 2021
  alphaa <- 1/15
  alphab <- 1/30
  
  for (y in 1:length(years)){
    time <- filter(ari,year == years[y])
    if(y == 1){
      # Set up the initial conditions
      A <- ari[year == 1950 & acat == "0-14", pop]
      B <- ari[year == 1950 & acat == "15-44", pop]
      C <- ari[year == 1950 & acat == "45+", pop]
      
      Ia[y] <- A * 0.1
      Ib[y] <- B * 0.1
      Ic[y] <- C * 0.1
      
      Sa[y] <- A-Ia[y]
      Sb[y] <- B-Ib[y]
      Sc[y] <- C-Ic[y]
      
      St[y] = Sa[y] + Sb[y] + Sc[y]
      It[y] = Ia[y] + Ib[y] + Ic[y]
      
    } else {
      birth <- ari[year == years[y] & acat == '0-14',births]
      
      morta <- ari[year == years[y] & acat == '0-14',mortrate]
      mortb <- ari[year == years[y] & acat == '15-44',mortrate]
      mortc <- ari[year == years[y] & acat == '45+',mortrate]
      
      lambdaa <- exp(ari[year == years[y] & acat == '0-14',lari])
      lambdab <- exp(ari[year == years[y] & acat == '15-44',lari])
      lambdac <- exp(ari[year == years[y] & acat == '45+',lari])
      
      #lambdaa <- exp(ari[year == years[y] & acat == '0-14',lower])
      #lambdab <- exp(ari[year == years[y] & acat == '15-44',lower])
      #lambdac <- exp(ari[year == years[y] & acat == '45+',lower])
      
      #lambdaa <- exp(ari[year == years[y] & acat == '0-14',upper])
      #lambdab <- exp(ari[year == years[y] & acat == '15-44',upper])
      #lambdac <- exp(ari[year == years[y] & acat == '45+',upper])
      
      Sa[y] = Sa[y-1] - lambdaa*Sa[y-1] + gamma*Ia[y-1] - morta*Sa[y-1] - alphaa*Sa[y-1] + birth
      Ia[y] = Ia[y-1] + lambdaa*Sa[y-1] - gamma*Ia[y-1] - morta*Ia[y-1] - alphaa*Ia[y-1]
      Sb[y] = Sb[y-1] - lambdaa*Sb[y-1] + gamma*Ib[y-1] - mortb*Sb[y-1] + alphaa*Sa[y-1] - alphab*Sb[y-1]
      Ib[y] = Ib[y-1] + lambdaa*Sb[y-1] - gamma*Ib[y-1] - mortb*Ib[y-1] + alphaa*Ia[y-1] - alphab*Ib[y-1]
      Sc[y] = Sc[y-1] - lambdaa*Sc[y-1] + gamma*Ic[y-1] - mortc*Sc[y-1] + alphab*Sb[y-1]
      Ic[y] = Ic[y-1] + lambdaa*Sc[y-1] - gamma*Ic[y-1] - mortc*Ic[y-1] + alphab*Ib[y-1]
      St[y] = Sa[y] + Sb[y] + Sc[y]
      It[y] = Ia[y] + Ib[y] + Ic[y]
    }
  }
  df <- data.frame("iso3"=country, "year"=years, cbind(Sa,Ia,Sb,Ib,Sc,Ic,St,It))
  list_df[[c]] <- df
}

mtb <- do.call("rbind",list_df)
export(mtb,here("data","mtb","Mtb.Rdata")) # Save data frame
#export(mtb,here("data","mtb","Mtblo.Rdata")) # Save data frame
#export(mtb,here("data","mtb","Mtbhi.Rdata")) # Save data frame
rm(list = ls())

# Melt parameters - Model.R
eARI <- reshape2::melt(ARI[,c("year","iso3","acat","lari","births","mortrate","gamma","alphax","alphay")],
                       id=c("year","iso3","acat"))
eARI <- na.omit(eARI)
paramsevent <- c()

for(i in 1:nrow(eARI)){
  if(eARI[i,"acat"]=="0-14" & eARI[i,"variable"]=="lari") paramsevent[i] <- "lambdaa"
  if(eARI[i,"acat"]=="15-44" & eARI[i,"variable"]=="lari") paramsevent[i] <- "lambdab"
  if(eARI[i,"acat"]=="45+" & eARI[i,"variable"]=="lari") paramsevent[i] <- "lambdac"
  if(eARI[i,"acat"]=="0-14" & eARI[i,"variable"]=="mortrate") paramsevent[i] <- "mua"
  if(eARI[i,"acat"]=="15-44" & eARI[i,"variable"]=="mortrate") paramsevent[i] <- "mub"
  if(eARI[i,"acat"]=="45+" & eARI[i,"variable"]=="mortrate") paramsevent[i] <- "muc"
  if(eARI[i,"acat"]=="0-14" & eARI[i,"variable"]=="births") paramsevent[i] <- "theta"
  if(eARI[i,"acat"]=="0-14" & eARI[i,"variable"]=="gamma") paramsevent[i] <- "gamma"
  if(eARI[i,"acat"]=="15-44" & eARI[i,"variable"]=="gamma") paramsevent[i] <- "gamma"
  if(eARI[i,"acat"]=="45+" & eARI[i,"variable"]=="gamma") paramsevent[i] <- "gamma"
  if(eARI[i,"acat"]=="0-14" & eARI[i,"variable"]=="alphax") paramsevent[i] <- "alphax"
  if(eARI[i,"acat"]=="15-44" & eARI[i,"variable"]=="alphax") paramsevent[i] <- "alphax"
  if(eARI[i,"acat"]=="45+" & eARI[i,"variable"]=="alphax") paramsevent[i] <- "alphax"
  if(eARI[i,"acat"]=="0-14" & eARI[i,"variable"]=="alphay") paramsevent[i] <- "alphay"
  if(eARI[i,"acat"]=="15-44" & eARI[i,"variable"]=="alphay") paramsevent[i] <- "alphay"
  if(eARI[i,"acat"]=="45+" & eARI[i,"variable"]=="alphay") paramsevent[i] <- "alphay"
}

eARI$var <- paramsevent
eARI$acat <- NULL
eARI$variable <- NULL
colnames(eARI)[1] <- c("time")
eARI$method <- "rep"
eARI <- eARI[!duplicated(eARI),]

write.csv(eARI,"eARI.csv",row.names = FALSE)

## Population and global estimates - MtbBurden.R

ggplot(mtb_glob, aes(x=year,y=It)) +
  geom_line() +
  scale_x_continuous("year", expand=c(0, 0), breaks = seq(1950, 2050, 10)) +
  scale_y_continuous("# viable Mtb infections", expand=c(0, 0)) +
  coord_cartesian(xlim = c(1949,2051))

ggplot(mtb_glob, aes(x=year,y=pIt)) +
  geom_line() +
  scale_x_continuous("year", expand=c(0, 0), breaks = seq(1950, 2050, 10)) +
  scale_y_continuous("% viable Mtb infections", expand=c(0, 0), breaks = seq(0,0.12,0.01)) +
  coord_cartesian(xlim = c(1949,2051))

POP <- POP %>%
  rename(iso3 = ISO3_code, year = Time, acat = AgeGrp, pop = PopTotal) %>%
  select(iso3,year,acat,pop) %>%
  mutate_at(c('iso3'), ~na_if(., '')) %>%
  filter(!is.na(iso3))

POPt <- POP %>%
  group_by(iso3,year) %>%
  summarise(pop = sum(pop)*1e3) %>%
  inner_join(WHOkey, by = "iso3") %>%
  rename(reg = g_whoregion) %>%
  select(iso3,reg,year,pop)

POPgp <- POP %>% 
  mutate(acat = case_when(
    acat %in% c('0-4','5-9','10-14') ~ '0-14',
    acat %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15-44',
    acat %in% c('45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-99','100+') ~ '45+')) %>%
  group_by(iso3,year,acat) %>%
  summarise(pop = sum(pop)*1e3) %>% 
  inner_join(WHOkey, by = "iso3") %>%
  rename(reg = g_whoregion) %>%
  select(iso3,reg,year,acat,pop) %>% 
  pivot_wider(names_from = acat, values_from = pop) %>% 
  rename(popa = "0-14", popb = "15-44", popc = "45+")
rm(POP,WHOkey)

# Extracting ARI from high prevalence settings - DataPrep.R
IHME <- as.data.table(import(here("data","global","Global TB prevalence IHME per country (1990-2019).csv")))
ARI <- as.data.table(import(here("data","ari","ARI.Rdata")))
ARIrev <- as.data.table(import(here("data","ari","ARIrev.Rdata")))

fARI <- ARI %>% 
  filter(type == "Prevalence estimate") %>% 
  select(iso3,year,ari)

fARIrev <- ARIrev %>% 
  filter(type == "Prevalence estimate") %>%
  mutate(arirev = ari) %>% 
  select(iso3,year,arirev) %>% 
  left_join(fARI, by= c("iso3","year"))

IHMEhighTB <- IHME %>% 
  filter(e_prev_100k >= 480) %>% 
  select(iso3,year,e_prev_100k,e_prev_100k_lo,e_prev_100k_hi) %>% 
  inner_join(fARIrev, by= c("iso3","year"))

write_csv(IHMEhighTB, here("data","others","High TB Prev - ARIs.csv"), append = FALSE)

# Differential equations - Model.R
dS0004  <- -(lambda_0014*S0004) + (gammax*I0004a) + (gammax*I0004b) + (gammay*I0004c) - (mu_0004*S0004) - (alpha*S0004) + 
  (theta*(S0004+I0004a+I0004b+I0004c))
dI0004a <-  (lambda_0014*S0004) - (gammax*I0004a) - (delta*I0004a) - (mu_0004*I0004a) - (alpha*I0004a)
dI0004b <- -(gammax*I0004b) + (delta*I0004a) - (delta*I0004b) - (mu_0004*I0004b) - (alpha*I0004b)
dI0004c <- -(gammay*I0004c) + (delta*I0004b) - (mu_0004*I0004c) - (alpha*I0004c)

dS0509  <- -(lambda_0014*S0509) + (gammax*I0509a) + (gammax*I0509b) + (gammay*I0509c) - (mu_0509*S0509) - (alpha*S0509) + (alpha*S0004)
dI0509a <-  (lambda_0014*S0509) - (gammax*I0509a) - (delta*I0509a) - (mu_0509*I0509a) - (alpha*I0509a)
dI0509b <- -(gammax*I0509b) + (delta*I0509a) - (delta*I0509b) - (mu_0509*I0509b) - (alpha*I0509b) + (alpha*I0004a)
dI0509c <- -(gammay*I0509c) + (delta*I0509b) - (mu_0509*I0509c) - (alpha*I0509c) + (alpha*I0004b) + (alpha*I0004c)

dS1014  <- -(lambda_0014*S1014) + (gammax*I1014a) + (gammax*I1014b) + (gammay*I1014c) - (mu_1014*S1014) - (alpha*S1014) + (alpha*S0509)
dI1014a <-  (lambda_0014*S1014) - (gammax*I1014a) - (delta*I1014a) - (mu_1014*I1014a) - (alpha*I1014a)
dI1014b <- -(gammax*I1014b) + (delta*I1014a) - (delta*I1014b) - (mu_1014*I1014b) - (alpha*I1014b) + (alpha*I0509a)
dI1014c <- -(gammay*I1014c) + (delta*I1014b) - (mu_1014*I1014c) - (alpha*I1014c) + (alpha*I0509b) + (alpha*I0509c)

dS1519  <- -(lambda_1544*S1519) + (gammax*I1519a) + (gammax*I1519b) + (gammay*I1519c) - (mu_1519*S1519) - (alpha*S1519) + (alpha*S1014)
dI1519a <-  (lambda_1544*S1519) - (gammax*I1519a) - (delta*I1519a) - (mu_1519*I1519a) - (alpha*I1519a)
dI1519b <- -(gammax*I1519b) + (delta*I1519a) - (delta*I1519b) - (mu_1519*I1519b) - (alpha*I1519b) + (alpha*I1014a)
dI1519c <- -(gammay*I1519c) + (delta*I1014b) - (mu_1519*I1519c) - (alpha*I1519c) + (alpha*I1014b) + (alpha*I1014c)

dS2024  <- -(lambda_1544*S2024) + (gammax*I2024a) + (gammax*I2024b) + (gammay*I2024c) - (mu_2024*S2024) - (alpha*S2024) + (alpha*S1519)
dI2024a <-  (lambda_1544*S2024) - (gammax*I2024a) - (delta*I2024a) - (mu_2024*I2024a) - (alpha*I2024a)
dI2024b <- -(gammax*I2024b) + (delta*I2024a) - (delta*I2024b) - (mu_2024*I2024b) - (alpha*I2024b) + (alpha*I1519a)
dI2024c <- -(gammay*I2024c) + (delta*I2024b) - (mu_2024*I2024c) - (alpha*I2024c) + (alpha*I1519b) + (alpha*I1519c)

dS2529  <- -(lambda_1544*S2529) + (gammax*I2529a) + (gammax*I2529b) + (gammay*I2529c) - (mu_2529*S2529) - (alpha*S2529) + (alpha*S2024)
dI2529a <-  (lambda_1544*S2529) - (gammax*I2529a) - (delta*I2529a) - (mu_2529*I2529a) - (alpha*I2529a)
dI2529b <- -(gammax*I2529b) + (delta*I2529a) - (delta*I2529b) - (mu_2529*I2529b) - (alpha*I2529b) + (alpha*I2024a)
dI2529c <- -(gammay*I2529c) + (delta*I2529b) - (mu_2529*I2529c) - (alpha*I2529c) + (alpha*I2024b) + (alpha*I2024c)

dS3034  <- -(lambda_1544*S3034) + (gammax*I3034a) + (gammax*I3034b) + (gammay*I3034c) - (mu_3034*S3034) - (alpha*S3034) + (alpha*S2529)
dI3034a <-  (lambda_1544*S3034) - (gammax*I3034a) - (delta*I3034a) - (mu_3034*I3034a) - (alpha*I3034a)
dI3034b <- -(gammax*I3034b) + (delta*I3034a) - (delta*I3034b) - (mu_3034*I3034b) - (alpha*I3034b) + (alpha*I2529a)
dI3034c <- -(gammay*I3034c) + (delta*I3034b) - (mu_3034*I3034c) - (alpha*I3034c) + (alpha*I2529b) + (alpha*I2529c)

dS3539  <- -(lambda_1544*S3539) + (gammax*I3539a) + (gammax*I3539b) + (gammay*I3539c) - (mu_3539*S3539) - (alpha*S3539) + (alpha*S3034)
dI3539a <-  (lambda_1544*S3539) - (gammax*I3539a) - (delta*I3539a) - (mu_3539*I3539a) - (alpha*I3539a)
dI3539b <- -(gammax*I3539b) + (delta*I3539a) - (delta*I3539b) - (mu_3539*I3539b) - (alpha*I3539b) + (alpha*I3034a)
dI3539c <- -(gammay*I3539c) + (delta*I3539b) - (mu_3539*I3539c) - (alpha*I3539c) + (alpha*I3034b) + (alpha*I3034c)

dS4044  <- -(lambda_1544*S4044) + (gammax*I4044a) + (gammax*I4044b) + (gammay*I4044c) - (mu_4044*S4044) - (alpha*S4044) + (alpha*S3539)
dI4044a <-  (lambda_1544*S4044) - (gammax*I4044a) - (delta*I4044a) - (mu_4044*I4044a) - (alpha*I4044a)
dI4044b <- -(gammax*I4044b) + (delta*I4044a) - (delta*I4044b) - (mu_4044*I4044b) - (alpha*I4044b) + (alpha*I3539a)
dI4044c <- -(gammay*I4044c) + (delta*I4044b) - (mu_4044*I4044c) - (alpha*I4044c) + (alpha*I3539b) + (alpha*I3539c)

dS4549  <- -(lambda_4500*S4549) + (gammax*I4549a) + (gammax*I4549b) + (gammay*I4549c) - (mu_4549*S4549) - (alpha*S4549) + (alpha*S4044)
dI4549a <-  (lambda_4500*S4549) - (gammax*I4549a) - (delta*I4549a) - (mu_4549*I4549a) - (alpha*I4549a)
dI4549b <- -(gammax*I4549b) + (delta*I4549a) - (delta*I4549b) - (mu_4549*I4549b) - (alpha*I4549b) + (alpha*I4044a)
dI4549c <- -(gammay*I4549c) + (delta*I4549b) - (mu_4549*I4549c) - (alpha*I4549c) + (alpha*I4044b) + (alpha*I4044c)

dS5054  <- -(lambda_4500*S5054) + (gammax*I5054a) + (gammax*I5054b) + (gammay*I5054c) - (mu_5054*S5054) - (alpha*S5054) + (alpha*S4549)
dI5054a <-  (lambda_4500*S5054) - (gammax*I5054a) - (delta*I5054a) - (mu_5054*I5054a) - (alpha*I5054a)
dI5054b <- -(gammax*I5054b) + (delta*I5054a) - (delta*I5054b) - (mu_5054*I5054b) - (alpha*I5054b) + (alpha*I4549a)
dI5054c <- -(gammay*I5054c) + (delta*I5054b) - (mu_5054*I5054c) - (alpha*I5054c) + (alpha*I4549b) + (alpha*I4549c)

dS5559  <- -(lambda_4500*S5559) + (gammax*I5559a) + (gammax*I5559b) + (gammay*I5559c) - (mu_5559*S5559) - (alpha*S5559) + (alpha*S5054)
dI5559a <-  (lambda_4500*S5559) - (gammax*I5559a) - (delta*I5559a) - (mu_5559*I5559a) - (alpha*I5559a)
dI5559b <- -(gammax*I5559b) + (delta*I5559a) - (delta*I5559b) - (mu_5559*I5559b) - (alpha*I5559b) + (alpha*I5054a)
dI5559c <- -(gammay*I5559c) + (delta*I5559b) - (mu_5559*I5559c) - (alpha*I5559c) + (alpha*I5054b) + (alpha*I5054c)

dS6064  <- -(lambda_4500*S6064) + (gammax*I6064a) + (gammax*I6064b) + (gammay*I6064c) - (mu_6064*S6064) - (alpha*S6064) + (alpha*S5559)
dI6064a <-  (lambda_4500*S6064) - (gammax*I6064a) - (delta*I6064a) - (mu_6064*I6064a) - (alpha*I6064a)
dI6064b <- -(gammax*I6064b) + (delta*I6064a) - (delta*I6064b) - (mu_6064*I6064b) - (alpha*I6064b) + (alpha*I5559a)
dI6064c <- -(gammay*I6064c) + (delta*I6064b) - (mu_6064*I6064c) - (alpha*I6064c) + (alpha*I5559b) + (alpha*I5559c)

dS6569  <- -(lambda_4500*S6569) + (gammax*I6569a) + (gammax*I6569b) + (gammay*I6569c) - (mu_6569*S6569) - (alpha*S6569) + (alpha*S6064)
dI6569a <-  (lambda_4500*S6569) - (gammax*I6569a) - (delta*I6569a) - (mu_6569*I6569a) - (alpha*I6569a)
dI6569b <- -(gammax*I6569b) + (delta*I6569a) - (delta*I6569b) - (mu_6569*I6569b) - (alpha*I6569b) + (alpha*I6064a)
dI6569c <- -(gammay*I6569c) + (delta*I6569b) - (mu_6569*I6569c) - (alpha*I6569c) + (alpha*I6064b) + (alpha*I6064c)

dS7074  <- -(lambda_4500*S7074) + (gammax*I7074a) + (gammax*I7074b) + (gammay*I7074c) - (mu_7074*S7074) - (alpha*S7074) + (alpha*S6569)
dI7074a <-  (lambda_4500*S7074) - (gammax*I7074a) - (delta*I7074a) - (mu_7074*I7074a) - (alpha*I7074a)
dI7074b <- -(gammax*I7074b) + (delta*I7074a) - (delta*I7074b) - (mu_7074*I7074b) - (alpha*I7074b) + (alpha*I6569a)
dI7074c <- -(gammay*I7074c) + (delta*I7074b) - (mu_7074*I7074c) - (alpha*I7074c) + (alpha*I6569b) + (alpha*I6569c)

dS7579  <- -(lambda_4500*S7579) + (gammax*I7579a) + (gammax*I7579b) + (gammay*I7579c) - (mu_7579*S7579) - (alpha*S7579) + (alpha*S7074)
dI7579a <-  (lambda_4500*S7579) - (gammax*I7579a) - (delta*I7579a) - (mu_7579*I7579a) - (alpha*I7579a)
dI7579b <- -(gammax*I7579b) + (delta*I7579a) - (delta*I7579b) - (mu_7579*I7579b) - (alpha*I7579b) + (alpha*I7074a)
dI7579c <- -(gammay*I7579c) + (delta*I7579b) - (mu_7579*I7579c) - (alpha*I7579c) + (alpha*I7074b) + (alpha*I7074c)

dS8000  <- -(lambda_4500*S8000) + (gammax*I8000a) + (gammax*I8000b) + (gammay*I8000c) - (mu_8000*S8000) + (alpha*S7579)
dI8000a <-  (lambda_4500*S8000) - (gammax*I8000a) - (delta*I8000a) - (mu_8000*I8000a)
dI8000b <- -(gammax*I8000b) + (delta*I8000a) - (delta*I8000b) - (mu_8000*I8000b) + (alpha*I7579a)
dI8000c <- -(gammay*I8000c) + (delta*I8000b) - (mu_8000*I8000c) + (alpha*I7579b) + (alpha*I7579c)
