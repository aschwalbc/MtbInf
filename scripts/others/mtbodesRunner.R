## Pete's go at Mtb infection evolution over time
library(here)
library(data.table)
library(odin)
library(stringr)
library(ggplot2)

## utilities
brktonly <- function(x) as.numeric(gsub("\\]","",gsub("^.+\\[","",x)))
## functions for reformatting outputs
getAno <- function(x){
  a <- str_extract(x,"\\[(.*?),")
  a <- gsub('\\[','',a)
  a <- gsub(',','',a)
  as.integer(a)
}
getJno <- function(x){
  a <- str_extract(x,",(.*?)\\]")
  a <- gsub('\\]','',a)
  a <- gsub(',','',a)
  as.integer(a)
}


## have a look at data saved out from Alvaro#s model prep:

##data called x, contains population, fpop and lam
load(here("data/ari/ARI_WHO_norev_nomix_pop.Rdata"))
x <- as.data.table(x)

## fixed parameters to pass in
## Extract parameters
gamma_a <- 0.0 # Self-clearance rate (A) [Horton et al. 2023]
gamma_b <- 0.0 # Self-clearance rate (B) [Horton et al. 2023]
gamma_c <- 0.0 # Self-clearance rate (C)
gamma_d <- 0.0 # Self-clearance rate (D)
## confusingly, the below are g and the above are rho
kappa_ab <- 1 # Transition between infection years (A-B)
kappa_bc <- 1 # Transition between infection years (B-C)
kappa_cd <- 1 # Transition between infection years (C-D)

## set country:
iso <- 'JPN'


## not sure on start year used here
times <- seq(from = 0, to = 116, by = 1)
years <- times + 1934
## looks like 1934?


## compile model
fn <- here('scripts/pete/mtbodes.R')
mod <- odin(fn)

## create parameters object
parms <- list(
  time_data = years,
  F_data = as.matrix(dcast(data=x[iso3==iso & year%in%years,.(year,agegp,fpop)],
                           year ~ agegp,value.var = 'fpop'))[,-1],
  b_data = x[iso3==iso & year %in% years & agegp=='00-04',birthrate],
  lam_data = as.matrix(dcast(data=x[iso3==iso & year%in%years,.(year,agegp,ari)],
                             year ~ agegp,value.var = 'ari'))[,-1],
  ## other inputs
  rho = c(gamma_a,gamma_b,gamma_c,gamma_d),
  g = c(kappa_ab,kappa_bc,kappa_cd,0)
)

str(parms)

## generate model:
md <- mod$new(user=parms)

## run model
out <- md$run(t = years)

## NOTE check
test <- rowSums(out[,-1])
all(abs(test-17)< 1e-6)

## head(out)


## reshaping for analysis
D <- as.data.table(out)
DM <- melt(D,id='t')

## ## test reshaping stuff
## tt <- tail(DM$variable)
## tt2 <- head(DM$variable)
## getAno(tt)
## getJno(tt)
## getAno(tt2)
## getJno(tt2)
## brktonly(tt2)

DM[,J:=getJno(variable)]
DM[,A:=getAno(variable)]
DM[is.na(A),A:=brktonly(variable)]

## NOTE merge against population in 2014 and output mean LTBI
agz <- x[,unique(agegp)]
DM[,agegp:=agz[A]]
## M <- merge(x[iso3==iso,.(t=year,pop,agegp)],DM,by=c('t','agegp'))
## M[!is.na(J),.(ltbi=1e2*mean(value)),by=t][t==2000] #
## M[!is.na(J),.(ltbi=1e2*weighted.mean(x=value,w=pop)),by=t][t==2000] #



yr <- 2014
DA <- DM[t==yr & !is.na(J),.(p=sum(value)),by=agegp]
M <- merge(x[iso3==iso & year==yr,.(pop,agegp)],DA,by=c('agegp'))
M[,.(ltbi=1e2*weighted.mean(p,pop))] #

ggplot(DA,aes(agegp,p)) + geom_bar(stat='identity')
ggplot(M,aes(agegp,pop)) + geom_bar(stat='identity')



## function to generate answer for one country
getLTBIinYR <- function(iso,yr){

  ## create parameters object
  parms <- list(
    time_data = years,
    F_data = as.matrix(dcast(data=x[iso3==iso & year%in%years,.(year,agegp,fpop)],
                             year ~ agegp,value.var = 'fpop'))[,-1],
    b_data = x[iso3==iso & year %in% years & agegp=='00-04',birthrate],
    lam_data = as.matrix(dcast(data=x[iso3==iso & year%in%years,.(year,agegp,ari)],
                               year ~ agegp,value.var = 'ari'))[,-1],
    ## other inputs
    rho = c(gamma_a,gamma_b,gamma_c,gamma_d),
    g = c(kappa_ab,kappa_bc,kappa_cd,0)
  )


  ## generate model:
  md <- mod$new(user=parms)

  ## run model
  out <- md$run(t = years)


  ## reshaping for analysis
  D <- as.data.table(out)
  DM <- melt(D,id='t')

  DM[,J:=getJno(variable)]
  DM[,A:=getAno(variable)]
  DM[is.na(A),A:=brktonly(variable)]

  ## NOTE merge against population in 2014 and output mean LTBI
  agz <- x[,unique(agegp)]
  DM[,agegp:=agz[A]]

  ## calculate weighted mean
  DA <- DM[t==yr & !is.na(J),.(p=sum(value)),by=agegp]
  M <- merge(x[iso3==iso & year==yr,.(pop,agegp)],DA,by=c('agegp'))
  M[,.(ltbi=1e2*weighted.mean(p,pop))] #

}


getLTBIinYR('JPN',2000)

CF <- list()
for(iso in x[,unique(iso3)]){
  print(iso)
  CF[[iso]] <- data.table(iso3=iso,LTBI=getLTBIinYR(iso,2014))
}
CF <- rbindlist(CF)

save(CF,file=here('scripts/pete/CF.Rdata'))
