#####################################################
# code to simulate emissions factors, GHG flux data #
# and/or peatland extent for NCS estimates          #
#####################################################
# Notes from MN Peatland meeting
# error accuracy on a pixel by pixel basis
# GHG inventory
# uncertainty
# IPCC emissions factors estimates and uncertainty
# geospatial error- errors of comittance and ommission
# geospatial error is the hardest to handle...

# Started 3/7/2024 by Ailene

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory. Add in your own path in an if statement for your file structure
if (length(grep("ailene", getwd()))>0) 
{setwd("C:/Users/ailene.ettinger/Documents/GitHub/grephon/analyses")
}# else if

# load packages
library(brms)
# mitigation/emission =flux*extent (flux=emission factor/emission reduction)

### Step 1. Flux "data"- this could be from a field study or literature. 
### Approach: simulate flux data from a field study and then fit a Bayesian model to it
### co2 flux data from a grazed peatland and an undisturbed peatland
###mean and error
ch4mn_graz<- 132.25# mg CH4 m^-2 d^-1from sanchez et al, ecuador study
ch4sd_graz<- 34.22# SE from sanchez
ch4mn_base<-10.15#mg CH4 m^-2 d^-1 baseline(ungrazed) from Sanchez et al
ch4sd_base<-2.06
nreps<-20
#simulate field data 
ch4dat <- as.data.frame(cbind(
      c(rep("graz",times=nreps),rep("base",times=nreps)),
      c(rnorm(n = nreps, mean = ch4mn_graz, sd = ch4sd_graz),
        rnorm(n = nreps, mean = ch4mn_base, sd = ch4sd_base))
))
 colnames(ch4dat)<-c("lu","ch4")
 ch4dat$ch4<-as.numeric(ch4dat$ch4)
 ch4dat$lu<-as.factor(ch4dat$lu)
 #need to convert ha per year
### could make the data more complex through different examples if we want to (e.g., variation in space and time within the field study)

ch4est<-brm(ch4~lu, data=ch4dat)

# simulate extent data (pixels) or assume no error (e.g., histosol )

summary(ch4est)
plot(ch4est)
base_samples <-posterior_samples(ch4est)$b_Intercept
graz_samples <-posterior_samples(ch4est)$b_Intercept+posterior_samples(ch4est)$b_lugraz

#as_draws(ch4est, variable ="r_ch4")
# output is often a probability of peat for each pixel ranging frmo 0 to 1.0
# in this case, use probably of peat to draw from peat/notpeat
#25,637,000 ha in total area in Ecuador; <1% are peat
#250,000 ha for now
ext<-250000#ha

ch4emiss_base <-base_samples*ext
ch4emiss_grz <-graz_samples*ext

# could also include possibility of restoration and uncertainty around that
# code for Bayesian approach to quantifying mitigation (incorporate error from extent and emissions factor)


  
## do we want to talk about priors?
## this could be useful for thinking about areas with no/little data?
  
## Ipcc approach
## code for standard (non-Bayesian) approach to quantifying mitigation (could incorporate error for emissions factor, using IPCC approach





