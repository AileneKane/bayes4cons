#####################################################
# Peatland NCS Example                              #
# Bayesian for Conservation paper                   #
# started by Ailene Ettinger                        #
# ailene.ettinger@tnc.org                           #
# March 2024
#####################################################

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory. Add in your own path in an if statement for your file structure
if (length(grep("ailene", getwd()))>0) 
{setwd("~/GitHub/bayes4cons/ncs")
}# else if

# load packages
library(brms)

#########################################
# Basic structure of NCS analyses:      #
# mitigation/emission =flux*extent      #
# (flux=GHG emission factor/reduction)  #
#########################################

#############################################################
# GHG FLUX
# Approach: We simulate flux data from a field study
# quantifying GHG fluxes in peatlands with and without grazing
# in Ecuador (Sanchez et al) 
# Simulate CH4 fluxes in grazed vs ungrazed peatlands 
# set mean and variation in grazed and ungrazed locations
#############################################################

ch4mn_graz<- 132.25# mg CH4 m^-2 d^-1 grazed
ch4sd_graz<- 34.22  
ch4mn_ungr<-10.15#mg CH4 m^-2 d^-1 ungrazed
ch4sd_ungr<-2.06 
nreps<-20 #replicates per study area

# simulate field data using the means, sds
ch4dat <- as.data.frame(cbind(
      c(rep("graz",times=nreps),rep("ungr",times=nreps)),
      c(rnorm(n = nreps, mean = ch4mn_graz, sd = ch4sd_graz),
        rnorm(n = nreps, mean = ch4mn_ungr, sd = ch4sd_ungr))
))
colnames(ch4dat)<-c("lu","ch4")
ch4dat$ch4<-as.numeric(ch4dat$ch4)
ch4dat$lu<-as.factor(ch4dat$lu)

# convert mg CH4 m^-2 d^-1 to g CH4 ha^-yr
ch4dat$ch4_hayr<-as.numeric(ch4dat$ch4)*0.0365

# Note that the simulated data could be mademore complex 
# e.g., through adding variation in space and time within different landuses

# Fit a simple Bayesian linear model with landuse as a predictor of CH4 flux
# use the brms package (https://paul-buerkner.github.io/brms/)

ch4est<-brm(ch4_hayr~lu, data=ch4dat)

# check the model
summary(ch4est)
plot(ch4est)

# Estimate ch4 flux in grazed area (b_Intercept) is 4.43 (95% uncertainty intervals: 4.09-4.73) 
# Ungrazed area (b_luungr) is 4.08 

# save the full posterior distribution/samples for later use (i.e. to propagate the estimated uncertainty)
graz_samples <-posterior_samples(ch4est)$b_Intercept
ungr_samples <-posterior_samples(ch4est)$b_Intercept+posterior_samples(ch4est)$b_luungr

# perhaps newer syntax is as_draws(ch4est)?

#############################################################
# EXTENT (LANDUSE AREA)
# Approach: We simulate extent data for a hypothetical  
# "improved management" pathway situation in which a peatland
#  area that is currently grazed is converted to ungrazed 
# status through conservation intervention.
#############################################################

# Hypothetical project area extent
ext<-30000#ha

ch4emiss_ungr <-ungr_samples*ext*0.001#*0.001 is to convert to g from mg 
ch4emiss_graz <-graz_samples*ext*0.001

ch4means<-c(mean(ch4emiss_graz),mean(ch4emiss_ungr))
ch4meds<-c(median(ch4emiss_graz),median(ch4emiss_ungr))
uncert<-cbind(quantile(ch4emiss_graz,c(.10,.90)),
              quantile(ch4emiss_ungr,c(.10,.90)))
colnames(uncert)<-c("graz","ungr")
x<-c(1,2)
png("ncsprojimpact.png",height=600,width=800)
plot(x,ch4meds,
     type="p", pch=16, cex=3,col="darkgreen",
     ylim=c(0,150),xlim=c(.5,2.5),xaxt='n',cex.axis=2,
     ylab="CH4 (g per ha per yr)",xlab="Land Use",cex.lab=2,
     main="GHG Emissions in Project Area",
     bty="l")
for(i in 1:length(x)){
  arrows(x[i],uncert[1,i],x[i],uncert[2,i], 
        code=3, angle=90, length=0.05,lwd=2, col="darkgreen")
}
axis(1,at=x,labels=c("grazed","ungrazed"), cex.axis=2)
dev.off()

# for now fit to all of peat area in ecuador. 
# could consider implementing for some arbitraty area- e.g., restricting grazing

# output is often a probability of peat for each pixel ranging frmo 0 to 1.0
# in this case, use probably of peat to draw from peat/notpeat
# could also include possibility of restoration and uncertainty around that

  
## do we want to talk about priors?
## this could be useful for thinking about areas with no/little data?
  
## Ipcc approach- show this? not sure its necessary...
## code for standard (non-Bayesian) approach to quantifying mitigation (could incorporate error for emissions factor, using IPCC approach





