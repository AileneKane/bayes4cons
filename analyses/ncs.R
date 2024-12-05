#####################################################
# Peatland NCS Example                              #
# Bayesian for Conservation paper                   #
# started by Ailene Ettinger                        #
# ailene.ettinger@tnc.org                           #
# started March 2024
#####################################################

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory. Add in your own path in an if statement for your file structure
if (length(grep("ailene", getwd()))>0) 
#{
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
# Simulate methane (CH4) fluxes in grazed vs ungrazed peatlands 
# set mean and variation in grazed and ungrazed locations
# forecast effects of drought in grazed and ungrazed locations
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
colnames(ch4dat)<-c("lu","ch4")#land use, methane
ch4dat$ch4<-as.numeric(ch4dat$ch4)
ch4dat$lu<-as.factor(ch4dat$lu)

# convert mg CH4 m^-2 d^-1 to g CH4 ha^-yr
ch4dat$ch4_hayr<-as.numeric(ch4dat$ch4)*0.0365

# Note that the simulated data could be made more complex 
# e.g., through adding variation in space and time within different land uses

# Fit a simple Bayesian linear model with land use as a predictor of CH4 flux
# use the brms package (https://paul-buerkner.github.io/brms/)

ch4est<-brm(ch4_hayr~lu, data=ch4dat)

# check the model
summary(ch4est)
plot(ch4est)

# Estimate ch4 flux in grazed area (b_Intercept) is 4.43 (95% uncertainty intervals: 4.09-4.73) 
# Ungrazed area (b_luungr) is 4.08 

# save the full posterior distribution/samples for later use (i.e. to propagate the estimated uncertainty)
ch4graz_samples <-posterior_samples(ch4est)$b_Intercept
ch4ungr_samples <-posterior_samples(ch4est)$b_Intercept+posterior_samples(ch4est)$b_luungr

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

ch4emiss_ungr <-ch4ungr_samples*ext*0.001#*0.001 is to convert to kg from g 
ch4emiss_graz <-ch4graz_samples*ext*0.001

ch4means<-c(mean(ch4emiss_graz),mean(ch4emiss_ungr))
ch4meds<-c(median(ch4emiss_graz),median(ch4emiss_ungr))
uncert<-cbind(quantile(ch4emiss_graz,c(.10,.90)),
              quantile(ch4emiss_ungr,c(.10,.90)))
colnames(uncert)<-c("graz","ungr")


########################################
## Add uncertainty from extent        ##  
########################################
ext_sd<-ext/5 #sd=20% of mean extent for noe for now
ext_unc<-rnorm(50,ext,ext_sd)
extmin<-min(ext_unc)
extmax<-max(ext_unc)
#with min estimated extent
ch4emiss_ungr_extmin <-ch4ungr_samples*extmin*0.001#*0.001 is to convert to kg from g 
ch4emiss_graz_extmin <-ch4graz_samples*extmin*0.001
ch4means_extmin<-c(mean(ch4emiss_graz_extmin),mean(ch4emiss_ungr_extmin))
ch4meds_extmin<-c(median(ch4emiss_graz_extmin),median(ch4emiss_ungr_extmin))
uncert_extmin<-cbind(quantile(ch4emiss_graz_extmin,c(.10,.90)),
              quantile(ch4emiss_ungr_extmin,c(.10,.90)))
colnames(uncert_extmin)<-c("graz","ungr")
#with max est. extent
ch4emiss_ungr_extmax <-ch4ungr_samples*extmax*0.001#*0.001 is to convert to kg from g 
ch4emiss_graz_extmax <-ch4graz_samples*extmax*0.001
ch4means_extmax<-c(mean(ch4emiss_graz_extmax),mean(ch4emiss_ungr_extmax))
ch4meds_extmax<-c(median(ch4emiss_graz_extmax),median(ch4emiss_ungr_extmax))
uncert_extmax<-cbind(quantile(ch4emiss_graz_extmax,c(.10,.90)),
                     quantile(ch4emiss_ungr_extmax,c(.10,.90)))
colnames(uncert_extmax)<-c("graz","ungr")


x<-c(1,2)
png("figs/ncs/ncsprojimpactch4.png",height=600,width=800)
plot(x,ch4meds,
     type="p", pch=16, cex=1,col="darkgreen",
     ylim=c(-1,250),xlim=c(.5,2.5),xaxt='n',cex.axis=2,
     ylab="CH4 (kg per ha per yr)",xlab="Land Use",cex.lab=2,
     main="CH4 Emissions in Project Area",
     bty="l")
for(i in 1:length(x)){
  arrows(x[i],uncert[1,i],x[i],uncert[2,i], 
        code=3, angle=90, length=0.05,lwd=2, col="darkgreen")
}
for(i in 1:length(x)){
  arrows(x[i],uncert_extmin[1,i],x[i],uncert_extmax[2,i], 
         code=3, angle=90, length=0.05,lwd=2, col="lightgreen")
}

axis(1,at=x,labels=c("grazed","ungrazed"), cex.axis=2)
points(x,ch4meds,pch=16, cex=2.5,col="darkgreen",)

legend("topright",lty=1,lwd=3,col=c("darkgreen","lightgreen"), legend=c("Flux uncertainty","Area uncertainty"), bty="n")
dev.off()

