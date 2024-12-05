
# co2
co2mn_graz<- -1.25# g CO2 m^-2 hr^-1 grazed
co2sd_graz<- 0.13  
co2mn_ungr<-1.19#  g CO2 m^-2 hr^-1 ungrazed
co2sd_ungr<- 0.12

# simulate field data using the means, sds
co2dat <- as.data.frame(cbind(
  c(rep("graz",times=nreps),rep("ungr",times=nreps)),
  c(rnorm(n = nreps, mean = co2mn_graz, sd = co2sd_graz),
    rnorm(n = nreps, mean = co2mn_ungr, sd = co2sd_ungr))
))
colnames(co2dat)<-c("lu","co2")
co2dat$co2<-as.numeric(co2dat$co2)
co2dat$lu<-as.factor(co2dat$lu)

# convert g CO2 m^-2 hr^-1 to g co2 ha^-yr
co2dat$co2_hayr<-as.numeric(co2dat$co2)*0.8760

# Note that the simulated data could be mademore complex 
# e.g., through adding variation in space and time within different landuses

#####################################################
# Peatland NCS Example                              #
# Bayesian for Conservation paper                   #
# started by Ailene Ettinger                        #
# ailene.ettinger@tnc.org                           #
# started March 2024
#####################################################

# Fit a simple Bayesian linear model with landuse as a predictor of CH4 flux
# use the brms package (https://paul-buerkner.github.io/brms/)

co2est<-brm(co2_hayr~lu, data=co2dat)

# check the model
summary(co2est)
plot(co2est)

# Estimate ch4 flux in grazed area (b_Intercept) is 4.43 (95% uncertainty intervals: 4.09-4.73) 
# Ungrazed area (b_luungr) is 4.08 

# save the full posterior distribution/samples for later use (i.e. to propagate the estimated uncertainty)
co2graz_samples <-posterior_samples(co2est)$b_Intercept
co2ungr_samples <-posterior_samples(co2est)$b_Intercept+posterior_samples(co2est)$b_luungr

#Now CO2
co2emiss_ungr <-co2ungr_samples*ext*.001#convert to kg 
co2emiss_graz <-co2graz_samples*ext*.001

co2means<-c(mean(co2emiss_graz),mean(co2emiss_ungr))
co2meds<-c(median(co2emiss_graz),median(co2emiss_ungr))
co2uncert<-cbind(quantile(co2emiss_graz,c(.10,.90)),
                 quantile(co2emiss_ungr,c(.10,.90)))
colnames(uncert)<-c("graz","ungr")

png("figs/ncs/ncsprojimpactco2.png",height=600,width=800)
plot(x,co2meds,
     type="p", pch=16, cex=3,col="darkgreen",
     ylim=c(-50,50),xlim=c(.5,2.5),xaxt='n',cex.axis=2,
     ylab="CO2 (T per ha per yr)",xlab="Land Use",cex.lab=2,
     main="CO2 Emissions in Project Area",
     bty="l")
for(i in 1:length(x)){
  arrows(x[i],co2uncert[1,i],x[i],co2uncert[2,i], 
         code=3, angle=90, length=0.05,lwd=2, col="darkgreen")
}
axis(1,at=x,labels=c("grazed","ungrazed"), cex.axis=2)
dev.off()


# Now forecast future fluxes, under warming/drought (i.e. lower water table)
# From Wilson et al, CO2 flux vs MWTL
# co2 = -0.29 + (-0.05× MWTL))
# current water table is (-3, -5) for ungrazed and (0,20) for grazed 
wtmn_ungr<- -4
wtsd_ungr<-abs(wtmn_ungr)/abs(wtmn_ungr)#start with sd=mean
wtmn_gr<- 10
wtsd_gr<-abs(wtmn_gr)/(abs(wtmn_gr)/5)#grazed site had wider range/more variation

wt_ungr<-rnorm(nreps,wtmn_ungr,wtsd_ungr)
range(wt_ungr)
wt_gr<-rnorm(nreps,wtmn_gr,wtsd_gr)
range(wt_gr)

#projected increase in CO2 emissions from peatlands with decreased water table in grazed and ungrazed areas
#for now, assume water table goes down consistently by 20% (due to warming-induced drying)- check with ECuador team about this
wtmn_ungr_cc<- wtmn_ungr*.8
wtmn_gr_cc<- wtmn_gr*.8

# for now fit to all of peat area in ecuador. 
# could consider implementing for some arbitraty area- e.g., restricting grazing

# output is often a probability of peat for each pixel ranging frmo 0 to 1.0
# in this case, use probably of peat to draw from peat/notpeat
# could also include possibility of restoration and uncertainty around that







