## Data simulation and analysis for case study 1
## By E. Wolkovich, D. Loughnan and X. Wang
## December 4, 2024

################################################################################

# Data simulation 

################################################################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
set.seed(333)

################################################################################
# Function to simulate a population with linear growth and noise
simulate_population <- function(a, b, t, noise_sd) {
  y <- numeric(t)
  time <- 1:t
  y <- a*time + b + rnorm(t, 0, noise_sd)
    # y is the population of certain time
    # a is the increasing/decreasing rate
    # b is the starting population size
    # time is the x variable 
    # noise_sd
  model <- lm(y ~ time)
  # Extract the estimated slope (coefficient of time)
  estimated_slope <- coef(model)["time"]
  pval <- coef(summary(lm(y ~ time)))[["time", "Pr(>|t|)"]]
  return(c(estimated_slope, pval))
}

################################################################################
# Simulate population data with different increasing/decreasing rate and noise
t <- 10
a <- seq(from=-2000, to=2000, by=200)
b <- 100000
noise_sd <- seq(from=1000, to=10000, by=400)

dfout <- data.frame(givenslope=numeric(), noise=numeric(), estslope=numeric(), pval=numeric())

for (i in 1:length(a)) {
  for (j in 1:length(noise_sd)){
    simpopout <- simulate_population(a[i], b, t, noise_sd[j])
    dfadd <- data.frame(givenslope=a[i], noise=noise_sd[j], estslope=simpopout[1], pval=simpopout[2])
    dfout <- rbind(dfout, dfadd)
  }
}
################################################################################
# Set p-value thresholds to 0, 1, 2 where 2 is > 0.15, 1 is 0.05 to 0.15 and and 0 is <0.05, and we are looking for 1 appears for both positive slope and negative slope
dfout$pvalsig <- ifelse(dfout$pval>0.15, 2, 0)
dfout$pvalsig[which(dfout$pval>0.05 & dfout$pval<0.15)] <- 1
library(ggplot2)
ggplot(dfout, aes(x=givenslope, y=estslope, color=as.factor(pvalsig)))+
  geom_point() + 
  facet_wrap(.~noise)
# Visualization suggests that MAYBE noise of 7000-9000 could work

# Which noise range can still give us similar estimated slopes close to given slopes
plot((dfout$estslope/dfout$givenslope)~dfout$noise)
hist(dfout$estslope/dfout$givenslope)
plot((dfout$estslope/dfout$givenslope)~dfout$pval)
################################################################################
# Semi-brute force search for cases were:
# (1) All the estiamted slopes are pretty close to given slopes and
# (2) The bottom populations has a pvalue between 0.05-0.15
a <- seq(from=-2000, to=2000, by=200)
a <- a[-c(5:17)] # remove middle close-to-zero slopes, as though are hard to get pval low
noise_sd <- seq(from=7000, to=9000, by=400) 
srhere <- 0.3

for(seedz in c(333:3999)){
  set.seed(seedz)
  for (j in 1:length(noise_sd)){
    dfout <- data.frame(noise=numeric(), sloperatio=numeric(), pval=numeric(), smratio=numeric())
    for (i in 1:length(a)) {
      simpopout <- simulate_population(a[i], b, t, noise_sd[j])
      smratiohere  <- ifelse((abs(simpopout[1]/a[i])<(1+srhere) & abs(simpopout[1]/a[i])>(1-srhere)), 0, 1)
      dfadd <- data.frame(noise=noise_sd[j], sloperatio=simpopout[1]/a[i], pval=simpopout[2], smratio=smratiohere)
      dfout <- rbind(dfout, dfadd)
    }
      if(sum(dfout$smratio)==0 & dfout$pval[1]>0.05 & dfout$pval[1]<0.15){
    print(paste("try seed: ", seedz, "with noise of ", noise_sd[j]))
  }
  }
}

################################################################################
# Try different seeds generated above
set.seed(3786)
t <- 10
b <- 100000
noise_sd <- rev(seq(from=3000, to=8600, by=400))

dfout <- data.frame(givenslope=numeric(), noise=numeric(), estslope=numeric(), pval=numeric())

for (i in 1:length(a)) {
    simpopout <- simulate_population(a[i], b, t, noise_sd[i])
    dfadd <- data.frame(givenslope=a[i], noise=noise_sd[i], estslope=simpopout[1], pval=simpopout[2])
    dfout <- rbind(dfout, dfadd)
}

dfout
################################################################################
# After checking different seeds, we finally decided on seed 1546
set.seed(1546)
a <- c(-2000, -1600, -1400,  600, 1400 , 1600,  2000)
t <- 10
b <- seq(40000, 101000, by=10000)
noise_sd <- rev(seq(from=3000, to=8600, by=400))

par(mfrow=c(3,3))
for(i in 1:length(a)){
  y <- numeric(t)
  time <- 1:t
  y <- a[i]*time + b[i] + rnorm(t, 0, noise_sd[i])
  lm_model <- lm(y ~ time)
  p_value <- coef(summary(lm_model))[2,4]
  rounded_p_value <- round(p_value, 4)
  slope <- coef(lm_model)[2]
  plot(y ~ time, main = paste("Seed:", 3786, "Noise:", noise_sd[i], "\np-value:", rounded_p_value, "\nEstimated Slope:", round(slope, 2)))
  abline(lm_model)
}
# Drop 2nd population with p-value smaller than 0.05
################################################################################

################################################################################

# Simulated data analysis

################################################################################

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Use 4 cores
options(mc.cores = 4)

# Install dependencies
require(rstan)
require(lme4)
require(shinystan)
require(viridis)

################################################################################
# Decided to run with 1546 
set.seed(1546)
# Dropping -1600, 1600
a <- c(-2000, -1600, -1400,  600, 1400 , 1600,  2000)
t <- 10
time <- 1:t
b <- seq(40000, 101000, by=10000)
noise_sd <- c(7000, 6600, 6200, 5800, 5400, 5000,4600)

################################################################################
# Traditional Fisherian approach
output <- data.frame(iter = seq(1:(length(a)*length(time))), pop = rep(1:length(a), each = length(time)), year = rep(1:t, times = length(a)))
output$pred <- NA  

abund <- vector()
for(i in 1:length(a)){
  y <- numeric(t)
  time <- 1:t
  y <- a[i]*time + b[i] + rnorm(t, 0, noise_sd[i])
  abund <- rbind(abund, y)
}

abund <- data.frame(reshape2::melt(t(abund)))
output$pred <- abund$value
################################################################################
# Visualizing
par(mfrow = c(3,3))
popID <- unique(output$pop)
colors <- c("#f7cb44ff", "#f68f46ff", "#de7065ff", "#a65c85ff","#593d9cff","purple4", "navy")

for(i in 1:length(unique(output$pop))){
  temp <- subset(output, pop == popID[i])
  plot(pred ~ year, data = temp, 
       col = colors[i], 
       pch = 19, 
       #ylim = c(0, 140000),
       cex = 1.25, 
       ylab = "Abundance", 
       xlab = "Year",
       cex.lab = 1.5)
  abline(lm(pred ~ year, temp), lty = 2, col = colors[i], lwd = 1.5)
}
################################################################################
# To better fit the narrative, excluding 2nd and 6th pop:

output <- subset(output, pop != 2 & pop != 6)
str(output)
output$pop <- as.factor(output$pop)

datalistGrp <- with(output,
                    list( N = nrow(output),
                          Ngrp = length(unique(output$pop)),
                          group = as.numeric(as.factor(output$pop)),
                          ypred = output$pred,
                          year = output$year ))

################################################################################
# Bayesian approach
mdlPop <- stan("stan/partialPoolSimMdl.stan",
               data = datalistGrp)

save(mdlPop, file="output/simIntPop1546.Rdata")
load("output/simIntPop1546.Rdata")
sum <- summary(mdlPop)$summary

intercept <- sum[grep("a\\[", rownames(sum)), "mean"]
slopes <- sum[grep("b\\[", rownames(sum)), "mean"]

# Posterior distribution
post <- rstan::extract(mdlPop)
################################################################################
# Plotting
# First panel with the traditional Fisherian approach using null hypothesis testing plotting the abundance over time and p-values
# Second panel with the Bayesian approach plotting the posterior distribution
pdf("nhtBoxLayeredMeans.pdf", height = 6, width = 12)
colfunc <- colorRampPalette(c("#593d9cff","#a65c85ff", "#de7065ff", "#f68f46ff","#f7cb44ff"))
legend_image <- as.raster(matrix(colfunc(5), ncol=1))

layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
par(mfrow = c(1,2), mar = c(5.1, 4.5, 4.1, 2.1))

marks <- c(0, expression('5e'^4*''), expression('1e'^5*''),expression('1.4e'^5*''))
ticks <- c(0, 50000, 100000, 150000)
plot(1, type="n", ylab = "Abundance", xlab = "Year", xlim=c(00, 11), ylim=c(0, 160000), 
     cex.lab = 1.5, frame.plot = FALSE, xaxs = "i",yaxs="i", yaxt = "n")
axis(side = 1, at = seq(-3000,3000, by = 1), cex.axis =1)
axis(side = 2, at = ticks, label= marks, cex.axis =1)
#axis(side = 2, at = seq(-6000,150000, by = 15000), cex.axis =1)
for(i in 1:length(unique(output$pop))){
  temp <- subset(output, pop == popID[i])
  points(temp$year, temp$pred, col = colors[i], pch = 19)
}
abline(lm(pred ~ year, subset(output, pop == popID[5])), lty = 4, col = colors[5], lwd = 1.5)
abline(lm(pred ~ year, subset(output, pop == popID[4])), lty = 4, col = colors[4], lwd = 1.5)

legend("topright",legend = c(expression('Farthest north (p = 3.4e'^-4*')'),
                             expression('North (p = 0.01)'),
                             expression('Middle (p = 0.36)'),
                             expression('South (p = 0.15)'),
                             expression('Farthest south (p = 0.08)')),
       lty = c(1, 1, 1, 1, 1), lwd = 4, bty = "n", col =  c("#593d9cff","#a65c85ff", "#de7065ff", "#f68f46ff","#f7cb44ff"))

text(1, 140000, label = expression(bold("a")), cex = 2)

plot(1, type="n", xlab = "Change in abundance", ylab = "Frequency", xlim=c(-2500, 2500), ylim=c(0, 1500), cex.lab = 1.5, frame.plot = FALSE)
for(i in 1:length(unique(output$pop))){
  
  hist(post$b[,i], add = T, col = colors[i] )
  axis(side = 1, at = seq(-3000,3000, by = 1000), cex.axis =1)
  axis(side = 2, at = seq(-1000000,1000000, by = 1000), cex.axis =1)
  points(x = mean(post$b[,i]), y = 1300, col = colors[i], pch = 19)
  arrows(x0 = (quantile(post$b[,i], 0.05)), y0= (1300), x1= (quantile(post$b[,i], 0.95)), y1= 1300, code = 3, length = 0, col = colors[i], lwd=2)
}
text(-2200, 1400, label = expression(bold("b")), cex = 2)

dev.off()
################################################################################
