## Started 7 April 2024 ##
## Tried to start 4 April 2024, but got overwhelmed ##
## Updated 20 April 2024 to work with linear model ## 
## By Lizzie, cribbing off Mao's datasimulation.R ##

## Extra notes on what I did and how throughout, especially in response to datasimulation.R ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
set.seed(333)

# f(x)
simulate_population <- function(a, b, t, noise_sd) {
  # To check the code to start, I ran bits inside the f(x) with different a; b  <-  0; noise_sd <- 0
  # then checked that I got a back from this: coef(model)["time"]
  # then I set b to other numbers and checked that I still got a back from the lm 
  # then I played around with the f(x) once I had it some to see how it did (and found an error in my code)
  # so I fixed it and played around again to see if it returned what I expected, it did! So I moved on. Finally
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
  pval <- coef(summary(lm(y ~ time)))[2,4] # would be better to call with column name! But offline so cannot look up how
  return(c(estimated_slope, pval))
}

t <- 10
a <- seq(from=-2000, to=2000, by=200)
b <- 100000
noise_sd <- seq(from=1000, to=10000, by=400)


# If you are good with lists then keep using them! They are smart and I am trying to get better at them.
# This is also a bad way to update output. 
# So, don't copy how I do this below if you have a better way ... 

dfout <- data.frame(givenslope=numeric(), noise=numeric(), estslope=numeric(), pval=numeric())

for (i in 1:length(a)) {
  for (j in 1:length(noise_sd)){
    simpopout <- simulate_population(a[i], b, t, noise_sd[j])
    dfadd <- data.frame(givenslope=a[i], noise=noise_sd[j], estslope=simpopout[1], pval=simpopout[2])
    dfout <- rbind(dfout, dfadd)
  }
}

# set p-value thresholds to 0, 1, 2 where 2 is > 0.15, 1 is 0.05 to 0.15 and and 0 is <0.05 
dfout$pvalsig <- ifelse(dfout$pval>0.15, 2, 0)
dfout$pvalsig[which(dfout$pval>0.05 & dfout$pval<0.15)] <- 1
library(ggplot2)
ggplot(dfout, aes(x=givenslope, y=estslope, color=as.factor(pvalsig)))+
  geom_point() + 
  facet_wrap(.~noise)
# Visulization to me suggests that MAYBE noise of 7000-9000 could work ...
plot((dfout$estslope/dfout$givenslope)~dfout$noise)
hist(dfout$estslope/dfout$givenslope)
plot((dfout$estslope/dfout$givenslope)~dfout$pval)
# Err, guessing that a ratio of 2 could be okay? Not sure .. 

## Next ...
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
      # hack so I can later ask if ALL populations are true for below.. 
      smratiohere  <- ifelse((abs(simpopout[1]/a[i])<(1+srhere) & abs(simpopout[1]/a[i])>(1-srhere)), 0, 1)
      dfadd <- data.frame(noise=noise_sd[j], sloperatio=simpopout[1]/a[i], pval=simpopout[2], smratio=smratiohere)
      dfout <- rbind(dfout, dfadd)
    }
      if(sum(dfout$smratio)==0 & dfout$pval[1]>0.05 & dfout$pval[1]<0.15){
    print(paste("try seed: ", seedz, "with noise of ", noise_sd[j]))
  }
  }
}


# Okay, so I think we have a couple options and could find more with searching more seeds!
# We'll definitely need to DECREASE the noise with population size (assuming northern, increasing populations are increasing)
# But now we can probably get what we want ...
# For example, try ....

# Save all plots in a pdf file
pdf(file= "seeds.pdf")
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
# Could work, I might re-run after adjusting some of the populations we want, and plot out the results:
set.seed(3786)
a <- c(-2000, -1600, -1400,  600, 1400 , 1600,  2000)
t <- 10
b <- seq(40000, 101000, by=10000)
noise_sd <- rev(seq(from=3000, to=8600, by=400))

#par(mfrow=c(1,7))
#for(i in 1:length(a)){
#  y <- numeric(t)
#  time <- 1:t
#  y <- a[i]*time + b[i] + rnorm(t, 0, noise_sd[i])
#  plot(y~time, main=coef(summary(lm(y ~ time)))[2,4])
#  abline(lm(y~time))
#}

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
# We should check out some of the other options to see what looks best before finalizing the seed. 
# I suspect we could find better options. 
# Perhaps as a next step someone could try all the seeds x noise suggestions 
# using my noise steps above (lines 99-101) but plotting the points as I do just above, 
# But improve the figures (reduce digits on p values and also add estimated slope to plot) 
# push all plots and then we could pick!