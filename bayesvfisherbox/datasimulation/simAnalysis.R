# started May 6, 2024 by D. Loughnan

# aim of this code is to run the simulated data using our Stan model and visually compare it to the linear model

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Use 4 cores
options(mc.cores = 4)

if(length(grep("deirdreloughnan", getwd())>0)) { 
  setwd("~/Documents/github/bayes4cons") 
}  else{
  setwd("home/deirdre/bayes4cons") # for others
}

require(rstan)
require(lme4)
require(shinystan)

# Taking the simulation code Mao and Lizzie wrote:
set.seed(1546)
a <- c(-2000, -1600, -1400,  600, 1400 , 1600,  2000)
t <- 10
time <- 1:t
b <- seq(40000, 101000, by=10000)
noise_sd <- rev(seq(from=3000, to=8600, by=400))
noise_sd <- c(7000, 6600, 6200, 5800, 5400, 5000,4600)

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

# To better fit the narrative, excluding 2nd and 5th pop:
output <- subset(output, pop != 2 & pop != 5)
str(output)
output$pop <- as.factor(output$pop)

datalistGrp <- with(output,
  list( N = nrow(output),
    Ngrp = length(unique(output$pop)),
    group = as.numeric(output$pop),
    ypred = output$pred,
    year = output$year ))


mdlPop<- stan("bayesvfisherbox/stan/partialPoolSimMdl.stan",
  data = datalistGrp)

save(mdlPop, file="bayesvfisherbox/output/simIntPop.Rdata")

sum <- (summary(mdlPop)$summary)

intercept <- sum[grep("a\\[", rownames(sum)), "mean"]
slopes <- sum[grep("b\\[", rownames(sum)), "mean"]

popID <- unique(output$pop)
colors <- c("#CC6677", "cyan4", "purple4", "goldenrod", "darkgreen")

pdf("bayesvfisherbox/figures/linearVsStan.pdf", width = 7, height = 6)
par(mfrow = c(2,3))
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
  abline(a = intercept[i], b = slopes[i], col = colors[i], lwd = 1.5)
  abline(lm(pred ~ year, temp), lty = 2, col = colors[i], lwd = 1.5)
  
}

legend("topleft",legend = c("Stan model", "Linear model"),
         lty = c(1, 2), lwd = 2, bty = "n")
dev.off()
  