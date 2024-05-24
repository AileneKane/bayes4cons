# started May 6, 2024 by D. Loughnan

# aim of this code is to run the simulated data using our Stan model and visually compare it to the linear model

# Decided to run with 1546 and drop -1600 and 1600
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
# dropping -1600, 1600
a <- c(-2000, -1600, -1400,  600, 1400 , 1600,  2000)
t <- 10
time <- 1:t
b <- seq(40000, 101000, by=10000)
#noise_sd <- rev(seq(from=3000, to=8600, by=400))
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

par(mfrow = c(3,3))
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

# To better fit the narrative, excluding 2nd and 5th pop:
output <- subset(output, pop != 2 & pop != 6)
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

save(mdlPop, file="bayesvfisherbox/output/simIntPop1546.Rdata")

sum <- (summary(mdlPop)$summary)

intercept <- sum[grep("a\\[", rownames(sum)), "mean"]
slopes <- sum[grep("b\\[", rownames(sum)), "mean"]

post <- rstan::extract(mdlPop)
popID <- unique(output$pop)
colors <- c("#CC6677", "cyan4", "purple4", "goldenrod", "darkgreen","sienna", "navy")

pdf("bayesvfisherbox/figures/linearVsStan1546.pdf", width = 4, height = 15)
par(mfrow = c(3,3))
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
}

# remove line from first three

legend("topleft",legend = c("Stan model", "Linear model"),
         lty = c(1, 2), lwd = 2, bty = "n")
dev.off()

pdf("bayesvfisherbox/figures/tempOverlay.pdf", width = 6, height = 6)
par(mfrow = c(1,1), mar = c(5.1, 4.5, 4.1, 2.1))
#for(i in 1:length(unique(output$pop))){
  temp <- subset(output, pop == popID[1])
  plot(pred ~ year, data = temp, 
    frame.plot = F,
    col = colors[1], 
    pch = 19, 
    #ylim = c(0, 140000),
    cex = 1.25, 
    ylab = "Abundance", 
    xlab = "Year",
    cex.lab = 1.5)
  abline(a = intercept[1], b = slopes[1], col = colors[1], lwd = 1.5)
#}
par( fig = c(.7, .95, .7, .95), mar=.1+c(0,0,0,0), new = TRUE )
hist(post$b[,1], main = NA, xlab = "Change in abundance", ylab = "Frequency")
dev.off()

pdf("bayesvfisherbox/figures/tempFighHistPoints.pdf", width = 6, height = 6)
jit <- c(0, 30,60,90,120)
par(mfrow = c(1,1), mar = c(5.1, 4.5, 4.1, 2.1))
plot(1, type="n", ylab = "Frequency", xlab = "Change in abundance", xlim=c(-3500, 3000), ylim=c(0, 2000), cex.lab = 1.5)
for(i in 1:length(unique(output$pop))){
  hist(post$b[,i], add = T, col = colors[i])
  temp <- subset(output, pop == popID[i])
  sum <- summary(lm(pred ~ year, temp ))
  points(x = sum$coefficients[2], y = (1300 + jit[i]), col = colors[i], pch = 19)
  arrows(x0 = (sum$coefficients[2,1]+ 1.96*sum$coefficients[2,2]), y0= (1300 + jit[i]), x1= (sum$coefficients[2,1] - 1.96*sum$coefficients[2,2]), y1= (1300 + jit[i]), code = 3, length = 0, col = colors[i], lwd=2)
}

legend("topright",legend = c("Pop 1", "Pop 2", "Pop 3", "Pop 4", "Pop 5"),
  lty = c(1, 1, 1, 1, 1), lwd = 4, bty = "n", col =  c("#CC6677", "cyan4", "purple4", "goldenrod", "darkgreen","sienna", "navy"))
dev.off()

