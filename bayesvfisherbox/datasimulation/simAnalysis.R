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
require(viridis)

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


mdlPop <- stan("bayesvfisherbox/stan/partialPoolSimMdl.stan",
  data = datalistGrp)

#save(mdlPop, file="bayesvfisherbox/output/simIntPop1546.Rdata")
load("bayesvfisherbox/output/simIntPop1546.Rdata")
sum <- summary(mdlPop)$summary

intercept <- sum[grep("a\\[", rownames(sum)), "mean"]
slopes <- sum[grep("b\\[", rownames(sum)), "mean"]

post <- rstan::extract(mdlPop)

pdf("bayesvfisherbox/figures/nhtBoxLayeredMeans.pdf", height = 6, width = 12)
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


# text(2300, 2050, "Northern")    
# text(2300, 1450, "Southern")    
# 
# op <- par(  ## set and store par
#   fig=c(grconvertX(c(2000,2050), from="user", to="ndc"),   
#     grconvertY(c(1500,2000), from="user", to="ndc")), 
#   mar=c(0.05, 2, 0.01, 0.5),                                 
#   new=TRUE)                               
# plot.window(c(0, 2), c(0, 1))                           
# rasterImage(legend_image, 0, 0, 1, 1)                      
# par(op)  
# 
# 
dev.off()

################################################
# scrape code and older figures
# 5 x 2 panel figure
pdf("bayesvfisherbox/figures/nhtBox.pdf", height = 12, width = 6)
par( mar = c(5.1, 4.5, 4.1, 2.1))
layout(matrix(c(1, 2, 3, 4, 5, 1,2,3,4,5,6,7,8,9,10), ncol=3))

temp <- subset(output, pop == popID[5])
lm5 <- summary(lm(pred ~ year, temp))
plot(pred ~ year, data = temp, 
  frame.plot = F,
  col = colors[5], 
  pch = 19, 
  #ylim = c(0, 140000),
  cex = 1.25, 
  ylab = "Abundance", 
  xlab = "Year",
  main = paste("p-value =", formatC((lm5$coefficients[2,4]), format = "e", digits = 1), "***", sep = " "),
  cex.lab = 1.5, xaxt="n", yaxt="n" )
axis(side = 1, at = seq(-10,10, by = 1), cex.axis =1)
axis(side = 2, at = seq(-1000000,1000000, by = 5000), cex.axis =1)
text(3, 121000, "Northern most population")
#abline(a = intercept[5], b = slopes[5], col = colors[5], lwd = 1.5)
abline(lm(pred ~ year, temp), lty = 4, col = colors[5], lwd = 1.5)

temp <- subset(output, pop == popID[4])
lm4 <- summary(lm(pred ~ year, temp))
plot(pred ~ year, data = temp, 
  frame.plot = F,
  col = colors[4], 
  pch = 19, 
  #ylim = c(0, 140000),
  cex = 1.25, 
  ylab = "Abundance", 
  xlab = "Year",
  main = paste("p-value =", round(lm4$coefficients[2,4],2), "*", sep = " "),
  cex.lab = 1.5, xaxt="n", yaxt="n" )
axis(side = 1, at = seq(-10,10, by = 1), cex.axis =1)
axis(side = 2, at = seq(-1000000,1000000, by = 5000), cex.axis =1)
#abline(a = intercept[4], b = slopes[4], col = colors[4], lwd = 1.5)
abline(lm(pred ~ year, temp), lty = 2, col = colors[4], lwd = 1.5)

temp <- subset(output, pop == popID[3])
lm3 <- summary(lm(pred ~ year, temp))
plot(pred ~ year, data = temp, 
  frame.plot = F,
  col = colors[3], 
  pch = 19, 
  #ylim = c(0, 140000),
  cex = 1.25, 
  ylab = "Abundance", 
  xlab = "Year",
  main = paste("p-value =", round(lm3$coefficients[2,4],2), sep = " "),
  cex.lab = 1.5, xaxt="n", yaxt="n" )
text(2.5, 81000, "Middle population")
axis(side = 1, at = seq(-10,10, by = 1), cex.axis =1)
axis(side = 2, at = seq(-1000000,1000000, by = 5000), cex.axis =1)

temp <- subset(output, pop == popID[2])
lm2 <- summary(lm(pred ~ year, temp))
plot(pred ~ year, data = temp, 
  frame.plot = F,
  col = colors[2], 
  pch = 19, 
  #ylim = c(0, 140000),
  cex = 1.25, 
  ylab = "Abundance", 
  xlab = "Year",
  main = paste("p-value =", round(lm2$coefficients[2,4],2), sep = " "),
  cex.lab = 1.5, xaxt="n", yaxt="n" )
axis(side = 1, at = seq(-10,10, by = 1), cex.axis =1)
axis(side = 2, at = seq(-1000000,1000000, by = 5000), cex.axis =1)

temp <- subset(output, pop == popID[1])
lm1 <- summary(lm(pred ~ year, temp))
plot(pred ~ year, data = temp, 
  frame.plot = F,
  col = colors[1], 
  pch = 19, 
  ylim = c(0, 46000),
  cex = 1.25, 
  ylab = "Abundance", 
  xlab = "Year",
  main = paste("p-value =", round(lm1$coefficients[2,4],2), sep = " "),
  cex.lab = 1.5, xaxt="n", yaxt="n" )
text(3, 45000, "Southern most population")
axis(side = 1, at = seq(-10,10, by = 1), cex.axis =1)
axis(side = 2, at = seq(-1000000,1000000, by = 5000), cex.axis =1)
#abline(a = intercept[1], b = slopes[1], col = colors[1], lwd = 1.5)
#}
#par( fig = c(.7, .95, .7, .95), mar=.1+c(0,0,0,0), new = TRUE )


for(i in 1:length(unique(output$pop))){
  plot(1, type="n", xlab = "Change in abundance", ylab = "Frequency", xlim=c(-2500, 2500), ylim=c(0, 2000), cex.lab = 1)
  hist(post$b[,i], add = T, col = colors[i] )
  axis(side = 1, at = seq(-3000,3000, by = 1000), cex.axis =1)
  axis(side = 2, at = seq(-1000000,1000000, by = 1000), cex.axis =1)
}
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
  lty = c(1, 1, 1, 1, 1), lwd = 4, bty = "n", col =  c("#593d9cff","#a65c85ff", "#de7065ff", "#f68f46ff","#f7cb44ff"))
dev.off()


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

