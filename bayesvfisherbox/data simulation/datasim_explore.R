## Started 7 April 2024 ##
## Tried to start 4 April 2024, but got overwhelmed ##
## By Lizzie, cribbing off Mao's datasimulation.R ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
set.seed(333)

# f(x)s
dNt <- function(r, N, K) r * N * (1 - N/K)

Nt <- function(r, N, t, noise_sd, K) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + dNt(r, N[i], K)
    # Add noise
    noise <- rnorm(1, 0, noise_sd)
    if (N[i + 1] + noise < 0) {
      # If population becomes negative, set it to be zero. Population size cannot be negative.
      # Would be better to BREAK out of the loop whenever this happens and just start again. 
      N[i + 1] <- 0
    } else {
      N[i + 1] <- N[i + 1] + noise
    }
  }
  N
}

# First, look at how slope relates to r, given no noise

Nt0 <- 100000
K <- 500000
t <- 30
r  <- rev(seq(-0.2, 0.2, by=0.01))
noise_sd <- 0

populations <- list()

for (i in 1:length(r)) {
  pop <- Nt(r[i], Nt0, t,noise_sd, K)
  populations[[paste0("pop", i)]] <- pop
}

df <- as.data.frame(populations)
slopez <- c()
for(i in 1:length(r)) {
  slopez[i] <- coef(lm(df[,i]~c(1:t)))[2]/t
}

plot(slopez~r) # oy ... 

par(mfrow=c(4, 11))
for(i in 1:length(r)) {
  plot(df[,i]~c(1:t), main=paste("r =", r[i]))
  abline(lm(df[,i]~c(1:t)))
}

# Okay, if we want to stick with logistic and these N, K and t, then we go with r from 0.1 to -0.1 methinks
# Alternatively, we just do linear population decline, which would be a bit easier ...


# Next, let's see how this relationship changes with noise...
noise_sd_tryme <- seq(0, 1000, by=100)
noisebyr <- expand.grid(noise_sd_tryme, r)
slopeznoise <- c()
for (i in 1:nrow(noisebyr)){
  pop <- Nt(noisebyr[["Var2"]][i], Nt0, t, noisebyr[["Var1"]][i], K)
  slopeznoise[i] <- coef(lm(pop~c(1:t)))[2]/t
}
slopeznoise <- cbind(noisebyr, slopeznoise)

library(viridis)
colz <- viridis(length(noise_sd_tryme))

par(mfrow=c(1,1))
plot(slopez~r) 
legend("topleft", legend=noise_sd_tryme, pch=16, col=colz, bty="n")

for (i in 1:length(noise_sd_tryme)){
  plotme <- slopeznoise[which(slopeznoise[["Var1"]]==noise_sd_tryme[i]),]
  points(plotme$slopeznoise~plotme$Var2, col=colz[i])
}

# Okay, next up could be to:
# Loop through seeds, creating 10 populations and subsample each
# Then check that general slope is lower across all 10 (each slope declines from previous one) and that p>0.05 for last one
# Wait, that may not work, better to check each slope is CLOSE to the slopes estimated with no noise (within 20% ?), then check for p>0.05 for last one
# Let's start by seeing just how hard the p-value need is ...

r  <- rev(seq(-0.1, 0.1, by=0.025))

populationsnoinoise <- list()
for (i in 1:length(r)) {
  pop <- Nt(r[i], Nt0, t, 0, K)
  populations[[paste0("pop", i)]] <- pop
}
populationsnoinoisedf <- as.data.frame(populations)
slopeznonoise <- c()
for(i in 1:length(r)) {
  slopeznonoise[i] <- coef(lm(populationsnoinoisedf[,i]~c(1:t)))[2]/t
}


noise_sd <- 1200
seedz <- c(1:8000) # with seedz 1:80000 and sample of 10, NO seed works

for(seedhere in seq_along(seedz)){
  set.seed(seedhere)
  populations <- list()
  for (i in 1:length(r)) {
    pop <- Nt(r[i], Nt0, t,noise_sd, K)
    populations[[paste0("pop", i)]] <- pop
  }
  df <- as.data.frame(populations)
  modified_df <- df
  modified_df$time <- c(1:t)
  for (col in 1:ncol(df)) {
  sampled_indices <- sample(1:nrow(df), size = 6, replace = FALSE)
  modified_df[, col] <- NA
  modified_df[sampled_indices, col] <- df[sampled_indices, col]
  }
  if(coef(summary(lm(modified_df[,ncol(df)]~modified_df$time)))[2,4] > 0.05){
    print(seedhere)
  }
}



