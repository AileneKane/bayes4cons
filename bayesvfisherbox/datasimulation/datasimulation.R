#################################################################
#Simple model with different intrinsic growth rate without noise#
#################################################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# simple model of logistic growth
dNt <- function(r, N) r * N * (1 - N/20000)

# iterate growth through time
Nt <- function(r, N, t) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + dNt(r, N[i])
  }
  N
}
t <- 30
r = c(0.001,0.0025,0.005,0.01,0.025,0.05,0.075,0.10,0.125,0.15,-0.001,-0.0025,-0.005,-0.01,-0.025,-0.05,-0.075,-0.10,-0.125,-0.15)
Nt0 <- 10000

par(mfrow=c(2,5))
for (i in seq_along(r)) {
  plot(1:t, Nt(r[i], Nt0, t), type = 'l', xlab = 'time', ylab = 'Population size',
       main = paste('r =', r[i]), ylim =c(0, 20000))
  abline(h = 10000, lty = 2, col='grey')
}

populations <- list()

for (i in 1:length(r)) {
  pop <- Nt(r[i], Nt0, t)
  populations[[paste0("pop", i)]] <- pop
}


df <- as.data.frame(populations)

time<-c(1:30)
df<-cbind(df,time)

# Sample for several populations
pop_sample <- c("pop2","pop4","pop8","pop10")

# Create a copy of the original data frame
modified_df <- df

# Loop over each column
for (col in pop_sample) {
  # Get 5 random indices from the current column
  sampled_indices <- sample(1:nrow(df), size = 5, replace = FALSE)
  
  modified_df[, col] <- NA
  modified_df[sampled_indices, col] <- df[sampled_indices, col]
}


#linear regression for each population
num_pops <- 10 

lm_models <- list()
mod_summaries <- list()

for (i in 1:num_pops) {
  #Use lm() to check for significance for every population
  lm_models[[i]] <- lm(modified_df[[paste0("pop", i)]] ~ modified_df$time)
  mod_summaries[[i]] <- summary(lm_models[[i]])
}
mod_summaries

# Everything is very significant...Wait, noise!

##############################################################
#Simple model with different intrinsic growth rate with noise#
##############################################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set seed to get reproducible simulations
set.seed(16)
#Adding in the noise 
#simple model of logistic growth
dNt <- function(r, N) r * N * (1 - N/20000)

# iterate growth through time
#Nt <- function(r, N, t,noise_sd) {
#  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
#    N[i + 1] <- N[i] + dNt(r, N[i])
    # Add noise
#    N[i + 1] <- N[i + 1] + rnorm(1, 0, noise_sd)
#  }
#  N
#}
#population will go negative when running logistic growth with noise
# iterate growth through time
Nt <- function(r, N, t, noise_sd) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + dNt(r, N[i])
    # Add noise
    noise <- rnorm(1, 0, noise_sd)
    if (N[i + 1] + noise < 0) {
      # If population becomes negative, set it to be zero. Population size cannot be negative.
      N[i + 1] <- 0
    } else {
      N[i + 1] <- N[i + 1] + noise
    }
  }
  N
}
# Parameters
t <- 30
r = c(0.2,0.15,0.125,0.10,0.09,0.08,0.075,0.06,0.05,0.04,-0.04,-0.05,-0.06,-0.075,-0.08,-0.09,-0.10,-0.125,-0.15,-0.2)
Nt0 <- 10000
noise_sd <- 500  # Standard deviation of noise

populations <- list()

for (i in 1:length(r)) {
  pop <- Nt(r[i], Nt0, t,noise_sd)
  populations[[paste0("pop", i)]] <- pop
}


df <- as.data.frame(populations)

time<-c(1:30)
df<-cbind(df,time)

par(mfrow=c(2,5))
# Plot population growth over time with noise

for (i in seq_along(r)) {
  plot(df$time, df[[i]], type = 'l', xlab = 'time', ylab = 'Population size',
       main = paste('r =', r[i]), ylim = c(0, 20000))
  abline(h = 10000, lty = 2, col = 'grey')
}

# Sample for several populations
pop_sample <- c("pop2","pop4","pop8","pop10","pop12","pop14","pop16","pop18","pop20")

modified_df <- df

# Loop over each column
for (col in pop_sample) {
  # Get 10 random number from the current column
  sampled_indices <- sample(1:nrow(df), size = 10, replace = FALSE)

  modified_df[, col] <- NA
  modified_df[sampled_indices, col] <- df[sampled_indices, col]
}

#linear regression for each population
num_pops <- 20 

lm_models <- list()
mod_summaries <- list()

for (i in 1:num_pops) {
  #Use lm() to check for significance for every population
  lm_models[[i]] <- lm(modified_df[[paste0("pop", i)]] ~ modified_df$time)
  mod_summaries[[i]] <- summary(lm_models[[i]])
}
mod_summaries
#We could make the population declining the fast to be non-significance by introducing noise, but I might need to try better number combinations...

# Create a separate plot for each column which stands for a population
for (i in 1:num_pops) {
  # Create a new plot for each population
  plot(modified_df$time, modified_df[[paste0("pop", i)]], pch = 20, type = 'p', las = 1,
       xlab = 'Time',
       ylab = 'Population size')
  
  lm_model <- lm(modified_df[[paste0("pop", i)]] ~ modified_df$time)
  abline(lm_model)
  # Adding p-value to the plot
  my.p <- summary(lm_model)$coefficients[2,4]
  my.p<-format(my.p, digits = 3)
  #mtext(my.p, side=3)
  legend('topleft', legend = my.p, bty = 'n')
}

##############################################################
##############Bayesian analysis using stan####################
##############################################################


library(rstan)

model1<-stan("stan/simpleSimMdl.stan",
             data = list(N=nrow(modified_df),
                         year = modified_df$time,
                         ypred = modified_df$pop1))
summary(model1)$summary
par(mfrow=c(1,1))
plot(modified_df$time,modified_df$pop1)
abline(a=29,b=129)
##############################################################
#############Linear decline without noise#####################
##############################################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# iterate growth through time
Nt <- function(r, N, t, noise_sd) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + r
  }
  N
}
t <- 20
r <- c(400, 300, 200, 100, -100, -200, -300, -400)
Nt0 <- 10000
Nt(r[i],Nt0,t,noise_sd[i])

par(mfrow=c(2,4))
for (i in seq_along(r)) {
  plot(1:t, Nt(r[i], Nt0, t), type = 'l', xlab = 'time', ylab = 'Population size',
       main = paste('r =', r[i]), ylim =c(0, 20000))
  abline(h = 10000, lty = 2, col='grey')
}
##############################################################
#############Linear decline with noise########################
##############################################################

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

set.seed(121)
# iterate growth through time
Nt <- function(r, N, t, noise_sd) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + r
    # Add noise
    noise <- rnorm(1, 0, noise_sd)
    if (N[i + 1] + noise < 0) {
      # If population becomes negative, set it to be zero. Population size cannot be negative.
      N[i + 1] <- 0
    } else {
      N[i + 1] <- N[i + 1] + noise
    }
  }
  N
}
t <- 20
r <- c(2000, 1800, 1600, 1200, -1200, -1600, -1800, -2000)
Nt0 <- 100000
noise_sd <- c(4000,5000,6000,7000,8000,9000,10000,11000)

populations <- list()

for (i in 1:length(r)) {
  pop <- Nt(r[i], Nt0, t,noise_sd[i])
  populations[[paste0("pop", i)]] <- pop
}


df <- as.data.frame(populations)

time<-c(1:20)
df<-cbind(df,time)
par(mfrow = c(2, 4))
# Plot population growth over time with noise

for (i in seq_along(r)) {
  plot(df$time, df[[i]], xlab = 'time', ylab = 'Population size',
       main = paste('r =', r[i]), ylim = c(0, 200000))
  abline(h = 100000, lty = 2, col = 'grey')
}
# linear regression for each population
num_pops <- 8 

lm_models <- list()
mod_summaries <- list()
for (i in 1:num_pops) {
  # Use lm() to check for significance for every population
  lm_models[[i]] <- lm(df[[paste0("pop", i)]] ~ df$time)
  mod_summaries[[i]] <- summary(lm_models[[i]])
}
mod_summaries
# We could make the population declining the fast to be non-significance by introducing noise, but I might need to try better number combinations...

# Create a separate plot for each column which stands for a population
for (i in 1:num_pops) {
  # Create a new plot for each population
  plot(df$time, df[[paste0("pop", i)]], pch = 20, type = 'p', las = 1,
       xlab = 'Time',
       ylab = 'Population size')
  
  lm_model <- lm(df[[paste0("pop", i)]] ~ df$time)
  abline(lm_model)
  # Adding p-value to the plot
  my.p <- summary(lm_model)$coefficients[2,4]
  my.p<-format(my.p, digits = 3)
  # mtext(my.p, side=3)
  legend('topleft', legend = my.p, bty = 'n')
}

# extract estimated slopes
est <- list()

for (i in 1:num_pops) {
  lm_model[[i]] <- lm(df[[paste0("pop", i)]] ~ df$time)
  est[[paste0("pop", i)]] <- summary(lm_model[[i]])$coefficients[2, 1]
}
est <- t(as.data.frame(est))
colnames(est)[1]<-"estimates"
population = c("pop1","pop2","pop3","pop4","pop5","pop6","pop7","pop8")
est<-cbind(est,r,population)
est<-as.data.frame(est)

# plot true slope vs. lm estimated slope
par(mfrow = c(1, 1))
plot(est$estimates,est$r)
text(est$estimates, est$r, labels=population, cex= 1)
abline(a=0,b=1)

##############################################################
#############18 April 2024 linear model with noise############
##############################################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

simulate_population <- function(a, b, t, noise_sd) {
  y <- numeric(t)
  time <- 1:t
  for (i in time ) {
    # population at next time step is population at current time + pop growth
    y[i] <- a*i + b # EMW: Here, we add b at every time step is not the same as an intercept in linear regression
    # EMW cont: it's also no longer 'the starting population size' so I think this is part of the problem
    # y is the population of certain time
    # a is the increasing/decreasing rate
    # b is the starting population size
    # x is the time
    # Add noise
    noise <- rnorm(1, 0, noise_sd)
    if (y[i] + noise < 0) {
      # If population becomes negative, set it to be zero. Population size cannot be negative.
      y[i] <- 0
    } else {
      y[i] <- y[i] + noise
    }
    # Fit a linear model to the simulated data
    model <- lm(y ~ time)
    # Extract the estimated slope (coefficient of time)
    estimated_slope <- coef(model)["time"]
    # Get estimated slope - true slope
    slope_difference <- estimated_slope - a # EMW: ratio likely better
    }
  list(
    populations = y,
    slope = estimated_slope,
    slope_difference = slope_difference
  )
}



t <- 10
a <- c(2000, 1800, 1600, 1200, -1200, -1600, -1800, -2000)
b <- 100000
noise_sd <- 4000

populations <- list()

for (i in 1:length(a)) {
  result <- simulate_population(a[i], b, t, noise_sd)
  populations[[paste0("pop", i)]] <- list(result$populations,
                                          result$slope,
                                          result$slope_difference
  )
}
print(populations)

# Comments from Lizzie

# 1. 
# In general we rarely write code that erases the workspace in the middle (sort of endless new code).
# Instead we would push the code and then as we update it, we would delete old code we don't need.
# Here it was tricky as we're exploring. I would have probably either:
# (a) kept this file and deleted what was in it mostly (as I did for my file) OR
# (b) wrote a new file for the linear work and then evenually delete the older file when happy with new file  

# 2. 
# Always BUILD you code ... and have a set of logical tests you run to make sure it works. 
# I am not sure what happened here, but I am not sure this f(x) ever worked because 
# When I tried to test that the estimated and given slope matched (with 0 noise) via  plot, they did not.
# When building a f(x) you either need to usually start with one working piece, get the basics to work, then build the f(x) from it. 
# And either way, you need more tests of it. 
# Here though, I know we are trying to get this done FAST so I understand if you ran out of time...
# but better to one piece that is correct and not done. 
# You may know all of this, but a good reminder either way!

