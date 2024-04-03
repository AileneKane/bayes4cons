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
