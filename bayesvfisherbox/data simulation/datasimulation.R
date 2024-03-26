#################################################################
#Simple model with different intrinsic growth rate without noise#
#################################################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# simple model of logistic growth
dNt <- function(r, N) r * N * (1 - N/2000)

# iterate growth through time
Nt <- function(r, N, t) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + dNt(r, N[i])
  }
  N
}
t <- 20
r = c(0.3,0.25,0.2,0.15,0.1,-0.1,-0.15,-0.2,-0.25,-0.3)
Nt0 <- 1000

par(mfrow=c(2,5))
for (i in seq_along(r)) {
  plot(1:t, Nt(r[i], Nt0, t), type = 'l', xlab = 'time', ylab = 'Population size',
       main = paste('r =', r[i]), ylim =c(0, 2100))
  abline(h = 2000, lty = 2, col='grey')
}

populations <- list()

for (i in 1:length(r)) {
  pop <- Nt(r[i], Nt0, t)
  populations[[paste0("pop", i)]] <- pop
}


df <- as.data.frame(populations)

time<-c(1:20)
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
#Adding in the noise 
#simple model of logistic growth
dNt <- function(r, N) r * N * (1 - N/2000)

# iterate growth through time
Nt <- function(r, N, t,noise_sd) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + dNt(r, N[i])
    # Add noise
    N[i + 1] <- N[i + 1] + rnorm(1, 0, noise_sd)
  }
  N
}

# Parameters
t <- 20
r = c(0.3,0.25,0.2,0.15,0.1,-0.1,-0.15,-0.2,-0.25,-0.3)
Nt0 <- 1000
noise_sd <- 150  # Standard deviation of noise

par(mfrow=c(2,5))
# Plot population growth over time with noise
for (i in seq_along(r)) {
  plot(1:t, Nt(r[i], Nt0, t, noise_sd), type = 'l', xlab = 'time', ylab = 'Population size',
       main = paste('r =', r[i]), ylim =c(0, 2100))
  abline(h = 1000, lty = 2, col='grey')
}

populations <- list()

for (i in 1:length(r)) {
  pop <- Nt(r[i], Nt0, t,noise_sd)
  populations[[paste0("pop", i)]] <- pop
}


df <- as.data.frame(populations)

time<-c(1:20)
df<-cbind(df,time)

# Sample for several populations
pop_sample <- c("pop2","pop4","pop8","pop10")

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
