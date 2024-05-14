library(rstan)
library(bayesplot)
library(ggplot2)
# To run in parallel on multiple cores
options(mc.cores=parallel::detectCores())
# To avoid recompiling unchanged Stan program
rstan_options(auto_write=TRUE)

# This code is based on appendix S1 of Auger-Méthé et al. 2021 Ecological Monographs



# Simulate the data

# Create a vector that will keep track of the states
# It's of length T + 1 (+1 for t=0)
TT <- 200
z <- numeric(TT + 1)

# Standard deviation of the process variation
sdp <- 0.1

# Set the seed, so we can reproduce the results
set.seed(87687)
# For-loop that simulates the state through time, using i instead of t,
for(i in 1:TT){
  # This is the process equation
  z[i+1] <- z[i] + rnorm(1, 0, sdp)
  # Note that this index is shifted compared to equation in text,
  # because we assume the first value to be at time 0
}

# Create a vector that will keep track of the observations
# It's of length T
y <- numeric(TT)
# Standard deviation of the observation error
sdo <- 2
# For t=1, ... T, add measurement error
# Remember that z[1] is t=0
y <- z[2:(TT+1)] + rnorm(TT, 0, sdo)

# Fit the model with stan
dataStan <- list(y=y, TT=TT, z0=0)


### Vague priors

m.vague <- stan(file = "ssm_vague.stan",
                data = dataStan,
                chains = 3, iter = 30000)


# Traceplot for sdp
traceplot(m.vague, pars="sdp", inc_warmup = TRUE)
# All good

print(m.vague, max = 50)
sdo.est.vague <- rstan::extract(m.vague, pars=c("sdo"))[[1]]
sdp.est.vague <- rstan::extract(m.vague, pars=c("sdp"))[[1]]

# Posterior

m.vague.post <- as.matrix(m.vague)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(m.vague.post,
           pars = c("sdo", "sdp"),
           prob = 0.8) + plot_title +
  geom_vline(aes(xintercept = sdp), col="hotpink") +
  geom_vline(aes(xintercept = sdo), col="hotpink")

# Quick plot of priors
x <- seq(0, 200, by=0.1)
plot(dnorm(x, 0, 10) ~ x, ty="l")


#### Informed priors

m.informed <- stan(file = "ssm_informed.stan",
                data = dataStan,
                chains = 3, iter = 30000)


# Traceplot for sdp
traceplot(m.informed, pars="sdp", inc_warmup = TRUE)
# All good

print(m.vague, max = 50)
sdo.est.informed <- rstan::extract(m.informed, pars=c("sdo"))[[1]]
sdp.est.informed <- rstan::extract(m.informed, pars=c("sdp"))[[1]]

data.frame(sdo = c(sdo, median(sdo.est.vague), median(sdo.est.informed)), 
           sdo.025 = c(NA, quantile(sdo.est.vague, probs = 0.025), quantile(sdo.est.informed, probs = 0.025)),
           sdo.975 = c(NA, quantile(sdo.est.vague, probs = 0.975), quantile(sdo.est.informed, probs = 0.975)),
           sdp = c(sdp, median(sdp.est.vague), median(sdp.est.informed)),
           sdp.025 = c(NA, quantile(sdp.est.vague, probs = 0.025), quantile(sdp.est.informed, probs = 0.025)),
           sdp.975 = c(NA, quantile(sdp.est.vague, probs = 0.975), quantile(sdp.est.informed, probs = 0.975)))

# Posterior

m.informed.post <- as.matrix(m.informed)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(m.informed.post,
           pars = c("sdo", "sdp"),
           prob = 0.8) + plot_title +
  geom_vline(aes(xintercept = sdp), col="hotpink") +
  geom_vline(aes(xintercept = sdo), col="hotpink")

# Quick plot of priors
x <- seq(0, 200, by=0.1)
plot(dnorm(x, 1, 1) ~ x, ty="l")
