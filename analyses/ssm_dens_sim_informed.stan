/*----------------------- Data --------------------------*/
data {
  int TT; // Length of state and observation time series
  vector[TT] y; // Observations
  real w0; // Initial state/population value (log)
  real sd0; // Standard deviation for initial state value
}

/*----------------------- Parameters --------------------------*/
parameters {
  real<lower=0> sdp; // Standard deviation of the process equation
  real<lower=0> sdo; // Standard deviation of the observation equation
  real<lower=0> beta0; // median rate of population growth rate when pop. size is 0
  real<upper=0> beta1; // how much the growth rate decreases with an increasing pop. size (we assume density dependence, > 0)
  vector[TT] w; // State time series
  }

/*----------------------- Model --------------------------*/
model {
  // Prior distributions
  sdo ~ normal(10, 0.1); // Half-normal since real<lower=0> above and mean 0 - Vague
  sdp ~ normal(0.05, 0.01); // Half-normal since real<lower=0> above and mean 0 - Vague
  beta0 ~ normal(log(1.3), 0.01);
  beta1 ~ normal(-0.005, 0.001); // Density dependence 
  // Distribution for the first state - set to be 20
  w[1] ~ normal(w0, sd0);
  // Distributions for all other states
  for(t in 2:TT){
    w[t] ~ normal(w[t-1] + beta0 + beta1*exp(w[t-1]), sdp);
    }
  // Distributions for the observations
  for(t in 1:TT){
    y[t] ~ normal(exp(w[t]), sdo);
    }
}
