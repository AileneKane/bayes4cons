/*----------------------- Data --------------------------*/
data {
  int TT; // Length of state and observation time series
  vector[TT] y; // Observations
  real w0; // Initial state value
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
  sdo ~ normal(0, 100); // Half-normal since real<lower=0> above and mean 0 - Vague
  sdp ~ normal(0, 100); // Half-normal since real<lower=0> above and mean 0 - Vague
  beta0 ~ normal(0, 100);
  beta1 ~ normal(0, 100); 
  // Distribution for the first state
  w[1] ~ normal(w0, sdp);
  // Distributions for all other states
  for(t in 2:TT){
    w[t] ~ normal(w[t-1] + beta0 + beta1*exp(w[t-1]), sdp);
    }
  // Distributions for the observations
  for(t in 1:TT){
    y[t] ~ normal(exp(w[t]), sdo);
    }
}
