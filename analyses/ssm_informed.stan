/*----------------------- Data --------------------------*/
data {
  int TT; // Length of state and observation time series
  vector[TT] y; // Observations
  real z0; // Initial state value
}

/*----------------------- Parameters --------------------------*/
parameters {
  real<lower=0> sdp; // Standard deviation of the process equation
  real<lower=0> sdo; // Standard deviation of the observation equation
  vector[TT] z; // State time series
  }

/*----------------------- Model --------------------------*/
model {
  // Prior distributions
  sdo ~ normal(1, 1); // Truncated-normal since real<lower=0> above and mean 1 - vague
  sdp ~ normal(0, 10); // Half-normal since real<lower=0> above - still vague
  // Distribution for the first state
  z[1] ~ normal(z0, sdp);
  // Distributions for all other states
  for(t in 2:TT){
    z[t] ~ normal(z[t-1], sdp);
    }
  // Distributions for the observations
  for(t in 1:TT){
    y[t] ~ normal(z[t], sdo);
    }
}
