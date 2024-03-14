//Started March 2024---D. Loughnan

// Simplest model for simulation demonstration (Box 1)
// Intercept + slope---no partial pooling

data {
  int<lower=0> N; 
  vector[N] year;
  vector[N] ypred; 
}

parameters {
  
  real b;
  real a;
  real<lower=0> sigma_y; //measurement error, noise 
 
}

transformed parameters {
  vector[N] mu;
  mu = a + b * year;
}

model {
  // priors
  a ~ normal(0, 10);
  b ~ normal(0, 10);
  sigma_y ~ normal(0,10); 
  // likelihood
  ypred ~ normal(mu, sigma_y);
}

