//Started March 2024---D. Loughnan

// Partial pooling model for simulation demonstration (Box 1)
// Intercept + slope---partial pooling on intercept

data {
  int<lower=0> N; //No. obs
  int<lower=0> Ngrp; //No. in group---population or species
  int group[N]; // Group type
  vector[N] year;
  real ypred[N]; //response
}

parameters {
  real a; 
  real b[Ngrp]; 

  real<lower=0> sigma_y; 

  real mu_b; 
  real<lower=0> sigma_b;

}

model {
    real mu_y[N]; 
  
  for(i in 1:N){
    mu_y[i]= a +b[group[i]]*year[i]; 
  }
  b ~ normal(mu_b, sigma_b);
  
  a ~ normal(180, 50); 
  mu_b ~normal(5,3); 
  sigma_b ~normal(0,10); 
  sigma_y ~normal(0,10); 
  ypred ~ normal(mu_y, sigma_y);

}


