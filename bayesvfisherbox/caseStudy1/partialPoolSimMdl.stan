data {
  int<lower=0> N; //No. obs
  int<lower=0> Ngrp; //No. in group---population or species
  int group[N]; // Group type
  vector[N] year;
  real ypred[N]; //response
}

parameters {
  real a[Ngrp] ;
  real b[Ngrp]; 
  real mu_a; 
  real<lower=0> sigma_a;
  real mu_b; 
  real<lower=0> sigma_b; 
  real<lower=0> sigma_y; 
  
}

model {

real mu_y[N]; 

for(i in 1:N){
     mu_y[i] = a[group[i]] + b[group[i]] * year[i];
  }
  
a ~ normal(mu_a, sigma_a);
b ~ normal(mu_b, sigma_b);

//Priors
mu_a ~ normal(188, 50); 
sigma_a ~ normal(0,50);
mu_b ~ normal(0,10); 
sigma_b ~ normal(0,10); 
sigma_y ~ normal(0,10); 

ypred ~ normal(mu_y, sigma_y);
}

