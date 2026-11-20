data {
  int<lower=2> TT;
  vector[TT] y;
  real w0;  // Known initial state
}

parameters {
  real beta0;
  real beta1;

  real<lower=0> sdo;
  real<lower=0> sdp;

  vector[TT] z_raw;
}

transformed parameters {
  vector[TT] z;

  z[1] = w0;

  // Non-centered 
  for (t in 2:TT) {
    z[t] = beta0 + beta1 * z[t-1]
           + sdo * z_raw[t-1];
  }
}

model {
beta0 ~ normal(0, 100);
  beta1 ~ normal(0, 100);
  
  sdo ~normal(0, 100);
  sdp ~ normal(0, 100);

  z_raw ~ std_normal();

  y ~ normal(z, sdp);
}


