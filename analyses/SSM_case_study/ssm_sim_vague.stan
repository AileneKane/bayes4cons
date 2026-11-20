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

  // First latent state, conditional on known z0
  z[1] = w0;

  // Non-centered process model
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
  // sdo ~ exponential(1);
  // sdp ~ exponential(1);

  z_raw ~ std_normal();

  y ~ normal(z, sdp);
}


// data {
//   int<lower=2> TTT;
//   vector[TT] y;
//   real z0;
// }
// 
// parameters {
//   real beta0;
//   real beta1;
// 
//   real<lower=0> sdo;
//   real<lower=0> sdp;
// 
//   // real z0;
//   //vector[TT] z_raw;
//   vector[TT] z;
// }
// 
// 
// 
// model {
//   beta0 ~ normal(0, 100);
//   beta1 ~ normal(0, 100);
// 
//   sdo ~ exponential(1);
//   sdp ~ exponential(1);
// 
//   z0 ~ normal(0, 100);
//   //z_raw ~ std_normal();
//       z[1] ~ normal(z0 + beta0 + beta1*exp(z0), sdp);
//   // Distributions for all other states
//   for(t in 2:TT){
//     z[t] ~ normal(z[t-1] + beta0 + beta1*exp(z[t-1]), sdp);
//     }
//   // Distributions for the observations
//   for(t in 1:TT){
//     y[t] ~ normal(exp(z[t]), sdo);
//     }
// //  y ~ normal(z, sdp);
// }
// 
