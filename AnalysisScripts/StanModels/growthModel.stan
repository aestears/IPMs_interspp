//## basic growth model: size_tplus1 ~ normal(alpha + Beta_1 * basalArea_genet + Beta_2 * age , sigma)

data {
  int<lower=0> N; // number of data items
  int<lower=0> K; // number of predictors 
  matrix[N, K] x;   // predictor matrix
  //vector[N] x; //predictor vector
  real y[N];      // outcome vector
}
parameters {
  real alpha;           // intercept
  vector[K] beta;      // coefficients for predictors
  real<lower = 0> sigma;  // error scale
}
model {
  // priors
  alpha ~ normal(0., 10);
  beta ~ normal(0., 10);
  // likelihood function
  y ~ normal(alpha + x * beta, sigma);  
}

