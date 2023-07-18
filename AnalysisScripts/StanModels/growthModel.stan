//## basic growth model: size_tplus1 ~ normal(alpha + Beta_1 * basalArea_genet + Beta_2 * age , sigma)

data {
  int<lower=0> N; // number of data items
  int<lower=0> K; // number of predictors 
  matrix[N, K] x;   // predictor matrix
  //vector[N] x; //predictor vector
  vector[N] y;      // outcome vector
}
parameters {
  real alpha;           // intercept
  vector[K] beta;      // coefficients for predictors
  real<lower = 0> sigma;  // error scale
}
model {
  //linear predictor mu
  vector[N] mu; 
  // calculate mu
  mu = alpha + x * beta;
  // likelihood function
  y ~ normal(mu, sigma);  
}

