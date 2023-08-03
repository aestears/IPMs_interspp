// //## basic growth model: size_tplus1 ~ normal(alpha + Beta_1 * basalArea_genet + Beta_2 * age , sigma)
// 
// data {
//   int<lower=0> N; // number of data items
//   int<lower=0> K; // number of predictors 
//   matrix[N, K] x;   // predictor matrix
//   //vector[N] x; //predictor vector
//   real y[N];      // outcome vector
// }
// parameters {
//   real alpha;           // intercept
//   vector[K] beta;      // coefficients for predictors
//   real<lower = 0> sigma;  // error scale
// }
// model {
//   // priors
//   alpha ~ normal(0., 10);
//   beta ~ normal(0., 10);
//   // likelihood function
//   y ~ normal(alpha + x * beta, sigma);  
// }

// generated with brms 2.19.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += normal_lpdf(b | 0,1000);
  lprior += student_t_lpdf(Intercept | 3, -4.1, 2.5);
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += normal_id_glm_lpdf(Y | Xc, Intercept, b, sigma);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}

