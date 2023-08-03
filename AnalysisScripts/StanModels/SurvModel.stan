// //## basic survival model: survival ~ binomial(alpha + Beta_1 * basalArea_genet + Beta_2 * age , sigma)
// 
// // bernoulli_logit_1.stan
// data {
//   // number of observations
//   int<lower=1> N;
//   // response
//   int y[N];
//   // number of columns in the design matrix X
//   int<lower=1>K;
//   // design matrix X
//   // should not include an intercept
//   matrix [N, K] x;
//   int prior_only;  // should the likelihood be ignored?
// }
// transformed data {
//   int Kc = K - 1;
//   matrix[N, Kc] Xc;  // centered version of X without an intercept
//   vector[Kc] means_X;  // column means of X before centering
//   for (i in 2:K) {
//     means_X[i - 1] = mean(x[, i]);
//     Xc[, i - 1] = x[, i] - means_X[i - 1];
//   }
// }
// parameters {
//   // regression coefficient vector
//   real alpha;
//   vector[Kc] beta;
// }
// transformed parameters {
//   real lprior = 0;  // prior contributions to the log posterior
//   lprior += normal_lpdf(beta | 0,1000);
//   lprior += student_t_lpdf(alpha | 3, 0, 2.5);
// }
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += bernoulli_logit_glm_lpmf(y | Xc, alpha, beta);
//   }
//   // priors including constants
//   target += lprior;
// }
// generated quantities {
//   // actual population-level intercept
//   real b_Intercept = alpha - dot_product(means_X, beta);
// }

// generated with brms 2.19.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
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
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += normal_lpdf(b | 0,1000);
  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += bernoulli_logit_glm_lpmf(Y | Xc, Intercept, b);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}