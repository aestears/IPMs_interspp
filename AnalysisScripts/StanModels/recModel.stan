// generated with brms 2.19.0
functions {
  /* hurdle gamma log-PDF of a single response
   * Args:
   *   y: the response value
   *   alpha: shape parameter of the gamma distribution
   *   beta: rate parameter of the gamma distribution
   *   hu: hurdle probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_gamma_lpdf(real y, real alpha, real beta, real hu) {
    if (y == 0) {
      return bernoulli_lpmf(1 | hu);
    } else {
      return bernoulli_lpmf(0 | hu) +
             gamma_lpdf(y | alpha, beta);
    }
  }
  /* hurdle gamma log-PDF of a single response
   * logit parameterization of the hurdle part
   * Args:
   *   y: the response value
   *   alpha: shape parameter of the gamma distribution
   *   beta: rate parameter of the gamma distribution
   *   hu: linear predictor for the hurdle part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_gamma_logit_lpdf(real y, real alpha, real beta, real hu) {
    if (y == 0) {
      return bernoulli_logit_lpmf(1 | hu);
    } else {
      return bernoulli_logit_lpmf(0 | hu) +
             gamma_lpdf(y | alpha, beta);
    }
  }
  // hurdle gamma log-CCDF and log-CDF functions
  real hurdle_gamma_lccdf(real y, real alpha, real beta, real hu) {
    return bernoulli_lpmf(0 | hu) + gamma_lccdf(y | alpha, beta);
  }
  real hurdle_gamma_lcdf(real y, real alpha, real beta, real hu) {
    return log1m_exp(hurdle_gamma_lccdf(y | alpha, beta, hu));
  }

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
  real<lower=0> shape;  // shape parameter
  real<lower=0,upper=1> hu;  // hurdle probability
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += normal_lpdf(b | 0,1000);
  lprior += student_t_lpdf(Intercept | 3, -0.7, 2.5);
  lprior += gamma_lpdf(shape | 0.01, 0.01);
  lprior += beta_lpdf(hu | 1, 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    mu = exp(mu);
    for (n in 1:N) {
      target += hurdle_gamma_lpdf(Y[n] | shape, shape / mu[n], hu);
    }
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}