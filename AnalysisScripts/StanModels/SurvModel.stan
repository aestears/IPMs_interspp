//## basic survival model: survives_tplus1 ~ binomial(alpha + Beta_1 * basalArea_genet + Beta_2 * age , sigma)
// use QR reparameterization ("Consequently, this QR reparameterization is recommended for linear and generalized linear models in Stan whenever 
//K > 1 and you do not have an informative prior on the location of  β" - from the Stan manual)

data {
  int<lower=0> N; // number of data items
  int<lower=0> K; // number of predictors 
  matrix[N, K] x;   // predictor matrix
  vector[N] y;      // outcome vector
}
transformed data {
  matrix[N, K] Q_ast;
  matrix[K, K] R_ast;
  matrix[K, K] R_ast_inverse;
  // thin and scale the QR decomposition
  Q_ast = qr_Q(x)[, 1:K] * sqrt(N - 1);
  R_ast = qr_R(x)[1:K, ] / sqrt(N - 1);
  R_ast_inverse = inverse(R_ast);
}
parameters {
  real alpha;           // intercept
  vector[K] theta;      // coefficients on Q_ast
  real<lower=0> sigma;  // error scale
}
model {
  y ~ normal(Q_ast * theta + alpha, sigma);  // likelihood
}
generated quantities {
  vector[K] beta;
  beta = R_ast_inverse * theta; // coefficients on x
}
