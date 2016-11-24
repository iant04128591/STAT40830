data {
  int<lower=1> N;
  vector[N] x;
  vector[N] y;
}
transformed data {
  vector[N] mu;
    for (i in 1:N) mu[i] = 0;
}
parameters {
  real<lower=0> sig_sq;
  real<lower=0> inv_rho_sq;
  real<lower=0> tau_sq;
}
transformed parameters {
  real<lower=0> rho_sq;
  rho_sq = inv(inv_rho_sq);
}
model {
  matrix[N, N] Sigma;

  // off-diagonal elements
  for(i in 1:(N-1)) {
    for(j in (i+1):N) {
      Sigma[i, j] = sig_sq * exp(-rho_sq * pow(x[i] - x[j],2));
      Sigma[j, i] = Sigma[i, j];
    }
  }
  // diagonal elements
  for (k in 1:N) {
    Sigma[k, k] = sig_sq + tau_sq;
  }

  // Priors/constraints
  sig_sq ~ cauchy(0, 5);
  inv_rho_sq ~ cauchy(0, 5);
  tau_sq ~ cauchy(0, 5);

  // Likelihood
  y ~ multi_normal(mu, Sigma);
}
