data {
  # Define all the data here
  int<lower=0> N; # number of observations
  int<lower=0> K; # number of explanatory variables
  matrix[N, K] x; # explanatory variables
  vector[N] y; # response variable
}
parameters {
  # Define parameters here
  real alpha; # intercept
  vector[K] beta; # slope
  real<lower=0> sigma; # residual sd
}
model {
  # In this version X is a matrix
  y ~ normal(alpha + x * beta, sigma);
}
