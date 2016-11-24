data {
  # Define all the data here
  int<lower=0> N; # number of observations
  int<lower=0> N_cat; # number of categories for intercept/slope
  vector[N] x; # explanatory variable
  vector[N] y; # response variable
  int<lower=0> cat[N]; # categorical variable
}
parameters {
  # Define parameters here
  real alpha[N_cat]; # intercept
  real beta[N_cat]; # Slope
  real<lower=0> sigma; # residual sd
}
model {
  # Write out the model likelihood here
  for(i in 1:N) {
    y[i] ~ normal(alpha[cat[i]] + beta[cat[i]] * x[i], sigma);
  }
}
