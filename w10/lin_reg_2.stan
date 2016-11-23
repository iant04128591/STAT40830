data {
  # Define all the data here
  int<lower=0> N; # number of observations
  vector[N] x; # explanatory variable
  vector[N] y; # response variable
}
parameters {
  # Define parameters here
  real alpha; # intercept
  real beta; # slope
  real<lower=0> sigma; # residual sd
}
model {
  # This version has a loop instead - slower but (perhaps) more readable
  for(i in 1:N) {
    y[i] ~ normal(alpha + beta * x[i], sigma);
  }
}
