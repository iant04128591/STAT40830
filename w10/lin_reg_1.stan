data {
  # Define all the data here
  #int<lower=0, upper =10> N; exception thrown if N < 0, N > 10 
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
  # Write out the model likelihood here
  y ~ normal(alpha + beta * x, sigma);
}
