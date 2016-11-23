data {
  # Define all the data here
  int<lower=0> N; # number of observations
  int<lower=0> K; # number of explanatory variables
  matrix[N, K] x; # explanatory variables
  int y[N]; # response variable
}
parameters {
  # Define parameters here
  real alpha; # intercept
  vector[K] beta; # slope
}
transformed parameters {
  vector[N] logit_p;
  logit_p = alpha + x * beta;
}
model {
  # Binomial(N, p)
  y ~ binomial_logit(1, logit_p); # Note you give it the logit of p rather than p
}
generated quantities {
  real p[N];
  for(i in 1:N)
    p[i] = exp(logit_p[i])/ (1 + exp(logit_p[i]));
}
