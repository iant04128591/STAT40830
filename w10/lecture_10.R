# Lecture 10 - Stan

# Useful documents:
# The Stan manual: http://mc-stan.org/documentation/ Possibly the longest manual of all time (500+ pages)
# The main stan website http://mc-stan.org/
# https://github.com/stan-dev/example-models/wiki - example models
# More generally the github page https://github.com/stan-dev/
# Users group https://groups.google.com/forum/?fromgroups#!forum/stan-users

# Beware - the stan package (especially the syntax) changes often - report to discussion board if anything doesn't work

# Stan is not really useful for big data, but it's brilliant for small/middle data and for prototyping complex models

# Set up ------------------------------------------------------------------

# What is stan?
# Stan does complex statistical model building in very simple language. You write the code for the model, and stan does all the fitting
# You can do standard (and penalised) maximum likelihood and Bayesian analysis
# It's quite similar to WinBUGs and JAGS (which you might have used in other courses) but a bit more flexible, more widely supported, and much better documented
# It's named after the guy who invented Monte Carlo

# Installing stan - it's not just a standard package
# see https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
#install.packages('rstan')
library(rstan)
# The extras you need are the same as devtools so hopefully you already have them

# Always good to enable parallel running if available:
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Workflow
# 1) Write a stan model in a separate file (named .stan)
# 2) Wrangle your data into the correct format, save it as a list with the same names as in the stan file
# 3) Create a stan_model object (this will also check for syntax errors)
# 4) Fit the model with either the optimizing (max likelihood) function or sampling (Bayes) functions
# 5) Play with the results either in R or using shiny

# Note that you can do steps 3 and 4 in one go if doing full Bayes with the stan function. I prefer to do it separately

# Simple regression -------------------------------------------------------

# The .stan file - lin_reg_1.stan
# Different blocks for data, parameters and the model
# Every line ends in ;
# Uses # or // for comments
# Strongly typed with some (confusing) vector/range constraints

# The Rcode - go back to lecture 2
library(boot)
head(motor)

# Set it up in a list with the same names as the stan file
motor_data_lr = list(N = nrow(motor),
                 x = scale(motor$times)[,1],
                 y = scale(motor$accel)[,1])

# Maximum likelihood version
stan_model_lr = stan_model('./lin_reg_1.stan')
stan_run_lr_ml = optimizing(stan_model_lr, data = motor_data_lr)
print(stan_run_lr_ml)
# Not particularly friendly output - note - no uncertainties

# Plot
library(magrittr)
motor %$% plot(motor$times, motor$accel)
abline(a = stan_run_lr_ml$par['alpha'], b = stan_run_lr_ml$par['beta'])

# The full Bayesian way
stan_run_lr_bayes = sampling(stan_model_lr, data = motor_data_lr)
print(stan_run_lr_bayes)
plot(stan_run_lr_bayes) # Not always helpful if parameters on very different scales
plot(stan_run_lr_bayes, par="beta") # Not always helpful if parameters on very different scales
plot(stan_run_lr_bayes, par="alpha") # Not always helpful if parameters on very different scales

# The Shiny version is also now pretty useful
#install.packages('shinystan')
library(shinystan)
launch_shinystan(stan_run_lr_bayes)

# Go and have a look at the lin_reg_1.stan file, and compare to lin_reg_2.stan lin_reg_3.stan - all the same model

# Version 2 has a loop
stan_model_lr_2 = stan_model('./lin_reg_2.stan')
stan_run_lr_ml_2 = optimizing(stan_model_lr_2, data = motor_data_lr)
print(stan_run_lr_ml_2)

# Version 3 has a matrix of explanatory variables
motor_data_lr$K = 1
motor_data_lr$x = matrix(motor_data_lr$x, ncol = 1)
stan_model_lr_3 = stan_model('./lin_reg_3.stan')
stan_run_lr_ml_3 = optimizing(stan_model_lr_3, data = motor_data_lr)
print(stan_run_lr_ml_3)

# Remember these are stochastic optimisations (especially the Bayesian one) so you won't get the exact same results every time.

## EXERCISE 1
# Using version 3 of the stan code, fit Bayesian models which have multiple explanatory variables. 
# These extra explanatory variables should be powers of x (go back to lecture 2 e.g. lines 62 onwards to see how this is done). 
# Fit a model with intercept, linear, square and cubic components. Look at the 50% intervals produced by the model. What are the 
# 50% intervals (to 2dps) for each term
# Linear: 
# Quadratic: 
# Cubic: 

motor_data_lr_3 = list(
                     N = nrow(motor),
                     K = 3,
                     x = cbind(scale(motor$times)[,1],scale(motor$times)[,1]^2,scale(motor$times)[,1]^3),
                     y = scale(motor$accel)[,1])

stan_model_lr_3 = stan_model('./lin_reg_3.stan')

stan_run_lr_ml_3 = optimizing(stan_model_lr_3, data = motor_data_lr_3)
print(stan_run_lr_ml_3)

stan_run_lr_ml_3 = sampling(stan_model_lr_3, data = motor_data_lr_3)
print(stan_run_lr_ml_3)
plot(stan_run_lr_ml_3) # Not always helpful if parameters on very different scales


# Stan modelling structure ------------------------------------------------

# data block:
# Most common forms are:
# real and int
# vector and matrix
# more complex types: simplexes and arrays (not covered)

# Each of the above can have lower and upper limits
# Vectors and matrices require the dimension to be declared
# Some of the vector statements in stan can be very confusing

# parameters block
# Same types and restrictions as above
# Need to define every parameter you want to keep in the estimates at the end
# Objects can be defined in the model block but these will disappear

# model block
# Define the likelihood (completely necessary) that links the data to the parameters
# Optionally also define constraints (prior distributions in a Bayesian world)
# Unlike e.g. WinBUGS or JAGS you don't have to define constraints/priors on everything

# Two other useful blocks:
# transformed parameters
# If your parameters don't directly link with your model (e.g. logistic regression) you might create transformed parameteters (i.e. logit_p) that link the data with the parameters.
# Transformed parameters are reported as part of the standard parameter set

# generated quantities
# If there's a transformation of the parameters that isn't involved in the likelihood or the prior you can create it in this block
# If you create something here which is required in either the model block or the transformed parameters block you will run into problems

# Example of using transformed parameters and generated quantities
# See lin_reg_4.stan
motor_data_lr = list(N = nrow(motor),
                     K = 1,
                     x = matrix(scale(motor$times)[,1], ncol = 1),
                     y = scale(motor$accel)[,1])

stan_model_lr_4 = stan_model('./lin_reg_4.stan')
stan_run_lr_ml_4 = optimizing(stan_model_lr_4, data = motor_data_lr)
print(stan_run_lr_ml_4) # Now have the fits and residuals

# Stan tips
# You can use standard matrix calculations in lots of places
# You can define your own functions and probability distributions
# Ths big drawback of Stan is that you can't have discrete parameters. A big chunk of the manual is devoted to ways round this

# Print by de-bug
# You can shove a print command in any stan model and it will print out values
# The error messages have got a lot better

## EXERCISE 2
# a) Returning to the simple linear regression model, edit the parameters block of the lin_reg_4.stan file to add an upper 
#    bound of 0.9 on sigma, and a lower and upper bound of 0 and 1 respectively on alpha. Run the model using the sampling 
#   function to produce full probability distributions on the parameters. What is the median value of beta (to 2 dp)?

motor_data_lr = list(N = nrow(motor),
                     K = 1,
                     x = matrix(scale(motor$times)[,1], ncol = 1),
                     y = scale(motor$accel)[,1])

stan_model_lr_4__exe2a = stan_model('./lin_reg_4__exe2.stan')
stan_run_lr_gs_4 = sampling(stan_model_lr_4__exe2a, data = motor_data_lr)
print(stan_run_lr_gs_4) # Now have the fits and residuals

# b) Edit the model block of the lin_reg_4.stan (removing any changes you made from part a) to produce the same 
#    constraints (hint: add in a uniform prior on sigma and alpha) What are your two extra lines in the model block? 
#   (hint 2: you can check this worked because you will get the same value of beta from part a)

motor_data_lr = list(N = nrow(motor),
                     K = 1,
                     x = matrix(scale(motor$times)[,1], ncol = 1),
                     y = scale(motor$accel)[,1])
stan_model_lr_4__exe2b = stan_model('./lin_reg_4__exe2b.stan')
stan_run_lr_gs_4 = sampling(stan_model_lr_4__exe2b, data = motor_data_lr)
print(stan_run_lr_gs_4) # Now have the fits and residuals

# Simple classification ---------------------------------------------------

# A Binomial logit model - see bin_reg.stan
prostate = read.table('http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data', header = TRUE)

stan_data_bin_reg = list(N = nrow(prostate),
                         x = cbind(prostate$lcavol, prostate$age),
                         K = 2,
                         y = as.integer(prostate$gleason>6))

stan_model_bin_reg = stan_model('bin_reg.stan')
stan_run_bin_reg = optimizing(stan_model_bin_reg, data = stan_data_bin_reg)
print(stan_run_bin_reg)

# Look at the probabilities vs the data
p_vars = grep('^p\\[', names(stan_run_bin_reg$par))
p = stan_run_bin_reg$par[p_vars]
aggregate(p,
          by = list(stan_data_bin_reg$y),
          'mean')

# Looks ok
# Note that for models created using sampling, you can use extract
#//pars = extract(stan_model_bin_reg) //for models using the sampling function

# EXERCISE 3:
# Change the generated quantities block to calculate the Pearson residuals
# See e.g. http://data.princeton.edu/wws509/notes/c3s8.html for a definition
# What are the first three residual values (to 2 d.p.)

prostate = read.table('http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data', header = TRUE)

stan_data_bin_reg = list(N = nrow(prostate),
                         x = cbind(prostate$lcavol, prostate$age),
                         K = 2,
                         y = as.integer(prostate$gleason>6))

stan_model_bin_reg = stan_model('bin_reg_ex3.stan')
stan_run_bin_reg = optimizing(stan_model_bin_reg, data = stan_data_bin_reg)
print(stan_run_bin_reg)
p_vars = grep('^pear', names(stan_run_bin_reg$par))
stan_run_bin_reg$par[p_vars]


# Hierarchical regression -------------------------------------------------

# Earnings data - want to estimate log earnings from height based on age and ethnicity categories
earnings = read.csv('https://raw.githubusercontent.com/andrewcparnell/bhm_course/master/data/earnings.csv')
head(earnings)
ggplot(earnings, aes(x = height_cm, y = y)) +
  geom_jitter(aes(colour = as.factor(eth))) +
  xlab('height (cm)') +
  ylab('log(Earnings ($))') +
  theme_bw()

# Simple regression model
earnings_data_lr = list(N = nrow(earnings),
                        x = earnings$height_cm,
                        y = earnings$y)

earnings_model_lr = stan_model('./lin_reg_1.stan')
earnings_run_lr_bayes = sampling(earnings_model_lr, data = earnings_data_lr)
#launch_shinystan(earnings_run_lr_bayes)
plot(earnings_run_lr_bayes)
print(earnings_run_lr_bayes)

# This fits a model with one overall slope. But what it the slopes vary across different categories?

# Consider a model now where we estimate the slope and intercept for different ethnic groups
earnings_data_lr_5 = list(N = nrow(earnings),
                          N_cat = 4,
                          x = earnings$height_cm,
                          y = earnings$y,
                          cat = earnings$eth)

# Look at the clever indexing in this model
earnings_model_lr_5 = stan_model('./lin_reg_5.stan')
earnings_run_lr_bayes_5 = sampling(earnings_model_lr_5, data = earnings_data_lr_5)
plot(earnings_run_lr_bayes_5) # Now have a intercept and slope for each
plot(earnings_run_lr_bayes_5, pars = 'beta') # Now have a intercept and slope for each

# A 'superior' model can be created by constraining the intercepts and slopes to be similar across groups
# e.g. alpha[eth[i]] ~ normal(mu_alpha, sigma_alpha)
# beta[eth[i]] ~ normal(mu_beta, sigma_beta)
# This is a *hierarchical* model
earnings_model_lr_3 = stan_model('./lin_reg_6.stan')
earnings_run_lr_bayes_3 = sampling(earnings_model_lr_3,
                                   data = earnings_data_lr_2) # Note the warnings
plot(earnings_run_lr_bayes_3) # Now have a intercept and slope for each
plot(earnings_run_lr_bayes_3, pars = 'beta') # Now have a intercept and slope for each
#launch_shinystan(earnings_run_lr_bayes_3)

## EXERCISE 4
# Create a ggplot of the fitted lines from each group using the model in lin_reg_5.stan (adapted if required). 
# Use a facet command to put each ethnic group in a different panel. (Extra kudos if you can get the 50% uncertainty bands on the 
# plots too). Which ethnic group shows the weakest relationship between height and log earnings?

str(earnings)

ggplot(earnings, aes(x = height_cm, y = y)) +
  geom_jitter(aes(colour = as.factor(eth))) +
  xlab('height (cm)') +
  ylab('log(Earnings ($))') + 
  geom_abline(data =  vline.dat ,aes(intercept = alpha, slope = beta)) +
  geom_abline(data =  vline.dat ,aes(intercept = alpha25, slope = beta25)) +
  geom_abline(data =  vline.dat ,aes(intercept = alpha75, slope = beta75)) +
  facet_wrap(~ eth) + 
  theme_bw()


class(earnings_run_lr_bayes_5)
slotNames(earnings_run_lr_bayes_5)
earnings_run_lr_bayes_5_df <- as.data.frame(earnings_run_lr_bayes_5)
round(mean(earnings_run_lr_bayes_5_df$`alpha[1]`),2)
round(sd(earnings_run_lr_bayes_5_df$`alpha[1]`),2)
as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`alpha[1]`, probs=c(0.25)),2))

str(vline.dat)
vline.dat <- data.frame(
              eth=1:4, 
              alpha=c(
                round(mean(earnings_run_lr_bayes_5_df$`alpha[1]`),2),
                round(mean(earnings_run_lr_bayes_5_df$`alpha[2]`),2),
                round(mean(earnings_run_lr_bayes_5_df$`alpha[3]`),2),
                round(mean(earnings_run_lr_bayes_5_df$`alpha[4]`),2)
                ),
              alpha25=c(
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`alpha[1]`, probs=c(0.25)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`alpha[2]`, probs=c(0.25)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`alpha[3]`, probs=c(0.25)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`alpha[4]`, probs=c(0.25)),2))              
                ),
              alpha75=c(
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`alpha[1]`, probs=c(0.75)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`alpha[2]`, probs=c(0.75)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`alpha[3]`, probs=c(0.75)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`alpha[4]`, probs=c(0.75)),2))              
              ),
              beta=c(
                round(mean(earnings_run_lr_bayes_5_df$`beta[1]`),2),
                round(mean(earnings_run_lr_bayes_5_df$`beta[2]`),2),
                round(mean(earnings_run_lr_bayes_5_df$`beta[3]`),2),
                round(mean(earnings_run_lr_bayes_5_df$`beta[4]`),2)
              ),
              beta25=c(
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`beta[1]`, probs=c(0.25)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`beta[2]`, probs=c(0.25)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`beta[3]`, probs=c(0.25)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`beta[4]`, probs=c(0.25)),2))              
              ),
              beta75=c(
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`beta[1]`, probs=c(0.75)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`beta[2]`, probs=c(0.75)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`beta[3]`, probs=c(0.75)),2)),
                as.double(round(quantile(x=earnings_run_lr_bayes_5_df$`beta[4]`, probs=c(0.75)),2))              
              )
            )


head(extract(earnings_run_lr_bayes_5))
extract(earnings_run_lr_bayes_5,pars= 'alpha', permuted = TRUE)

class(earnings_run_lr_bayes_5)[['alpha[1]']]

print(earnings_run_lr_bayes_5)@alpha

# Gaussian processes ------------------------------------------------------

# Let's try and fit the Gaussian process model of lecture 2 (go back and read if required)
# Here: y ~ MVN(Mu, Sigma)
# where Mu = 0
# and Sigma_{i,j} = sig_sq * exp( - rho_sq * (x[i] - x[j])^2 )
# and Sigma_{i,i} = sig_sq + tau_sq

# We'll go back and fit it to the motor data (also used in lecture 2)
head(motor)

# Reminder of data
motor_data = list(N = nrow(motor),
                  x = scale(motor$times)[, 1],
                  y = scale(motor$accel)[, 1])

# Set up model - look at code closely
gp_model_bayes = stan_model('r_code/lecture_10_files/GP.stan')

# Sample from it (or run optimizing instead)
gp_run_bayes = sampling(gp_model_bayes,
                        data = motor_data)

# Plot output
plot(gp_run_bayes) # Now have a intercept and slope for each
#launch_shinystan(gp_run_bayes)

# From this can get predictions as in lecture 2
par_means = summary(gp_run_bayes)$summary[,'mean']
sig_sq = par_means['sig_sq']
rho_sq = par_means['rho_sq']
tau_sq = par_means['tau_sq']
x_g = pretty(motor_data$x, n = 100)

# Now get predictions
x = motor_data$x
y = motor_data$y
C = sig_sq * exp( - rho_sq * outer(x_g, x, '-')^2 )

# Next Sigma = sig_sq * exp( - rho_sq * (x[i] - x[j])^2 ) + diag(tau_sq)
Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))

# Now create predictions
pred = C %*% solve(Sigma, y)

# Finally plot
plot(x, y)
lines(x_g, pred, col = 'blue') # Good fit

## EXERCISE 5
# (A challenge and not assessed). See if you can create some stan code to put the GP model on the logit of the proportions in the bin_reg.stan model using just age as a covariate.
# If you get stuck put a question in the discussion forum, or if you think you've done it, produce a plot of age (x-axis) against p-hat (y-axis), and start thinking about whether you want to do a PhD in data analytics!
