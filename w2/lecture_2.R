# Lecture 2 - advanced computation in R

# This is a much adapted/shortened version of Simon Wood's R course
# http://www2.warwick.ac.uk/fac/sci/statistics/apts/programme/statcomp/

# Why matrices? -----------------------------------------------------------

# Two of the most common uses of matrices in statistics/machine learning are in turning the current data (covariate x, response y) into predictions for a new set of x values, say x_g

# We're going to use the motor data set from the boot package and try to predict acceleration (accel) over time - these data arise from a motorcycle accident and measure head acceleration (in g forces) over time in milliseconds
library(boot)
head(motor)
dim(motor)
with(motor, plot(times, accel))

# Standardise the data first
x = scale(motor$times)[,1]
y = scale(motor$accel)[,1]

# Create a grid of new x-values
x_g = pretty(x, n = 100)
dim(x_g)
# Method 1) The regression version
# We calculate pred = X_g %*% (t(X)%*%X)^-1 t(X)%*%y
# (t(X)%*%X)^-1 t(X)%*%y = beta.hat from regression theory
# Here X and X_g are a design matrices (a column of 1's and then the column of x's) appled 
# to the covariate and grid values respectively

# Create the design matrices
X = cbind(1, x)
X_g = cbind(1, x_g)
#use 'solve' to invert a matrix 
pred  = X_g %*% solve(t(X)%*%X, t(X)%*%y)
plot(x, y)
lines(x_g, pred) # not a very good fit

# Let's try adding some higher powers in
X = cbind(1, x, x^2, x^3, x^4)
X_g = cbind(1, x_g, x_g^2, x_g^3, x_g^4)
pred  = X_g %*% solve(t(X)%*%X, t(X)%*%y)
lines(x_g, pred, col = 'red') # slighly better, should we go to higher powers2?

# Method 2) The Gaussian Process version
# pred = C(x_g, x) %*% Sigma^-1 y
# or pred = C %*% solve(Sigma, y) in R code
# Here C is the covariance between the grid points and the data points, Sigma is the variance matrix of the data, and y is the data vector

# calculating the difference beteen observation i and gridded value j, squaring it and scaling it by rho_sq
# exp(??) below is called an autocorrelation function
# rho_sq = parmater that controls how quickly the correlation decays
# sig_ma turns it from a correlation into a covariance
#calculates the covariance between the observated data and the grided data
# Now create C[i,j] = sig_sq * exp( - rho_sq * (x[i] - x_g[j])^2 )
rho_sq = 10
sig_sq = 5
tau_sq = 2
C = sig_sq * exp( - rho_sq * outer(x_g, x, '-')^2 )

#measures the distance between the observations themselves
# Next Sigma = sig_sq * exp( - rho_sq * (x[i] - x[j])^2 ) + diag(tau_sq)
Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))

# Now create predictions
pred = C %*% solve(Sigma, y)
lines(x_g, pred, col = 'blue') # Much nicer

# Note the similarity in these calculuations - matrix multiplication, transposing, inversion

# Breaking R ------------------------------------------

# Let's use system.time and replicate on the regression version:
X = cbind(1, x, x^2, x^3, x^4)
X_g = cbind(1, x_g, x_g^2, x_g^3, x_g^4)

# Now write
A = X_g
B = solve(t(X)%*%X) %*% t(X)
# We are trying to calculate A %*% B %*% y

# Look at the times required for each of the below
system.time(replicate(n = 1e5, A %*% B %*% y))
system.time(replicate(n = 1e5, A %*% (B %*% y)))
# Why is one of these so much faster than the other?

# The previous fit wasn't very good, could we add more powers?
p = 20
x_rep = matrix(rep(x, p+1), ncol = p+1, nrow = length(x))
X = sweep(x_rep, 2, 0:p, '^')
X_g_rep = matrix(rep(x_g, p+1), ncol = p+1, nrow = length(x_g))
X_g = sweep(X_g_rep, 2, 0:p, '^')
#pred  = X_g %*% solve(t(X)%*%X, t(X)%*%y) # Oh dear

# Can we do it instead with the standard R functions?
mod = lsfit(X, y, intercept = FALSE)
lines(x_g, X_g%*%mod$coefficients, col = 'red', lty = 'dotted')

# What if we tried to do it without standardising the data?
p = 5
x = motor$times
y = motor$accel
x_g = pretty(x, n = 100)
x_rep = matrix(rep(x, p+1), ncol = p+1, nrow = length(x))
X = sweep(x_rep, 2, 0:p, '^')
X_g_rep = matrix(rep(x_g, p+1), ncol = p+1, nrow = length(x_g))
X_g = sweep(X_g_rep, 2, 0:p, '^')
#pred  = X_g %*% solve(t(X)%*%X, t(X)%*%y) # Can't even do 5 powers!

# Matrix computation ------------------------------------------------------

# Quote from Wood: "It is easy to ruin a beautiful piece of theory by poor use of numerical linear algebra."

# Go back to the A %*% B %*% y example

# We also have this same problem with the GP example

# Re-create the standardised data
x = scale(motor$times)[,1]
y = scale(motor$accel)[,1]
x_g = pretty(x, n = 100)

# Recall pred = C %*% solve(Sigma, y)
A = sig_sq * exp( - rho_sq * outer(x_g, x, '-')^2 )
Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))
B = solve(Sigma)

# Timing - slower than regression as Sigma is bigger
system.time(replicate(n = 1e4, A %*% B %*% y)) # total 4.593
system.time(replicate(n = 1e4, A %*% (B %*% y))) # total 0.185

# The reason for the difference lies in the precedence:
system.time(replicate(n = 1e4, (A %*% B) %*% y)) # - also slow

# The answer is to do with floating point operations - flops
# By default with A%*%B%*%y if forms A%*%B first, then post-multiplies by y
# With the brackets included it creates B%*%y first, then pre-multiplies by A
# A and B are both big, dense matrices so forming A%*%B is costly
# y however is just a vector, the number of calculations requires is much smaller

# As another example, consider finding the trace of a matrix, the sum of the diagonals. Here are three methods for finding the trace of X %*% t(X)

# The obvious one
system.time(replicate(n = 1e4, sum(diag(X %*% t(X)))))

# The other way round
system.time(replicate(n = 1e4, sum(diag(t(X) %*% X)))) # Way faster

# The non-obvious one
system.time(replicate(n = 1e4, sum(X * X))) # Even faster again!

# The last one makes use of the clever linear algebra identity
# tr( X%*%t(X) ) = sum_{ij} X_{ij}*X_{ij}

## EXERCISE - flop counting

# A flop operation is an elementary operation (+, - , *, /, ^, etc). Some people argue that this definition is a bit too approximate
# For a vector x computing t(x)%*%x = x[1]*x[1] + x[2]*x[2] + ... + x[n]*x[n]
# is n multiplications with (n-1) additions so 2n-1 flops. People usually just write 2n as the -1 doesn't really matter for big n

# 1. For an n by m matrix X, and an m by 1 matrix y, how many flops is X%*%y?     n * m^2
# 2. For an n by m matrix X, how many operations is t(X) %*% X?                   m^2 * n^2
# 3. If n is much bigger than m, which has fewer flops t(X)%*%X or X%*%t(X)       t(X)%*%X     
# 4. How many flops is sum(X*X) in the fastest version of the trace calculation?  2*n^n -n^n

# One last thing to note is that flops are not the only issue: you need to balance
# flop counting with storage issues - storing large matrices can make things very slow

# Optimisation ------------------------------------------------------------

# For the Gaussian process model suppose we didn't know the values of rho_sq, sig_sq, and tau_sq
# Instead we want to estimate them via maximum likelihood

# The likelihood is:
# dMVN(Mu, Sigma)
# where Mu = 0
# and Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))
# Here dMVN is the multivariate normal density - see https://en.wikipedia.org/wiki/Multivariate_normal_distribution

# We can compute this likelihood using the dmvnorm function in the mvtnorm package
library(mvtnorm) # Install this package if you haven't already got it
# Remember that maximising the likelihood is the same as minimising the log-likelihood
# All the optimisation functions in R work by minimising rather than maximising a function

# Let's create a function which computes the negative log likelihood to be minimised
GP_ll = function(p) {
  sig_sq = p[1]
  rho_sq = p[2]
  tau_sq = p[3]
  Mu = rep(0, length(x))
  Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))
  ll = dmvnorm(y, Mu, Sigma, log = TRUE)
  return(-ll)
}

# Try calculating it for a given set of values
x = scale(motor$times)[,1]
y = scale(motor$accel)[,1]
GP_ll(c(5, 10, 2))
# The key question is - are those values (5, 10, 2) the best or can we score a lower value

# Graphical attempts: hold one value constant and plot all the others - a slice likelihood

# Try sig_sq_grid
grid_size = 50
sig_sq_grid = seq(0.1, 20, length = grid_size)
sig_ll = rep(NA, grid_size)
for(i in 1:grid_size) sig_ll[i] = GP_ll(c(sig_sq_grid[i], 10, 2))
plot(sig_sq_grid, sig_ll, type = 'l') # Perhaps 5 isn't the best value?
# You could also try for other parameters

# What about two at the same time
tau_sq_grid = seq(0.1, 20, length = grid_size)
both_grid = expand.grid(sig_sq_grid, tau_sq_grid)
sig_ll = rep(NA, grid_size^2)
for(i in 1:length(sig_ll)) sig_ll[i] = GP_ll(c(both_grid[i,1], both_grid[i,2], 2))
plot(both_grid[,1], both_grid[,2], pch = 19, xlab = 'tau_sq', ylab = 'sigma_sq',
     col = rgb(0, 0, 0, alpha = exp(-sig_ll)/max(exp(-sig_ll))))

# How can we do this with three parameters!?
# Is this a good way of visualising the problem?

## EXERCISE

# Below is some code to produce a grid search of all of the likelihood:
grid_size = 20
tau_sq_grid = seq(0.1, 5, length = grid_size)
sig_sq_grid = seq(0.1, 5, length = grid_size)
rho_sq_grid = seq(0.1, 5, length = grid_size)
all_grid = expand.grid(sig_sq_grid, tau_sq_grid, rho_sq_grid)
sig_ll = rep(NA, grid_size^3)
for(i in 1:length(sig_ll)) sig_ll[i] = GP_ll(as.numeric(all_grid[i,]))
all_out = cbind(all_grid, sig_ll)
colnames(all_out) = c('sig_sq_grid', 'tau_sq_grid', 'rho_sq_grid', '-ll')



# 1 Which row of all_out contains the smallest log likelihood?
which.min(all_out$`-ll`)
all_out[which.min(all_out$`-ll`),]

# 2 What are the values of sig_sq, tau_sq and rho_sq respectively that minimise the log-likelihood (to 3 d.p. - separate answers with a space)?
all_out[which.min(all_out$`-ll`),1:3]

# 3 The below function finds the minimum value of tau_sq across all values of sig_sq and rho_sq
# Fill in the missing values marked A and B
tau_sq_mins =aggregate(all_out$`-ll`, by = list(all_out$tau_sq_grid), 'min')
plot(tau_sq_grid, tau_sq_mins$x, type = 'l')
# This is known as a profile likelihood

# Optimisation methods ----------------------------------------------------

# The general rule for optimising (i.e. minimising) a function f(theta) is:
# 1) Guess at some initial values for theta
# 2) Use as much information as you have about f to choose a new value theta^new for which f(theta^new) should be lower. This information might concern the first and second derivatives of f if available, or anything else to hand. Many optimisation algorithms only update one element of theta at a time, but with more information it's usually more efficient to update multiple elements
# 3) If f(theta^new) is really similar to f(theta) then stop, otherwise repeat 2 again

# Perhaps the simplest (or most intuitive) way of optimising is to use a Taylor expansion of f(theta) and cut off at either one or two derivatives. 
#If you cut off at 1 derivative then you are doing 'steepest descent' (sometimes called gradient descent), 
#if you cut off at 2 you are doing Newton-Raphson

# Gradient descent works by setting:
# theta^new = theta - alpha * gradient(f(theta))
# where alpha is a chosen step-length parameter, and
# gradient(f(theta)) is the first derivative of f(theta) - this might be available algebraically or only numerically

# If numerically calculuating a gradient we can use:
# gradient(f(theta)) = (1/h) * (f(theta + h) - f(theta))
# This is sometimes a little bit imprecise, people sometimes recommend the slower:
# gradient(f(theta)) = (1/(2*h)) * (f(theta + h) - f(theta - h))

# Need to be careful here as these parameters are all constrained to be positive
# Instead specify the parameters on the log scale - they will now be unbounded
GP_ll2 = function(p) {
  sig_sq = exp(p[1])
  rho_sq = exp(p[2])
  tau_sq = exp(p[3])
  Mu = rep(0, length(x))
  Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))
  ll = dmvnorm(y, Mu, Sigma, log = TRUE)
  return(-ll)
}
GP_ll2(log(c(5, 10, 2)))

# Here is a function to numerically calculate the gradient of the Gaussian process:
grad_GP = function(p, h = 1e-3) {
  grad = rep(NA, 3)
  curr = GP_ll2(p)
  grad[1] = (1/h) * (GP_ll2(p+c(h,0,0)) - curr)
  grad[2] = (1/h) * (GP_ll2(p+c(0,h,0)) - curr)
  grad[3] = (1/h) * (GP_ll2(p+c(0,0,h)) - curr)
  return(grad)
}
# Test it
grad_GP(log(c(5, 10, 2)))

# Now run the optimisation
SD_optim = function(start, alpha = 1e-4, tol = 1e-4) {
  theta = start
  err = tol + 1
  while(err>tol) {
    theta_new = theta - alpha * grad_GP(theta)
    err = abs(GP_ll2(theta_new) - GP_ll2(theta))
    theta = theta_new
    #print(theta)
    print(GP_ll2(theta))
  }
  return(theta)
}

# Now run it
#answer = SD_optim(start = c(-0.3, 1.4, -1.7))
answer = SD_optim(start = rep(0, 3))
GP_ll2(answer)
exp(answer)

# Did this work?
rho_sq = exp(answer[1])
sig_sq = exp(answer[2])
tau_sq = exp(answer[3])

# Create covariance matrices
C = sig_sq * exp( - rho_sq * outer(x_g, x, '-')^2 )
Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))

# Now create predictions
pred = C %*% solve(Sigma, y)
plot(x, y)
lines(x_g, pred, col = 'blue') # Not too bad

# Newton-Raphson ----------------------------------------------------------

# You can go one further and try to estimate the second derivative and use the N-R method
# but things start to get a little numerically unstable unless you're very careful

# The N-R method is implemented in R's function nlminb
answer_NR = nlminb(start = rep(0, 3), objective = GP_ll2)
GP_ll2(answer_NR$par) # A much better job!

# There are other optimisation routines in R's optim function

# 1) BFGS - a fancier Newton-type method
answer_BFGS = optim(rep(0, 3), GP_ll2, method = 'BFGS')
GP_ll2(answer_BFGS$par)

# 2) Nelder-Mead - a simplex/heuristic method
answer_NM = optim(rep(0, 3), GP_ll2, method = 'Nelder-Mead')
GP_ll2(answer_NM$par)

# 3) Simulated annealing - a cool method based on thermodynamics - requires no derivatives
answer_SANN = optim(rep(0, 3), GP_ll2, method = 'SANN')
GP_ll2(answer_SANN$par)

# These methods also allow you to perform constrained optimisation by restricting the values of some/all of the parameters

## EXERCISE

# In the previous exercise we created profile likelihoods by varying one parameter and optimising the other two over a grid of values. We're now in a position to write some code which, for every value on a grid for one parameter, optimises the remaining parameters.

# Write some code (using optim or nlminb) which for the following grid:
tau_sq_grid = seq(0.1, 2, length = 500)
# a) optimises the values of sig_sq and tau_sq for every value on the grid
# b) plots the tau_sq_grid values against the nagative log likelihood
# Hint 1: you'll need to re-write the GP_ll2 (or GP_ll) function to hold the tau_sq values constant
# Hint 2: use the <<- to globally assign the current value of tau_sq_grid
# What's the value of tau_sq that provides the minimum value (to 3 d.p.)?

# Generating random numbers -----------------------------------------------

# It's very important to be able to generate good uniform random numbers for all kinds of statistical purposes:
# - For simulating from other statistical probability distributions for all kinds of statistical modelling and machine learning tasks
# - For computing numerical integrals via Monte Carlo or similar
# - For optimisation methods such as simulated annealing
# - For running lotteries!
# - For cryptography and other security tasks

# Generating truly 'random' numbers, even uniformly on (0,1), is actually impossible.
# Most people use an equation of the form X_{i+1} = (a * X_i + b ) mod M
# s mod t is the remaining after dividing s by t. A good generator will have:
# - No patterns
# - Full period, i.e. doesn't revisit itself until it has cycled through all of the values

# For each value of X we can create a value between 0 and 1 by computing X/M

# Consider the generator where a = 65539, and M = 2^31 - seems reasonable?
# This was used once by IBM and is known as the infamous RandU sequence
# Let's generate some
n = 100000
a = 65539
M = 2^31
b = 0
x = rep(1, n)
for (i in 2:n) x[i] = (a * x[i-1] + b) %% M
u = x/(M-1)

# OK let's check
hist(u, breaks = 30) # Pretty good
qqplot((1:n - 0.5) / n, sort(u)) # Also good!
acf(u) # Also also good

# What about lags in multiple dimensions
U = data.frame(u1 = u[1:(n-2)],
                u2 = u[2:(n-1)],
                u3 = u[3:n])
plot(U$u1,U$u2,pch=".") # Any patterns?

# If you start to plot a cloud of points though
library(lattice)
cloud(u1 ~ u2 * u3, U, pch=".", col = 1,
      screen = list(z = 40, x = -70, y = 0)) # Starts to look a little bit suspicious

# What if you rotate?
cloud(u1 ~ u2 * u3, U, pch=".", col=1, screen = list(z = 40, x = 70, y = 0))
# Oh dear!

# If you're now performing a stochastic method which requires equal coverage in high dimensions you're going to run into problems.

# A better example is from Marsaglia's Die Hard battery of tests:
a = 69069
b = 1
M = 2^32
x = rep(1, n)
for (i in 2:n) x[i] = (a * x[i-1] + b) %% M
u = x/(M-1)
U = data.frame(u1 = u[1:(n-2)],
               u2 = u[2:(n-1)],
               u3 = u[3:n])
cloud(u1 ~ u2 * u3, U, pch=".", col = 1,
      screen = list(z = 40, x = -70, y = 0)) # Much better

# There are much more fancy and better generators out there, some of which combine multiple generators together

# Here's a really cool combined generator (From L’Ecuyer (1999))
X_1 = X_2 = Y = rep(1, n)
M_1 = 2^32 - 209
M_2 = 2^32 - 22853
for(i in 4:n) {
  X_1[i] = (1403580 * X_1[i-2] - 810728 * X_1[i-3]) %% M_1
  X_2[i] = (527612 * X_2[i-1] - 1370589 * X_2[i-3]) %% M_2
}
Y = (X_1 - X_2) %% M_1
u = Y/(M_1-1)

# Try some checks on this
hist(u, breaks = 100)
qqplot((1:n - 0.5) / n, sort(u))
acf(u)
plot(U$u1,U$u2,pch=".")
U = data.frame(u1 = u[1:(n-2)],
               u2 = u[2:(n-1)],
               u3 = u[3:n])
cloud(u1 ~ u2 * u3, U, pch=".", col=1, screen = list(z = 40, x = 70, y = 0))
# The period length for this is about 2^191 = 3.2 * 10^57

###

# Once you've got a U(0,1) variable you can transform it to something else using the inversion method
X = qnorm(u)
hist(X)

X = qgamma(u, shape = 1)
hist(X)

# There are more efficient methods for creating these random numbers than using the inversion method

## EXERCISE

# Let's play with some random number generators

# One thing that's often interesting to look for is how often certain orderings occur, e.g. u[i-1] < u[i+1] < u[i]. 
# This order should happen about one sixth of the time

# For the Marsaglia sequence, approx how often does the above relationship occur? (Hint: use the ordering in the U data frame)

# Here's another RNG called an additive congruential generator
# x[i] = (x[i-1] + x[i-2]) mod M
# so we need two starting values to get this working
# Using the seeds x[1] = x[2] = 1 and M as before (in the Marsaglia sequence), 
# generate a large set of values from this sequence
# First check uniformity (using e.g. qqplot and the cloud plots)
# Then check the ordering relationship. How often does the ordering relationship occur?




