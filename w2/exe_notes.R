n <- 10
m <- 2
X <- matrix(rep(1,n*m), byrow=TRUE, nrow=n, ncol=m) 
X
y <- matrix(rep(1,m), byrow=TRUE, nrow=1, ncol=m)
y
X%*%t(y)
#ans1
m*(2*n-1)
n*(2*m-1)
(2*m-1)*n
2*m*n-n


#ans2

t(X)%*%X
(2*n-1)*m^(2)
2*n*m^(2)-m^(2)


#ans3
X%*%t(X)
n^2*(2*m-1)

sum(diag(X*X))


n <- 10
m <- 3
X <- matrix(rep(1,n*m), byrow=TRUE, nrow=n, ncol=m) 
X
Y <- matrix(rep(1,m), byrow=TRUE, nrow=m, ncol=n)
Y

X%*%Y
(2*m-1)*n^2

Y%*%X
(2*n-1)*m^2




df1 <- data.frame(x=rnorm(1000), y=rnorm(1000),z=rep(1,1000))
df2 <- data.frame(x=rnorm(1000, mean = 4,sd = 1), y=rnorm(1000, mean = 4,sd = 1),z=rep(2,1000))
df3 <- data.frame(x=rnorm(1000, mean = -11,sd = 2), y=rnorm(1000, mean = 10,sd = 3),z=rep(3,1000))

ggplot(data=df1, aes(x=x,y=y,color=z)) + geom_point() + geom_point(data = df2) + geom_point(data = df3)


install.packages('mvtnorm')
library(mvtnorm) # Install this package if you haven't already got it


GP_ll = function(p) {
  sig_sq = p[1]
  rho_sq = p[2]
  tau_sq = p[3]
  Mu = rep(0, length(x))
  Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))
  browser()
  ll = dmvnorm(y, Mu, Sigma, log = TRUE)
  return(-ll)
}


#Below is some code to produce a grid search of all the likelihood:
grid_size = 20
tau_sq_grid = seq(0.1, 5, length = grid_size)
sig_sq_grid = seq(0.1, 5, length = grid_size)
rho_sq_grid = seq(0.1, 5, length = grid_size)
all_grid = expand.grid(sig_sq_grid, tau_sq_grid, rho_sq_grid)
sig_ll = rep(NA, grid_size^3)
for(i in 1:length(sig_ll)) sig_ll[i] = GP_ll(as.numeric(all_grid[i,]))
all_out = cbind(all_grid, sig_ll)
head(all_out)
colnames(all_out) = c('sig_sq_grid', 'tau_sq_grid', 'rho_sq_grid', '-ll')


##exe 3
GP_ll2_tmp = function(p) {
  sig_sq = exp(p[1])
  rho_sq = exp(p[2])
  tau_sq = tau_sq_grid[cntr]
  Mu = rep(0, length(x))
  Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))
  ll = dmvnorm(y, Mu, Sigma, log = TRUE)
  print(paste(cntr,tau_sq,-ll))
  if(min_ll_prev > -ll){
    min_ll_prev <<- -ll
    min_tau_idx <<- cntr 
  }
  cntr <<- cntr + 1
  return(-ll)
}
iters <-500
min_tau_idx = -1
min_ll_prev = Inf
tau_sq_grid = seq(0.1, 2, length = iters)
cntr <- 1
answer_NR = nlminb(start = rep(0, 2), objective = GP_ll2_tmp)
answer_NR
GP_ll2_tmp(answer_NR$par)
round(tau_sq_grid[min_tau_idx],3)
