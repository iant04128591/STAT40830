# Lecture 4 - magrittr

# Sources
# Why bother with magrittr: http://civilstat.com/2015/10/why-bother-with-magrittr/
# Pipes: http://blog.revolutionanalytics.com/2014/07/magrittr-simplifying-r-code-with-pipes.html
# Magrittr vignette https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html

# Introducing magrittr ----------------------------------------------------

# Why use this? Let's start with an example

# A common function use in classification is the logsumexp function
# It's not in the base package but is in lots of fancier matrix and others
# It's definition is the log of the sum of the

# At its simplest the function is log( sum ( exp( some_data ) ) )
# So first exponentiate, then sum, then log
my_vec = 1:4
log(sum(exp(my_vec)))

# There's a bit of cognitive overload associated with this function as you have to read from right to left to see what's happening with your data

# Enter magrittr
library(magrittr)
my_vec %>% exp %>% sum %>% log

# We've re-written the function from left to right in a readable order
# The operators are separated by %>% - known as pipes
# Far fewer brackets - much more easily readable code
# Those of you who are familiar with other programming languages will be used to using piipes

# This will change the way you write R code!

# Magrittr: the basics  --------------------------------------------------

# The key to magrittr is understanding the pipe %>%
# Here are some simple examples
my_vec %>% log
my_vec %>% sum
my_vec %>% ( function(x) x^2 ) # Don't forget parentheses
my_fun = function(x, y = 2) x*y
my_vec %>% my_fun
my_vec %>% my_fun(y = 4)
my_fun2 = function(x, y) x*y
# my_vec %>% my_fun2 # This will not work
my_vec %>% my_fun2(y = -1)
my_vec %>% my_fun2(x = 3) # Interestingly this will

# You can then chain them up for real fanciness
my_vec %>% log %>% sum
my_vec %>% my_fun2(y = 4) %>% exp

# Magrittr has some alias functions which are useful for everyday commands
rnorm(1000) %>%
  multiply_by(7) %>%
  add(6) %>%
  hist

# You can also use slightly shorter (but uglier) code:
rnorm(1000) %>%
  '*'(7) %>%
  '+'(6) %>%
  hist

# You can also assign these things to objects for manipulation
new_data = rnorm(1000) %>%
  '*'(7) %>%
  '+'(6)

## EXERCISE 1

# a) Which of the following will find the log of the square root of the sequence 1 to 10? (Tick all that apply)

1:10 %>% log %>% sqrt
log(sqrt(1:10))           #**
1:10 %>% sqrt %>% log     #**
1:10 %>% `^`(0.5) %>% log #**
sqrt(log(1:10))
1:10 %>% sqrt %>% log(base = exp(1)) #**
1:10 %>% `^`(0.5) %>% log #**

# b) The function below to takes 1000 uniform random variables, converts them to normal using qnorm, then produces a histogram. Re-write it in magrittr format (for Blackboard marking purposes keep everything on one line and use spaces between %>% and =, etc)
hist(qnorm(runif(1000)), breaks = 30)

runif(1000) %>% qnorm %>% hist(breaks = 30)

# magrittr: typical workflow --------------------------------------------

# Have a look at the mtcars data set
head(mtcars)

# Consider the following things you want to do:
# 1) Transform miles per gallon into km per litre (by multiplying it by 0.4251)
# 2) Taking only those cars with horsepower bigger than 100
# 3) Aggregating the data set by cylinders and computing the means of all the remaining variables
# 4) Saving this new data set into a data frame

# Let's do it the traditional way
my_data = mtcars
my_data$kpl = my_data$mpg * 0.4251
my_data2 = subset(my_data, hp > 100)
my_data3 = aggregate(. ~ cyl, data = my_data2, FUN = 'mean')

# Or alternatively in one big line:
my_data4 = transform(aggregate(. ~ cyl,
                               data = subset(mtcars, hp > 100),
                               FUN = mean),
                     kpl = mpg * 0.4251)

# Now the magrittr way
my_data5 = mtcars %>%
  subset(hp > 100) %>%
  aggregate(. ~ cyl, data = ., FUN = 'mean') %>%
  within(., kpl <- mpg %>% multiply_by(0.4251))

# A few things to note about the above:
# - Notice that . is used everywhere to mark the full data frame
# - The within statement allows us to create a new variable (within is a base function) whilst keeping the whole data frame (different from with)
# - We have to use <- inside within not =
# - We can use nested chains of %>% inside other functions!

# Let's see which is faster
fun1 = function() {
  my_data4 = transform(aggregate(. ~ cyl,
                                 data = subset(mtcars, hp > 100),
                                 FUN = function(x) round(mean(x, 2))),
                       kpl = mpg*0.4251)
}
fun2 = function() {
  my_data4 = mtcars %>%
    subset(hp > 100) %>%
    aggregate(. ~ cyl, data = ., FUN = 'mean') %>%
    within(., kpl <- mpg %>% multiply_by(0.4251))
}
system.time(replicate(1e7, fun1))
system.time(replicate(1e7, fun2)) # Almost identical
# You don't lose any speed by using magrittr  

## Exercise 2

# Take the iris data (included by default in R) and write magrittr code that:
# 1) takes only those observations with sepal width > 3
# 2) computes the Petal.area as pi * Petal.length / 2 * Petal.width / 2
# 3) aggregates to produce the median Petal.area across species
# (Include spaces between %>%, =, *, ~, /, etc)

iris %>% head
iris %>% subset(Sepal.Width > 3) %>% head
iris %>% subset(Sepal.Width > 3) %>% transform(Petal.area = pi * Petal.Length / 2 * Petal.Width / 2) %>% head
iris %>% subset(Sepal.Width > 3) %>% within(., Petal.area <- pi * Petal.Length / 2 * Petal.Width / 2) %>% head
iris %>% subset(Sepal.Width > 3) %>% with(., data.frame(Species = Species, Petal.area = pi * Petal.Length / 2 * Petal.Width / 2)) %>% head

iris %>% subset(Sepal.Width > 3) %>% with(., data.frame(Species = Species, Petal.area = pi * Petal.Length / 2 * Petal.Width / 2)) %>% aggregate(Petal.area ~ Species, data = . , FUN = 'median') %>% head
iris %>% 
    subset(Sepal.Width > 3) %>% 
    with(., data.frame(Species = Species, Petal.area = pi * Petal.Length / 2 * Petal.Width / 2)) %>% 
    aggregate(Petal.area ~ Species, data = . , FUN = 'median') 
iris %>%  subset(Sepal.Width > 3) %>%  within(., Petal.area <- pi * Petal.Length / 2 * Petal.Width / 2) %>% subset(., select = c(Species, Petal.area)) %>% aggregate(Petal.area ~ Species, data = .,  FUN = 'median') 
iris %>%  subset(Sepal.Width > 3) %>%  within(., Petal.area <- pi * Petal.Length / 2 * Petal.Width / 2) %>% aggregate(Petal.area ~ Species, data = .,  FUN = 'median') 
iris %>%  subset(Sepal.Width > 3) %>%  within(., Petal.area <- pi * Petal.Length / 2 * Petal.Width / 2) %>% subset(., select = c(Species, Petal.area)) %>% aggregate(Petal.area ~ Species, data = . ,  FUN = 'median') 

##ans
iris %>% subset(Sepal.Width > 3) %>% within(., Petal.area <- pi * Petal.Length / 2 * Petal.Width / 2) %>% aggregate(Petal.area ~ Species, data = ., FUN = 'median')
#

# Aliases -----------------------------------------------------------------

# I can never remember what all the different aliases do and how they work so here's a much better list than the help file in ?multiply_by

# First extract
mtcars %>% ?extract(, 1) # Same as [ - here mtcars[,1]
mtcars %>% '['(,1) # Exactly the same but less readable
mtcars %>% extract('wt') # Or by name
mtcars %>% extract(c('wt', 'am')) # Or multiple names
mtcars %>% extract(4, 3) # Same as mtcars[4, 3]

# Now extract2 - same as '[[' - i.e. indexing a list
mtcars %>% extract2(1) # Now returns it as a vector - same as mtcars[[1]]

# inset - same as the replacement function '[<-'
mtcars %>% inset('new_value', value = rnorm(nrow(.))) # Same as mtcars$new_value = rnorm(nrow(mtcars))
# inset2 like extract2 but the replacement function.

# use_series same as $
mtcars %>% use_series('wt')

# add/subtract/multiply_by/raise_to_power/divide_by
# All pretty obvious
mtcars %>% within(., wt <- wt %>% add(200)) %>% head

# and, or, equals, is_greater_than, is_weakly_greater_than
mtcars %>% subset(cyl %>% is_weakly_greater_than(6))
mtcars %>% subset(and(mpg %>% is_greater_than(21),
                      cyl %>% is_weakly_less_than(6)))
# Set colnames, etc
mtcars %>% extract(,1:2) %>%
  set_colnames(c('miles per gallon', 'cylinders'))

## EXERCISE 3

library(?magrittr)
# Write magrittr code to find the following subsets in the mtcars data set
# All cars with disp < 200 or wt > 3.3
mtcars %>% subset(disp %>% `<`(200) | wt %>% `>`(3.3)) %>% head
mtcars %>% subset(disp %>% `<`(200) | wt %>% `>`(3.3)) %>% dim
mtcars %>% subset(disp %>% `<`(200) %>% `|`(wt %>% `>`(3.3))) %>% dim
mtcars %>% subset(disp %>% is_less_than(200) %>% or(wt %>% is_greater_than(3.3))) %>% dim
mtcars %>% subset(disp %>% is_less_than(200) %>% or(wt %>% is_greater_than(3.3))) %>% dim
# All cars with gear greater than or equal to 4 and cylinders equal to 6
mtcars %>% subset(gear %>% `>=`(4) & cyl %>% `==`(6)) %>% head
mtcars %>% subset(gear %>% `>=`(4) & cyl %>% `==`(6)) 
mtcars %>% subset(gear %>% `>=`(4) & cyl %>% `==`(6)) %>% dim 
mtcars %>% subset(gear %>% `>=`(4) %>% `&`(cyl %>% `==`(6))) %>% dim 
mtcars %>% subset(gear %>% is_weakly_greater_than(4) & cyl %>% equals(6)) 
mtcars %>% subset(gear %>% is_weakly_greater_than(4) %>% and(cyl %>% equals(6))) %>% dim
# All cars with mpg between 15 and 23 (non-inclusive), or wt between 2.5 and 3.6 (non-inclusive)
mtcars %>% subset((mpg %>% `>`(15) & mpg %>% `<`(23)) | (wt %>% `>`(2.5) & wt %>% `<`(3.6)) ) %>% dim 
mtcars %>% subset(mpg %>% `>`(15) %>% `&`(mpg %>% `<`(23) ) %>% `|`(wt %>% `>`(2.5) %>% `&`(wt %>% `<`(3.6)))) %>% dim 
mtcars %>% subset(mpg %>% `>`(15) %>% and(mpg %>% `<`(23) ) %>% or(wt %>% `>`(2.5) %>% and(wt %>% `<`(3.6)))) %>% dim 
mtcars %>% subset(mpg %>% is_greater_than(15) %>% and(mpg %>% is_less_than(23)) %>% or(wt %>% is_greater_than(2.5) %>% and(wt %>% is_less_than(3.6)))) %>% dim 
mtcars %>% subset(mpg %>% is_greater_than(15) %>% and(mpg %>% is_less_than(23)) %>% or(wt %>% is_greater_than(2.5) %>% and(wt %>% is_less_than(3.6))))

# Again using the Iris data set, I want to use the quantile and findInterval functions to place each observation in a decile according to its Petal.Length.
# Here's how I would do it using standard R code
iris_pet_len_dec = quantile(iris$Petal.Length, probs = seq(0, 1, length = 11))
class = findInterval(iris$Petal.Length, iris_pet_len_dec, all.inside = TRUE)
table(class)
# Write a magrittr version of this code

iris$Petal.Length %>% findInterval(., quantile(., probs = seq(0, 1, length = 11)), all.inside = TRUE) %>% table

# Other pipes -------------------------------------------------------------

# %>T% for returning the other side. Try
mtcars %>% extract(, 1)
mtcars %T>% extract(, 1) # Returns the left hand side instead

ans1 = mtcars %>%
  subset(hp > 100) %>%
  extract(, 1:2) %>%
  plot
# Nothing saved in ans1

# Compare with this (watch out for the >T)
ans2 = mtcars %>%
  subset(hp > 100) %>%
  extract(, 1:2) %T>%
  plot
# More useful - ans2 contains something

# The %$% operator - exposes the names to the right hand side
# mtcars %>%
#   subset(hp > 100) %>%
#   cor(cyl, disp)
# Doesn't work!
mtcars %>%
  subset(hp > 100) %$%
  cor(cyl, disp)
# Works properly
# Use this whenever you want to refer to variable names in a subsequent step but it's not passing them properly
# Will likely occur whenever there isn't a data argument in a function

# The %<>% operator - saves a bit of typing

# Previously
mtcars2 = mtcars
mtcars2 = mtcars2 %>% within(., wt <- wt %>% add(200))
# Now
mtcars2$wt %<>% add(200)

## EXERCISE 4

# I want to create a scatter plot of log(mpg) vs log(wt). Which of the following work?
# mtcars %$% plot(mpg %>% log, wt %>% log)
# mtcars %>% plot(mpg %>% log, wt %>% log)
# mtcars %$% plot(log(mpg), log(wt))
# mtcars %>% within(., log_mpg <- mpg %>% log) %>%
#   within(., log_wt <- wt %>% log) %>%
#   extract(c('log_mpg', 'log_wt')) %>%
#   plot

# Fill in the blanks so that the 2-column data set used to create the plot is returned as part of the call
# mtcars %>% within(., log_mpg <- mpg %>% log) [A]
#   within(., log_wt <- wt %>% log) [B]
#   extract(c('log_mpg', 'log_wt')) [C]
#   plot

# Other clever features ---------------------------------------------------

# piping into functions
mtcars %>%
  (function(x) {
    if (nrow(x) > 2)
      rbind(head(x, 1), tail(x, 1))
    else x
  })

# Equivalent to:
my_fun = function(x) {
  if (nrow(x) > 2)
    rbind(head(x, 1), tail(x, 1))
  else x
}
my_fun(mtcars)

# Another example
mtcars %>% {
  n <- sample(1:10, size = 1)
  H <- head(., n)
  T <- tail(., n)
  rbind(H, T)
  } %>%
  summary

# This is the same as:
my_fun = function(x) {
  n <- sample(1:10, size = 1)
  H <- head(x, n)
  T <- tail(x, n)
  return(rbind(H, T))
}
summary(my_fun(mtcars)) %>% dim

# Using magrittr to create functions
mae = . %>% abs %>% mean(na.rm = TRUE)
mae(rnorm(10))

# That’s equivalent to:
mae <- function(x) {
  mean(abs(x), na.rm = TRUE)
}
mae(rnorm(10))

# or, for more complicated functions which return multiple arguments
med_mean = . %>%  { c(median(.), mean(., na.rm = TRUE)) }
med_mean(rnorm(10))

# EXERCISE 5

# I want to create my own function that produces the mean, sd, and length for use in an aggregate call
# Write a magrittr defined function to return the mean, sd, and length of a vector
# It should work with
myfun = . %>% {c(mean(., na.rm = TRUE),sd(., na.rm = TRUE),length(.))}

myfun = . %>% {
  data.frame(m= mean(.), sd = sd(.), l=length(.)) 
} 

%T>% set_colnames(c('mpg (mean)', 'mpg (sd)', 'mpg (length)'))

mtcars %>% aggregate(mpg ~ cyl, data = ., myfun) 
mtcars %>% aggregate(mpg ~ cyl, data = ., myfun) %>% str
mtcars %>% aggregate(mpg ~ cyl, data = ., myfun) %>% dim

set_colnames(c('cyl','mpg'))

?extract


c(1) %>% cbind(c(2)) %>% cbind(c(3)) %>% cbind(c(4)) 

# Here is a pretty inelegant function that provides the negative log likelihood for a given vector assuming the data come from a N(0, 1) distribution
dat = rnorm(10)
nll = function(x) -1 * sum(log(dnorm(x)))
nll(dat)
# Re-write nll in magrittr format

nll = . %>% dnorm %>% log %>% sum %>% '*'(-1)

nll(dat)

# End ---------------------------------------------------------------------
