## Lecture 1 - revision

# First a reminder about RStudio, and getting help with ? and ??, = vs <-, and comments

# Object and variable types -----------------------------------------------

# c, character, vector, matrix, list, data frame,
# Numeric, integer, factors, L notation

# Going to use the str command lots

# Vectors
x = c(2.3, -4.6, 5.7)
str(x) # A numeric vector
x = c(1, 4, 6) # Still numeric
str(x)
x = c(1L, 4L, 6L) # Force it to be an integer
str(x)
x = c(FALSE, TRUE, TRUE) # Boolean/logical
str(x)

# Matrices
x = matrix(1:4, ncol = 2, nrow = 2) # An integer matrix
str(x)
x = matrix(runif(4), ncol = 2, nrow = 2) # A numeric matrix
str(x)

# Indexing
x[1, ] # The first row of x
x[1, , drop = FALSE] # The first row, but keep as a matrix
x[2, 1] # The second row and the first column

str(x[1,])
str(x[1, , drop = FALSE])

# Can add row names of column names if required
colnames(x)
colnames(x) = c('one', 'two')
x

# Lists
x = list(p = 1L, q = runif(4), r = c(2 + 3i, 7 - 2i))
x$r
x[[2]] # Alternative ways to index
x[[2]][1:2]
x$q[3:4]

# Factors
x = factor(rep(1:3, length = 12), labels = c('low', 'medium', 'high'))
str(x)
x + 2 # Gives a warning

# An ordered factor
x = factor(rep(1:3, length = 12),
           labels = c('low', 'medium', 'high'),
           ordered = TRUE)
str(x)

# Change levels
levels(x) = c('small', 'medium', 'big')

# Can make a difference with various plotting functions and some statistical models

# Data frames
x = data.frame(a = 1:4,
               col2 = runif(4),
               z = factor(rep(1:2,2),
                          labels = c('no','yes')))
str(x)
# A data frame is just a list but can also be referenced like a matrix
x$col2[1:3]
x[,2:3] # Second and third columns

# Can even have long names but referencing can get a big messy
names(x) = c('A long name', 'A very long name', 'An even longer name')
x[1:3,1:2]
x$`A very long name`

# Writing functions -------------------------------------------------------

# The most basic
sq = function(x) return(x^2)
sq(2)

# Multiple arguments
pow = function(x, p) return(x^p)
pow(2,2)

# Multiple lines
pow = function(x, p) {
  return(x^p)
}
pow(2,2)
# Return is optional but highly recommended

# Default arguments
pow = function(x, p = 2) {
  return(x^p)
}
pow(2)

# Can also name them if specifying in weird order
pow(p = 3, x = 4)

# Advisable to use invisible if you don't want it to print
pow = function(x, p = 2) {
  invisible(x^p)
}
pow(3)
y = pow(3)

# If returning multiple objects use a list
pow = function(x, p = 2) {
  return(list(arguments = c(x, p), output = x^p))
}
pow(2)

# Most functions are automatically vectorised
pow(1:3)

# .. but you need to be a little bit careful
pow(x = 1:3, p = 1:3) # Works ok
pow(x = 1:3, p = 1:2) # Was that what you wanted?

# Ifelse ------------------------------------------------------------------

# if, ifelse
x = runif(1)
if(x < 0.5) print('x < 0.5!')

# Can make these more complicated
x = runif(1)
if(x < 0.5) {
  y = rnorm(1)
  print(x*y)
}

# Can have compound statements using & (and) and | (or)
x = runif(1)
y = runif(1)
if( (x + y < 1) | (x*y < 0.2) ) {
  print(x + y)
  print(x*y)
}
# Make sure to add in parentheses

# Can add in else and else if statements
x = runif(1)
if(x < 0.5) {
  print('x < 0.5')
} else {
  print('x > 0.5')
}

# Of else-if statements
x = runif(1)
y = runif(1)
if(x < 0.5) {
  print('x < 0.5')
} else if(y < 0.5) {
  print('x > 0.5 and y < 0.5')
} else {
  print('x > 0.5 and y > 0.5')
}

# If you just have something very simple can use ifelse
x = runif(1)
ifelse(x < 0.5, 2, 1)

# You can also easily store this in a variable
y = ifelse(x < 0.5, 2, 1)
# It can also be extended to compound statements like if above

# Loops -------------------------------------------------------------------

# for, repeat, while

# for loops
for (i in 1:10) print(i)

# Can expand with curly brackets
for (i in 1:5) {
  current = i^2
  cat('i =',current, '\n')
}

# Can nest loops together
for (i in 1:5) {
  for (j in 1:5) {
    cat('i =', i, 'j =', j, ', i*j = ',i*j, '\n')
  }
}

# Doesn't have to be a sequence in the loop
x = runif(10)
for (i in x) {
  print(i < 0.5)
}

# While loops slightly different
x = 1
while(x < 10) {
  print(x)
  x = x + 1
}
# All of the same rules as for loops apply
# Don't forget to adjust the boolean statement in the loop so you don't get stuck

# Perhaps even more basic if repeat
x = 1
repeat {
  print(x)
  x = x + 1
  if(x == 10) break
}
# Don't forget the break statement!
# I don't often use repeat loops

# Plots -------------------------------------------------------------------

# Use the prostate data
prostate = read.table('http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data', header = TRUE)

# A basic scatter plot
plot(prostate$age, prostate$lcavol)

# Change the axis labels and add a title
plot(prostate$age, prostate$lcavol,
     xlab = 'Age',
     ylab = 'Log(cancer volume)',
     main = 'Scatter plot')

# Use e.g. paste or paste0 to add in objects
var_1 = 'age'
var_2 = 'lcavol'
plot(prostate[,var_1], prostate[,var_2],
     xlab = var_1,
     ylab = var_2,
     main = paste('Scatter plot of',var_1,'vs',var_2))

# Change the plotting type
plot(prostate[,var_1], prostate[,var_2],
     pch = 19)
plot(prostate[,var_1], prostate[,var_2],
     type = 'l') # Yuck
?pch # Look at different point types

# Changing colours
plot(prostate[,var_1], prostate[,var_2],
     pch = 19, col = 'blue')

# Transparency
plot(prostate[,var_1], prostate[,var_2],
     pch = 19, col = rgb(1, 0, 0, alpha = 0.2))

# Changing some options with par
par(mar = c(2, 2, 2, 2), las = 1) # Margins - see ?par for more
plot(prostate[,var_1], prostate[,var_2])
# Be careful - these options are persistent
par(mar = c(5, 4, 4, 2) + 0.1)

# Add to plots with points and lines
plot(prostate[,var_1], prostate[,var_2], type = 'n')
points(prostate[,var_1], prostate[,var_2], col='red')
lines(prostate[,var_1], prostate[,var_2], col='green')

# Add a legend
legend('topleft', legend = c('points', 'lines'),
       pch = c(1, -1),
       lty = c(-1, 1),
       col = c('red', 'green'))

# Histograms
hist(prostate$lweight)

# Better bins:
hist(prostate$lweight, breaks = 30)
hist(prostate$lweight, breaks = seq(2,5, by = 0.2))

# Better x axis
hist(prostate$lweight, breaks = seq(2,5, by = 0.2), xaxt = 'n')
axis(side = 1, at = seq(2, 5, by = 0.2))

# Bar charts (called bar plots)
table(prostate$gleason)
barplot(table(prostate$gleason)) # No axis labels
barplot(table(prostate$gleason), horiz = TRUE)

# Boxplots
boxplot(prostate$lpsa)

# Careful - this does not give you what you might expect
boxplot(prostate$gleason, prostate$lpsa)
# Proper way
boxplot(prostate$lpsa ~ prostate$gleason) # this is a formula
# Lots of extra options regarding direction, style, shape, colour, etc.

# pairs - matrix scatter plots
pairs(prostate)

# Regression -------------------------------------------------------

# Fit a simple regression model
lm(lpsa ~ lcavol, data = prostate)

# Doesn't have to come from a data frame
lm(prostate$lpsa ~ prostate$lcavol)

# Save as an object and then can manipulate
model_1 = lm(lpsa ~ lcavol, data = prostate)
summary(model_1)

# model_1 is now a list
str(model_1)

# Some of the useful things in it
model_1$coefficients
model_1$residuals
model_1$fitted.values

# Can also directly plot but a bit confusing
plot(model_1)

# Given the above though it's easy to plot the output
plot(prostate$lcavol, prostate$lpsa)
lines(prostate$lcavol, model_1$fitted.values)

# A binomial glm
model_2 = glm(svi ~ lcavol, data = prostate, family = binomial)
summary(model_2)

# Opening and saving files ------------------------------------------------

# read/write.csv, read.table, load, save, scan, saving plots with e.g. pdf

# Most useful function is probably read.csv
#prostate = read.csv('http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.csv')

# More general version is read.table
prostate = read.table('http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data')
# Useful options: header, sep, stringsAsFactors, skip, nrows

# Load in the first 50 rows but skip the first 10
prostate2 = read.table('http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data',
                       nrows = 50,
                       skip = 10)
str(prostate2)

# If you're dealing with people who only use R often safer to save directly as R format
#save(prostate, file = 'my_prostate_file.rda')
#save(prostate, file = 'path/to/my_prostate_file.rda')

# Load it back in
#load('path/to/my_prostate_file.rda')

# To save plots, use e.g. the pdf or jpeg function, and follow with dev.off()
# pdf(file = 'my_plot.pdf', width = 8, height = 6)
# plot(prostate$lcavol, prostate$lpsa)
# dev.off()

# Some other useful R functions ---------------------------------------------

# library/require
library(MASS)
help(package = 'MASS')

# Note the difference
library(bla) # Error
require(bla) # Warning

# Installing
#install.packages('bla')
#install.packages(c('bla', 'bla2'))

# apply
x = matrix(runif(20), ncol = 2, nrow = 10)
apply(x, 1, 'sum') # rows
apply(x, 2, 'sum') # columns

# Lots of different versions for e.g. lists/dataframe (lapply), tapply (ragged arrays), sapply (simple version of lapply)
# Much more on these later
lapply(prostate, 'prod')
# Must need to use unlist
unlist(lapply(prostate, 'prod'))
apply(prostate, 2, 'prod')

# head/tail
head(prostate)
head(prostate, 5)
tail(prostate, 2)

# aggregate
aggregate(prostate$lpsa, by = list(prostate$gleason), 'mean')
aggregate(prostate$lpsa, by = list(prostate$gleason, prostate$age), 'length')

# with
with(prostate, plot(age, lpsa))

# which

# Find all the locations where it matches
good = 6
which(prostate$gleason == good)

# Find the min/max
which.min(prostate$age) # Only gives the first match

# Just find out if the value is in there
good %in% prostate$gleason

# subset
prostate3 = subset(prostate, age < 60)

# Or more complicated
prostate4 = subset(prostate, (age < 60) & (gleason == 6))

# scale
par(mfrow=c(1,2))
hist(prostate$lpsa, breaks = 30)
hist(scale(prostate$lpsa), breaks = 30)
par(mfrow=c(1,1))

# Just subtract off the mean
scale(prostate$lpsa, scale = FALSE)

# Also see sweep

# cat/paste/print
print("Hello world!")
cat("Hello world!\n") # Slightly neater - \n for a new line (also \t for tab, \r for remove)
word = 'World'
cat("Hello", word, '\n')
result = paste("Hello", word, '\n')
cat(result)
result = paste("Hello", word, '\n', sep = ',')
cat(result)

# order
plot(prostate$age, prostate$lcavol, type = 'l') # Messy
ord = order(prostate$age)
plot(prostate$age[ord], prostate$lcavol[ord], type = 'l')


# EXERCISE ----------------------------------------------------------------

# There are two marks available for this exercise:
# 1) Create a cool plot of the prostate data exploring the use of colours, labels. See if you can find something interesting about it that otherwise might not be obvious. Try to use some commands that haven't been used elsewhere, e.g. polygon, image, etc.
# 2) Save your plot and post it (and the code) to the week 1 discussion board. Include a caption which states what you plotted and why it is interesting (1 mark for any decent plot, 1 mark for well commented code)

# End ---------------------------------------------------------------------


