# Lecture 12 - big data, ML, and Apache Spark in R

# Resources:
# The mlr package: http://mlr-org.github.io/mlr-tutorial/release/html/index.html
# Sparklyr - http://spark.rstudio.com/
# A nice simple explanation of Spark and Hadoop: https://www.simplilearn.com/spark-vs-hadoop-article
# h2o - http://www.h2o.ai
# R for programmers by Zhang

# Machine learning in R ---------------------------------------------------

# Many of you will have previously covered a bit of machine learning in other modules
# Here is a quick revision of the kind of thing we're doing:
# In supervised learning we have y = f(X) + e
# where y is a univariate response (or target), X is a (possibly large) vector of features, and e is a residual term
# We want to estimate f (call the estimate f_hat) and for new X values called X* create predictions f_hat(X*)

# If y is a continuous variable then this is called regression. If y is categorical this is called classification

# There are lots of 'black box' methods for creating f_hat. The most popular ones are: random forests, neural networks (aka deep learning), and support vector machines

# In mlr things are separated into different bits:
# A task which includes a data set, a list of features and a target
# A learner which represents the methods used, e.g. random forests or neural net
# A model which is the learner fitted to the task

# Let's look at a regression example and a classification example

# Regression
# install.packages('mlbench')
library(mlbench) # Useful package for ML data sets
data('BostonHousing2')
# Not so big: 506 cases and 19 variables
# y = medv - median value of home
# X = all other variables except cmedv, tract and town
bh2 = BostonHousing2[,-c(1, 2, 6)]

# Classificaton
data("BreastCancer")
# 699 cases and 11 variables
# Consider:
# y = Class (benign vs malignant)
# X = all other variables (minus ID)
bc2 = BreastCancer[,-c(1,7)] # Col 7 has missing values

# The mlr package ---------------------------------------------------------

# The mlr package is a wrapper around lots of different R packages which perform supervised (and unsupervised) learning
# I think it aims to be R's equivalent of Scikit-learn
#install.packages('mlr')
library(mlr)

# Let's run an RF on a regression model

# The first step in mlr is to create a 'task' specifying the model type and the data
bh_task = makeRegrTask(id = "bh", data = bh2, target = "medv")
bh_task
# If it turns out that you don't need all the data in the cases/features, you can use subsetTask

# To fit the model you have to create a 'learner'. You can find the full list:
# http://mlr-org.github.io/mlr-tutorial/release/html/integrated_learners/index.html#regression-61
# Note that these learners borrow from other packages - it will give an error if you don't have them installed
# You can use, e.g.
listLearners("regr")
# Though this only shows ones for packages you have installed

# Let's fit these models to a subset of the data and test performance on the left out part - a train/test split
n = getTaskSize(bh_task)
set.seed(123)
test_set = sample(n, size = n/4)
train_set = (1:n)[-test_set]

# Let's fit a random forest
#install.packages('randomForest')
bh_rf = makeLearner('regr.randomForest')
# Now we train it
mod_rf = train(bh_rf, bh_task, subset = train_set)
# Get some basic info
getLearnerModel(mod_rf)

# Now get predictions on the test set
pred_rf = predict(mod_rf,
                  task = bh_task,
                  subset = test_set)

# You can extract bits of this via
getPredictionTruth(pred_rf)
getPredictionResponse(pred_rf)

# You can plot output with
# plotLearnerPrediction(bh_rf,
#                       features = 'lstat',
#                       task = bh_task)
# Choose two features for a multivariate type plot

# Can now perform a huge number of different performance metrics
performance(pred_rf) # Just gives the default mse
performance(pred_rf, measures = mae) # Mean absolute error
# Here's the full list http://mlr-org.github.io/mlr-tutorial/release/html/measures/index.html
performance(pred_rf, measures = rmse) # RMSE - my preferred
# 4.359777 (approx - will likely change)

## EXERCISE 1
# Install the package randomForestSRC and then use the generateFilterValuesData on the training set to produce feature importances.
# a) Which three variables seem to be the most important?
# (provide variable names in alphabetical order separated by a space)
# b) Re-fit the model using only these three variables. What is the RMSE now on the test set? (to 1 d.p.)

#a)
install.packages('randomForestSRC')
library(randomForestSRC)
bh_task = makeRegrTask(id = "bh", data = bh2, target = "medv")
getTaskFeatureNames(bh_task)
bh_task
bh_rf = makeLearner('regr.randomForestSRC')
mod_rf = train(bh_rf, bh_task, subset = train_set)
getLearnerModel(mod_rf)
generateFilterValuesData(bh_task)
round(performance(pred_rf, measures = rmse),1) # Just gives the default mse


#b)
library(dplyr)
library(magrittr)
bh2 %>% head
bh2_3 = bh2 %>% select(crim, indus, lon, medv) 
bh2_3 %>% head
bh_task = makeRegrTask(id = "bh", data = bh2_3, target = "medv")
getTaskFeatureNames(bh_task)
bh_task
mod_rf = train(bh_rf, bh_task, subset = train_set)
getLearnerModel(mod_rf)
pred_rf = predict(mod_rf,
                  task = bh_task,
                  subset = test_set)
round(performance(pred_rf, measures = rmse),1) # Just gives the default mse
generateFilterValuesData(bh_task)


# Comparing different models ----------------------------------------------

# Let's now fit some other models.

# Try an svm
#install.packages('e1071')
bh_svm = makeLearner('regr.svm')
# Now we train it
mod_svm = train(bh_svm, bh_task, subset = train_set)

# Predict
pred_svm = predict(mod_svm,
                  task = bh_task,
                  subset = test_set)
# Get rmse
performance(pred_svm, measures = rmse)

# Now try a neural network
#install.packages('nnet')
bh_nn = makeLearner('regr.nnet')
# Now we train it
mod_nn = train(bh_nn, bh_task, subset = train_set)

# Predict
pred_nn = predict(mod_nn,
                   task = bh_task,
                   subset = test_set)
# Get rmse
performance(pred_nn, measures = rmse)

## EXERCISE 2
# Write a loop which trains bh_task on all of the following learners (The packages you'll need to install are in brackets):
# Blackboost (mboost, party),
# Bayesian Treed Gaussian process (tgp),
# Conditional Inference Trees (party),
# Extreme Gradient Boosting (xgboost)
# Report the rmse for each on the test set.
# Which model works best?
# (Hint: you will need to look up their names in the list of learners)
# (Hint 2: set the seed as 123 before your loop starts to get the same results as mind)

install.packages('mboost')
install.packages('party')
install.packages('tgp')
install.packages('xgboost')
install.packages('RRF')


library(mboost)
library(party)
library(tgp)
library(xgboost)
library(RRF)

bh_task = makeRegrTask(id = "bh", data = bh2, target = "medv")
bh_task

rmse.arr <- sapply(X = c('regr.blackboost','regr.btgp','regr.ctree','regr.xgboost','regr.RRF'),FUN = function(x){
  bh_learner = makeLearner(x)
  mod = train(bh_learner, bh_task, subset = train_set)
  pred = predict(mod,
                     task = bh_task,
                     subset = test_set)
  # Get rmse
  rmse = performance(pred, measures = rmse)
  resp = list()
  resp['learner'] = bh_learner 
  resp['rmse'] = rmse
  rmse
  })

rmse.arr

# Classification problems -------------------------------------------------

# It's much the same - create task, train, predict, evaluate
bc2 %>% str
bc2  %>% head
bc2  %>% dim
bc_task = makeClassifTask(id = "bc", data = bc2, target = "Class")
bc_task

# Note here that there's quite a class imbalance - see exercise

# Let's run a Cross validation version on these:
class_learn = makeLearner("classif.randomForest",
                          predict.type = "prob", # Get predicted probabilities
                          fix.factors.prediction = TRUE) # Don't know why this isn't the default - stops factor levels from being dropped during training

# Set up 5-fold CV
#divide dataset into 5 chunks, drop a chunk and run model on other 4, repeat for each chunk
desc = makeResampleDesc("CV", iters = 5)
# See also crossval which does the same thing (with less flexibility)

# Now run the CV
#install.packages('ROCR') # Required for AUC = Area under the reciever classification curve, if AUC is high => generally good sign the model is working 
library(ROCR)
class_rf = resample(class_learn,
                    task = bc_task,
                    resampling = desc,
                    measures = list(mmce, auc))
# Also get the overall classification error at the end - neat

# The class_rf object contains lots of other useful things
str(class_rf)

# You can get even more information by running benchmark
bench_rf = benchmark(class_learn, bc_task, desc)
getBMRPerformances(bench_rf) #mis classification estimate
all_out = getBMRPredictions(bench_rf, as.df = TRUE)
head(all_out)

# This is really just scratching the surface of mlr - lots more on parallelisation, visualisation, benchmarking, etc, etc

## EXERCISE 3
# Have a read of the information at http://mlr-org.github.io/mlr-tutorial/release/html/over_and_undersampling/index.html about 
#imbalanced class problems
# If you run
all_out %>% select(response, truth) %>% table
# you'll find that there are 7 women classified as benign when actually they are malignant. We want to reduce this number. 
#Set the seed as 123 and use makeOversampleWrapper at rate = 1.9 to re-run the 5-fold CV. How many benign women in the data 
#set are now incorrectly classified as as malignant?

set.seed(123)
bc2 %>% head
bc_task = makeClassifTask(id = "bc", data = bc2, target = "Class")
bc_task
class_learn = makeLearner("classif.randomForest",
                          predict.type = "prob", # Get predicted probabilities
                          fix.factors.prediction = TRUE) # Don't know why this isn't the default - stops factor levels from being dropped during training
class_learn_over = makeOversampleWrapper(class_learn, osw.rate = 1.9)
desc = makeResampleDesc("CV", iters = 5)
class_rf = resample(class_learn_over,
                    task = bc_task,
                    resampling = desc,
                    measures = list(mmce, auc))
str(class_rf)
class_rf$pred$data %>% select(response, truth) %>% table
bench_rf = benchmark(class_learn.over, bc_task, desc)
getBMRPerformances(bench_rf) #mis classification estimate
all_out = getBMRPredictions(bench_rf, as.df = TRUE)
head(all_out)
all_out %>% select(response, truth) %>% table


# spaklyr -----------------------------------------------------------------

# This is an 'engine' for large scale data processing
# Supposed to be much faster than Hadoop (though built on it)

# Install the package
#install.packages("sparklyr")

# Call the package and also install spark
library(sparklyr)
#spark_available_versions() - look to see what versions are available
#spark_install(version = '2.0.1', hadoop_version = '2.7') # Can optionally specifiy spark/hadoop versions
# Note that this is a big installation ~250Mb
# You will also need to install a Java Development Toolkit from http://www.oracle.com/technetwork/java/javase/downloads/index.html
# This is another big downlaod
# If you were doing this for really big data you would be installing all this on e.g. Rstudio server on an amazon web services server: https://aws.amazon.com/
# See http://www.louisaslett.com/RStudio_AMI/ for how to create an Rstudio instance

# Connect to spark (locally)
sc = ?spark_connect(master = "local") # Might take a few seconds to connect

# Load in dplyr and copy some data across to the 'server'
library(dplyr)
library(nycflights13)
flights_tbl = copy_to(sc, flights)
# See also ?spark_read_csv

# Look at what you've copied across
src_tbls(sc)

# You can now analyse data 'remotely'
flights_tbl %>% filter(dep_delay == 2)

# Can even create plots and the like as in lecture 5
delay = flights_tbl %>%
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect()
## The collect command here forces computation to happen 'on the server' with the resulting data frame only returned to R

# plot delays
library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

# You can even use SQL within a Spark cluster (warning I know nothing about this)
library(DBI)
dbGetQuery(sc, "SELECT * FROM flights LIMIT 10")
dbGetQuery(sc, "SELECT distinct origin FROM flights")
dbGetQuery(sc, "SELECT hour,count(1) freq  FROM flights group by hour order by 2 desc")
# iris_preview

# Sparkly ML --------------------------------------------------------------

# Create a linear regression

# Use the flights data set
# This is a pretty big data set 340k rows
# Consider:
# y = arrival delay
# X = carrier, origin, destination, distance, hour of day

partitions = flights_tbl %>%
  select(hour, carrier, origin, dest, distance, arr_delay) %>%
  filter(arr_delay < 100) %>% # Some hugely delayed flights
  na.omit %>%
  sdf_partition(training = 0.99, test = 0.01, seed = 123)
# Data set so big happy to keep just 1% as test set - still ~3k obs in test set

# fit a linear model to the training dataset
fit_lasso = partitions$training %>%
  ml_linear_regression(response = "arr_delay",
                       features = c("carrier",
                                    "origin",
                                    "dest",
                                    "distance",
                                    "hour"),
                       alpha = 1)
summary(fit_lasso)

# Create predictions
pred_lasso = sdf_predict(fit_lasso, partitions$test) %>% collect
library(magrittr)
pred_lasso %$% cor(arr_delay, prediction)

ggplot(pred_lasso, aes(x = arr_delay, y = prediction)) +
  geom_abline(lty = 'dashed', col = 'red') +
  geom_smooth() +
  geom_point()
# Not a great model, but still might be useful.

# Let's try a random forest
fit_rf = partitions$training %>%
  ml_random_forest(response = "arr_delay",
                   features = c("carrier",
                                    "origin",
                                    "dest",
                                    "distance",
                                    "hour"))
summary(fit_rf) # Not very helpful

# Create predictions
pred_rf = sdf_predict(fit_rf, partitions$test) %>% collect
pred_rf %$% cor(arr_delay, prediction) # RF very slightly better

ggplot(pred_rf, aes(x = arr_delay, y = prediction)) +
  geom_abline(lty = 'dashed', col = 'red') +
  geom_smooth() +
  geom_point()

## EXERCISE 4
# Use the following code to load the bc2 data into Spark:
library(mlbench)
data("BreastCancer")
bc2 = BreastCancer[,-c(1,7)]
bc2$Class = as.integer(bc2$Class == 'malignant')
bc2_tbl = copy_to(sc, bc2, overwrite = TRUE) # Note the .s have been changed to _
bc_part = bc2_tbl %>%
  sdf_partition(training = 0.75, test = 0.25, seed = 123)
# Now run the breast cancer classification (using data set bc2) through Spark's gradient boosted trees algorithm 
# with Class as the response, and all other variables as features. Use ml_binary_classification_eval to report the 
# AUC value on the test set. What is the AUC value (to 2 d.p.)?


fit_rf = bc_part$training %>% ml_gradient_boosted_trees(
    response = "Class",
    features = c("Cl_thickness","Cell_size","Cell_shape","Marg_adhesion","Epith_c_size","Bl_cromatin","Normal_nucleoli","Mitoses" )
)

summary(fit_rf)

# Create predictions
pred_rf = sdf_predict(fit_rf, bc_part$test) 
ml_binary_classification_eval(pred_rf, label="Class", score="prediction")
# H2o ---------------------------------------------------------------------

# An alternative Machine Learning library is provided by H20
# This package can also be used with mlr
# See http://spark.rstudio.com/h2o.html

# Remove previous versions of h2o R package
if ("package:h2o" %in% search()) detach("package:h2o", unload=TRUE)
if ("h2o" %in% rownames(installed.packages())) remove.packages("h2o")

# Next, we download R package dependencies
pkgs <- c("methods","statmod","stats","graphics",
          "RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
  if (!(pkg %in% rownames(installed.packages()))) install.packages(pkg)
}

# Download h2o package version 3.10.0.6
install.packages("h2o", type = "source",
                 repos = "http://h2o-release.s3.amazonaws.com/h2o/rel-turing/6/R")
library(h2o)

library(devtools)
devtools::install_github("h2oai/sparkling-water", subdir = "/r/rsparkling")

library(sparklyr)
spark_install(version = "1.6.2")
spark_available_versions()
library(rsparkling)
library(dplyr)

options(rsparkling.sparklingwater.version = '1.6.7')
#sc = spark_connect("local")
spark_disconnect(sc)
sc = spark_connect("local", version = "1.6.2")

# transform our data set, and then partition into 'training', 'test'
library(readr)
library(magrittr)
library(nycflights13)

flights2 = flights %>%
  select(hour, carrier, origin, dest, distance, arr_delay) %>%
  filter(arr_delay < 100) %>% # Some hugely delayed flights
  mutate_each(funs(as.factor), carrier, origin, dest) %$%
  # DL doesn't like categorical variables so convert to factors
  model.matrix(~ hour + carrier + origin + dest + distance + arr_delay - 1) %>%
  as_data_frame

# Get into the right h2o format
flights2 = copy_to(sc, flights2)
src_tbls(sc)
flights3 = as_h2o_frame(sc, flights2)
splits = h2o.splitFrame(flights3, seed = 123)

# Set up varaiable names
x = colnames(flights3)[-ncol(flights3)]
y = "arr_delay"

# Run a deep learning model
dl_fit = h2o.deeplearning(x = x,
                          y = y,
                          training_frame = splits[[1]],
                          epochs = 15,
                          activation = "Rectifier",
                          hidden = c(10, 5, 10),
                          input_dropout_ratio = 0.7)

# Judge performance
h2o.performance(dl_fit, newdata = splits[[2]])

# Let's see how that compares to random forests
rf_fit = h2o.randomForest(x = x, y = y,
                          training_frame = splits[[1]])

h2o.performance(rf_fit, newdata = splits[[2]])
# RF wins!

## NO MORE EXERCISES!
# If you wanted to play around with this I would suggest expanding out the feature set.

