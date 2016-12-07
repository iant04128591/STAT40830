# Lecture 5 - dplyr and tidyr

# Sources
# dplyr vignette: https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# dplyr introduction: https://blog.rstudio.org/2014/01/17/introducing-dplyr/
# Introducing tidyr: https://blog.rstudio.org/2014/07/22/introducing-tidyr/
# tidyr paper: http://vita.had.co.nz/papers/tidy-data.pdf
# tidyr vignette: https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html

# dplyr -------------------------------------------------------------------

# A really useful package for manipulating large data sets
library(dplyr)

# Just a few main functions:
# filter/slice
# arrange
# select and rename
# distinct
# mutate and transmute
# summarise
# sample_n and sample_frac
# group_by

# You can use dplyr with magrittr for really beautiful, powerful code

# Let's use the flights data for a simple example
install.packages('nycflights13') # Install if necessary
library(nycflights13) # Install if necessary
str(flights) # It's a tbl_df which means that it's a special kind of dplyr data frame

# Get much neater output for big data sets
head(flights)
# Now same as
flights
# Note that this is a tibble, formed via as_tibble

# You can look at a tibble by either printing it like above or with
glimpse(flights)
flights %>% glimpse
# Tibbles are nicer for printing and also for warning
mtcars$something_else # NULL
flights$something_else # Warning message!

# Filter and arrange ------------------------------------------------------

# filter - like subset but neater - make this as long as required
filter(flights, month == 1, day == 1)
filter(flights, month == 1, day == 1, carrier == 'B6')

# Easy to change into magrittr format
flights %>% filter(month == 1, day == 1, carrier == 'B6')

# Now go back and compare with horrifically ugly R code
flights[flights$month == 1 & flights$day == 1 & flights$carrier == 'B6', ]

# You might think: why am I not using subset? Well filter is neater because it takes as many arguments as you like and 
# joins them together with &

# You can do the same thing as subset though by combining boolean operators
flights %>% filter(month == 1 & day == 1 & carrier == 'B6')
# or, uglier
library(magrittr)
flights %>% filter(and(month %>% equals(1), and(day %>% equals(1), carrier %>% equals('B6'))))

# Or replace with or
flights %>% filter(month == 1 | carrier == 'B6')

# Use slice to extract specific rows
slice(flights, 1)
slice(flights, 5:10)
slice(flights, nrow(flights))
slice(flights, n()) # n() used here for the number of rows
flights %>% slice(1:2)

# arrange
# R is a pain to sort by multiple columns in a df. arrange makes this easier
?arrange(flights, sched_dep_time)
arrange(flights, month, day, dest)

# Use desc for decreasing
arrange(flights, desc(sched_dep_time))

# Neater magrittr version
flights %>% arrange(desc(sched_dep_time))

# Again, you might think 'why not use order?' but of course that would require lots more typing!

## EXERCISE 1
# By default only the first few columns and rows of a tibble are printed. There is an options command to show all of the columns. What is it? (Hint: you'll need to do some Googling to find this)
flights
options(tibble.width = Inf)



# Write a magrittr command that selects only January flights, then sorts the flights data by origin (ascending) and distance (descending), and finally glimpses the results
flights %>% filter(month == 1) %>% arrange(origin, desc(distance)) %>% glimpse
flights %>% filter(month == 1) %$% unique(month) #check filter

# select, distinct and mutate ---------------------------------------------

# select is a quick way of looking at the columns of interest
flights %>% select(distance, minute, arr_delay)

# Can also store in an object
flights_small = flights %>% select(distance, minute, arr_delay)
# Also a tibble

# select allows for indexing on names
flights %>% select(distance:arr_delay)

# ... and negation of said indexes
flights %>% select(-(distance:arr_delay))

# If you've got large numbers of columns then you can use some clever arguments
flights %>% select(ends_with('e'))
flights %>% select(starts_with('a'))
flights %>% select(contains('time'))
# Also matches, which allows for a regexp

# Remember: filter for selecting rows, select for selecting columns

# A related function to select is rename which will rename a column
flights %>% rename(depart_time = dep_time)
# This is not persistent unless saved into a new tibble

# Use distinct as a (supposedly) much faster alternative to unique
flights %>% distinct(carrier)

# How much faster? - not at all
system.time(replicate(1e2, ans <- flights %>% distinct(origin)))
system.time(replicate(1e2, ans <- distinct(as_data_frame(flights$origin))))
system.time(replicate(1e2, ans <- flights %>% extract('origin') %>% unique))
system.time(replicate(1e2, ans <- unique(flights$origin)))

# you can also do it with multiple arguments
flights %>% distinct(carrier, origin)

# Is this faster?
system.time(replicate(1e2, ans <- flights %>% distinct(carrier, origin)))
system.time(replicate(1e2, ans <- distinct(flights[,c('origin', 'carrier')])))
system.time(replicate(1e2, ans <- flights %>% extract(c('origin','carrier')) %>% unique))
system.time(replicate(1e2, ans <- unique(flights[,c('origin','carrier')])))
# Now distinct seems to be faster

# Add new columns with mutate
flights %>% mutate(gain = arr_delay - dep_delay) %>% select(arr_delay, dep_delay, gain)

# Mutate will let you create multiple new columns this way
flights %>% mutate(gain = arr_delay - dep_delay, speed = distance / air_time * 60) %>% select(speed, gain)

# One nice thing about mutate is that you can refer to already created variables when creating new ones
flights %>% mutate(gain = arr_delay - dep_delay, gain_per_hour = gain / (air_time / 60)) %>% select(gain_per_hour, gain)

# You can get round using the select command above by using transmute instead
flights %>% transmute(gain = arr_delay - dep_delay,
                      gain_per_hour = gain / (air_time / 60))

## EXERCISE 2

# Use suitable commands to determine the number of distinct values of:
# destinations
flights %>% distinct(dest) %>% nrow
# destination/origin combinations
flights %>% distinct(dest,origin) %>% nrow
# destination/origin/carrier combinations
flights %>% distinct(dest,origin,carrier) %>% nrow
# Input your answers

# select, rename, distinct, mutate, transmute
# Write a magrittr format command that, for flights originating from JFK, creates the average speed in miles per hour as:
# speed = distance / (air time / 60)
# as above and then outputs the correlation between speed and arr_delay
# Hint: use the extra argument use = 'complete.obs' in your cor command
flights %>% filter(origin == 'JFK') %>% transmute(speed = distance / (air_time / 60), arr_delay) %$% cor(speed, arr_delay, use = 'complete.obs')
flights %>% filter(origin == "JFK") %>% mutate(speed = distance / (air_time / 60)) %>% transmute(cor(speed, arr_delay, use = "complete.obs")) %>% distinct
flights %>% filter(origin == "JFK") %>% mutate(speed = distance / (air_time / 60)) %$% cor(speed, arr_delay, use = "complete.obs")
?magrittr
# summarise, sample and group_by ------------------------------------------

# summarise takes a set of values and puts it down to a single value
flights %>% summarise(mean_arr_delay = arr_delay %>% mean(na.rm = TRUE))

# Use sample_n and sample_frac to take samples of rows

# sample_n for a fixed number
flights %>% sample_n(10) # 10 random rows

# sample_frac for a proportion
flights %>% sample_frac(0.01)

# Use replace = TRUE for a bootstrap sample
flights %>% sample_frac(0.01, replace = TRUE)

# These functions by themselves are pretty useless. They come into their own when used with group_by to group the data into groups
# Here's a really cool example from the vignette
# 1) Group the data into individual planes (via tailnum) and count the number of flights
# 2) Compute the mean distance of the flights
# 3) Compute the mean arrival delay
# then plot

# Use magrittr notation to do this in order
out_tbl = flights %>%
  group_by(tailnum) %>% # Group by tail number
  summarise(count = n(), # n() here used to get number of rows in each group
            dist = mean(distance, na.rm = TRUE), # mean distance
            delay = mean(arr_delay, na.rm = TRUE)) %>% # mean arrival delay
  filter(count > 20, dist < 2000)

# Now plot
library(ggplot2)
ggplot(out_tbl, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()
# How cool is that!

# The summarise function now comes into its own, because we get something similar (and much more intuitive) than the aggregate function, in far fewer lines of code

## EXERCISE 3

# Use suitable group_by, summarise, and other dplyr/magrittr commands to answer the following questions:
# Which plane (i.e. the tail number) spent the most amount of time in the air?
# Which carrier has highest average delay (use arr_delay)?
# How many different planes travelled the furthest possible distance in a single flight?

# tidyr/readr --------------------------------------------------------------

library(tidyr)
# Like most of the Hadleyverse, tidyr is based on a philosophy for statistical computing
# The basic idea is that, for a tidy data set:
# 1) Every variable is a column
# 2) Every observation is a row
# 3) Each type of observational unit forms a table
# I think number 3 means that we can either group_by the different observational units to produce neat tables, or that each observational unit contains its own value

library(readr)
# readr is a neat package which contains some useful functions to read in tabular data much faster than the base packages, and with better defaults and structures for reading in non-standard data

# The idea of the tidyr package is to get all different types of data into this format
# The main functions are gather and spread, roughly equivalent to the melt and cast functions in reshape2 that we previously met, but more advanced.

# In the vignette (https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html) he goes through five examples. We'll do two of them

# Consider this data set
pew = read_csv('https://raw.githubusercontent.com/hadley/tidyr/master/vignettes/pew.csv')
# Look - read_csv. Automatically kept strings and read the column names correctly!
# Use spec(pew) to see exactly how it read everything in
glimpse(pew)

# The columns here are actually values- how can we fix?
# What we would like is 3 variables: religion, income, frequency, and a lot more rows
# Similar to long format which we previously discussed

# Use gather
pew2 = pew %>% gather(key = income,
                      value = frequency,
                      `<$10k`:`Don't know/refused`)
# key is the first variable to create which will take values from the column headings
# value is the second variable to create which will count up the number in each new row
# The third argument is a list of all the columns that need to be gathered up

# You can also do this with negative indexing
pew2 = pew %>% gather(key = income,
                      value = frequency,
                      -religion) # Gather up everything but religion
# This is perhaps more elegant

## EXERCISE 4
# You can download the billboard data containing all the US billboard data from 2000 via
billboard = read_csv("https://raw.githubusercontent.com/hadley/tidyr/master/vignettes/billboard.csv")
# These data have the same problem as above in that each week of the chart is given as a column. Write a gather command which creates a new data frame billboard2 which puts all the weeks as a new variable (hint: try and do this yourself, the answer is in the vignette if you get stuck)

# Use billboard2 and dplyr to answer the following questions:
# Which artist spent the most time in the charts (i.e. appears the most in the artist column)?
# Which artist had the longest average song length
# Which artist spent the most time at number 1?

# tidyr part 2: separate and spread ---------------------------------------

# Here are some data on tuberculosis
tb = read_csv("https://raw.githubusercontent.com/hadley/tidyr/master/vignettes/tb.csv")
# These data are counts of cases by year for different countries
# The column names are are mixture of sex (m/f) and age group (0-14, 5-14, etc)

# First gather up all of these except iso2 and year
tb2 = tb %>% ?gather(key = demographic,
                    value = cases,
                    -iso2, -year,
                    na.rm = TRUE)

# Now the problem is that the demographic variable contains both sex and age, which we'd like to split up. Could use strsplit, a regular expression, or the separate command
tb3 = tb2 %>% separate(col = demographic, # Original column
                       into = c("sex", "age"), # New columns to separate into
                       sep = 1) # sep value.

# Consider this slightly richer data set
weather = read_csv("https://raw.githubusercontent.com/hadley/tidyr/master/vignettes/weather.csv")

glimpse(weather)
# What a mess! This is weather data from a station in Mexico for 2010 looking at min and max temperature for each day. The column are day1, day2, etc, up to day 31

# What do we want to do?
# Make it so we've got 6 columns: id, year, month, day, t_min, t_max, in that order
# Get rid of all those horrible NAs
# Sort it so that it can be read more neatly

# Let's start by gathering it all together
weather2 = weather %>% gather(key = day, # Create new variable day
                              value = value, # Create new variable called value
                              d1:d31, # Columns to gather up
                              na.rm = TRUE) # Remove all those horrible NAs

# Half way there - let's tidy up by putting the columns in the right order, fixing the day variable and sorting by day/month
weather3 = weather2 %>%
  mutate(day = parse_number(day)) %>% # Get rid of the 'd' in e.g. 'd3'
  select(id, year, month, day, element, value) %>% # Order the columns
  arrange(id, year, month, day) # Sort properly

# Final thing - we've still got a column called 'element' containing tmin and tmax
# Use spread to spread the data out and create separate columns
weather4 = weather3 %>%
  spread(key = element,
         value = value) # Spread across element

## EXERCISE 5:

# Use a suitable spread command to turn pew2 back into pew in the examples above. Fill in the blanks (don't worry about the column order)
# pew2 %>% spread(key = [A],
#                 value = [B])

# Use a suitable spread command to turn the weather2 data above back into the weather data. Fill in the blanks (don't worry about the column order)
# weather2 %>% spread(key = [A],
#                     value = [B])

# Use the command:
titanic = tbl_df(Titanic)
# to turn the titanic data (included with R) into a tibble. Now use the spread function so that Survival (No or Yes) becomes a column instead of a row. Fill in the blanks below.
# titanic %>% spread(key = [A],
#                    value = [B])



