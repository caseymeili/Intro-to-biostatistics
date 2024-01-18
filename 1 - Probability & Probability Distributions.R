# Probability & Probability Distributions (Lecture) and Working with R (TBL)
# Jan 16 and Jan 18 2024

################################################################

# Probability & Probability Distributions Lecture

# BINOMIAL DISTRIBUTION: yes/no, true/false, alive/dead, success/failure
# dbinom(k, n, p)
# n = number of trials, p = certain success rate, k = k successes
# we conduct 30 trials (n) with a success rate of 0.3 (p). How often do we expect 10 successes (k)?
dbinom(10, 30, 0.3)

# cumulative mass function
# sum of all success rates up to and including 10
pbinom(10, 30, 0.3)
1 - pbinom(10, 30, 0.3) # probability of having more than 10 successes

# how often do you expect more than 13 successes?
1 - pbinom(13, 30, 0.3)


# GEOMETRIC DISTRIBUTION: a series of independent trials with two possible outcomes
# dgeom(gap between events, frequency of events)
# CMF = pgeom(number of non-events, frequency)

# you are a doctor in a clinic testing patients 1/10 of the population has COVID
# chance of n non-COVID patients between COVID patients = (0.9)n * 0.1
# 0.9 = chance of being COVID negative, 0.1 = chance of being COVID positive
# chance of next patient being seen being COVID positive?
dgeom(1, 0.1)


# POISSON DISTRIBUTION: random, independent events scattered over a period (time, space, etc)
# based entirely on mean (m) frequency
# dpois(number of events to be calculated, mean)
# CMF = ppois(number events up to certain value, mean)

# zero term: when no event occurs
# 100 people are exposed to COVID on average 2 times each (m=2). How many people would you expect never to have been exposed?
dpois(0, 2) # frequency
100 * dpois(0, 2) # number of people

################################################################

# Team Based Learning (TBL) - Working with R

# GAE 1
help(tidyverse)



# GAE 2
# create vectors
teamnames <- c("Aaron", "David", "Casey", "Lo", "Xinyi")
birthdays <- c("December", "July", "April", "September", "April")
birthmonth_num <- c(12, 7, 4, 9, 4)

# select team champion
champion_index <- which(teamnames == "David")

# list team champion's information 
champion <- list(Name = teamnames[champion_index],
  Birthday = birthdays[champion_index],
  Birthmonth_Num = birthmonth_num[champion_index])
champion



# GAE 3
# create a vector with numbers 1-100
v1 <- 1:100

# create a vector with upper case letters
v2 <- LETTERS

# create a list with data from v1 and v2
long_list <- list(v1, v2)

# create a vector with data from v1 and v2
long_vec <- c(v1, v2)



# GAE 4
# create a vector called "prior"
prior <- rep(0.2, 5)

# create a vector called "likelihood"
likelihood <- seq(0, 1, by = 0.25)

# calculate a vector called "unnorm"
unnorm <- prior * likelihood

# calculate a variable called "norm"
norm <- sum(unnorm)

# calculate a vector called "posterior"
posterior <- unnorm / norm
posterior 

# for loop to display poterior probabilities for each bowl
for (i in 1:5) {
  cat("Bowl", i, ":", posterior[i], "\n")
}



# GAE 5
# set the value of n (number of bowls)
n <- 5

# create a vector called "prior"
prior <- rep(1/n, n)

# create a vector called "likelihood" with percentages from 0 to 100 in n steps
likelihood <- seq(0, 1, length.out = n)

# calculate a vector called "unnorm"
unnorm <- prior * likelihood

# calculate a variable called "norm"
norm <- sum(unnorm)

# calculate a vector called "posterior"
posterior <- unnorm / norm
posterior

# for loop to display poterior probabilities for each bowl
for (i in 1:n) {
  cat("Bowl", i, ":", posterior[i], "\n")
}



# GAE 6
# set the value of n (number of bowls)
n <- 5

# create a vector called "prior"
prior <- rep(1/n, n)

# create a vector called "likelihood"
likelihood <- rep(c(0.5, 0.5, rep(1, 2)), each = n-2)

# calculate a vector called "unnorm"
unnorm <- prior * likelihood

# calculate a variable called "norm"
norm <- sum(unnorm)

# calculate a vector called "posterior"
posterior <- unnorm / norm

# for loop to display poterior probabilities for each bowl
for (i in 1:n) {
  cat("Bowl", i, ":", posterior[i], "\n")
}






