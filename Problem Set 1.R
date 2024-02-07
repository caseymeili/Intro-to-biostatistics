# Problem Set 1
# Casey Meili

# load packages
library(rstatix)

# set working directory
setwd("/Users/caseymeili/Desktop/Biostats/PS1")

################################################################

# QUESTIONS 1-9
# import all datasets
dat1A <- read.csv("Data1A.csv", header=TRUE)
dat1B <- read.csv("Data1B.csv", header=TRUE)
dat2A <- read.csv("Data2A.csv", header=TRUE)

dat2B <- read.csv("Data2B.csv", header=TRUE)
dat3A <- read.csv("Data3A.csv", header=TRUE)
dat3B <- read.csv("Data3B.csv", header=TRUE)
dat4A <- read.csv("Data4A.csv", header=TRUE)
dat4B <- read.csv("Data4B.csv", header=TRUE)

# convert data class to numeric rather than data.frame
dat1A <- dat1A$x
dat1B <- dat1B$x
dat2A <- dat2A$x
dat2B <- dat2B$x
dat3A <- dat3A$x
dat3B <- dat3B$x
dat4A <- dat4A$x
dat4B <- dat4B$x

# check normality for each dataset
shapiro_test(dat1A) # normally distrbuted, p-value = 0.738
shapiro_test(dat1B) # normally distrbuted, p-value = 0.807
shapiro_test(dat2A) # normally distrbuted, p-value = 0.150
shapiro_test(dat2B) # normally distrbuted, p-value = 0.420
shapiro_test(dat3A) # not normally distributed, p-value < 0.05
shapiro_test(dat3B) # not normally distributed, p-value < 0.05
shapiro_test(dat4A) # not normally distributed, p-value < 0.05
shapiro_test(dat4B) # not normally distributed, p-value < 0.05
# normally distrubuted: 1A/1B and 2A/2B
# not normally distributed: 3A/3B and 4A/4B

# test variance of normally distributed pairs
var.test(dat1A, dat1B, conf.level = 0.95) # variances are significantly different, p-value < 0.05
var.test(dat2A, dat2B, conf.level = 0.95) # variances are not significantly different, p-value = 0.3499

# t-test to compare two unknown distributions
t.test(dat1A, dat1B, var.equal = F, conf.level = 0.95) # p-value = 0.0042
t.test(dat2A, dat2B, var.equal = T, conf.level = 0.95) # p-value = 0.8012

# u-test for not normally distributed pairs 
wilcox.test(dat3A, dat3B) # fail to reject null hypothesis, no significant difference between datasets (p-value = 0.1988)
wilcox.test(dat4A, dat4B) # reject null hypothesis that datasets are the same (p-value = 0.007389)

# alpha of 0.05 to reject or not reject hypothesis
# NULL HYPOTHESIS - each pair is not different from one other

# ANSWERS
# 1A/AB: reject null hypothesis (datasets are significantly different) -> TRUE
# 2A/2B: fail to reject null hypothesis (no significant difference between datasets) -> FALSE
# 3A/3B: fail to reject null hypothesis (no significant difference between datasets) -> FALSE
# 4A/4B: reject null hypothesis (datasets are significantly different) -> TRUE



# QUESTION 10
# import dataset
scatter <- read.csv('plotdata.csv')

# create scatter plot with customized settings
plot(scatter$x2, scatter$y2, 
     main = "Hello, my name is Casey",
     col = "hotpink", # point color
     pch = 20,        # point pattern
     cex = 1.5,       # point size
     xlab = "x2",     # x-axis label
     ylab = "y2")     # y-axis label



# QUESTION 11
# load the mtcars dataset
data(mtcars)

# define given parameters
X <- mean(mtcars$mpg)  # mean mpg in 1974
A <- 26.4  # mean mpg in 2024
sigma <- 6.1  # sd of mpg in 2024
n <- nrow(mtcars)  # total number of samples

# calculate the z-score using provided equation
z_score <- (X - A) / (sigma / sqrt(n)) 
z_score # z-score = -5.851019

# calculate cumulative p-value
pnorm(z_score) # p-value = 2.442855e-09

# NULL HYPOTHESIS - no change in mean mpg between 1974 and 2024

# ANSWER
# because 2.442855e-09 (the p-value) is < 0.05 we reject the null hypothesis
# there was a statistically significant increase in mean mpg from 1974 to 2024



# QUESTION 12
# probability of success in one minute
p <- 1/12

# number of minutes until next success (less than 5) 
q <- 4

# calculate the probabilities for a success in less than 5 minutes
prob <- pgeom(q = q, prob = p)

# round to 2 decimal places
round(prob, 2)

# ANSWER
# probability the mouse will press the lever again in less than 5 minutes = 0.35



# QUESTION 13
# calculate lambda (rate of doses per student) 
l <- 200/55

# calculate the probability a student receives 6 or more doses
more6 <- 1 - ppois(5, lambda = l) # p = 0.1609
    # can also calculate using: more6 <- ppois(5, lambda = l, lower.tail = FALSE) 

# calculate the total number of students receiving at least 6 doses out of 55 total students
total <- round(more6 * 55)
total

# ANSWER
# number of students likely to get sick = 9



# QUESTION 14
# import given information (number of submissions to each journal and probability of acceptance)
s_nature <- 10
prob_nature <- 0.05
s_genetics <- 5
prob_genetics <- 0.4

# generate all combinations of successful submissions as vectors
# number of successful submissions to Genetics for each combination of successful submissions to Nature
c_genetics <- rep(1:s_genetics, each = s_nature) # vector from 1-5 repeated s_nature times
# number of successful submissions to Nature for each combination of successful submissions to Genetics
c_nature <- rep(0:(s_nature - 1), times = s_genetics) # vector from 0-10 repeated s_genetics times, -1 to include zero successful submissions

# filter combinations where successful submissions to Nature are fewer than successful submissions to Genetics
valid_combos <- c_nature < c_genetics

# calculate probabilities for valid combinations
valid_combo_p <- dbinom(c_genetics[valid_combos], size = s_genetics, prob = prob_genetics) * dbinom(c_nature[valid_combos], size = s_nature, prob = prob_nature)

# sum all probabilities
total_probability <- sum(valid_combo_p)

# round to 2 decimal places
round(total_probability, 2) 

# ANSWER
# probability of publishing more papers in Genetics than in Nature = 0.79


