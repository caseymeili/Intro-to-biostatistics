# Summary Statistics, Hypothesis Testing (parametric) and TBL
# Jan 22, 23 and 25 2024

################################################################

# Summary Statistics Lecture

# PDF for normal distribution
# dnorm(range, mean, sd)
# range is a vector 

# ranges in discrete distributions were defined by “Cumulative mass functions” (CMF)
# ranges in continuous distributions are defined by “Cumulative density functions” (CDF)

# What fraction of this distribution falls between 20 and 40? mean = 40, SD = 10 
# pnorm(range, mean, sd)
pnorm(40, 40, 10) - pnorm(20, 40, 10)

# What percentage of this distribution falls between 30 and 80? mean = 40, SD = 10 
pnorm(80, 40, 10) - pnorm(30, 40, 10)

# MEAN: a value related to the most typical value for a probability distribution
# mean(values)

# geometric with success frequency = 0.1 = p 
R = 0:100
G = dgeom(R, 0.1)
W = G*R
sum(W)
# average distance between successes = 9, most common distance = 0

################################################################

# Hypothesis Testing (parametric) Lecture

# VARIANCE: a measure of dispersion around the mean (based on population, not population size)
# var(data)

# STANDARD DEVIATION: the (positive) square root of variance
# sd(data)


# PRACTICE QUESTION
# A student is given a multiple choice exam with 10 questions, each question with five possible answers. He guesses randomly for each question. 
# p = 0.2, n =10

# assign parameters
p <- 0.2  # probability of getting a question correct
n <- 10   # number of questions

# expected number of correct answers
n * p

# probability of getting exactly 6 questions correct
dbinom(6, n, p)

# probability of getting at least 6 questions correct
pbinom(5, n, p, lower.tail = FALSE)

# standard deviation
sqrt(n * p * (1 - p))


# PRACTICE QUESTION
# Suppose the average number of lions seen on a 1- day safari is 5. What is the probability that tourists will see fewer than four lions on the next 1-day safari?

# assign parameters
lions <- 5  # average number of lions seen

# probability of seeing fewer than four lions
ppois(3, lions)


# PRACTICE QUESTION
# Among females in the US between the ages of 18 and 74, diastolic blood pressure is normally distributed with mean 77 mm Hg and standard deviation of 11.6 mm Hg.
# What is the probability that a randomly selected woman has:

# assign arameters
mean_dbp <- 77     # mean diastolic blood pressure
sd_dbp <- 11.6     # standard deviation of diastolic blood pressure

# probability of diastolic blood pressure less than 60
pnorm(60, mean_dbp, sd_dbp)

# probability of diastolic blood pressure greater than 90
1 - pnorm(90, mean_dbp, sd_dbp)

# probability of diastolic blood pressure between 60 and 90
pnorm(90, mean_dbp, sd_dbp) - pnorm(60, mean_dbp, sd_dbp)


# ONE SAMPLE Z TEST: testing data against a known distribution
# how likely is our data value comes from this null hypothesis distribution?
# qnorm(p, mean, sd)

# for what value of x does pnorm(x, 0, 2) = 0.95
qnorm(0.95, 0, 2)

# PRACTICE QUESTION
# Normal fasting glucose blood levels in a normal population have a mean of 85 mg/dl with a standard deviation of 15 mg/dl.
# We treat a group of 30 individuals with a new medication and find that they have a mean fasting glucose level of 100 mg/dl
# Is this significant?


# T TEST: testing two distrubutions against each other
# different t-statistic is used depending if the two sample distributions have statistically different variances
# var.test(A, B, conf.level = 0.95), where A and B are vectors containing your two data sets
# t.test(A, B, var.equal = F, conf.level = 0.95), where A and B are vectors containing your two data sets var.equal = F if variance is not the same, T if it is conf.level is 1-alpha

# make two distributions:
x = 1:10 
y = 7:20

# equal variances?
var.test(x, y, conf.level = 0.95)

# p-value of hypothesis variances are equal = 0.3343 
# do not reject hypothesis
t.test(x, y, var.equal = T, conf.level = 0.95)

# p-value = 3.691e-05
# can reject hypothesis that the two distributions are the same
# reject the null hypothesis (that they were the same)

# make two distributions:
x = 1:10
y = c(7:20, 200)

#equal variances?
var.test(x, y, conf.level = 0.95)

# p-value of hypothesis variances are equal = 1.257e-09 
# reject hypothesis (variances are different)
t.test(x, y, var.equal = F, conf.level = 0.95)

# p-value = 0.1245
# cannot reject hypothesis that the two distributions are the same
# high variance reduces confidence

# paired t-test
# t.test(A, B, paired = T, conf.level = 0.95) where A and B are vectors containing your (matched) data sets
# two datasets must match, samples must be in the same order

################################################################

# Team Based Learning (TBL) - Hypothesis Testing (parametric)

# GAE 2B
# create data sets
wild_type <- c(1, 2, 3, 19, 20, 20, 22, 28, 29, 30, 31, 32, 34, 35, 35, 36)
mutant <- c(25, 26, 27, 28, 28, 28, 29, 30, 30, 31, 32, 32, 34, 35, 35, 36)

# function for caluclating summary statistics (mean, median, and standard deviation)
summary_stats <- function(data) {
  mean_val <- mean(data)
  median_val <- median(data)
  sd_val <- sd(data)
  return(c(Mean = mean_val, Median = median_val, SD = sd_val))
}

# original summary stats
summary_stats(wild_type)
summary_stats(mutant)

# T-test
t_test_result <- t.test(wild_type, mutant)
t_test_result
















# removing outliers (e.g., values greater than 32)
wild_type_no_outliers <- subset(wild_type, wild_type <= 32)

# Summary stats after removing outliers
trimmed_summary <- summary_stats(wild_type_no_outliers)

# T-test after removing outliers
t_test_result_no_outliers <- t.test(wild_type_no_outliers, mutant)
t_test_result_no_outliers


# Print results
cat("Original Summary Statistics:\n", original_summary, "\n\n")
cat("T-Test Result (Original Data):\n", t_test_result, "\n\n")

cat("Summary Statistics after Removing Outliers:\n", trimmed_summary, "\n\n")
cat("T-Test Result after Removing Outliers:\n", t_test_result_no_outliers, "\n")








