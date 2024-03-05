# Problem Set 2
# Casey Meili

# load packages
library(rstatix)
library(Rmisc)
library(tidyverse)
library(effsize)
library(pwr)

# set working directory
setwd("/Users/caseymeili/Desktop/Biostats/PS2")

################################################################

# QUESTIONS 1 & 2
# import bird dataset
load("/Users/caseymeili/Desktop/Biostats/PS2/Bird_data.RData")

# 2A - calculate Chi-square value, p value, and DF for the table
# load the dataset
load("/Users/caseymeili/Desktop/Biostats/PS2/Bird_data.RData")
chisq.test(bird_data) # X-squared = 149.84, df = 11, p-value < 2.2e-16

# 2B - identify which of the birds show a different distribution in the two habitats
# create an empty vector for chi-squares to fill in loop.
chisq_values <- c()

# create an empty vector for p values to fill in loop.
pvalues <- c()

# a sum of the two columns
total <- c(sum(bird_data[,1]), sum(bird_data[,2]))

for (i in 1:nrow (bird_data)) {
  row <- bird_data[i,] # get row from matrix
  test <- chisq.test(rbind(row, total - row)) # perform chi-squared test comparing each pair of species
  chisq_values <- append(chisq_values, test$statistic) # collect variables using the array
  pvalues <- append(pvalues, test$p.value) # collect outputs of p values
  adj_pvalues <- p.adjust(pvalues, method = "bonferroni") # bonferroni corrections for question 1C
}

# create a data frame with chi-squared and p-values along with species names and print
summary_df <- data.frame(Species = rownames(bird_data), ChiSquared = chisq_values, PValue = pvalues, AdjustedPValue = adj_pvalues)
print(summary_df)

# 2C - which species differ(s) from the other birds using the Bonferroni correction and a significance level of 0.05
# uncommon, ruby-crowned kinglet, white-crowned sparrow, lincoln's sparrow, hermit thrush, dark-eyed junco, 

# QUESTION 3
# create the given vectors and combine as data frame
gym <- c(1, 2, 3, 4, 5, 6)
num_athletes <- c(2, 3, 1, 1, 1, 7)
correlation <- c(0.7, 0.2, NA, NA, NA, 1.0)

gym_data <- data.frame(gym, num_athletes, correlation)

# calculate effective sample size for each gym (with more than 1 member)
effective_ss_1 <- (2)/(1 + (2 - 1)*0.7)
effective_ss_2 <- (3)/(1 + (3 - 1)*0.2)
effective_ss_6 <- (7)/(1 + (7 - 1)*1)

# set effective sample size to 1 for gyms with 1 member
effective_ss_3 <- 1
effective_ss_4 <- 1
effective_ss_5 <- 1

# calculate total effective sample size
total_ess <- effective_ss_1 + effective_ss_2 + effective_ss_3 + effective_ss_4 + effective_ss_5 + effective_ss_6
total_ess # total effective sample size = 7.319328

# add 100 athletes to gym 6 and calculate effective sample size 
effective_ss_6_new <- (107)/(1 + (107 - 1)*1)
effective_ss_6_new # effective sample size = 1 
# this does not change the effective sample size at gym 6, correlation coefficient = 1



# QUESTION 4
# create the data set
calories <- c(186, 181, 176, 149, 184, 190, 158, 139, 175, 148, 152, 111, 141, 153, 190, 157, 131, 149, 135, 132)

# calculate 90% confidence interval 
tt_result <- t.test(calories, conf.level = 0.90)

# print mean
tt_result$estimate # mean = 156.85 

# print upper and lower CI values 
tt_result$conf.int # lower = 148.0956, upper = 165.6044 
# we are 90% confident that the true mean number of calories in all beef hot dogs falls within this interval



# QUESTION 5
# Load the dataset
load("linear_sets.Rdata")

# add column names "x" and "y" to each data frame
colnames(set1) <- c("x", "y")
colnames(set2) <- c("x", "y")
colnames(set3) <- c("x", "y")
colnames(set4) <- c("x", "y")

# change class to data frame instead of matrix
set1 <- data.frame(set1)
set2 <- data.frame(set2)
set3 <- data.frame(set3)
set4 <- data.frame(set4)

# fit each data set to linear model
lm1 <- lm(y ~ x, data = set1) # alpha = 27.472, beta = 0.977 
lm2 <- lm(y ~ x, data = set2) # alpha - 55.418, beta = 1.023 
lm3 <- lm(y ~ x, data = set3) # alpha = -27.7818, beta = 0.8994
lm4 <- lm(y ~ x, data = set4) # alpha = 75.3064, beta = 0.9692 

# print R squared from the summary of each model
summary(lm1)$r.squared # R2 = 0.665159
summary(lm2)$r.squared # R2 = 0.6191053
summary(lm3)$r.squared # R2 = 0.5522789
summary(lm4)$r.squared # R2 = 0.9502664

# display plots in a 2x2 grid
par(mfrow = c(2, 2))  

# plots for each data set
plot(set1, main = "Set 1", xlab = "X", ylab = "Y", pch = 19, cex = 0.25)
plot(set2, main = "Set 2", xlab = "X", ylab = "Y", pch = 19, cex = 0.25)
plot(set3, main = "Set 3", xlab = "X", ylab = "Y", pch = 19, cex = 0.25)
plot(set4, main = "Set 4", xlab = "X", ylab = "Y", pch = 19, cex = 0.25)

# plots for residuals from each data set
plot(lm1$residuals, main = "Set 1", xlab = "X", ylab = "Y", pch = 19, cex = 0.25)
plot(lm2$residuals, main = "Set 2", xlab = "X", ylab = "Y", pch = 19, cex = 0.25)
plot(lm3$residuals, main = "Set 3", xlab = "X", ylab = "Y", pch = 19, cex = 0.25)
plot(lm4$residuals, main = "Set 4", xlab = "X", ylab = "Y", pch = 19, cex = 0.25)

# data set 1 and 2 can be appropriately fitted to a linear model while set 3 and 4 cannot
# key: residual plots and linear model assumption of homoscedasticity



# QUESTION 6
# calculate Pearson correlation coefficient for each matrix
cor_set1 <- cor(set1)
cor_set2 <- cor(set2)
cor_set3 <- cor(set3)
cor_set4 <- cor(set4)

# print the correlation coefficients
print(cor_set1) # 0.816, strong linear correlation
print(cor_set2) # 0.787, strong linear correlation
print(cor_set3) # 0.743, moderate? linear correlation
print(cor_set4) # 0.975, very strong linear correlation

# No - the correlation coefficient does not distinguish between linear and non-linear relationships
# It only measures the strength of the linear association between variables. 



# QUESTION 7 
# load the data for FJ and other group
glucoseFJ <- read.csv("glucoseFJ.csv")
glucoseOther <- read.csv("glucoseOther.csv")

# convert the glucose1 and glucose2 columns to numeric
glucoseFJ$glucose1 <- as.numeric(glucoseFJ$glucose1)
glucoseFJ$glucose2 <- as.numeric(glucoseFJ$glucose2)
glucoseOther$glucose1 <- as.numeric(glucoseOther$glucose1)
glucoseOther$glucose2 <- as.numeric(glucoseOther$glucose2)

# 7A - null hypothesis
# for both groups, the null hypothesis is that there is no significant difference 
# between blood glucose levels before and after drinking juice or in the absence of fasting, respectively.

# check normality of both data sets (before and after)
shapiro.test(glucoseFJ$glucose1) # normally distributed, p value = 0.6424
shapiro.test(glucoseFJ$glucose2) # normally distributed, p value = 0.5201
shapiro.test(glucoseOther$glucose1) # normally distributed, p value = 0.3595
shapiro.test(glucoseOther$glucose2) # normally distributed, p value = 0.183

# test variance of normally distributed pairs
var.test(glucoseFJ$glucose1, glucoseFJ$glucose2, conf.level = 0.99) # variances are significantly different, p-value < 0.05 (0.04879)
var.test(glucoseOther$glucose1, glucoseOther$glucose2, conf.level = 0.99) # variances are significantly different, p-value < 0.05 (0.01532)

# perform paired t-test for both groups
t_test_fj <- t.test(glucoseFJ$glucose2, glucoseFJ$glucose1, paired = TRUE, var.equal = F, alternative = "two.sided", conf.level = 0.99)
t_test_fj # p-value = 3.296e-06

t_test_other <- t.test(glucoseOther$glucose2, glucoseOther$glucose1, paired = TRUE, var.equal = F, alternative = "two.sided", conf.level = 0.99)
t_test_other # p-value = 0.1007

# 7C - calculate effect size (Cohen's d) for each group
effect_size_fj <- cohen.d(glucoseFJ$glucose2, glucoseFJ$glucose1, paired = TRUE)
effect_size_fj # d estimate: 2.587956 (large)
# substantial difference between the mean glucose levels before and after fruit juice

effect_size_other <- cohen.d(glucoseOther$glucose2, glucoseOther$glucose1, paired = TRUE)
effect_size_other # d estimate: 0.7839755 (medium)
# moderated difference between the mean glucose levels between two fasting glucose tests 

# 7D - calculate the power for each group
power_fj <- pwr.t.test(d = 2.587956, n = 2, sig.level = 0.01, power = NULL, type = "paired", alternative = "two.sided")
power_fj # power = 0.04584395
# very low probability of detecting a true effect (if it exists) with the given sample size and effect size, leading to a high Type II error rate

power_other <- pwr.t.test(d = 0.7839755, n = 2, sig.level = 0.01, power = NULL, type = "paired", alternative = "two.sided")
power_other # power = 0.01558502
# very low probability of detecting a true effect (if it exists) with the given sample size and effect size, leading to a high Type II error rate

# 7E - determine the sample size required to achieve a power of 0.8 for each group
sample_size_fj <- pwr.t.test(n = NULL, d = 2.587956, power = 0.8, sig.level = 0.01, type = "paired", alternative = "two.sided")
sample_size_fj # n = 6 pairs of data

sample_size_other <- pwr.t.test(n = NULL, d = 0.7839755, power = 0.8, sig.level = 0.01, type = "paired", alternative = "two.sided")
sample_size_other # n = 23 pairs of data
