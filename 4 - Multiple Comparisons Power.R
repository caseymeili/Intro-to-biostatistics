# Multiple Comparisons: Power
# Feb 5, 6 and 8

################################################################

# Multiple Comparisons: Power Lecture

# CENTRAL LIMIT THEOREM: if you have a normal distribution then the distribution of sample means has a normal distribution
# if you have (almost) any distribution then the distribution of sample means (or sums) has a normal distribution

# EFFECT SIZE: magnitude of difference between distributions
# d = estimated difference in means / pooled estimated standard deviations
# how to make the effect size larger: increase estimated difference in means OR decrease pooled estimated standard deviations

# type 1 error: false positive - rejecting a null hypothesis that is actually true probability = "alpha"
# type 2 error: false negative - failing to reject a null hypothesis that is actually false probability = "beta"

# POWER: the probability that we will correctly reject the null hypothesis
# power is not: the probability that we will reject the null hypothesis OR the significance level
# if there is no difference between datasets, power doesn't apply
    # if the null hypothesis is false -> apply hypothesis testing -> incorrectly fail to reject (type II error: beta) OR correctly reject (Power: 1 - beta)
    # trade off between type I and II errors: decreasing alpha, type I errors and power results in increased type II errors
    # similarly, decreasing type II errors increases power, type I errors and alpha
# how much power is enough power? generally 0.8 is used

################################################################

# Multiple Comparisons Problem Solving and Discussion

# how do you perform statistics on multiple comparisons? 2 approaches
# APPROACH 1: adjust your p-value
    # Bonferroni is common but VERY conservative: reduce false positives at expense of getting many more false negatives
    # less stringent p-value corrections: Holm's, Hochberg's, and Hommel
    # p.adjust(p-value vector, method = "BY")

# generate a random test array of 100 p values (0-n)
n = 0.1
test_array = sample(0:1000, 100, replace = T) / (1000 / n)

# sort array for visualization purposes
sorted_array = sort(test_array)

# plot sorted array
plot(sorted_array, ylim = 0:1)

# apply error correction
corrected = p.adjust(sorted_array, method = "hommel") # can put bonferroni, holm, hochberg, hommel, or BH here

# plot corrected array
points(corrected, pch = 19)

# nominal alpha of 0.05
lines(x = 0:100, y = rep(0.05,101), lwd = 2) # open circles are uncorrected, closed circles are corrected


# APPROACH 2: accept a defined error rate in your conclusions
    # False Discovery Rate (FDR) saves many true positives at the expense of a certain rate of false positives (typically around alpha)
    # p.adjust(p-value vector, method = "BH")

# generate a random test array of 100 p values (0-n)
n = 0.1
test_array = sample(0:1000, 100, replace = T) / (1000 / n)

# sort array for visualization purposes
sorted_array = sort(test_array)

# plot sorted array
plot(sorted_array, ylim = 0:1)

# apply error correction
corrected = p.adjust(sorted_array, method = "BH") # BH = Benjamini/Hochberg (also called false discovery rate)

# plot corrected array
points(corrected, pch = 19)

# nominal alpha of 0.05
lines(x = 0:100, y = rep(0.05,101), lwd = 2) # open circles are uncorrected, closed circles are corrected


# ANOVA (Analysis of Variance): are there mean differences between 2 or more independent levels?
    # used for parametric data
    # works by decomposing the variance within the groups and across the groups, as well as the impact of the sample size in the variance
# one way ANOVA has one independent variable (with 2 or more groups) e.g. affect of drug on anxiety  
# ANOVA produces a test statistic called F which can be used to produce a p-value

# converting wide data to long data 
library(reshape2)
long_data <- melt(wide_data) # converts wide data to long

# convert character columns to factors
long_data$County = as.factor(long_data$County)

# ANOVA command: aov
# anova_drugs <- aov(formula = Effect ~ Dose, data = drugs_long)
    # analyze the data "Effects" with respect to the factor "Dose"
    # summary(anova_drugs)

# if anova is signigicant, utilize TukeyHSD to determine what factor is making it significant
# TukeyHSD: finds the direction, magnitude, and p values between each column
# TukeyHSD(anova_drugs)

# for non parametric data use the "Kruskal-Wallis" test
# input data in the same format as ANOVA (long data with a factors column)
# kt_drugs <- kruskal.test(Effect ~ Dose, data = drugs_long)

# no equivalent to TukeyHSD for non parametric data so we have to perform multiple comparisons using the Wilcoxon
# pairwise.wilcox.test(drugs_long$Effect, drugs_long$Dose, p.adjust.method = "bonferroni")
    # values, factors, and p-adjustment for multiple comparisons

################################################################

# Team Based Learning (TBL) - Multiple Comparisons: Power
# determining effect size
library(effsize)
# cohen.d(a, b)

# GAE 2 
# calculating power
library(pwr)
# pwr.t.test(n, effect size, sig.level, power), specify 3 and it will caluclate the 4th 
    # can also specify type of t test

pwr.t.test(n = 75, d = 0.5, sig.level = 0.05)
    # power = 0.8603675
pwr.t.test(, 0.2, 0.05, 0.8)
    # n = 394

# GAE 3 
# set working directory
setwd("/Users/caseymeili/Desktop/Biostats/PS1")

# import all datasets
dat1A <- read.csv("Data1A.csv", header=TRUE)
dat1B <- read.csv("Data1B.csv", header=TRUE)
dat2A <- read.csv("Data2A.csv", header=TRUE)
dat2B <- read.csv("Data2B.csv", header=TRUE)

# convert data.frame to numeric 
dat1A <- dat1A$x
dat1B <- dat1B$x
dat2A <- dat2A$x
dat2B <- dat2B$x

# calculate cohen's d (effect size)
cohd1 <- cohen.d(dat1A, dat1B, paired = FALSE)
cohd2 <- cohen.d(dat2A, dat2B, paired = FALSE)

# print values and call effect size
cohd1 # d = -0.4106344
cohd2 # d = 0.03566115

cohd1$estimate


# GAE 4 
# power analysis with different numbers of samples for the two conditions
# pwr.t2n.test(n1 = NULL, n2 = NULL, d = NULL, sig.level = 0.05, power = NULL, alternative = c("two.sided", "less", "greater"))
pwr.t.test(d = 0.2, sig.level = 0.05, power = 0.7) # n = 310
pwr.t2n.test(n1 = 310, n2 = 250, d = 0.2, sig.level = 0.05) # power = 0.6512943


# GAE 5 
setwd("/Users/caseymeili/Downloads")

# import csv
drink <- read.csv("drink_test.csv", header=TRUE)

# convert columns to factors (fasting (yes/no) and beverage)
drink$Fasting <-  as.factor(drink$Fasting)
drink$Beverage <- as.factor(drink$Beverage)

# ANOVA test
anova_fast <- aov(formula = After ~ Fasting, data = drink) # interpretation: non-fasting subjects show a significantly higher blood glucose level (+10.1) than fasting subjects
anova_drink <- aov(formula = After ~ Beverage, data = drink) # p value is not significant, so don't run Tukey test on ANOVA analysis

# find  direction, magnitude, and p values between each column
TukeyHSD(anova_drink)



