# Hypothesis Testing (non-parametric)
# Jan 29, 30 and Feb 1

################################################################

# Hypothesis Testing (non-parametric) Lecture

# Checking if a distribution is normal
library(rstatix) 
# shapiro_test(data vector)
# p value indicates if data is normal, if p >0.05 data is normal, use t-test or z-test
# if not normal use non-parametric test

# MANN-WHITNEY (WILCOXON) U-TEST: to compare two unpaired distributions
# wilcox.test(x, y), where x and y are the two distributions to compare
# insensitive to outliers, quiet conservative (not many false positives, some false negatives)

# WILCOXON SIGNED RANK TEST: compare two unpaired distributions
# wilcox.test(x, y, paired = TRUE)

################################################################

# Problem Solving/Discussion: R Session 1

# PART 1 - DATAFRAME TOOLS
# Load dataset - motor trend car road tests from 1974 (called mtcars)
mycars <- mtcars

# list data types in a dataframe
str(mycars)

# accessing data in a dataframe by coordinates
# dataframe[row, column]
print(mycars[5,4])
print(mycars[4,5])
print(mycars[10,11])

# to get a column: dataframe[, column]
print(mycars[,4])
print(mycars[,11])

# to get a row: dataframe[row,]
print(mycars[12,])
print(mycars[28,])

# data can be passed to vectors for analysis
hp <- mycars[,4]
mean(hp)
median(hp)

# class exercise - what is the mean and median values for column "disp"?
disp <- mycars[,3]
mean(disp)
median(disp)

# columns can be identified by name
carweight <- mycars$wt 
print(carweight)

# class exercise - Are the following values (mpg and cyl) normally distributed? 
shapiro_test(mycars$mpg) # yes, normally distributed
shapiro_test(mycars$cyl) # no, not normal

# converting column types 
mycars$cyl <- as.integer(mycars$cyl)

# modifying columns 
# create a new column called index with values 1-32 
mycars$index <- 1:32

# remove a column from a dataframe
mycars$qsec <- NULL

# rename a column in a dataframe
colnames(mycars)[2] <- "cylinders"

# replace data in a column
mycars$index <- 32:1

# looking at a new dataframe - list of cars from 1993 and their specs
library(MASS)
my93 <- Cars93
str(my93)

# covert a factor to characters, replace the factor column “Model” with characters
my93$Model <- as.character(my93$Model) 

# class exercise - Did the fuel economy of cars significantly improve from 1974 to 1993?
shapiro_test(mycars$mpg) # yes, normally distributed
shapiro_test(my93$MPG.city) # no, not normal
wilcox.test(mycars$mpg, my93$MPG.city) # reject null hypothesis that the MPGs are the same

# PART 2 - DATA GENERATING TOOLS
# generating random data: sample(vector, number of samples, replace = T)
sample(1:10, 20, replace = T)

# simulate coin flipping
sample(c("H", "T"), 10, replace = T)

# use data from a dataframe
sample(my93$EngineSize, 10, replace = T)

# simulate 2 dice rolls
sample(1:6, 10, replace = T) + sample(1:6, 10, replace = T)

# generating random data without replacement: 
sample(my93$Model, 10, replace = F)

# class exercise - what if you set “number of samples” = total number of values in column?
sample(my93$Model, 93, replace = F)
# produces your list but in a randomized order

# class exercise - what if you set “number of samples” greater than the total number of values in column?
sample(my93$Model, 95, replace = F)
# produces an error, setting as replace = T will allow you to generate more values than the total number of values

# making a vector with repeating values: rep(vector to rep, number of replications)
rep(1, 30)
rep(1:2, 15)
rep(c("Good", "Better", "Best"), 4)

# class exercise - what about rep(sample(1:10, 1), 30)?
rep(sample(1:10, 1), 30)
# produces the same number every time
# rep does not rerun the function, it calculates the function and THEN replicates it

# changing a dataframe 
mycars$index <- rep(1:2, 16) 
View(mycars)

mycars$badindex <- sample(1:32, 32, replace = T) 
View(mycars)
# bad index because values are repeating, replace should = T 

mycars$badindex <- sample(1:32, 32, replace = F) 
colnames(mycars)[12] = "goodindex" 
View(mycars)

# sorting a vector
# generate a list of random numbers and sort them
random <- sample(1:100, 20)
sort(random)

# class exercise - what happens when you run the following commands
sort(my93$Model) 
# sorts numbers first, then alphabetical

sort(my93$Horsepower, decreasing =T)
# sorts high to low
# doesn't actually sort the dataframe, just produces a vector of the sorted data

# rnorm: generates a vector containing randomly sampled values from a normal distribution
# rnorm(size, mean = x, sd = y)
# similarly rbinom, rpois, and rgeom also generate vectors using randomly sampled values

# PART 3 - SIMPLE GRAPHING TOOLS
# use dnorm from 1:10 in steps of 0.1 
# mean = 5, sd = 1
normplot <- dnorm(1:100 * 0.1, 5, 1) 
plot(normplot)

# symbols can be different sizes, colors, shapes etc
plot(normplot, pch = 4, col = "hotpink", cex = 0.5)
# pch is symbol shape, col is color, cex is symbol size

plot(normplot, type = "line")

# scatter plots
xvector = c(55.3846,51.5385,46.1538,42.8205,40.7692,38.7179,35.6410,33.0769,28.9744,26.1538,23.0769,22.3077, 22.3077,23.3333,25.8974,29.4872,32.8205,35.3846,40.2564,44.1026) 
yvector = c(97.1795,96.0256,94.4872,91.4103,88.3333,84.8718,79.8718,77.5641,74.4872,71.4103,66.4103,61.7949, 57.1795,52.9487,51.0256,51.0256,51.0256,51.4103,51.4103,52.9487)
plot(xvector, yvector)

# histograms
hist(mycars$mpg)

# can change the number of bins with breaks = vector
hist(mycars$mpg, breaks = c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40))

# density plots using ggpubr
# similar to histogram but rolling
library(ggpubr)
ggdensity(mycars$mpg, fill = "hotpink") 
ggdensity(my93$MPG.city, fill = "darkturquoise")

################################################################

# Team Based Learning (TBL) - Hypothesis testing (non-parametric)

# using R for chi-square calculations
# chisq.test(x, y) or chisq.test(x, p = vector of expected frequencies)

# GAE 2A
# observed counts
observed <- c(56, 84, 50)

# expected counts for a 1:2:1 ratio
expected <- c(0.25, 0.5, 0.25)

# perform chi-square test
chi_square_result <- chisq.test(observed, p = expected)
chi_square_result


# GAE 2B
observed2 <- c(56, 84, 25)
chisq.test(observed2, p = expected)

observed3 <- c(56, 84, 0)
chisq.test(observed3, p = expected)

