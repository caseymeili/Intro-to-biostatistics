# Sampling and Comparing Samples
# Feb 20 and 22

################################################################

# Sampling and Comparing Samples Lecture

# LOOPS 
# for (i in 1:n){whatever code you want}, will run whatever code you want n times
n <- 10
for (i in 1:n){print("Hello")}

for (i in 1:n){
  print(sample(1:10, 3, replace = T))
  }

# simulating the central limit theorem 
# import a distribution to sample
sample_to_test = sample(1:1000,100)

# number of iterations to sample
n <- 10000

#size of samples to take
q <- 100

# reset vector of means
mean_vector = NULL

# loop of n times
for (i in 1:n){
  sample = sample(sample_to_test, q, replace =T) # take random sample out of test distribution
  mean_sample = mean(sample) # calculate mean of random sample
  mean_vector = append(mean_vector, mean_sample) # make a vector reporting the means of all the random samples 
  } 

# histogram of sample means
hist(mean_vector) 

# class exercise 
# import a distribution to sample
sample_to_test = dgeom(1:100,0.03)

# number of iterations to sample
n <- 1000

# size of samples to take
q <- 40

# reset vector of means
mean_vector = NULL

# loop of n times
for (i in 1:n){
  sample = sample(sample_to_test, q, replace =T) # take random sample out of test distribution
  mean_sample = mean(sample) # calculate mean of random sample
  mean_vector = append(mean_vector, mean_sample) # make a vector reporting the means of all the random samples
  }

# histogram of sample means
hist(mean_vector) 

# class exercise
# import a distribution to sample
sample_to_test = dnorm(1:100*0.1,6,2)

# number of iterations to sample
n <- 10000

# size of samples to take
q <- 40

# reset vector of means
mean_vector = NULL

# loop of n times
for (i in 1:n){
  sample = sample(sample_to_test, q, replace =T) # take random sample out of test distribution
  mean_sample = mean(sample) # calculate mean of random sample
  mean_vector = append(mean_vector, mean_sample)  #make a vector reporting the means of all the random samples
  } 

# histogram of sample means
hist(mean_vector) 

# print summary statistics 
print(c("n of sampled distribution ", NROW(sample_to_test))) # number of samples in original distribution
print(c("mean of sampled distribution ", mean(sample_to_test))) # mean of samples in original distribution
print(c("sd of sampled distribution ", sd(sample_to_test))) # sd of samples in original distribution
print(c("Mean of means ",mean(mean_vector))) # mean of sample means
print(c("sd of means ", sd(mean_vector))) # sd of sample means

# calculate 
sample_to_draw = data.frame(c(1:NROW(sample_to_test)), sample_to_test)
colnames(sample_to_draw)=c("x","y")
hist_data = hist(mean_vector, plot = F)
xd = tail(hist_data$breaks,1)-head(hist_data$breaks,1)
yd = max(hist_data$counts)

# plot line on graph
sample_to_draw$x = sample_to_draw$x*xd/NROW(sample_to_test)+head(hist(mean_vector, plot = F)$breaks,1)
sample_to_draw$y = sample_to_draw$y*yd/max(sample_to_draw$y)
points(sample_to_draw)

# NESTED LOOPS - iterates from inside out 
# for (i in 1:n){whatever code you want
  # for (j in 1:m){whatever code you want} }

n <- 10
for (i in 1:n){test = character(0) 
for (j in 1:5){test = c(test, "Hello") }
  print(test) 
  }

n <- 10
for (i in 1:n){
  print(sample(1:9, i, replace =T))
  }

vec <- c(10,8,4,2,1,2,4,6,8,10) 
for (i in vec){
  print(sample(1:9, i, replace =T))
  }

vec <-  c(0.8, -3, 2.5, 7, 10, -2) 
for (i in vec){
  print(i^2) 
  }

vec <- c("Utah", "Guinea Pig", "Chevy Nova") 
for (i in vec){
  print(i) 
  }

vec = c("Utah", "Guinea Pig", "Chevy Nova") 
for (i in vec){
  print(i)
  print(intToUtf8(rev(utf8ToInt(i)))) # reverse string 
  }
  
# CONDTIONAL LOOPS - will repeat whatever code you want until condition statement is met
# repeat{whatever code you want
    # Conditional if statement {break} }

# import a distribution to sample
sample_to_test = sample(1:100,100, replace = T)

# size of samples to take
q = 40

# reset count & mean_vector
count = 1
mean_vector = NULL

# create first value in mean_vector
sample = sample(sample_to_test, q, replace =T) # take random sample out of test distribution
mean_sample = mean(sample) # calculate mean of random sample
mean_vector = append(mean_vector, mean_sample) # make a vector reporting the means of all the random samples #start repeat loop

repeat{
  sample = sample(sample_to_test, q, replace =T) # take random sample out of test distribution
  mean_sample = mean(sample) # calculate mean of random sample
  mean_vector = append(mean_vector, mean_sample) # make a vector reporting the means of all the random samples count = count +1 #increase count by 1
  test_sd = sd(mean_vector)/count # calculate average sd
  # if average sd less than 1%, stop iterating
  if(test_sd <= 0.01) {
    break }
  } 

# print number of counts to get there (sd < 1%) and final sd
print(c("count", count)) 
print(c("final sd", test_sd))

# CORRELATION COEFFICIENT 
# cor(x, y, method = c("pearson", "kendall", "spearman")

################################################################

# Team Based Learning (TBL) - Sampling and Comparing Samples
# using next within a loop - jumps to end of loop without running the next lines of code
n <- 10
for (i in 1:n){
  if (i == 5) {next} print(paste("Hello" , i))
}

# GAE 1
# set working directory
setwd("/Users/caseymeili/Desktop/Biostats/PS1")

# import dataset
dat1A <- read.csv("Data1A.csv", header=TRUE)
dat1A <- dat1A$x

# calculate mean
mean_Data1A <- mean(dat1A)

# calculate SEM and 90% CI
tt_result <- t.test(dat1A, conf.level = 0.90)
tt_result$conf.int # lower = 7.244971, upper = 7.407607
tt_result$stderr # 0.0489753

# alternative ways to calculate SEM
library(Rmisc)
CI(dat1A, ci = 0.90)

# display results rounded using "paste"
print(paste(
  "The mean of Data1A is", signif(Data1A_tt$estimate, 3),
  ", SEM =", signif(Data1A_tt$stderr, 2),
  ", 90% CI =", signif(Data1A_tt$conf.int[1], 3),
  " to", signif(Data1A_tt$conf.int[2], 3)
))



# GAE 2
# set working directory
# uploaded data through R environment, could not be imported through read.csv (symbols)
test_set

# empty vector to store duplicated values
duplicated_values <- c()

# loop through each element in the matrix
for (i in 1:nrow(test_set)) {
  for (j in 1:ncol(test_set)) {
    # check if the current element is duplicated
    if (sum(test_set == test_set[i, j]) > 1) {
      duplicated_values <- c(duplicated_values, test_set[i, j])
    }
  }
}

print(duplicated_values)



# GAE 3 
# method 1 using cor
r_1A_1B <- cor(Dat1A$x, Dat1B$x, method = "pearson")
r_1A_1B

# method 2 using lm 
# run the linear model
model_1A_1B <- lm(Dat1A$x ~ Dat1B$x)
summary(model_1A_1B)

# get r from square root of R^2
sqrt(summary(model_1A_1B)$r.squared)

# is the correlation negative or positive?
# look at the beta (slope)
coef(model_1A_1B) # value is negative


