# Computing Probability Using Simulations
# Feb 26 (last class)

################################################################

# Computing Probability using Simulations Lecture

# simulate multiple scenarios and produce an empirically derived p value
# does not require binomial distribution or probability rules

# PS1 Q14 (Submissions to Nature vs Genetics)
sims <- 10000
counter <- 0

for (i in 1:sims){
  # generate a list of 10 numbers each a random number between 0 and 1000
  nature_sample = sample(0:1000, 10, replace = T)
  # make a logical vector of all times the random number less than 5%
  nature_lessthen5 = nature_sample <= 50
  # sum of TRUEs = Number of papers
  nature_total = sum(nature_lessthen5)
  genetics_sample = sample(0:1000, 5, replace = T)
  genetics_lessthen40 = genetics_sample <= 400
  genetics_total = sum(genetics_lessthen40)
  # compare number of Nature papers to Genetics papers
  if (genetics_total > nature_total) {
    counter = counter +1}
}

print(counter/sims) # Probability (one run) = 0.78

# SIMULATIONS
# two methods: bootstrapping and permutation tests

# BOOTSTRAPPING 
  # Take a sample of a data set with replacement
  # Calculate some property of the sample (mean, variance, etc)
  # Repeat multiple times
  # Simulated summary statistics = collections of means etc


# Code simulating the CLT
# import a distribution to sample
sample_to_test <- sample(1:1000,100)

# number of iterations to sample
n <- 10000

# size of samples to take
q <- 100

# reset vector of means
mean_vector = NULL

# start loop of n times
for (i in 1:n){
  # take random sample out of test distribution
  sample = sample(sample_to_test, q, replace =T)
  # calculate mean of random sample
  mean_sample = mean(sample)
  # make a vector reporting the means of all the random samples
  mean_vector = append(mean_vector, mean_sample)
} # close loop

hist(mean_vector) # histogram of sample means
# Example of bootstrapping
  # Entire dataset is 1000 values, samples are 100 values, samples are replaced


# PERMUTATION TESTS
# random sample generated p values
# starting data is "control" & "experimental"
control_length = length(control)
experimental_length = length(experimental)

# combine vectors & measure length
combined = c(control, experimental)
comb_length = length(combined)

# calculate difference in means of control and test set
starting_meandiff = mean(experimental) - mean(control)
direction = sign(starting_meandiff)

# choose number of iterations
sim = 10000
collect_means = NULL
extreme_count = 0

for(i in 1:sim){
  test_sample = sample(combined, comb_length, replace = F) # make random sample of data
  sample_control = head(test_sample, control_length)  # separate control sample
  sample_experimental = tail(test_sample, experimental_length) # separate experimental sample
  sample_meandiff = mean(sample_experimental) - mean(sample_control)  # calculate difference in means of samples
  
  collect_means = c(collect_means, sample_meandiff) # keep track of all means generated
  
  # if magnitude and direction of new mean greater than starting mean, add one to extreme count
  if ((abs(sample_meandiff) >= abs(starting_meandiff) & (sign(sample_meandiff) == direction))){extreme_count = extreme_count +1}
  
}

print(paste("p value is ", extreme_count/sim))
print(paste ("t-test value is ", t.test(control, experimental)$p.value))
# Note: replace = FALSE


# MEDIAN PERMUTATION TESTING
# random sample generated p values of medians
# starting data is "control" & "experimental"
control_length = length(control)
experimental_length = length(experimental)

# combine vectors & measure length
combined = c(control, experimental)
comb_length = length(combined)

# calculate difference in medians of control and test set
starting_mediandiff = median(experimental) - median(control)
direction = sign(starting_mediandiff)

# choose number of iterations
sim = 10000
count = 0
for(i in 1:sim){
  test_sample = sample(combined, comb_length, replace = F) # make random sample of data
  sample_control = head(test_sample, control_length) # separate control sample
  sample_experimental = tail(test_sample, experimental_length) # separate experimental sample
  sample_mediandiff = median(sample_experimental) - median(sample_control) # calculate difference in medians
  # if magnitude and direction of new median greater than starting median, add one to extreme count
  if ((abs(sample_mediandiff) >= abs(starting_mediandiff) & (sign(sample_mediandiff) == direction))){count = count +1}
}
print(paste ("Calculated median p-value = ", count/sim), quote = F)

