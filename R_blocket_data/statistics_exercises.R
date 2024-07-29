
# Some Statistics in R ----------------------------------------------------


# Normal Distribution -----------------------------------------------------

# Let X be a random variable with mean = 5 and standard deviation = 3. Calculate
# the following probabilities: 
#   1. P(X <= 7)
#   2. P(X < 7)
#   3. P(X > 7)
#   4. P(4 <= X <= 7)

# -------------------------------------------------------------------------
help(rnorm)
mu <- 5
std <- 3
#   1. P(X <= 7)
p1 <- pnorm(7, mean = mu, sd = std )
print(p1)

#   2. P(X < 7)
p2 <- pnorm(7, mean = mu, sd = std)
print(p2)

#   3. P(X > 7)
p3 <- pnorm(7, mean = mu, sd = std, lower.tail = FALSE)
print(p3)

#   4. P(4 <= X <= 7)
p4 <- pnorm(7, mean = mu, sd = std)-pnorm(4, mean=mu, sd = std)
print(p4)

# Explain what you see ----------------------------------------------------
# två normalfördelade kurvor med samma SD, men runt olika medelvärden. Ex samma prov har skrivits i sverige men olika klasser har olika medelvärde, men liknande spridning, dvs lika ojämga eller jämna i resultatet? 
x <- seq(from = -15, to = 25, by = 0.01)
y1 <- dnorm(x, mean = 7, sd = 3)
y2 <- dnorm(x, mean = 0, sd = 3)

plot(x, y1, type = "l", col = 'blue')
lines(x, y2,col="green")


# Explain what you see ----------------------------------------------------
# Två normalfördelande kurvor med samma medelvärde, fast olika spridning. Ett exempel är att man för samma prov har samma medelvärde, men olika spridning. De med mindre spriding har fått alla i klassen på en liknande nivå
x <- seq(from = -15, to = 25, by = 0.01)
y1 <- dnorm(x, mean = 5, sd = 4)
y2 <- dnorm(x, mean = 5, sd = 1)

plot(x, y1, type = "l", col = 'blue')
lines(x, y2,col="green")


# Binomial Distribution ---------------------------------------------------

# Simulate 10000 samples from bin(n=100, p=0.5) and plot a histogram. 
# Then simulate 10000 samples from bin(n= 100, p = 0.1) and plot a histogram. 
# In the plots, what is the "middle" of the plot? 
# Which plot has a "wider" distribution? 

# What is the theoretical/true Expected Value and Variance of the two distributions? 

# Estimate the mean and variance of the two samples that you simulated by using 
# the functions mean() and var(). 
# Explain your observations. 

# Simulate 10000 samples from bin(n=100, p=0.5) and plot a histogram. 
samples <- rbinom(n = 1000, size = 100, prob = 0.5)

hist(samples, main = "Histogram of Binomial Distribution", xlab = "Number of Successes in 1000 Trials", ylab = "Frequency", breaks = 50)
#väntevärdet är 10, varians 9
mean_01 <- mean(samples)
variance_01 <- var(samples)

# Then simulate 10000 samples from bin(n= 100, p = 0.1) and plot a histogram. 
samples2 <- rbinom(n = 1000, size = 100, prob = 0.1)

hist(samples2, main = "Histogram of Binomial Distribution", xlab = "Number of Successes in 1000 Trials", ylab = "Frequency", breaks = 50)
#väntevärdet är 50, varians 25
mean_02 <- mean(samples2)
variance_02 <- var(samples2)

print(paste("Mean for p=0.5:", mean_01, "Variance for p=0.5:", variance_01))
print(paste("Mean for p=0.1:", mean_02, "Variance for p=0.1:", variance_02))

#plot 1 har större varians

#E[X] = n*p
#Var(X)= n*p*(1-p)
#vilket blir för 1: 50 och 25, 2 blir 10 och 9

#Vi ligger nära det förväntade värdet 
#[1] "Mean for p=0.5: 50.066 Variance for p=0.5: 23.381025025025"
#[1] "Mean for p=0.1: 10.009 Variance for p=0.1: 9.04396296296296"


# Confidence Intervals ----------------------------------------------------
# Explain the code/results you see below 
# -------------------------------------------------------------------------
help(t.test)
# normalfördelad data, små urvalsstorlekar, STD okänt? t-testet ger ett p-värde 
#som hjälper till att bedöma om skillnaderna mellan grupper eller behandlingseffekter är statistiskt signifikanta
data <- rnorm(4322, mean = 5, sd = 4)
t.test(data, conf.level = 0.95)$conf.int
#stickprovsmedelvärde är en rimlig uppskattning av populationens medelvärde, med den angivna konfidensnivån


# Hypothesis Testing ------------------------------------------------------
# In the code below, can we reject the hypothesis that mu = 0? 
# Can we reject the hypothesis that mu = 5? 
# How do you interpret the p-value?
# -------------------------------------------------------------------------
help(t.test)

x <- rnorm(100, mean = 0, sd = 4)

t.test(x, mu = 0)
t.test(x, mu = 5)
#0 finns det inte statistiskt signifikant bevis för att medelvärdet skiljer sig från 0
#5, förkasta nollhypotesen om att det sanna medelvärdet är 5. det signifikant bevis för att medelvärdet för populationen inte är 5

# Testing for normality ---------------------------------------------------
# The Shapiro wilk normality test is a test of normality, the null
# hypothesis is that the data comes from a normal distribution.
# Comment on the results below.
# -------------------------------------------------------------------------
?shapiro.test

x1 <- rnorm(741, mean = 8, sd = 30)
x2 <- rpois(93, lambda = 3)
x3 <- rpois(301, lambda = 70)

shapiro.test(x1)
shapiro.test(x2)
shapiro.test(x3)

#p-värdet för x1 är större än 0.05, så det finns inge grund för att förkasta nollhypotesen om att datan är normalfördelad.
#p-värdet x2 är däremot lågt.Inte normalfördelat
#x3 är osäkert
# För x3
par(mfrow = c(2, 1)) # Arrangerar plotterna vertikalt
hist(x3, main = "Histogram for x3", xlab = "Values", col = "blue", breaks = 30)
qqnorm(x3, main = "QQ-Plot for x3")
qqline(x3, col = "red")

par(mfrow = c(2, 1)) # Arrangerar plotterna vertikalt
hist(x2, main = "Histogram for x2", xlab = "Values", col = "blue", breaks = 30)
qqnorm(x2, main = "QQ-Plot for x2")
qqline(x2, col = "red")
# -------------------------------------------------------------------------
# Generate 10 samples from N(5, 10) and then estimate
# the expected value (by using the mean() function)
# and standard deviation (by using the sd() function) of the data. 
# Do the same thing but use 100 samples instead. 
# Do the same thing but use 10000 samples instead. 

# When estimating fixed, but unknown parameters in reality, 
# should you strive for many or few samples/observations? Why?
# -------------------------------------------------------------------------
data <- rnorm(10, mean = 5, sd=10)
estimated_mean <- mean(data)
estimated_sd <- sd(data)
print(paste("Estimated mean 10:", estimated_mean))
print(paste("Estimated standard deviation:", estimated_sd))

data2 <- rnorm(100, mean = 5, sd=10)
estimated_mean <- mean(data2)
estimated_sd <- sd(data2)
print(paste("Estimated mean 100:", estimated_mean))
print(paste("Estimated standard deviation:", estimated_sd))

data3 <- rnorm(1000, mean = 5, sd=10)
estimated_mean <- mean(data3)
estimated_sd <- sd(data3)
print(paste("Estimated mean 1000:", estimated_mean))
print(paste("Estimated standard deviation:", estimated_sd))

#
# Law of Large Numbers (LLN) ----------------------------------------------

# Illustration of the Law of Large Numbers (LLN) [Called "De relative frekvensernas stabilitet" in the book
# "Statistisk Dataanalys"p.14], the code is provided below. 
# 
#   1. Simulate 600 dice throws. 
#   Hint: use the sample() function. You should also create an empty vector that
#   you append with the relative frequencies for 1, 2, ... 600 throws.
#   
#   2. Plot the relative frequency of the number of times three dots occur for 1, 2, ..., 600 throws.
#   3. To what number does the relative frequency converge to? 
#       What is the probability of getting three dots when throwing a dice? Do these two numbers coincide?
#   4. Explain what you observe. 
# -------------------------------------------------------------------------
empty_vec <- c()

for (i in seq(600)){
  throws <- sample(1:6, i, TRUE)
  relative_freq <- sum(throws == 3)/length(throws)
  empty_vec <- c(empty_vec, relative_freq)
}

plot(empty_vec, type="l", ylab='Relative Frequency', xlab='Number of Throws', main='Illustrating LLN')
abline(h=1/6, col = 'red')




# The rest of the script is "bonus material" and can be skipped -----------


# Confidence interval for proportions -------------------------------------
# Assume we ask 1000 people who they will vote for and 4.2% answer Centerpartiet (C).
# Create a 90% Confidence Interval to get the proportion that would vote for C
# if we surveyed the whole population ("Totalundersökning").
# OBS: note we want a 90% confidence interval.
# -------------------------------------------------------------------------
p <- 0.042
n <- 1000
z <- 1.6449
s_p <- sqrt(p*(1-p)/n)

ci_lower <- p - z*s_p
ci_upper <- p + z*s_p

print(ci_lower)
print(ci_upper)

# -------------------------------------------------------------------------
# Recall that the poisson distribution is a discrete distribution which
# can take the values 0, 1, 2, 3, ... . 
# From the plot below you see that the distribution is not the same as the
# normal distribution, since it for example is not symmetric and can only attain values >= 0.
# However, the lambda parameter changes the look of the distribution, 
# feel free to experiment with it to see the effect. 
# -------------------------------------------------------------------------
plot(dpois(x=0:50,lambda=3),type="b")

# -------------------------------------------------------------------------
# By using the Central Limit Theorem (CLT), explain what you see below. 
# -------------------------------------------------------------------------
x1 <- rpois(100, lambda = 7)
x2 <- rpois(1000, lambda = 7)
x3 <- rpois(10000, lambda = 7)
x4 <- rpois(100000, lambda = 7)
x5 <- rpois(1000000, lambda = 7)
x6 <- rpois(10000000, lambda = 7)
x7 <- rpois(100000000, lambda = 7)

par(mfrow=c(3,3))
hist(x1)
hist(x2)
hist(x3)
hist(x4)
hist(x5)
hist(x6)
hist(x7)


# Confidence Interval -----------------------------------------------------
# Assuming X is N(10, 3), we know (see example calculation below) that
# a random sample x will with probability 95% be between: 
#   
#   ci_lower = 10 - 1.96 * 3 and
#   ci_upper = 10 + 1.96 * 3
# 
# Example, for the ci_upper, we do the following calculation: 
# P(X > ci\_upper) = P(X > 10 + 1.96 * 3) = P(Z > 1.96) = 0.025
# where we used standardization in the penultimate step. 

# This means that if you get observations from the normal distribution above, 
# you would "expect" 95% of them to be between ci_lower and ci_upper. 

# Your task is now to simulate 10, 100, 1000 and 10000 normally distributed samples
# from N(10, 3) and check the relative frequency (%) of how many samples fall
# outside the confidence interval. How many % should it theoretically be?
# -------------------------------------------------------------------------
ci_lower <- 10 - 1.96*3 
ci_upper <- 10 + 1.96*3 

print(ci_lower)
print(ci_upper)

x <- rnorm(100, mean = 10, sd = 3)

result <- c()
for (i in x) {
  if (i >= ci_lower & i <= ci_upper) {
    result <- c(result, 0)
  }
  else {
    result <- c(result, 1)
  }
}

# print(result)
rel_freq_of_exceptions <- mean(result)
print(rel_freq_of_exceptions)

# -------------------------------------------------------------------------
# Notation: N(10, 5) is a normal distributed variable with mean 10 and standard
# deviation 5. 


# Create two empty vectors, x1 and x2. 
# Now do a for loop 100 times, where you for each iteration: 
#   1) Take the mean of 80 samples from N(10, 5) and store it in x1. 
#   2) Take the mean of 80 samples from N(10, 20) and store it in x2.

# So, your vectors x1 and x2 should contain 100 values. 

# (From p.148 in the book "Statistisk Dataanalys"), we know that the mean is an unbiased estimate
# of the fixed, but unknown "True" mean often called "mu". 
# The nice thing is that in our Monte Carlo Simulation we know the true mean.
# Plot histograms of x1 and x2 and comment on the result, do we on "average"
# get the right mean? 

# Is it easier or harder to estimate the mean when the variance is low/high?
# How is this knowledge useful in reality?

x1 <- c()
x2 <- c()
for (i in 1:100){
  x1 <- c(x1, mean(rnorm(80, mean = 10, sd = 5)))
  x2 <- c(x2, mean(rnorm(80, mean = 10, sd = 20)))
}

# Plotting two histograms in same plot: 
# https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r .
h1 <- hist(x1)
h2 <- hist(x2)

plot(h1, col=rgb(0,0,1,1/4), xlim=c(2,19))  # first histogram
plot(h2, col=rgb(1,0,0,1/4), xlim=c(2,19), add=T)  # second

# -------------------------------------------------------------------------
# Redo the same analysis as above but with 10000 generated samples and see
# if there is any difference (copy the code from above). Comment on the results. 
# -------------------------------------------------------------------------
x1 <- c()
x2 <- c()
for (i in 1:100){
  x1 <- c(x1, mean(rnorm(10000, mean = 10, sd = 5)))
  x2 <- c(x2, mean(rnorm(10000, mean = 10, sd = 20)))
}

h1 <- hist(x1)
h2 <- hist(x2)

# Plotting two histograms in same plot: https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r .
plot(h1, col=rgb(0,0,1,1/4), xlim=c(2,19))  # first histogram
plot(h2, col=rgb(1,0,0,1/4), xlim=c(2,19), add=T)  # second

