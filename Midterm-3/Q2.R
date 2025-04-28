# Question 2 - small sample size

n <- 16
prob <- 0.70

# a) Exact probability of getting 4 successes
print(dbinom(4, n, prob))

# b) Probability of getting 1 or more successes
options(digits = 15)
print(1 - pbinom(0, n, prob))

# c) Probability of getting 13 or more successes
print(1 - pbinom(12, n, prob))

# d) Find the corresponding mean and variance.
mean <- n * prob
variance <- n * prob * (1 - prob)

print(paste("mean:", mean))
print(paste("variance:", variance))

# e) What is the exact distribution? Explain your reasoning.
# The exact distribution is a binomial distribution
# with parameters n = 16 and p = 0.70.
# This is because the number of successes in a
# fixed number of independent Bernoulli trials follows
# a binomial distribution.
