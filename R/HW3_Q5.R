# HW 3 Question 5

# Data
data <- c(29.6, 30.1, 24.7, 21.5, 33.4, 29.8, 41.5, 32.9, 33.7, 34.6, 44.9, 37.8)


# A) Construct and interpret a 90% confidence interval for population mean.
print("(A) Construct and interpret a 90% confidence interval for population mean.")

#CI
n <- length(data)
mean <- mean(data)
sd <- sd(data)

alpha_90 <- 0.10
t_90 <- qt(1 - alpha_90/2, df = n-1)

error_90 <- t_90 * (sd / sqrt(n))
CI_90 <- c(mean - error_90, mean + error_90)

# CI interptation and interval
print("We are 90% confident that the true population mean of thyme weights lie within this interval.")
print(CI_90)


# B) Construct and interpret a 99% confidence interval for population mean.
# Some vars such as mean and sd are from prev problem
print("(B) Construct and interpret a 99% confidence interval for population mean.")

# CI
alpha_99 <- 0.01
t_99 <- qt(1 - alpha_99/2, df = n-1)

error_99 <- t_99 * (sd / sqrt(n))
CI_99 <- c(mean - error_99, mean + error_99)

# CI interpretation and interval
print("We are 99% confident that the true population mean of thyme weights lie within this interval.")
print(CI_99)


# C) Assumptions necessary for this confidence interval to be valid.
print("(C) Assumptions necessary for this confidence interval to be valid:")
print("- The sample is randomly selected from the population.")
print("- Since sample size isn't large enought for clt, we assume the population follows a normal distribution.")
print("- The observations are independent of each other.")


# D) Hypothesis test: Is the mean different from 25 mg?
print("(D) Hypothesis test: Is the mean different from 25 mg?")

# Hypotheses
print("Null Hypothesis (Ho): The true mean weight is 25 mg (u = 25 mg)")
print("Alternative Hypothesis (Ha): The true mean weight is not 25 mg (u =/= 25 mg)")

# Perform t-test
t_test <- t.test(data, mu = 25, alternative = "two.sided", conf.level = 0.90)

# Extract values
t_value <- t_test$statistic
df <- t_test$parameter
p_value <- t_test$p.value

# Output test results
print(paste("Test Statistic (t-value):", round(t_value, 3)))
print(paste("Degrees of Freedom:", df))
print(paste("P-value:", round(p_value, 4)))

# Conclusion based on alpha = 0.10
if (p_value < 0.10) {
  print("Since the p-value is less than 0.10, we reject the null hypothesis.")
  print("There is enough evidence to say the true mean weight is different from 25 mg.")
} else {
  print("Since the p-value is greater than 0.10, we fail to reject the null hypothesis.")
  print("There is not enough evidence to conclude the true mean weight is different from 25 mg.")
}
