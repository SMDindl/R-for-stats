# Question 3 - Midterm 3
line <- "----------------------------------------"
print_line <- function() {
  cat(line, "\n")
}

sink("Midterm-3/Q3.txt")

n <- 7
pair <-	c(1,	2,	3,	4,	5,	6,	7)
diet_1 <-	c(559,	360,	524,	445,	583,	418,	538)
diet_2 <-	c(489,  422,  468,  485,  503,  525,  593)

cat("Hand calculations:\n")
x2 <- mean(diet_1)
x1 <- mean(diet_2)
cat("Mean of diet 2 (x1):", x1, "\n")
cat("Mean of diet 1 (x2):", x2, "\n")

cat("Mean difference (x1 - x2):", x1 - x2, "\n")
cat("variance of diet 2 (s1^2):", var(diet_2), "\n")
cat("variance of diet 1 (s2^2):", var(diet_1), "\n")
cat("Standard deviation of diet 2 (s1):", sd(diet_2), "\n")
cat("Standard deviation of diet 1 (s2):", sd(diet_1), "\n")


# Question 3 - A
print("Question 3 - A")
print_line()
# a) Construct and interpret an appropriate 90% confidence interval 
# for the mean difference between the diets.

# Calculate the differences
differences <- diet_1 - diet_2

# Perform a t-test for paired samples
t_test_result <- t.test(differences, conf.level = 0.90)

# Extract and print the confidence interval
conf_interval <- t_test_result$conf.int

# Personal print of CI
cat("90% Confidence Interval for the Mean Difference:\n")
cat("Lower Bound:", conf_interval[1], "\n")
cat("Upper Bound:", conf_interval[2], "\n")
cat("Mean Difference:", t_test_result$estimate, "\n")

# Print results of the t-test
print(t_test_result)

# Question 3 -  B
print("Question 3 - B")
print_line()
# b) At the level of significance 10%, perform an appropriate statistical test
# for the mean difference between the diets and state your conclusion.

# Perform a t-test for paired samples
t_test_result <- t.test(
  differences, mu = 0, alternative = "two.sided", conf.level = 0.90
)
# Extract the p-value
p_value <- t_test_result$p.value
# Print the p-value
cat("P-value:", p_value, "\n")

if (p_value < 0.10) {
  cat("Reject the null hypothesis: 
  There is a significant difference between the diets.\n"
  )
} else {
  cat("Fail to reject the null hypothesis: 
  There is no significant difference between the diets.\n"
  )
}
cat("\n")

# Question 3 - C

# c) State the assumptions needed for this test and interval to be valid.
# d) What are the consequences of violating the normality assumption? 
# What alternatives could you use?

sink()