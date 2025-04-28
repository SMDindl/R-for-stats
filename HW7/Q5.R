log_file <- "HW7/Q5_output.txt"
sink(log_file)

# HW7 Q5
# Data
size <- c(1077, 1288, 842, 1210, 820, 945, 1192)
rent <- c(1075, 1270, 760, 1175, 845, 885, 1035)

# setup data reference
cat("Data Reference:\n")
print(196745.7 / 210243.7)
print(1006.43 - .9358 * 1053.43)
print(sqrt(20821.41 / 5))
# .05/2, n = 7-2 = 5
t_value <- qt(0.975, df = 5)
print("t_value")
print(t_value)

print(64.53 / sqrt(210243.7))

print("CI")
print(.9358 - t_value * 64.53 / sqrt(210243.7))
print(.9358 + t_value * 64.53 / sqrt(210243.7))

print("C ")
print(20.6302 + .9358 * 2000)

print("D")
print(20.6302 + .9358 * 950)

se_test <- 64.53 * sqrt(1 / 5 + (950 - 1053.43)^2 / 210243.7)
print(se_test)

print("CI")
print(909.64 - t_value * se_test)
print(909.64 + t_value * se_test)

# Compute reference values for manual calculations
cat("Reference Values for Manual Calculations:\n")

# Means of size and rent
mean_size <- mean(size)
mean_rent <- mean(rent)
cat("Mean of size:", mean_size, "\n")
cat("Mean of rent:", mean_rent, "\n")

# SSxx, SSyy, and SSxy
ssxx <- sum((size - mean_size)^2)
ssyy <- sum((rent - mean_rent)^2)
ssxy <- sum((size - mean_size) * (rent - mean_rent))
cat("SSxx:", ssxx, "\n")
cat("SSyy:", ssyy, "\n")
cat("SSxy:", ssxy, "\n")

# Correlation coefficient (r)
r <- ssxy / sqrt(ssxx * ssyy)
cat("Correlation coefficient (r):", r, "\n")

# Coefficient of determination (R²)
r_squared <- r^2
cat("Coefficient of determination (R²):", r_squared, "\n")

# Regression coefficients
b1 <- ssxy / ssxx  # Slope
b0 <- mean_rent - b1 * mean_size  # Intercept
cat("Slope (b1):", b1, "\n")
cat("Intercept (b0):", b0, "\n")

# SSE (Sum of Squared Errors)
predicted_rent <- b0 + b1 * size
sse <- sum((rent - predicted_rent)^2)
cat("SSE (Sum of Squared Errors):", sse, "\n")

# Residual Standard Error (RSE)
n <- length(size)
rse <- sqrt(sse / (n - 2))
cat("Residual Standard Error (RSE):", rse, "\n")

# Standard Error of the Slope (SE)
se_slope <- rse / sqrt(ssxx)
cat("Standard Error of the Slope (SE):", se_slope, "\n")

# t-value for 95% confidence interval (df = n - 2)
t_value <- qt(0.975, df = n - 2)
cat("t-value (95% confidence, df = n - 2):", t_value, "\n")

# Confidence Interval for the Slope
ci_lower <- b1 - t_value * se_slope
ci_upper <- b1 + t_value * se_slope
cat("95% Confidence Interval for the Slope:", ci_lower, "to", ci_upper, "\n")

# Summary of the model
model <- lm(rent ~ size)
cat("\nModel Summary:\n")
print(summary(model))

print_line()

# b) Find and interpret the intercept and slope in context
cat("Question 5 - B\n")
cat("Intercept and Slope Interpretation:\n")
intercept <- summary(model)$coefficients[1, 1]
slope <- summary(model)$coefficients[2, 1]
cat("Intercept (b0):", intercept, "\n")
cat("Slope (b1):", slope, "\n")
cat("Interpretation: For each additional square foot of size, 
rent increases by", slope, "dollars.\n")
cat("When the size is 0, the rent is", intercept, 
  "dollars (though this may not be meaningful in context).\n"
)
print_line()

# c) Predict rent for size = 2000 and explain why it's not a good idea
cat("Question 5 - C\n")
predicted_rent_2000 <- intercept + slope * 2000
cat("Predicted rent for size = 2000:", predicted_rent_2000, "\n")
cat("Explanation: This is an extrapolation beyond the 
range of the data (size = 820 to 1288).\n")
cat("The model may not be reliable for sizes outside 
the observed range.\n")
print_line()

# d) Calculate residual for rent = 1000 when size = 950
cat("Question 5 - D\n")
predicted_rent_950 <- intercept + slope * 950
residual <- 1000 - predicted_rent_950
cat("Predicted rent for size = 950:", predicted_rent_950, "\n")
cat("Residual (actual - predicted):", residual, "\n")
if (residual > 0) {
  cat("The model is underestimating the rent.\n")
} else {
  cat("The model is overestimating the rent.\n")
}
print_line()

# e) Construct a 95% confidence interval for the slope
cat("Question 5 - E\n")
ci_slope <- confint(model, level = 0.95)["size", ]
cat("95% Confidence Interval for the slope:", ci_slope[1], 
"to", ci_slope[2], "\n")
cat("Interpretation: We are 95% confident that the true 
increase in rent per square foot lies within this range.\n")
print_line()

# f) Compute 95% confidence interval for the mean rent when size = 950
cat("Question 5 - F\n")
new_data <- data.frame(size = 950)
mean_ci <- predict(model, newdata = new_data,
  interval = "confidence", level = 0.95
)
cat("95% Confidence Interval for the mean rent when size = 950:\n")
cat("Lower bound:", mean_ci[1, "lwr"], "\n")
cat("Upper bound:", mean_ci[1, "upr"], "\n")
cat("Interpretation: We are 95% confident that the 
average rent for apartments of size 950 lies within this range.\n")
print_line()

sink()