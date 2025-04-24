# Question 4
# First column (est.) bo-b1

x <- c(10, 10, 13, 13, 18, 19, 22)
y <- c(66, 66, 108, 106, 161, 166, 88)

### Simple Linear Regression ####
print(summary(lm(y ~ x)))

# Residuals:
#       1       2       3       4       5       6       7
# -17.146 -17.146   9.513   7.513  36.945  36.831 -56.510

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   32.010     49.568   0.646    0.547

# Residual standard error: 36.47 on 5 degrees of freedom
# Multiple R-squared:  0.3417,    Adjusted R-squared:   0.21
# F-statistic: 2.595 on 1 and 5 DF,  p-value: 0.1681



### Confidence Interval ####

print(confint(lm(y ~ x), level = 0.9))

#                    5 %      95 %
# (Intercept) -67.871301 131.89078
# x            -1.282465  11.50974