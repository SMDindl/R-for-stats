# Question 4

sink("Midterm-3/Q4.txt")

gpa <- c(3.2, 4.0, 2.8, 3.4, 3.1, 3.3, 3.2, 3.5,
  3.6, 3.4, 3.5, 3.3, 3.4, 3.7, 3.6, 3.8, 3.9
)

al <- c("Fr", "Fr", "Fr", "So", "So", "So", "So", "So",
  "Jr", "Jr", "Jr", "Jr", "Jr", "Sr", "Sr", "Sr", "Sr"
)

al <- factor(al)

level_of_significance <- 0.10
conf_level <- 1 - level_of_significance

num_df <- length(levels(al)) - 1  # Numerator degrees of freedom
den_df <- length(gpa) - length(levels(al))  # Denominator degrees of freedom
critical_value <- qf(1 - level_of_significance, num_df, den_df)


# Question 4 - A
# Use ANOVA to determine if there is a significant difference in GPA among
# the different academic levels at 10% level of significance. In order to get
# full credits, clearly state your hypothesis, test statistic, and conclusion
# H0: There is no difference in GPA among the different academic levels
# H1: There is a difference in GPA among the different academic levels



# Test statistic
model <- aov(gpa ~ al)
print(summary(model))

cat("Test statistic: F = ", summary(model)[[1]][["F value"]][1], "\n")
cat("p-value: ", summary(model)[[1]][["Pr(>F)"]][1], "\n")
cat("Level of significance: ", level_of_significance, "\n")
cat("Critical value: ", critical_value, "\n")


# Conclusion
if (summary(model)[[1]][["Pr(>F)"]][1] < level_of_significance) {
  print("Significant at 10% level. Reject the null hypothesis")
  # Tukey's HSD test
  print("Tukey's HSD test:")
  tukey_result <- TukeyHSD(model, conf.level = conf_level)
  cat(tukey_result, "\n")
  plot(TukeyHSD(model, conf.level = conf_level), las = 1, col = "red")
  # Bonferroni
  print("Bonferroni test:")
  cat(pairwise.t.test(gpa, al, p.adj = "bonf"), "\n")
} else {
  cat("Not significant at 10% level. Fail to reject the null hypothesis", "\n")
}


sink()