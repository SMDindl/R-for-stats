# Question 3 - Show

nutr_1 <- c(22, 20, 21, 18, 16, 14)
nutr_2 <- c(2, 14, 15, 10, 9, 6)
nutr_3 <- c(7, 9, 7, 6, 5, 3)

starch_content <- c(nutr_1, nutr_2, nutr_3)

print(starch_content)

nutrients <- c(rep(1, length(nutr_1)),
               rep(2, length(nutr_2)),
               rep(3, length(nutr_3)))

nutrients <- factor(nutrients)

print(nutrients)

model <- aov(starch_content ~ nutrients)

print(summary(model))

level_of_significance <- 0.10

if (summary(model)[[1]][["Pr(>F)"]][1] < level_of_significance) {
  print("Significant at 10% level. Reject the null hypothesis")
  # Tukey's HSD test
  print("Tukey's HSD test:")
  tukey_result <- TukeyHSD(model, conf.level = 0.90)
  print(tukey_result)
  plot(TukeyHSD(model, conf.level = 0.90), las = 1, col = "red")
} else {
  print("Not significant at 10% level. Fail to reject the null hypothesis")
}

# $nutrients
#          diff        lwr       upr     p adj
# 2-1  -9.166667 -13.700439 -4.632894 0.0011845
# 3-1 -12.333333 -16.867106 -7.799561 0.0000636
# 3-2  -3.166667  -7.700439  1.367106 0.2965959