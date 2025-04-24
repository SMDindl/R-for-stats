# Question 5 - Show

driver_brand_1 <- c(251.2, 245.1, 248.0, 251.1, 260.5, 250.0, 253.9, 244.6)
driver_brand_2 <- c(263.2, 262.9, 265.0, 254.5, 264.3, 257.0, 262.8, 264.4)
driver_brand_3 <- c(269.7, 263.2, 277.5, 267.4, 270.5, 265.5, 270.7, 272.9)

brand_distances <- c(driver_brand_1, driver_brand_2, driver_brand_3)

brands <- c(rep(1, length(driver_brand_1)),
            rep(2, length(driver_brand_2)),
            rep(3, length(driver_brand_3)))

brands <- factor(brands)
print(brands)

model <- aov(brand_distances ~ brands)

print(summary(model))

level_of_significance <- 0.05

if (summary(model)[[1]][["Pr(>F)"]][1] < level_of_significance) {
  print("Significant at 5% level. Reject the null hypothesis")
  # Tukey's HSD test
  print("Tukey's HSD test:")
  tukey_result <- TukeyHSD(model, conf.level = 0.95)
  print(tukey_result)
  plot(TukeyHSD(model, conf.level = 0.95), las = 1, col = "red")
  # Bonferroni
  print("Bonferroni test:")
  print(pairwise.t.test(brand_distances, factor(brands), p.adj = "bonf"))
} else {
  print("Not significant at 5% level. Fail to reject the null hypothesis")
}