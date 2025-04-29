# Question 3
# Marijuana and Road Safety Gallup polled 635 Democrats and 372 Republicans
# on their opinion of driving while impaired by marijuana. 20% of Democrats
# and 35% of Republicans said that driving while impaired by marijuana poses
# a very serious threat, compared to alcohol, prescription painkillers, and
# prescription antidepressants.
sink("Midterm-3/Q5.txt")

line <- function() {
  cat("--------------------------------------------------", "\n")
}

democrats <- 635
republicans <- 372

democrats_very_serious <- 0.20 * democrats
republicans_very_serious <- 0.35 * republicans

democrats_not_very_serious <- democrats - democrats_very_serious
republicans_not_very_serious <- republicans - republicans_very_serious

democrats_proportion <- democrats_very_serious / democrats
republicans_proportion <- republicans_very_serious / republicans

cat("Democrats proportion: ", democrats_proportion, "\n")
cat("Republicans proportion: ", republicans_proportion, "\n")
line()



cat("Question 4 - A", "\n")
# A) Conduct a hypothesis test for evaluating whether the proportions
# of Democrats and Republicans who think that driving while impaired by
# marijuana poses a very serious threat are different.

# H0, p1 = p2
# H1, p1 != p2
# p1: proportion of Democrats who think that driving while impaired by
# marijuana poses a very serious threat
# p2: proportion of Republicans who think that driving while impaired by
# marijuana poses a very serious threat

p1 <- democrats_proportion
p2 <- republicans_proportion
p_hat <- (democrats_very_serious + republicans_very_serious) /
  (democrats + republicans)

n1 <- democrats
n2 <- republicans

cat("p1 - p2: ", p1 - p2, "\n")

nerm <- (0.20 * 635) + (0.35 * 372)
demom <- (635 + 372)
frac <- nerm / demom
print(frac)

# calulcate pooled and unpooled SE

z <- (p1 - p2) / sqrt(p_hat * (1 - p_hat) * (1 / n1 + 1 / n2))
cat("Test statistic: z = ", z, "\n")
cat("p-value: ", 2 * (1 - pnorm(abs(z))), "\n")
# calulcate pooled and unpooled SE
cat("Pooled SE: ", sqrt(p_hat * (1 - p_hat) * (1 / n1 + 1 / n2)), "\n")
cat("Unpooled SE: ", sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2), "\n")




sink()