# Question 6

gpa <- c(3.2, 4.0, 2.8,
         3.4, 3.1, 3.3, 3.2, 3.5,
         3.6, 3.4, 3.5, 3.3, 3.4,
         3.7, 3.6, 3.8, 3.9, 3.5)

al <- c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4) # Academic Level
al <- factor(al)

print(al)

model <- aov(gpa ~ al)
print(summary(model))


#             Df Sum Sq Mean Sq F value Pr(>F)
# al           3 0.4658 0.15526   2. 177  0.136
# Residuals   14 0.9987 0.07133


# Gpas
fr <- c(3.2, 4.0, 2.8)
fr_average <- mean(fr)
print(paste("Freshman Average:", fr_average))
# Variance
fr_variance <- var(fr)
print(paste("Freshman Variance:", fr_variance))

so <- c(3.4, 3.1, 3.3, 3.2, 3.5)
so_average <- mean(so)
print(paste("Sophomore Average:", so_average))
# Variance
so_variance <- var(so)
print(paste("Sophomore Variance:", so_variance))

jr <- c(3.6, 3.4, 3.5, 3.3, 3.4)
jr_average <- mean(jr)
print(paste("Junior Average:", jr_average))
# Variance
jr_variance <- var(jr)
print(paste("Junior Variance:", jr_variance))

sr <- c(3.7, 3.6, 3.8, 3.9, 3.5)
sr_average <- mean(sr)
print(paste("Senior Average:", sr_average))
# Variance
sr_variance <- var(sr)
print(paste("Senior Variance:", sr_variance))

gpa <- c(fr, so, jr, sr)
total_mean <- mean(gpa)
print(paste("Total Mean:", total_mean))

sst <- 3 * (fr_average - total_mean)^2 + 5 * (so_average - total_mean)^2 +
  5 * (jr_average - total_mean)^2 + 5 * (sr_average - total_mean)^2
print(paste("SST:", sst))

sse <- 17 * (fr_variance^2 + so_variance^2 + jr_variance^2 + sr_variance^2)
print(paste("SSE:", sse))

ssto <- sse + sst
print(paste("SSTO:", ssto))

mst <- sst / 3
print(paste("MST:", mst))

mse <- sse / 14
print(paste("MSE:", mse))

f <- mst / mse
print(paste("F:", f))
