# Define Y values
y_values <- c(1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5,
              5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9)

# Corresponding probabilities
p_y <- c(0.0625, 0.05, 0.1125, 0.065, 0.15, 0.0825,
         0.1175, 0.055, 0.1, 0.0525, 0.065, 0.03,
         0.035, 0.0125, 0.0075, 0.0025)

# Compute cumulative probability that Y <= 8
prob_y_leq <- sum(p_y[y_values <= 9])
print(prob_y_leq)