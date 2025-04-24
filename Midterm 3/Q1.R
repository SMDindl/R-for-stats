# Question 1

# 1. Suppose that W1 and W2 are independent random
# variables that have the following distribution:
# w 1 2 3 4 5 6
# p(w) 0.25 0.2 0.25 0.1 0.15 0.05
# Let Y be the average of W1 and 2W2. Find the following:
# • a) Find the sampling distribution of Y.


w <- 1:6
pw <- c(0.25, 0.2, 0.25, 0.1, 0.15, 0.05)

# Calcuate probabilies for each pair

w1 <- rep(w, each = length(w))
w2 <- rep(w, times = length(w))

p1 <- rep(pw, each = length(w))
p2 <- rep(pw, times = length(w))

# Calculate the average of W1 and 2W2
p <- p1 * p2
y <- (w1 + 2 * w2) / 2
# Calculate z based on the condition and include it in the table
z <- ifelse(w1 > 2 * w2, w1, 2 * w2)

# Print table of results
table <- data.frame(
                    pairs = paste("(", w1, ",", w2, ")", sep = ""),
                    prob = paste(p1, "*", p2, " = ", p, sep = ""),
                    y, z)

print(table, row.names = FALSE)

# table <- data.frame(y)
# print(table, row.names = FALSE)

table <- data.frame(z)
print(table, row.names = FALSE)