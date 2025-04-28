# Q4 by hand

log_file <- "HW7/Q4_calcs.txt"
sink(log_file)

x_vals <- c(10, 10, 13, 13, 18, 19, 22)
y_vals <- c(66, 66, 108, 106, 161, 166, 88)

# Sum of x

sum_x <- sum(x_vals)
print(sum_x)
print(sum_x / 7)
mean_x <- sum_x / 7


# for all x find SSxx
ssxx <- sum((x_vals - mean_x)^2)
print("SSxx")
print(ssxx)

# for all y find SSyy
mean_y <- sum(y_vals) / 7
print(mean_y)
ssyy <- sum((y_vals - mean(y_vals))^2)
print("SSyy")
print(ssyy)

# for all x and y find SSxy
ssxy <- sum((x_vals - mean_x) * (y_vals - mean_y))
print("SSxy")
print(ssxy)

# find r
r <- ssxy / sqrt(ssxx * ssyy)
print("r")
print(r)


# find b1
b1 <- ssxy / ssxx
print("b1")
print(b1)

b1 <- 5.11
ssyy <- 10101.43

# find SSE
sse <- ssyy - b1 * ssxy
print("SSE")
print(sse)

# sqrt(sse)
sse_sqrt <- sqrt(sse)
var <- sse / 5
print("var")
print(var)
print("sd")
sd <- sqrt(var)
print(sd)

print(sqrt(ssxx))

print("standard error")
se <- sd / sqrt(ssxx)
print(se)

# t value for df = 5, alpha = 0.05
t_value <- qt(0.95, 5)
print("t_value")
print(t_value) # its 2.015

print("ci")
ci_1 <- b1 - t_value * se
ci_2 <- b1 + t_value * se
print(paste(ci_1, ci_2))

t <- (b1 - 0) / se
print("t")
print(t)


sink()