# ANOVA Homework - Steven Dindl 

# Question 3 B
print("QUESTION 3")
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
print(model)

# Question 5
print("QUESTION 5")
brand_1 <- c(251.2, 245.1, 248.0, 251.1, 260.5, 250.0, 253.9, 244.6)
brand_2 <- c(263.2, 262.9, 265.0, 254.5, 264.3, 257.0, 262.8, 264.4)
brand_3 <- c(269.7, 263.2, 277.5, 267.4, 270.5, 265.5, 270.7, 272.9)

brand_distances <- c(brand_1, brand_2, brand_3)

brands <- c(rep(1, length(brand_1)), 
            rep(2, length(brand_2)), 
            rep(3, length(brand_3)), 
            rep(4, length(brand_4)))

distance <- factor(distance)

print(distance)

model <- aov(brand ~ brand)
print(model)


# Question 6
print("QUESTION 6")
gpa <- c(3.2, 4.0, 2.8, 
        3.4, 3.1, 3.3, 3.2, 3.5, 
        3.6, 3.4, 3.5, 3.3, 3.4, 
        3.7, 3.6, 3.8, 3.9, 3.5)

al <- c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4) # Academic Level
al <- factor(al)

print(al)

model <- aov(gpa ~ al)
print(model)
