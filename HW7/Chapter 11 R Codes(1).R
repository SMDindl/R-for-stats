##### Correlation and Simple Linear Regression R Codes ############


############## A Simple illustrating Example in Lecture Notes ###############

x <- c(2.78, 3.82, 3.16, 3.53, 2.96, 3.37)
y <- c(2.95, 3.64, 3.55, 3.89, 3.06, 3.31)


### Scatterplot #### 

plot(x, y, main = "Scatterplot of High vs College GPAs")
abline(lm(y~x), col = "darkgreen")



### Correlation Coefficient (r) #### 

cor(x, y)

cor.test(x, y)## Gives you more information 



### Simple Linear Regression #### 

summary(lm(y~x)) 



### Confidence Interval #### 

confint(lm(y~x),level=0.95)



### Confidence Interval for the Population Mean #####

predict(lm(y~x),data.frame(x=3.20), level=0.95, interval="confidence")



### Confidence Interval for the Population Mean #####

predict(lm(y~x),data.frame(x=3.20), level=0.95, interval="prediction")

predict(lm(y~x),data.frame(x=1.85), level=0.95, interval="prediction")