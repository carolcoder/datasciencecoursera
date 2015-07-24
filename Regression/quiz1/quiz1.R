# Question 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
sum(w * ( (x -1.077) ^2 ))
sum(w * ( (x -0.1471) ^2 ))
sum(w * ( (x -0.300) ^2 ))
sum(w * ( (x -0.0025) ^2 ))
answer <- sum(x*w)/7
answer

# Question 2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
yc <- y - mean(y)
xc <- x - mean(x)
answer <- sum(xc * yc) / sum (xc ^ 2)
answer

# Question 3
library("datasets")
y <- mtcars$mpg
x <- mtcars$wt
lma <- lm(y ~ x)
answer <- coef(lma)[2]
answer


# Question 6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
mu <- mean(x)
x1 <- x - mu
s <- sd(x1)
nomalization <- x1/s
nomalization


# Question 7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lma <- lm(y ~x)
answer <- coef(lma)[1]
