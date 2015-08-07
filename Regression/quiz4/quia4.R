# Question 1
# Consider the space shuttle data ?shuttle in the MASS library. 
# Consider modeling the use of the autolander as the outcome (variable name use). 
# Fit a logistic regression model with autoloader (variable auto) use (labeled as "auto" 1) 
# versus not (0) as predicted by wind sign (variable wind). 
# Give the estimated odds ratio for autoloader use comparing head winds, 
# labeled as "head" in the variable headwind (numerator) to tail winds (denominator).

library(MASS)
data(shuttle)
str(shuttle)
# convert outcome to 0 = noauto, 1 = auto
shuttle$useNum <- factor(shuttle$use, levels = c("auto", "noauto"), labels = c(1, 0))
shuttle$windFactor <- factor(shuttle$wind)
logAutoLander <- glm(shuttle$useNum ~ shuttle$wind - 1, family='binomial')
summary(logAutoLander)
windhead <- logAutoLander$coef[1]
windtail <- logAutoLander$coef[2]
exp(windtail)/exp(windhead)
# shuttle$windtail 
# 0.9686888 
logAutoLander2 <- glm(shuttle$useNum ~ shuttle$wind, family='binomial')
exp(logAutoLander2$coeff)
# (Intercept) shuttle$windtail 
# 0.7777778        0.9686888 

# Question 2
# Consider the previous problem. Give the estimated odds ratio for autoloader use 
# comparing head winds (numerator) to tail winds (denominator) adjusting for 
# wind strength from the variable magn.
library(MASS)
data(shuttle)
# convert outcome to 0 = noauto, 1 = auto
shuttle$useNum <- factor(shuttle$use, levels = c("auto", "noauto"), labels = c(1, 0))
shuttle$windFactor <- factor(shuttle$wind)
shuttle$windMagnFactor <- factor(shuttle$magn)
logAutoLander <- glm(shuttle$useNum ~ shuttle$windFactor + shuttle$windMagnFactor, family='binomial')
summary(logAutoLander)
exp(logAutoLander$coeff)
# 0.969

# Question 3
# If you fit a logistic regression model to a binary variable, for example use 
# of the autolander, then fit a logistic regression model for one minus the 
# outcome (not using the autolander) what happens to the coefficients?

library(MASS)
data(shuttle)
shuttle$auto <- as.numeric(shuttle$use=="auto")
fit <- glm(auto ~ wind,  binomial,  shuttle)
fit3 <- glm(1-auto ~ wind,  binomial, shuttle)
fit$coefficients
fit3$coefficients
# The coefficients reverse their signs.

# Question 4
# Consider the insect spray data InsectSprays. 
# Fit a Poisson model using spray as a factor level. 
# Report the estimated relative rate comapring spray A 
# (numerator) to spray B (denominator).
data(InsectSprays)
?InsectSprays
str(InsectSprays)
head(InsectSprays)
fit <- glm(count ~ spray  - 1, family = "poisson", data = InsectSprays)
exp(fit$coef[1])/exp(fit$coef[2])
boxplot(count ~ spray, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE, col = "lightgray")
# 0.9457

# Question 5
# Consider a Poisson glm with an offset, t. So, for example, a model of the form 
# glm(count ~ x + offset(t), family = poisson) where x is a factor variable 
# comparing a treatment (1) to a control (0) and t is the natural log of a 
# monitoring time. What is impact of the coefficient for x if we fit the model 
# glm(count ~ x + offset(t2), family = poisson) where t2 <- log(10) + t? In 
# other words, what happens to the coefficients if we change the units of the 
# offset variable. (Note, adding log(10) on the log scale is multiplying by 10 
# on the original scale.)
data(mtcars)
fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit1)
# Answer: The coefficient estimate is unchanged


# Question 6
# Using a knot point at 0, fit a linear model that looks like a hockey 
# stick with two lines meeting at x=0. Include an intercept term, x and 
# the knot point term. What is the estimated slope of the line after 0?
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots<-c(0)
splineTerms<-sapply(knots, function(knot)(x > knot)*(x - knot)) 
xMat<-cbind(1,x,splineTerms)
linearModel <- lm(y~xMat-1)
yhat<-predict(linearModel) 
plot(x,y,frame=FALSE,pch=21,bg="lightblue",cex=2) 
lines(x,yhat,col="red",lwd=2)
linearModel$coeff[1]
linearModel$coeff[2]
# Answer: -1.024158
