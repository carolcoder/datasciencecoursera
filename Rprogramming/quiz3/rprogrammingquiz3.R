## Quiz3

library(datasets)
data(iris)
?iris
dimnames(iris)

#Question 1
filter <- iris[iris$Species == 'virginica',]
mean(filter$Sepal.Length)

#Question 2
iris[,1:4]
apply(iris[,1:4], 1, mean)
apply(iris[,1:4], 2, mean)
apply(iris,2,mean)
apply(iris,1,mean)

library(datasets)
data(mtcars)
?mtcars
dimnames(mtcars)

#Question 3
lapply(mtcars, mean)
with(mtcars,tapply(mpg,cyl,mean))
mean(mtcars$mpg,mtcars$cyl)
tapply(mtcars$cyl,mtcars$mpg,mean)
tapply(mtcars$mpg,mtcars$cyl,mean)

#Question 4
tapply(mtcars$hp,mtcars$cyl,mean)
209.21429 - 82.63636

#Question 5
debug(ls)
ls()
