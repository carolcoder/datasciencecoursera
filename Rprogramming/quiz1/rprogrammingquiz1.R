datafile <- read.csv("C:\\Users\\sao-carolinamo\\Documents\\Carolina\\R\\coursera\\Rprogramming\\quiz1\\hw1_data.csv")
datamatrix <- data.matrix(datafile)
ozone <- datamatrix[,1]
na_tot <- is.na(ozone)
#Question 16
ozone[na_tot]
#Question 17
mean(ozone[!na_tot])
#Question 18
subset1 <- datafile$Ozone > 31
subset2 <- datafile$Temp > 90
subset1 <- datamatrix[,1] > 31
matrixsub1 = datamatrix[subset1,]
subset2 <- matrixsub1[,4] > 90
matrixsub2 <- matrixsub1[subset2,]
good <- complete.cases(matrixsub2)
matrixsub3 <- matrixsub2[good,]
mean(matrixsub3[,2])
#Question 19
subset3 <- datamatrix[,5] == 6
matrixsub4 <- datamatrix[subset3,]
mean(matrixsub4[,4])
#Question 20
subset4 <- datamatrix[,5] == 5
matrixsub5 <- datamatrix[subset4,]
good <- complete.cases(matrixsub5)
matrixsub6 <- matrixsub5[good,]
max(matrixsub6[,1])
