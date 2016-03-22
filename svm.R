#Author: Sneha Kadam
#Date: Mar 22, 2016
#Purpose: Support Vector Machines Exercise 7

rm(list = ls())
#install.packages("/scratch1/aditij/Installations/ISLR_1.0.tar.gz", repos = NULL)
#install.packages("/scratch1/aditij/Installations/e1071_1.6-7.tar.gz", repos=NULL)

#(a) (2 points) Create a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0 
#for cars with gas mileage below the median.
library(ISLR)
library(e1071)
attach(Auto)
med = median(mpg)
new.var = ifelse(mpg > med, 1, 0)
Auto$mpglevel = as.factor(new.var)

#(b) (6 points) Fit a support vector classifier to the data with various values of cost, in order to predict whether
#a car gets high or low gas mileage. Report the cross-validation errors associated with different values of this parameter. 
#Comment on your results.
set.seed(10)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bmodel =tune.out$best.model
summary (bmodel)

#(c) (6 points) Now repeat (b), this time using SVMs with radial and polynomial basis kernels, with different values of gamma 
#and degree and cost. Comment on your results.

#Radial
set.seed(10)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.1, 1, 5, 10), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bmodel =tune.out$best.model
summary (bmodel)

#polynomial
set.seed(10)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.1, 1, 5, 10), degree = c(2, 3, 4)))
summary(tune.out)
bmodel =tune.out$best.model
summary (bmodel)

#(d) (6 points) Make some plots to back up your assertions in (b) and (c).
#(See hint in book for how to create plots displaying pairs of variables.)

svm.linear = svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly = svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 10, 
    degree = 2)
svm.radial = svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 10, gamma = 0.01)
plotpairs = function(fit) {
    for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
        plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
    }
}
plotpairs(svm.linear)
plotpairs(svm.poly)
plotpairs(svm.radial)