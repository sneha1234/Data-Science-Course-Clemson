#Author: Sneha Kadam
#Date: Mar 16, 2016
#Purpose: Support Vector Machines Exercise 7

rm(list = ls())

install.packages("/scratch1/aditij/Installations/ISLR_1.0.tar.gz", repos = NULL)
install.packages("/scratch1/aditij/Installations/e1071_1.6-7.tar.gz", repos=NULL)
#Exercise 7, In this problem, you will use support vector approaches in order to predict whether a given car gets 
#high or low gas mileage based on the Auto data set.

#(a) (2 points) Create a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0 
#for cars with gas mileage below the median.
library(ISLR)
library(e1071)
attach(Auto)
mpg_median<-median(mpg)
mpg_median

#The binary Var
isMileageAboveMedian <- as.numeric(mpg>mpg_median)

#___________________________________


#(b) (6 points) Fit a support vector classifier to the data with various values of cost, in order to predict whether
#a car gets high or low gas mileage. Report the cross-validation errors associated with different values of this parameter. 
#Comment on your results.
dat=data.frame(Auto, isMileageAboveMedian = as.factor(mpg>mpg_median))
set.seed (1)
tune.out=tune(svm ,isMileageAboveMedian~.,data=dat ,kernel ="linear",  ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))

summary (tune.out)
bestmod =tune.out$best.model
summary (bestmod)

#___________________________________
#(c) (6 points) Now repeat (b), this time using SVMs with radial and polynomial basis kernels, with different values of gamma 
#and degree and cost. Comment on your results.

library(e1071)
#polynomial
set.seed(1)
tune.out = tune(svm, isMileageAboveMedian~ ., data = dat, kernel = "polynomial", ranges = list(cost = c(0.1,1, 5, 10), degree = c(2, 3, 4)))
summary(tune.out)
bestmod =tune.out$best.model
summary (bestmod)

#radial
set.seed (1)
tune.out=tune(svm , isMileageAboveMedian~., data=dat, kernel ="radial", ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.5,1,2,3,4) ))
summary (tune.out)

bestmod =tune.out$best.model
summary (bestmod)


#(d) (6 points) Make some plots to back up your assertions in (b) and (c).
#(See hint in book for how to create plots displaying pairs of variables.)
rm(list = ls())
set.seed (6)

library(ISLR)
dat <- Auto

dat$isMileageAboveMedian <- as.factor(dat$mpg > median(dat$mpg))

library(e1071)


for(param_cost in c(0.001 , 0.01, 0.1, 1,5,10,100))
{
  svm.linear = svm(isMileageAboveMedian~., data=dat, kernel="linear", cost=param_cost,decision.values =T)
  plot(svm.linear, dat, mpg ~ cylinders)
}

for(param_cost in c(0.001 , 0.01, 0.1, 1,5,10,100))
{
  svm.linear = svm(isMileageAboveMedian~., data=dat, kernel="linear", cost=100,decision.values =T)
  plot(svm.linear, dat, mpg ~ displacement)
}

for(param_cost in c(0.001 , 0.01, 0.1, 1,5,10,100))
{
  svm.linear = svm(isMileageAboveMedian~., data=dat, kernel="linear", cost=param_cost,decision.values =T)
  plot(svm.linear, dat, mpg ~ horsepower)
}

for(param_cost in c(0.001 , 0.01, 0.1, 1,5,10,100))
{
  svm.linear = svm(isMileageAboveMedian~., data=dat, kernel="linear", cost=param_cost,decision.values =T)
  plot(svm.linear, dat, mpg ~ weight)
}

for(param_cost in c(0.001 , 0.01, 0.1, 1,5,10,100))
{
  svm.linear = svm(isMileageAboveMedian~., data=dat, kernel="linear", cost=param_cost,decision.values =T)
  plot(svm.linear, dat, mpg ~ acceleration)
}

for(param_cost in c(0.001 , 0.01, 0.1, 1,5,10,100))
{
  svm.linear = svm(isMileageAboveMedian~., data=dat, kernel="linear", cost=param_cost,decision.values =T)
  plot(svm.linear, dat, mpg ~ year)
}

for(param_cost in c(0.001 , 0.01, 0.1, 1,5,10,100))
{
  
  svm.linear = svm(isMileageAboveMedian~., data=dat, kernel="linear", cost=param_cost,decision.values =T)
  plot(svm.linear, dat, mpg ~ origin)
}

for(param_cost in c(0.1,1, 5, 10))
{
  for(param_degree in c(2, 3, 4))
  {
    svm.poly = svm(isMileageAboveMedian~., data=dat, kernel="polynomial", cost=param_cost, degree=param_degree)
    plot(svm.poly, dat, mpg ~ cylinders)
   }
}

for(param_cost in c(0.1,1, 5, 10))
{
  for(param_degree in c(2, 3, 4))
  {    
    svm.poly = svm(isMileageAboveMedian~., data=dat, kernel="polynomial", cost=10, degree=2)
    plot(svm.poly, dat, mpg ~ displacement)
  }
}

for(param_cost in c(0.1,1, 5, 10))
{
  for(param_degree in c(2, 3, 4))
  {
    svm.poly = svm(isMileageAboveMedian~., data=dat, kernel="polynomial", cost=param_cost, degree=param_degree)
    plot(svm.poly, dat, mpg ~ horsepower)
  }
}

for(param_cost in c(0.1,1, 5, 10))
{
  for(param_degree in c(2, 3, 4))
  {
    svm.poly = svm(isMileageAboveMedian~., data=dat, kernel="polynomial", cost=param_cost, degree=param_degree)
    plot(svm.poly, dat, mpg ~ weight)
  }
}

for(param_cost in c(0.1,1, 5, 10))
{
  for(param_degree in c(2, 3, 4))
  {
    svm.poly = svm(isMileageAboveMedian~., data=dat, kernel="polynomial", cost=param_cost, degree=param_degree)
    plot(svm.poly, dat, mpg ~ acceleration)
  }
}

for(param_cost in c(0.1,1, 5, 10))
{
  for(param_degree in c(2, 3, 4))
  {
    svm.poly = svm(isMileageAboveMedian~., data=dat, kernel="polynomial", cost=param_cost, degree=param_degree)
    plot(svm.poly, dat, mpg ~ year)
  }
}

for(param_cost in c(0.1,1, 5, 10))
{
  for(param_degree in c(2, 3, 4))
  {
    svm.poly = svm(isMileageAboveMedian~., data=dat, kernel="polynomial", cost=param_cost, degree=param_degree)
    plot(svm.poly, dat, mpg ~ origin)
  }
}


for(param_cost in c(0.1 ,1 ,10 ,100 ,1000))
{
  for(param_gamma in c(0.5,1,2,3,4))
  {
    svm.radial = svm(isMileageAboveMedian~., data=dat, kernel="radial", cost=param_cost, gamma=param_gamma)
    plot(svm.radial, dat, mpg ~ cylinders)
  }
}


for(param_cost in c(0.1 ,1 ,10 ,100 ,1000))
{
  for(param_gamma in c(0.5,1,2,3,4))
  {
    svm.radial = svm(isMileageAboveMedian~., data=dat, kernel="radial", cost=1000, gamma=4)
    plot(svm.radial, dat, mpg ~ displacement)
  }
}


for(param_cost in c(0.1 ,1 ,10 ,100 ,1000))
{
  for(param_gamma in c(0.5,1,2,3,4))
  {
    svm.radial = svm(isMileageAboveMedian~., data=dat, kernel="radial", cost=param_cost, gamma=param_gamma)
    plot(svm.radial, dat, mpg ~ horsepower)
  }
}


for(param_cost in c(0.1 ,1 ,10 ,100 ,1000))
{
  for(param_gamma in c(0.5,1,2,3,4))
  {
    svm.radial = svm(isMileageAboveMedian~., data=dat, kernel="radial", cost=param_cost, gamma=param_gamma)
    plot(svm.radial, dat, mpg ~ weight)
  }
}


for(param_cost in c(0.1 ,1 ,10 ,100 ,1000))
{
  for(param_gamma in c(0.5,1,2,3,4))
  {
    svm.radial = svm(isMileageAboveMedian~., data=dat, kernel="radial", cost=param_cost, gamma=param_gamma)
    plot(svm.radial, dat, mpg ~ acceleration)
  }
}


for(param_cost in c(0.1 ,1 ,10 ,100 ,1000))
{
  for(param_gamma in c(0.5,1,2,3,4))
  {
    svm.radial = svm(isMileageAboveMedian~., data=dat, kernel="radial", cost=param_cost, gamma=param_gamma)
    plot(svm.radial, dat, mpg ~ year)
  }
}


for(param_cost in c(0.1 ,1 ,10 ,100 ,1000))
{
  for(param_gamma in c(0.5,1,2,3,4))
  {
    svm.radial = svm(isMileageAboveMedian~., data=dat, kernel="radial", cost=param_cost, gamma=param_gamma)
    plot(svm.radial, dat, mpg ~ origin)
  }
}



