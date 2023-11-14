#using intro to statistical learning (ISLR) package and other
install.packages("MASS")
install.packages("boot")

library(ISLR) #built with r version 4.3.2? Do I need to update?
library(MASS)
library(boot)

help(cv.glm)
help(sample)
set.seed(555)
train = sample(392,196)

#Use subset function in lm function to only use observations
#that are from the train sample
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)

#Use predict function to estimate response of all 392 obs
#use mean function to help calculate MSE of 196 obs in 
#validation set (-train is obs not in train set)
attach(Auto)
help(attach)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#MSE for linear regression is 24.72


#use poly function to estimate test error for higher order
#polynomial fits (quadratic, cubic, ...)

#quadratic
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
#MSE for quadratic regression is 18.21

#cubic
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#MSE for cubic regression is 18.34


#take different training set and perform same functions to
#determine if trend is steady throughout multiple samples

set.seed(5555)
train = sample(392,196)

#linear regression
lm.fit2.1 <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit2.1,Auto))[-train]^2)
#MSE is 24.72

lm.fit2.2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2.2,Auto))[-train]^2)
#MSE is 21.29

lm.fit2.3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit2.3,Auto))[-train]^2)
#MSE is 21.54

#MSE is worse of quad and cub, but both are still significantly
#better than linear and there is not much improvement from 
#quadratic to cubic --> favors quadratic