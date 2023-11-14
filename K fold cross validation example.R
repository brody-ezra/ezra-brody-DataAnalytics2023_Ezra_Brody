#cv.glm function will be used to implement k-fold cv
#seed will be set to retain random samples
#will determine cv errors corresponding to polynomial fits of
#order one to ten
#K will be set to 10 (why 10?)
library(ISLR)
library(MASS)
library(boot)

help("glm.fit")
help("cv.glm")
help(rep)
set.seed(15)

cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10) $delta[1]
}
cv.error.10
#shows that MSE doesnt really decrease after quadratic,
#similar as the result of the regression cv