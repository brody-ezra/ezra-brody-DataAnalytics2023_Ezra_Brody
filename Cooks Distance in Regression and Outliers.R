#Regression model with Cooks Distance
mtcars
head(mtcars)
str(mtcars)

model1 <- lm(mpg ~ cyl + wt, data = mtcars)
model1
help(cooks.distance)
plot(model1, pch = 18, col = "blue", which = c(4))
help(pch)
#find Cooks distance from for each observation
cooks.distance(model1)
CooksDistance <- cooks.distance(model1)
  #round to 5 decimals to make it more interpretable
round(CooksDistance, 5)
head(CooksDistance, 1)
sort(round(CooksDistance, 5))










#Outlier detection with Cooks distance
library(ISLR)
library(dplyr)
head(Hitters)
dim(Hitters)
is.na(Hitters)
HittersData <- na.omit(Hitters)
dim(HittersData)
glimpse(HittersData)
head(HittersData,1)

#regression for salary prediction based on all features
SalaryPredictModel1 <- lm(Salary ~., data = HittersData)
summary(SalaryPredictModel1)

#cooks distance
cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
  #cooks distance outliers defined as 3x larger than mean

#repeat regression without outliers
names_of_influential <- names(influential)
names_of_influential
outliers <- HittersData[names_of_influential,]
Hitters_Without_Outliers <- HittersData %>% anti_join(outliers)

SalaryPredictModel2 <-  lm(Salary ~., data = Hitters_Without_Outliers)
summary(SalaryPredictModel2)
#larger R squared than model 1