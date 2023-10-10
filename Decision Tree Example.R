#Decision tree example
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
head(iris)
str(iris)
dim(iris)


#Creating testing and training sets
s_iris<-sample(150,100) #Chooses 100 random numbers 1-150

iris_train<-iris[s_iris,] #Uses 100 random numbers for selected rows
irs_test<-iris[-s_iris,] #Uses other 50 random numbers for selected rows
TreeModel<-rpart(Species~., iris_train, method="class")
rpart.plot(TreeModel)
