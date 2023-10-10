#K-means example using preloaded iris dataset
library(ggplot2)
head(iris)
str(iris)
summary(iris)

help(sapply)
sapply(iris[,-5],var)
summary(iris)

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,col=Species))+
  geom_point()
ggplot(iris,aes(x=Petal.Length,y=Sepal.Width,col=Species))+
  geom_point()
ggplot(iris,aes(x=Petal.Length,y=Petal.Width,col=Species))+
  geom_point()

set.seed(300)
k.max<- 12

# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen
wss<-sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart=20,iter.max = 20)$tot.withinss})
#wss is within sum squares
plot(1:k.max,wss,type="b",xlab="Number of clusters (k)",ylab="Within cluster sum of squares")
icluster<-kmeans(iris[,3:4],3,nstart=20)
icluster
table(icluster$cluster,iris$Species)
table(iris[,5],icluster$cluster)
