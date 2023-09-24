install.packages(ggplot2)
library(ggplot2)
library(dplyr)

multivariate<-read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)

mm<-lm(Homeowners~Immigrant)
summary(mm)$coef
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
newImmigrantdata<-data.frame(immigrant=c(0,20))
mm%>%predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3)
mm$coefficients


#Cars and ggplot figures
cars<-mtcars
plot(cars$wt,cars$mpg)
qplot(cars$wt,cars$mpg)
qplot(wt,mpg,data=cars)
ggplot(cars, aes(x=wt,y=mpg))+
  geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
help(type)
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")
qplot(pressure$temperature,pressure$pressure, geom="line")
qplot(temperature,pressure,data=pressure, geom="line")
ggplot(pressure, aes(x=temperature,y=pressure))+
  geom_point()+
  geom_line()

barplot(BOD$demand, names.arg=BOD$Time)
table(cars$cyl)
barplot(table(cars$cyl))
qplot(cars$cyl)
qplot(factor(cyl),data=cars)
ggplot(cars,aes(x=factor(cyl)))+
  geom_bar()

hist(cars$mpg)
hist(cars$mps,breaks=10)
hist(cars$mps,breaks=5)
hist(cars$mps,breaks=10)
qplot(mpg,data=cars,binwidth=4)
ggplot(cars,aes(x=mpg))+
  geom_histogram(binwidth=4)
ggplot(cars,aes(x=mpg))+
  geom_histogram(binwidth=5)

plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp,data=ToothGrowth)
boxplot(len~supp + dose,data=ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len,geom="boxplot")
qplot(supp,len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+
  geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom="boxplot")
qplot(interaction(supp,dose),len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+
  geom_boxplot()

head(ToothGrowth)
head(mtcars)

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point()+
  geom_smooth()
