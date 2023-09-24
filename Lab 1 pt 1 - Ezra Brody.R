library(XLS)
installed.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
EPI_data<-read.xls("2010EPI_data.xls")
EPI_data2<-read.csv("2010EPI_data.csv", skip = 1)

summary(EPI_data2)
head(EPI_data2)
View(EPI_data2)
attach(EPI_data2)
help(attach)

#to change column names and not have an extra row
names(EPI_data2)<-as.matrix(EPI_data2[1,])
EPI_data2<-EPI_data2[-1,]
EPI_data2[]<-lapply(EPI_data2,function(x) type.convert(as.character(x)))
head(EPI_data2)

tf<-is.na(EPI)
E<-EPI[!tf]

shapiro.test(EPI_data2$EPI) #High p
shapiro.test(EPI_data2$DALY) #low p


#Stem plot + histograms
summary(EPI_data2$EPI)
help(fivenum)
fivenum(EPI_data2$EPI, na.rm=TRUE)
stem(EPI_data2$EPI)
hist(EPI_data2$EPI)
hist(EPI_data2$EPI, seq(30., 95., 1.), prob=TRUE) #use 30 and 95 as max and min (no EPI values outside that range)
help(seq)
lines(density(EPI_data2$EPI,na.rm=TRUE,bw=1.))
lines(density(EPI_data2$EPI,na.rm=TRUE,bw="SJ"))
rug(EPI_data2$EPI)
help(rug)

#Ex 1: fitting distrobutions beyond histograms

#Cumulative density
plot(ecdf(EPI_data2$EPI), do.points=FALSE, verticals=TRUE)

#Quantile-quantile
par(pty="s")
qqnorm(EPI_data2$EPI);qqline(EPI_data2$EPI)
x<-seq(30,95,1)
x
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

#looking at sulfur dioxide levels and air pollution
boxplot(EPI_data2$EPI,EPI_data2$SO2_raw,xlab="EPI and Raw SO2")
qqplot(EPI_data2$EPI,EPI_data2$SO2_raw)

boxplot(EPI_data2$EPI,EPI_data2$AIR_H,xlab="EPI and Air Pollution")
qqplot(EPI_data2$EPI,EPI_data2$AIR_H, xlab="a", ylab="b")

#looking at wider variety of measurements
boxplot(EPI_data2$EPI,EPI_data2$ENVHEALTH, EPI_data2$DALY,EPI_data2$AIR_H, EPI_data2$AIR_E, EPI_data2$WATER_H, EPI_data2$WATER_E, EPI_data2$BIODIVERSITY,xlab="EPI, ENV Health, DALY, AIR_H, AIR_E, WATER_H, WATER_E, BIODIVERSITY")
qqplot(EPI_data2$EPI,EPI_data2$ENVHEALTH)
qqplot(EPI_data2$EPI,EPI_data2$DALY)
qqplot(EPI_data2$EPI,EPI_data2$AIR_H)
qqplot(EPI_data2$EPI,EPI_data2$AIR_E)
qqplot(EPI_data2$EPI,EPI_data2$WATER_H)
qqplot(EPI_data2$EPI,EPI_data2$WATER_E)
qqplot(EPI_data2$EPI,EPI_data2$BIODIVERSITY)

#2016 dataset
EPI_2016<-read.csv("2016EPI_Raw_Data.csv")
EPI_2016$NO2.2011
EPI_data2$NOX_raw
qqplot(EPI_data2$NOX_raw,EPI_2016$NO2.1997)

low_NO2<-EPI_2016 %>% filter(NO2.2000 > -0.1)
qqplot(EPI_data2$NOX_raw,low_NO2$NO2.1997)
qqplot(EPI_data2$NOX_raw,low_NO2$NO2.2011)
#2016 file is just NO2 data from 1997-2011? None of the other 2016 files in box seem to be like the original EPI dataset


#Ex 2: filtering populations
EPILand<-EPI[!Landlock]
Eland<-EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland,seq(30.,95.,1.0),prob=TRUE)

plot(ecdf(ELand),do.points=FALSE,verticals=TRUE) #cant read Eland as data source?
qqnorm(Eland);qqline(Eland)

EPIEurope<-filter(EPI_data2,EPI_regions=='Europe')
EPIEurope$EPI
summary(EPIEurope$EPI)

EPIEastAsia<-filter(EPI_data2,EPI_regions=='East Asia and the Pacific')
qqplot(EPIEurope$EPI,EPIEastAsia$EPI)
summary(EPIEastAsia$EPI)
summary(EPIEurope$EPI)


#new data set
grump<-read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")
#trying to filter out all non-data (words at the bottom)
grump1<-filter(grump,Continent==1)
grump2<-filter(grump,Continent==2)
grump3<-filter(grump,Continent==3)
grump4<-filter(grump,Continent==4)
grump5<-filter(grump,Continent==5)
grump6<-filter(grump,Continent==6)
grump_dat<-rbind(grump1,grump2,grump3,grump4,grump5,grump6) #there is definitely a better way to do this
head(grump_dat)
Mean_pop_NA<-is.na(grump_dat$Mean.Extent..pop.)
Mean_pop<-grump_dat$Mean.Extent..pop.[!Mean_pop_NA]
Mean_land_NA<-is.na(grump_dat$Mean.Extent..sq.km.)
Mean_land<-grump_dat$Mean.Extent..sq.km.[!Mean_land_NA]
qqplot(Mean_land,Mean_pop)#why wont this work with NAs filtered out? does it have to be from data frame where row numbers are required?
class(Mean_land) #why is this character classification? all numbers?
Mean_land_NA

water<-read.csv("water-treatment.csv")
head(water)
water2<-water[water$DBO.E!="?"] #how to remove "?" since they are not "NA"
qqplot(water$PH.E,water$PH.D)
hist(water$PH.E)
hist(water$PH.D)
qqplot(water$COND.E,water$COND.D)
