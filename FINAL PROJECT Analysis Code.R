setwd("C:/Users/ezrab/Downloads/Fall 23/BCBP 4600/BCBP Final Project Data/COVID")
latest_cleaned <- read.csv("Latest extract cleaned and range.csv")
library(tidyverse)
library(lubridate)
library(ggplot2)
#remove extra column that got added somewhow?
latest_cleaned <- latest_cleaned[-c(1)]
#reorder rows by specimen date since they were messed up from
#extra column that was added
latest_cleaned <- latest_cleaned %>% arrange(specimen_date) #reorder


str(latest_cleaned)
latest_cleaned$extract_date <- as.Date(latest_cleaned$extract_date)
latest_cleaned$specimen_date <- as.Date(latest_cleaned$specimen_date)
str(latest_cleaned)

write.csv(latest_cleaned,file='Regression.csv')

#distributions of key variables
#summary statistics
summary(latest_cleaned$Number_tested)
summary(latest_cleaned$Number_confirmed)
summary(latest_cleaned$Number_hospitalized)
summary(latest_cleaned$Number_deaths)
summary(latest_cleaned$hosp_per_pos)
summary(latest_cleaned$death_per_pos)

shapiro.test(latest_cleaned$Number_tested)
shapiro.test(latest_cleaned$Number_confirmed)
shapiro.test(latest_cleaned$Number_hospitalized)
shapiro.test(latest_cleaned$Number_deaths)
shapiro.test(latest_cleaned$hosp_per_pos)
shapiro.test(latest_cleaned$death_per_pos)
#figures
ggplot(latest_cleaned, aes(x = specimen_date, y = Number_confirmed))+
  geom_point()+
  ggtitle("Number of Confirmed Cases over Time")+
  xlab("Specimen Date")+
  ylab("Number of Confirmed Cases")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))
  
ggplot(latest_cleaned, aes(x = specimen_date, y = Number_hospitalized))+
  geom_point()+
  ggtitle("Number of Hospitalizations over Time")+
  xlab("Specimen Date")+
  ylab("Number of Hospitalizatioms")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))

ggplot(latest_cleaned, aes(x = specimen_date, y = Number_deaths))+
  geom_point()+
  ggtitle("Number of Deaths over Time")+
  xlab("Specimen Date")+
  ylab("Number of Deaths")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))
  
#ecdfs
ggplot(latest_cleaned, aes(x = Number_confirmed))+
  stat_ecdf(geom = "smooth")

ggplot(latest_cleaned, aes(sample = Number_confirmed))+
  stat_qq()+
  stat_qq_line()

#HpC and DpC graphs
ggplot(latest_cleaned, aes(x=specimen_date, y=hosp_per_pos))+
  geom_point()+
  ggtitle("Hospitalizations per Confirmed Case over Time")+
  xlab("Specimen Date")+
  ylab("Hospitalizations per Confirmed Case")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))


ggplot(latest_cleaned, aes(x=specimen_date, y=death_per_pos))+
  geom_point()+
  ggtitle("Deaths per Confirmed Case over Time")+
  xlab("Specimen Date")+
  ylab("Deaths per Confirmed Case")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))





#Day of week and day type, visualization
#add column for day of the week
latest_cleaned$day <- weekdays(latest_cleaned$specimen_date)
#classify was weekend or weekday (struggling with or in ifelse,
#do the long way with filtering)
latest_cleaned$day_type <- with(latest_cleaned, ifelse(day == "Sunday" | day == "Saturday", "Weekend", "Weekday"))
#visualize to see if there are clear trends
ggplot(latest_cleaned, aes(x = specimen_date, y = Number_tested, col = day_type))+
  geom_point()+
  ggtitle("Number of Patients Tested over Time")+
  xlab("Specimen Date")+
  ylab("Number of Tests")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))

ggplot(latest_cleaned, aes(x = specimen_date, y = Number_confirmed, col = day_type))+
  geom_point()+
  ggtitle("Number of Confirmed Cases over Time")+
  xlab("Specimen Date")+
  ylab("Number of Confirmed Cases")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))
  
ggplot(latest_cleaned, aes(x = specimen_date, y = Number_hospitalized, col = day_type))+
  geom_point()+
  ggtitle("Number of Hospitalizatioms over Time")+
  xlab("Specimen Date")+
  ylab("Number of Hospitalizations")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))

ggplot(latest_cleaned, aes(x = specimen_date, y = Number_deaths, col = day_type))+
  geom_point()+
  ggtitle("Number of Deaths over Time")+
  xlab("Specimen Date")+
  ylab("Number of Deaths")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))

#table development of difference between weekend and weekday
#for each variable
weekday_cleaned <- filter(latest_cleaned, day_type == "Weekday")
weekend_cleaned <- filter(latest_cleaned, day_type == "Weekend")
#number tested
mean(weekday_cleaned$Number_tested)
mean(weekend_cleaned$Number_tested)
t.test(weekday_cleaned$Number_tested, weekend_cleaned$Number_tested)
#number confirmed
mean(weekday_cleaned$Number_confirmed)
mean(weekend_cleaned$Number_confirmed)
t.test(weekday_cleaned$Number_confirmed, weekend_cleaned$Number_confirmed)
#number hospitalized
mean(weekday_cleaned$Number_hospitalized)
mean(weekend_cleaned$Number_hospitalized)
t.test(weekday_cleaned$Number_hospitalized, weekend_cleaned$Number_hospitalized)
#number deaths
mean(weekday_cleaned$Number_deaths)
mean(weekend_cleaned$Number_deaths)
t.test(weekday_cleaned$Number_deaths, weekend_cleaned$Number_deaths)
#hospitalizations per confirmed case
mean(weekday_cleaned$hosp_per_pos)
mean(weekend_cleaned$hosp_per_pos)
t.test(weekday_cleaned$hosp_per_pos, weekend_cleaned$hosp_per_pos)
#deaths per confirmed case
mean(weekday_cleaned$death_per_pos)
mean(weekend_cleaned$death_per_pos)
t.test(weekday_cleaned$death_per_pos, weekend_cleaned$death_per_pos)





#attempt to visualization of peaks using median
#peaks will be decided from confirmed cases --> rates being looked
#at will be by confirmed cases
summary(latest_cleaned$Number_confirmed)
#median = 1470, look at graph with horizontal line here
ggplot(latest_cleaned, aes(x = specimen_date, y = Number_confirmed))+
  geom_point()+
  geom_hline(yintercept = 1470)+
  ggtitle("Number of Confirmed Cases over Time")+
  xlab("Specimen Date")+
  ylab("Number of Confirmed Cases")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))

  #looks like the median is a little low due to very low values at
  # end
  #can trim after 2021-04 as both confirmed and tests drop significantly

#trimmed to above guidelines (NOT STATISTICALLY DETERMINED)
latest_trimmed <- filter(latest_cleaned, specimen_date < "2021-04-01")
summary(latest_trimmed$Number_confirmed)
#median = 1739
ggplot(latest_trimmed, aes(x = specimen_date, y = Number_confirmed))+
  geom_point()+
  geom_hline(yintercept = 1739)+
  ggtitle("Number of Confirmed Cases over Time from Filtered Distribution")+
  xlab("Specimen Date")+
  ylab("Number of Confirmed Cases")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))


#day column to add vertical lines
latest_trimmed <- latest_trimmed %>% arrange(specimen_date) #reorder
latest_trimmed$day_since_03_11_2020 <- 1:385
ggplot(latest_trimmed, aes(x = day_since_03_11_2020, y = Number_confirmed))+
  geom_point()+
  geom_hline(yintercept = 1739)+
  geom_vline(xintercept = 5)+
  geom_vline(xintercept = 65)+
  geom_vline(xintercept = 245)+
  geom_vline(xintercept = 350)+
  ggtitle("Peak Distinction in Number of Confirmed Cases over Time")+
  xlab("Days Since 2020-03-11")+
  ylab("Number of Confirmed Cases")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))


#peak separation of data
first_peak <- filter(latest_trimmed, day_since_03_11_2020 < 66)
first_peak <- filter(first_peak, day_since_03_11_2020 > 4)
second_peak <- filter(latest_trimmed, day_since_03_11_2020 > 244)
second_peak <- filter(second_peak, day_since_03_11_2020 < 351)

first_peak$Peak <- "First"
second_peak$Peak <- "Second"                   

peaks <- rbind(first_peak, second_peak)

write.csv(peaks,file='Peaks.csv')


ggplot(peaks, aes(x = death_per_pos, y = hosp_per_pos, col = Peak))+
  geom_point()+
  ggtitle("Hospitalization and Death per Confirmed Case for Peak Data")+
  xlab("DpC")+
  ylab("HpC")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))


#comapring means (t-test) between peak rate data
mean(first_peak$hosp_per_pos)
mean(second_peak$hosp_per_pos)
t.test(first_peak$hosp_per_pos, second_peak$hosp_per_pos)

mean(first_peak$death_per_pos)
mean(second_peak$death_per_pos)
t.test(first_peak$death_per_pos, second_peak$death_per_pos)




#MODELS

#regression model for hosp_per_pos and death_per_pos over time
#look at some changing rates
ggplot(latest_cleaned, aes(x=specimen_date, y=hosp_per_pos))+
  geom_point()+
  ggtitle("Hospitalizations per Confirmed Case over Time")
#looks like some higher order regression will be best (CV)
ggplot(latest_cleaned, aes(x=specimen_date, y=death_per_pos))+
  geom_point()+
  ggtitle("Deaths per Confirmed Case over Time")
  #looks like some higher order regression will be best (CV)

#linear regression model application
#add day column
latest_cleaned$days_since_2020_03_11 <- 1:565
#hosp per pos
hpp_lin_reg <- lm(hosp_per_pos ~ days_since_2020_03_11, data = latest_cleaned)
summary(hpp_lin_reg)
#MSE
hpp_lin_reg_summ <- summary(hpp_lin_reg)
mean(hpp_lin_reg_summ$residuals^2)
#R2 = 0.2332m , MSE = 0.0024

#death per pos
dpp_lin_reg <- lm(death_per_pos ~ days_since_2020_03_11, data = latest_cleaned)
summary(dpp_lin_reg)
#MSE
dpp_lin_reg_summ <- summary(dpp_lin_reg)
mean(dpp_lin_reg_summ$residuals^2)
#R2 = 0.3995 , MSE = 0.00048

#CV k-fold for hpp
library(MASS)
library(boot)
set.seed(1331)
help(cv.glm)
cv.error.10.hpp = rep(0,10)
for(i in 1:10){
  glm.fit.hpp = glm(hosp_per_pos ~ poly(days_since_2020_03_11, i), data = latest_cleaned)
  cv.error.10.hpp[i] = cv.glm(latest_cleaned,glm.fit.hpp, K=10) $delta[1]
}
cv.error.10.hpp
cv_error_hpp <- data.frame(cv.error.10.hpp)
cv_error_hpp$Order <- 1:10
ggplot(cv_error_hpp, aes(x=Order, y=cv.error.10.hpp))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(breaks=c(0.0005,0.001,0.0015,0.002,0.0025))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("MSE from K-fold CV of Polynomial Orders for HpC Regression")+
  xlab("Polynomial Order")+
  ylab("MSE")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))

help(theme)
#appears that MSE doesnt decrease much beyond cubic
#decrease after cubic is small and slow


#CV k-fold for dpp
cv.error.10.dpp = rep(0,10)
for(i in 1:10){
  glm.fit.dpp = glm(death_per_pos ~ poly(days_since_2020_03_11, i), data = latest_cleaned)
  cv.error.10.dpp[i] = cv.glm(latest_cleaned,glm.fit.dpp, K=10) $delta[1]
}
cv.error.10.dpp
cv_error_dpp <- data.frame(cv.error.10.dpp)
cv_error_dpp$Order <- 1:10
ggplot(cv_error_dpp, aes(x=Order, y=cv.error.10.dpp))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("MSE from K-fold CV of Polynomial Orders for DpC Regression")+
  xlab("Polynomial Order")+
  ylab("MSE")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))

#appears that MSE doesnt decrease much beyond cubic
#decrease after cubic is small and slow

#hpp higher order regressions
hpp_lin_reg <- lm(hosp_per_pos ~ days_since_2020_03_11, data = latest_cleaned)
summary(hpp_lin_reg) #r2 0.2332
#mse
hpp_lin_reg_summ <- summary(hpp_lin_reg)
mean(hpp_lin_reg_summ$residuals^2)
  #0.002354321
hpp_reg2<- lm(hosp_per_pos ~ poly(days_since_2020_03_11, 2), data = latest_cleaned)
summary(hpp_reg2) #r2 0.4486
#mse
hpp_reg_summ_2 <- summary(hpp_reg2)
mean(hpp_reg_summ_2$residuals^2)
  #0.001692921
hpp_reg3 <- lm(hosp_per_pos ~ poly(days_since_2020_03_11, 3), data = latest_cleaned)
summary(hpp_reg3) #r2 0.7199
#mse
hpp_reg_summ_3 <- summary(hpp_reg3)
mean(hpp_reg_summ_3$residuals^2)
 #0.0008599778
library(ggplot)
ggplot(latest_cleaned, aes(x=days_since_2020_03_11, y=hosp_per_pos))+
  geom_point()+
  stat_smooth(se=F, method="lm", formula = y~poly(x,3))

#dpp higher order regressions
dpp_lin_reg <- lm(death_per_pos ~ days_since_2020_03_11, data = latest_cleaned)
summary(dpp_lin_reg)
#MSE
dpp_lin_reg_summ <- summary(dpp_lin_reg)
mean(dpp_lin_reg_summ$residuals^2)
  #0.0004835646
dpp_reg2 <- lm(death_per_pos ~ poly(days_since_2020_03_11, 2), data = latest_cleaned)
summary(dpp_reg2)
#MSE
dpp_reg_summ_2 <- summary(dpp_reg2)
mean(dpp_reg_summ_2$residuals^2)
  #0.0003463118
dpp_reg3 <- lm(death_per_pos ~ poly(days_since_2020_03_11, 3), data = latest_cleaned)
summary(dpp_reg3)
#MSE
dpp_reg_summ_3 <- summary(dpp_reg3)
mean(dpp_reg_summ_3$residuals^2)
  #0.0001924112

ggplot(latest_cleaned, aes(x=days_since_2020_03_11, y=death_per_pos))+
  geom_point()+
  stat_smooth(se=F, method="lm", formula = y~poly(x,3))

#weekend and weekday regression models (see if they are more accurate indeependently)
#day hpp
weekday_hpp_reg3 <- lm(hosp_per_pos ~ poly(days_since_2020_03_11, 3), data = weekday_cleaned)
summary(weekday_hpp_reg3)
#MSE
weekday_hpp_reg_summ_3 <- summary(weekday_hpp_reg3)
mean(weekday_hpp_reg_summ_3$residuals^2)
  #0.000703072
#end hpp
weekend_hpp_reg3 <- lm(hosp_per_pos ~ poly(days_since_2020_03_11, 3), data = weekend_cleaned)
summary(weekend_hpp_reg3)
#MSE
weekend_hpp_reg_summ_3 <- summary(weekend_hpp_reg3)
mean(weekend_hpp_reg_summ_3$residuals^2)
  #0.0008745755
#day dpp
weekday_dpp_reg3 <- lm(death_per_pos ~ poly(days_since_2020_03_11, 3), data = weekday_cleaned)
summary(weekday_dpp_reg3)
#MSE
weekday_dpp_reg_summ_3 <- summary(weekday_dpp_reg3)
mean(weekday_dpp_reg_summ_3$residuals^2)
  #0.000172556
#end dpp
weekend_dpp_reg3 <- lm(death_per_pos ~ poly(days_since_2020_03_11, 3), data = weekend_cleaned)
summary(weekend_dpp_reg3)
#MSE
weekend_dpp_reg_summ_3 <- summary(weekend_dpp_reg3)
mean(weekend_dpp_reg_summ_3$residuals^2)
  #0.0002273922

#cooks distance outlier removal of 3rd order polynomial regressions
library(dplyr)
#HpC
hpp_CooksD <- cooks.distance(hpp_reg3)
hpp_CooksD_outliers <- hpp_CooksD[(hpp_CooksD > (3 * mean(hpp_CooksD, na.rm = TRUE)))]
#42 outliers detected
#make new dataframe without outliers
names_hpp_outliers <- names(hpp_CooksD_outliers)
names_hpp_outliers
  #most outliers are at early points in the data
  #indicator that removal of the outliers may decrease fit since these are
  #highly infleuntial points without addressing cooks distances
hpp_CooksD_outliers_frame <- latest_cleaned[names_hpp_outliers,]
hpp_no_CooksD_outliers_frame <- latest_cleaned %>% anti_join(hpp_CooksD_outliers_frame)
#repeat regression without cooks D outliers and compare to original
hpp_no_CooksD_outliers_reg <- lm(hosp_per_pos ~ poly(days_since_2020_03_11, 3), data = hpp_no_CooksD_outliers_frame)
summary(hpp_no_CooksD_outliers_reg)
#mult R2 = 0.6961, adj = 0.6944  , p = 2.2e-16
#MSE
hpp_no_CooksD_outliers_reg_summ <- summary(hpp_no_CooksD_outliers_reg)
hpp_no_CooksD_outliers_reg_MSE <- mean(hpp_no_CooksD_outliers_reg_summ$residuals^2)
hpp_no_CooksD_outliers_reg_MSE
  #0.0004053526

#dpc
dpp_CooksD <- cooks.distance(dpp_reg3)
dpp_CooksD_outliers <- dpp_CooksD[(dpp_CooksD > (3 * mean(dpp_CooksD, na.rm = TRUE)))]
#42 outliers detected
#make new dataframe without outliers
names_dpp_outliers <- names(dpp_CooksD_outliers)
names_dpp_outliers
  #all outliers are at early points in the data
  #indicator that removal of the outliers may decrease fit since these are
  #highly infleuntial points without addressing cooks distances
dpp_CooksD_outliers_frame <- latest_cleaned[names_dpp_outliers,]
dpp_no_CooksD_outliers_frame <- latest_cleaned %>% anti_join(dpp_CooksD_outliers_frame)
#repeat regression without cooks D outliers and compare to original
dpp_no_CooksD_outliers_reg <- lm(death_per_pos ~ poly(days_since_2020_03_11, 3), data = hpp_no_CooksD_outliers_frame)
summary(dpp_no_CooksD_outliers_reg)
#mult R2 = 0.7569, adj = 0.7555 , p = 2.2e-16
#MSE
dpp_no_CooksD_outliers_reg_summ <- summary(dpp_no_CooksD_outliers_reg)
dpp_no_CooksD_outliers_reg_MSE <- mean(dpp_no_CooksD_outliers_reg_summ$residuals^2)
dpp_no_CooksD_outliers_reg_MSE
#8.251638e-05



#KNN classification model
#prepare dataframe with only hpp, dpp, and peak#
peaks_knn_prep <- data.frame(peaks[,7:11])
peaks_knn_prep <- peaks_knn_prep[-c(3:4)]
peaks_knn_prep$Peak <- as.factor(peaks_knn_prep$Peak)
str(peaks_knn_prep)
#normalize predictor variables (hpp and dpp)
set.seed(1331)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
peaks_knn_prep[1:2] <- as.data.frame(lapply(peaks_knn_prep[1:2], normalize))
str(peaks_knn_prep)#succesfully normalized
#create index for 70/30 train/test split
Ind <- sample(2, nrow(peaks_knn_prep), replace=TRUE, prob=c(0.7, 0.3))
knntrain <- peaks_knn_prep[Ind==1,]
knntest <- peaks_knn_prep[Ind==2,]
  #obs in training set is 106
sqrt(106)
  #use K = 11
library(class)
knnpred <- knn(train = knntrain[1:2], test = knntest[1:2], cl = knntrain$Peak, k = 11)
table(knnpred)
table(knntest$Peak)

#Assessing results, faster way
library(caret)
confusionMatrix(knnpred,knntest$Peak)
#try to find best k
i = 1
k.optm = 1
for (i in 1:30){ 
  knn.mod <-  knn(train = knntrain[1:2], test = knntest[1:2], cl = knntrain$Peak, k=i)
  k.optm[i] <- 100 * sum(knntest$Peak == knn.mod)/NROW(knntest$Peak)
  k=i  
  cat(k,'=',k.optm[i],'\n')
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")+
  title("Accuracies for various K values")


#optimization with different training/testing splits
#test 60/40, 70/30, 80/20 at 10 different seed values
#compare accuracies of model at K = odd number closest to square root #obs
#and the optimal accuracy at other K values

#70/30
set.seed(1331) #repeat for 1331-1340
Ind <- sample(2, nrow(peaks_knn_prep), replace=TRUE, prob=c(0.7, 0.3))
knntrain <- peaks_knn_prep[Ind==1,]
knntest <- peaks_knn_prep[Ind==2,]
#obs in training set is 106 (consistently 100+)
sqrt(106)
#use K = 11
knnpred <- knn(train = knntrain[1:2], test = knntest[1:2], cl = knntrain$Peak, k = 11)
confusionMatrix(knnpred,knntest$Peak)
#try to find best k
i = 1
k.optm = 1
for (i in 1:40){ 
  knn.mod <-  knn(train = knntrain[1:2], test = knntest[1:2], cl = knntrain$Peak, k=i)
  k.optm[i] <- 100 * sum(knntest$Peak == knn.mod)/NROW(knntest$Peak)
  k=i  
  cat(k,'=',k.optm[i],'\n')
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#60/40
set.seed(1340) #repeat for 1331-1340
Ind.60 <- sample(2, nrow(peaks_knn_prep), replace=TRUE, prob=c(0.6, 0.4))
knntrain.60 <- peaks_knn_prep[Ind.60==1,]
knntest.40 <- peaks_knn_prep[Ind.60==2,]
#obs in training set is 104 (for some seeds drops below 100 where K = 9 becomes better fit)
sqrt(104)
#use K = 11 (or 9)
knnpred <- knn(train = knntrain.60[1:2], test = knntest.40[1:2], cl = knntrain.60$Peak, k = 11)
confusionMatrix(knnpred,knntest.40$Peak)
#try to find best k
i = 1
k.optm = 1
for (i in 1:40){ 
  knn.mod <-  knn(train = knntrain.60[1:2], test = knntest.40[1:2], cl = knntrain.60$Peak, k=i)
  k.optm[i] <- 100 * sum(knntest.40$Peak == knn.mod)/NROW(knntest.40$Peak)
  k=i  
  cat(k,'=',k.optm[i],'\n')
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#80/20
set.seed(1331) #repeat for 1331-1340
Ind.80 <- sample(2, nrow(peaks_knn_prep), replace=TRUE, prob=c(0.8, 0.2))
knntrain.80 <- peaks_knn_prep[Ind.80==1,]
knntest.20 <- peaks_knn_prep[Ind.80==2,]
#obs in training set is 126 (consistently ~130)
sqrt(120)
#use K = 11
knnpred <- knn(train = knntrain.80[1:2], test = knntest.20[1:2], cl = knntrain.80$Peak, k = 11)
confusionMatrix(knnpred,knntest.20$Peak)
#try to find best k
i = 1
k.optm = 1
for (i in 1:40){ 
  knn.mod <-  knn(train = knntrain.80[1:2], test = knntest.20[1:2], cl = knntrain.80$Peak, k=i)
  k.optm[i] <- 100 * sum(knntest.20$Peak == knn.mod)/NROW(knntest.20$Peak)
  k=i  
  cat(k,'=',k.optm[i],'\n')
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#leave one out cross validation of all train/test splits
#70/30 split
library(class)
library(caret)
set.seed(1331) #repeat for 1331-1340
Ind <- sample(2, nrow(peaks_knn_prep), replace=TRUE, prob=c(0.7, 0.3))
knntrain <- peaks_knn_prep[Ind==1,]
sqrt(113)
knn_loocv <- knn.cv(train = knntrain, cl = knntrain$Peak, k = 11)
confusionMatrix(knn_loocv,knntrain$Peak)
  #CV accuracy of 100% for all 10 seeds

#80/20
set.seed(1331) #repeat for 1331-1340
Ind.80 <- sample(2, nrow(peaks_knn_prep), replace=TRUE, prob=c(0.8, 0.2))
knntrain.80 <- peaks_knn_prep[Ind.80==1,]
sqrt(126)
knn_loocv.80 <- knn.cv(train = knntrain.80, cl = knntrain.80$Peak, k = 11)
confusionMatrix(knn_loocv.80,knntrain.80$Peak)
  #CV accuracy of 100% for all 10 seeds

#60/40
set.seed(1331) #repeat for 1331-1340
Ind.60 <- sample(2, nrow(peaks_knn_prep), replace=TRUE, prob=c(0.6, 0.4))
knntrain.60 <- peaks_knn_prep[Ind.60==1,]
sqrt(104) #k = 9 or 11 based on seed
knn_loocv.60 <- knn.cv(train = knntrain.60, cl = knntrain.60$Peak, k = 9)
confusionMatrix(knn_loocv.60,knntrain.60$Peak)
  #CV accuracy of 100% for all 10 seeds



#svm classificatiom
library(e1071)
library(ggplot2)
#use pre-factors knn prep dataframe
set.seed(1331)
help(tune)
linear_tune <- tune(svm, Peak ~ hosp_per_pos + death_per_pos, ,data=peaks_knn_prep,kernel="linear", ranges=list(cost=c(0.00001,0.0001,0.001,0.01,0.1,1,10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10)))
#determine CV errors for each model using summary command
summary(linear_tune)
#tune() function stores best model, which can be accessed through following comamnd
bestmod=linear_tune$best.model
bestmod
  #best model is cost = 100, lowest error (same with costs of 10^2 thru 10^7)
linear_tune_performances <- linear_tune$performances
ggplot(linear_tune_performances, aes(x=cost, y=error))+
  geom_point()+
  geom_line()+
  scale_x_log10()

#linear SVM with no cost (cost = 1)
svm_1 <- svm(Peak ~ hosp_per_pos + death_per_pos, data = peaks_knn_prep, kernel = "linear")
plot(svm_1, peaks_knn_prep)
pred1 <- predict(svm_1, peaks_knn_prep)
table1 <- table(Predicted = pred1, Actual = peaks_knn_prep$Peak)
table1 #6 incorrect classifications
confusionMatrix(pred1,peaks_knn_prep$Peak)
  #96.41% accuracy
summary(svm_1)

#experiment with ideal cost based on CV (cost = 100) and scale = false
svm_2 <- svm(Peak ~ hosp_per_pos + death_per_pos, data = peaks_knn_prep, kernel="linear", cost=100,scale=FALSE)
svm_2
plot(svm_2, peaks_knn_prep)
pred2 <- predict(svm_2, peaks_knn_prep)
table2 <- table(Predicted = pred2, Actual = peaks_knn_prep$Peak)
table2 #5 incorrect classifications
confusionMatrix(pred2,peaks_knn_prep$Peak)
  #97.01% accuracy
#for figure
svm_peak_prep <- peaks_knn_prep  #new dataframe with renamed columns for figure)
names(svm_peak_prep)[names(svm_peak_prep) == "hosp_per_pos"] <- "HpC"
names(svm_peak_prep)[names(svm_peak_prep) == "death_per_pos"] <- "DpC"
svm_2.9 <- svm(Peak ~ HpC + DpC, data = svm_peak_prep, kernel="linear", cost=100,scale=FALSE)
plot(svm_2.9, svm_peak_prep)


#experiment with very high cost value (higher causes better accuray but worse margin)
svm_2.1 <- svm(Peak ~ hosp_per_pos + death_per_pos, data = peaks_knn_prep, kernel="linear", cost=10^5,scale=FALSE)
svm_2.1
plot(svm_2.1, peaks_knn_prep)
pred2.1 <- predict(svm_2.1, peaks_knn_prep)
table2.1 <- table(Predicted = pred2.1, Actual = peaks_knn_prep$Peak)
table2.1 #2 incorrect classifications
confusionMatrix(pred2.1,peaks_knn_prep$Peak)
  #98.80% accuracy with higher cost value, but very tight margin (different hyperplane
  #can easily be seen, way different angle to divide points by)

#look at polynomial kernel?
#CV through tune function to determine ideal cost
polynomial_tune <- tune(svm, Peak ~ hosp_per_pos + death_per_pos, ,data=peaks_knn_prep,kernel="polynomial", ranges=list(cost=c(0.00001,0.0001,0.001,0.01,0.1,1,10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10)))
#determine CV errors for each model using summary command
summary(polynomial_tune)
#tune() function stores best model, which can be accessed through following comamnd
bestmod2=polynomial_tune$best.model
bestmod2
  #cost of 10^6 and 10^7 have lowest cost function (use 10^6)

#polynomial at optimal cost value
svm_3 <- svm(Peak ~ hosp_per_pos + death_per_pos, data = peaks_knn_prep, kernel="polynomial", cost=10^6,scale=FALSE)
svm_3
  #3rd degree fit
plot(svm_3, peaks_knn_prep)
pred3 <- predict(svm_3, peaks_knn_prep)
table3 <- table(Predicted = pred3, Actual = peaks_knn_prep$Peak)
table3 #5 incorrect classifications
confusionMatrix(pred3,peaks_knn_prep$Peak)
  #96.41% accuracy, POLYNOMIAL KERNEL ONLY WORKS WITH HIGH COST FUNCTION
#for figure
svm_peak_prep <- peaks_knn_prep  #new dataframe with renamed columns for figure)
names(svm_peak_prep)[names(svm_peak_prep) == "hosp_per_pos"] <- "HpC"
names(svm_peak_prep)[names(svm_peak_prep) == "death_per_pos"] <- "DpC"
svm_3.9 <- svm(Peak ~ HpC + DpC, data = svm_peak_prep, kernel="polynomial", cost=10^6,scale=FALSE)
plot(svm_3.9, svm_peak_prep)
#polynomial at higher cost function
svm_3.1 <- svm(Peak ~ hosp_per_pos + death_per_pos, data = peaks_knn_prep, kernel="polynomial", cost=10^9,scale=FALSE)
svm_3.1
#3rd degree fit
plot(svm_3.1, peaks_knn_prep)
pred3.1 <- predict(svm_3.1, peaks_knn_prep)
table3.1 <- table(Predicted = pred3.1, Actual = peaks_knn_prep$Peak)
table3.1 #2 incorrect classifications
confusionMatrix(pred3.1,peaks_knn_prep$Peak)
  #98.80% accuracy with higher cost value, but very tight margin



#trying to plot SVM things cleanly in ggplot
#linear kernel validated at cost = 100 (svm_2)
#add support vector indicator
df_2 <- peaks_knn_prep; df_2
df_2 <- cbind(df_2, sv=rep(0,nrow(df_2)))
df_2[svm_2$index,]$sv = 1
#add grid for color output based on hyperplace
grid_svm_2 <- expand.grid(seq(min(peaks_knn_prep[, 1]), max(peaks_knn_prep[, 1]),length.out=100),                                                                                                         
                    seq(min(peaks_knn_prep[, 2]), max(peaks_knn_prep[, 2]),length.out=100)) 
names(grid_svm_2) <- names(peaks_knn_prep)[1:2]
Peak_prediction_2 <- predict(svm_2, grid_svm_2)
df_svm_2_pred <- data.frame(grid_svm_2, Peak_prediction)
ggplot(df_svm_2_pred, aes(x = death_per_pos, y = hosp_per_pos, fill = Peak_prediction_2))+
  geom_tile()
#final figure
peaks_knn_prep_svm_2_copy <- peaks_knn_prep
cols <- c('1' = 'red', '2' = 'black')
tiles <- c('1' = 'bisque', '2' = 'azure3')
shapes <- c('Support' = 4, 'Non-support' = 1)
peaks_knn_prep_svm_2_copy$Support <- 'Non-support'
peaks_knn_prep_svm_2_copy[svm_2$index, 'Support'] <- 'Support'

ggplot(df_svm_2_pred, aes(x = death_per_pos, y = hosp_per_pos))+
  geom_tile(aes(fill = Peak_prediction_2))+ 
  scale_fill_manual(values = tiles)+
  geom_point(data = peaks_knn_prep_svm_2_copy, aes(color = Peak, shape = Support), size = 4)+
  scale_color_manual(values = cols)+
  scale_shape_manual(values = shapes)+
  ggtitle('Linear Kernel SVM classification plot')+
  xlab('DpC')+
  ylab('HpC')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_blank())



#polynomail (cubic) svm kernel at cost = 10^6
df_3 <- peaks_knn_prep; df_3
df_3 <- cbind(df_2, sv=rep(0,nrow(df_3)))
df_3[svm_3$index,]$sv = 1
#add grid for color output based on hyperplace
grid_svm_3 <- expand.grid(seq(min(peaks_knn_prep[, 1]), max(peaks_knn_prep[, 1]),length.out=100),                                                                                                         
                          seq(min(peaks_knn_prep[, 2]), max(peaks_knn_prep[, 2]),length.out=100)) 
names(grid_svm_3) <- names(peaks_knn_prep)[1:2]
Peak_prediction_3 <- predict(svm_3, grid_svm_3)
Peak_prediction_3
df_svm_3_pred <- data.frame(grid_svm_3, Peak_prediction)
ggplot(df_svm_3_pred, aes(x = death_per_pos, y = hosp_per_pos, fill = Peak_prediction))+
  geom_tile()
#final figure
peaks_knn_prep_svm_3_copy <- peaks_knn_prep
cols <- c('1' = 'red', '2' = 'black')
tiles <- c('1' = 'bisque', '2' = 'azure3')
shapes <- c('Support' = 4, 'Non-support' = 1)
peaks_knn_prep_svm_3_copy$Support <- 'Non-support'
peaks_knn_prep_svm_3_copy[svm_3$index, 'Support'] <- 'Support'

ggplot(df_svm_3_pred, aes(x = death_per_pos, y = hosp_per_pos))+
  geom_tile(aes(fill = Peak_prediction))+ 
  scale_fill_manual(values = tiles)+
  geom_point(data = peaks_knn_prep_svm_3_copy, aes(color = Peak, shape = Support), size = 4)+
  scale_color_manual(values = cols)+
  scale_shape_manual(values = shapes)+
  ggtitle('Polynomial Kernel SVM classification plot')+
  xlab('DpC')+
  ylab('HpC')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


