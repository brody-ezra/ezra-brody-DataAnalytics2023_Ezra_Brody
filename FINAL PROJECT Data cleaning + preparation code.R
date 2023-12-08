setwd("C:/Users/ezrab/Downloads/Fall 23/BCBP 4600/BCBP Final Project Data/COVID")
crude <- read.csv("LEE COVID-19_Outcomes_by_Testing_Cohorts__Cases__Hospitalizations__and_Deaths_20231021 - Copy.csv")

library(tidyverse)
library(lubridate)

#Determining last extract date
#Make extract and specimen dates as.Date
str(crude)
crude$extract_date <- as.Date(crude$extract_date)
crude$specimen_date <- as.Date(crude$specimen_date)
str(crude) #successful
#summary statistics of extract date to determine max (latest)
summary(crude$extract_date)
#latest extract date is 2021-10-01, filter for only that
latest_extract <- filter(crude, extract_date == "2021-10-01")
latest_extract <- filter(latest_extract, specimen_date < "2023-01-01")

#remove invalid rows
latest_cleaned <- filter(latest_extract, Number_confirmed <= Number_tested)
latest_cleaned <- filter(latest_cleaned, Number_hospitalized <= Number_confirmed)
latest_cleaned <- filter(latest_cleaned, Number_deaths <= Number_confirmed)
    #deaths > hosp. is not invalid according to data description

#rate columns
latest_cleaned$hosp_per_pos <- latest_cleaned$Number_hospitalized / latest_cleaned$Number_confirmed
latest_cleaned$death_per_pos <- latest_cleaned$Number_deaths / latest_cleaned$Number_confirmed
latest_cleaned$death_per_hosp <- latest_cleaned$Number_deaths / latest_cleaned$Number_hospitalized

#filter based on test number < 1000
latest_cleaned_range <- filter(latest_cleaned, Number_tested >= 1000)

write.csv(latest_cleaned_range,file='Latest extract cleaned and range.csv')

#explaining why number_tested is not relevant (stays similar)
library(ggplot2)
ggplot(latest_extract, aes(x = specimen_date, y = Number_tested))+
  geom_point()+
  ggtitle("Number of Patients Tested over Time")+
  xlab("Specimen Date")+
  ylab("Number of Tests")+
  theme(plot.title=element_text(size=15))

ggplot(latest_cleaned, aes(x = specimen_date, y = Number_confirmed))+
  geom_point()+
  ggtitle("Number of Confirmed Cases over Time")

ggplot(latest_extract, aes(x = Number_tested))+
  geom_histogram(binwidth = 500, fill = "black")+
  ggtitle("Number of Patients Tested")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))+
  xlab("Number of Tests")+
  ylab("Count")+
  scale_y_continuous(breaks=c(10,20,30,40,50,60))


ggplot(latest_extract, aes(x = Number_tested))+
  geom_histogram(binwidth = 100, fill = "black")+
  ggtitle("Number of Patients Tested")+
  theme(plot.title=element_text(size=20), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))+
  xlab("Number of Tests")+
  ylab("Count")+
  scale_y_continuous(breaks=c(10,20,30,40,50,60))

help(theme)
