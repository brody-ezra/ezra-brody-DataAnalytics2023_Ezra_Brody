# Creating a data frame

days<-c('Mon','Tue', 'Wed','Thu','Fri','Sat','Sun')
temp<-c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed<-c('T','T','F','F','T','T','F')

help("data.frame")

RPI_Weather_Week<-data.frame(days,temp,snowed)
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,] #shows first row and all columns
RPI_Weather_Week[,1] #shows first column and all rows
RPI_Weather_Week[,'snowed']
RPI_Weather_Week['snowed',] #this version doesnt work; why? Because snowed is not a row
RPI_Weather_Week[,'temp']
RPI_Weather_Week[,'days']
RPI_Weather_Week[1:5,c("days","temp")] #view of rows 1-5 for days and temp data
RPI_Weather_Week[1:7,c("days","temp")]
RPI_Weather_Week$temp

subset(RPI_Weather_Week,subset=snowed==TRUE)
help("subset")
sorted.snowed<-order(RPI_Weather_Week['snowed']) #getting error with this command?

dec.snow<-order(-RPI_Weather_Week$temp) #no error code with this, maybe problem with sorted command?
head(dec.snow)
help("decompose")
dec.snow

#Creaing data frames
empty.DataFrame<-data.frame()
v1<-1:10
v1
letters
v2<-letters[1:10]
df<-data.frame(col.name.1=v1,col.name.2=v2)
head(df)

#Importing/exporting data
write.csv(df,file='saved_df1.csv')
help("write.csv")
df2<-read.csv("saved_df1.csv") #extra column named X, why?
