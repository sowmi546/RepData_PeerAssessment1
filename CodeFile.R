#1.loading and preprocessing of data
library(lattice)
myData<- read.csv("activity.csv")
summary(myData)
myData$date <- as.Date(myData$date,"%Y-%m-%d")

#2.What is mean total number of steps taken per day?
names(myData)
plot(steps~date,myData)
#tail(myData$date)

stepsByDay <- aggregate(steps~date,data=myData,sum,na.rm=TRUE)
names(stepsByDay)
#plotting the histogram for the number of steps taken
hist(stepsByDay$steps,xlab="Day",ylab="steps taken",main="Mean num of steps taken by day",col="red")
#calculating mean and median
mean(stepsByDay$steps)
median(stepsByDay$steps)

#3.What is the average daily activity pattern?
names(myData)
tmyData <- tapply(myData$steps, myData$interval, mean, na.rm = TRUE)
row.names(tmyData)
names(tmyData)
tmyData
plot(names(tmyData),tmyData,type="l",xlab="5 minute interval",ylab="avg steps for each interval",main="average daily activity pattern",col="red")

#maximum steps interval
max_interval <- which.max(tmyData)
names(max_interval)


#4.Inputting missing values
count <-sum(is.na(myData)) #counting number of missing values in data set
count

myData$interval



#replacing NA values with mean value for that interval
StepsAverage <- aggregate(steps~interval,data=myData,mean)
fillNA <- numeric()
for(i in 1:nrow(myData))
{
  r <-myData[i,]
  if(is.na(r$steps))
  {
    steps <- subset(StepsAverage,interval==r$interval)$steps
  }
  else
    steps <-r$steps
  fillNA <- c(fillNA,steps)
}


newData <- myData
newData$steps <- fillNA
head(newData)

newTotalSteps <- aggregate(steps ~ date, data = newData, sum, na.rm = TRUE)
hist(newTotalSteps$steps, main = "Total steps by day", xlab = "day", col = "blue")
mean(newTotalSteps$steps)
median(newTotalSteps$steps)

#5.Are there differences in activity patterns between weekdays and weekends?
#checking if the day is week day or weekend
dayType <- weekdays(myData$date)
levelOfDay <- vector()
for (i in 1:nrow(myData)) {
  if (dayType[i] == "Saturday" | dayType[i]=="Sunday") {
    levelOfDay[i] <- "Weekend"
   
  } else {
    levelOfDay[i] <- "Weekday"
  }
}
#adding one more column to the new Data set specifying if the date is weekend or weekday
names(newData)
newData$levelOfDay <- levelOfDay
#head(newData)
#converting level to factor variable
newData$levelOfDay <- factor(newData$levelOfDay)
stepsByEachDay <- aggregate(steps~interval+levelOfDay,data=newData,mean)
names(stepsByEachDay)
par(mfrow=c(2,1))
xyplot(steps~interval|levelOfDay,type="l",data=stepsByEachDay,xlab="Day Type",ylab="steps taken",main="difference in pattern on weekdays and weekends",col="red")






























