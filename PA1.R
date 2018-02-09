#loading and processing data
activity<-read.csv("activity.csv")
summary(activity)
activitydata<-as.data.frame(activity[complete.cases(activity),])
summary(activitydata)

# mean total number of steps taken per day
  #total number of steps taken for each day
totalsteps<-tapply(activitydata$steps,activitydata$date,sum)
  # histogram of the total number of steps taken each day
hist(totalsteps)
meantotsteps<-mean(totalsteps,na.rm=TRUE)
meantotsteps
medtotsteps<-median(totalsteps,na.rm = TRUE)
medtotsteps

#average daily activity pattern
avgdailypattern<-aggregate(activitydata$steps,by=list(activitydata$interval),FUN=mean)
summary(avgdailypattern)
plot(avgdailypattern[, 1], avgdailypattern[, 2], type = "l", 
     xlab = "5 min Intervals in a day", 
     ylab = "Average Number of Steps", 
     main = "The Average Daily Activity Pattern")
maxStepsInterval = avgdailypattern[which.max(avgdailypattern[, 2]), 1]
maxStepsInterval

#Imputing missing values
 # total number of missing values in the dataset:
sum(!complete.cases(activity))
newdata <-activity
len1 <- nrow(newdata)
len2 <-nrow(avgdailypattern)
for (i in 1:len1) {
  if (is.na(newdata$steps[i])) {
    for (j in 1:len2) {
      if (newdata$interval[i] == avgdailypattern[j, 1]) {
        newdata$steps[i] <-avgdailypattern[j, 2]
      }
    } 
  }    
}
summary(newdata)
 #the total number of steps taken for each day and histogram
totstepsnew<-tapply(newdata$steps,newdata$date,sum)
hist(totstepsnew)
 #calculate and report the mean and median total number of steps taken per day
meantotstepsnew<-mean(totstepsnew,na.rm=TRUE)
meantotstepsnew
medtotstepsnew<-median(totstepsnew,na.rm=TRUE)
medtotstepsnew

#Are there differences in activity patterns between weekdays and weekends?
 #create a new factor variable in the dataset with two levels - "weekday" and "weekend"
activitydata$weekday = TRUE
weekday = weekdays(as.POSIXct(activitydata$date, format = "%Y-%m-%d" ))
for (i in 1:length(weekday)) {
  if (weekday[i] == "Saturday" | weekday[i] == "Sunday") {
    activitydata$weekday[i] = FALSE
  }
}
dataWeekday = activitydata[which(activitydata$weekday == TRUE), ]
dataWeekend = activitydata[which(activitydata$weekday == FALSE), ]

avgWeekdayPattern = aggregate(dataWeekday$steps, 
                              by = list(dataWeekday$interval), 
                              FUN = mean)
names(avgWeekdayPattern) = c("interval", "steps")
avgWeekdayPattern$dayTag = "weekday"
avgWeekendPattern = aggregate(dataWeekend$steps, 
                              by = list(dataWeekend$interval), 
                              FUN = mean)
names(avgWeekendPattern)= c("interval", "steps")
avgWeekendPattern$dayTag = "weekend"

avgPattern = rbind(avgWeekdayPattern, avgWeekendPattern)

library(lattice)
xyplot(steps ~ interval | dayTag, data = avgPattern, 
       type = "l", layout = c(1, 2))




























