setwd("~/desktop/RepData_PeerAssessment1") ##Choose Wd
activity <- read.csv("activity.csv")    ##read file

agg<- aggregate(steps~date, activity, sum, na.rm=T)
head(agg)

hist(agg$steps, main="Total steps per day (Ignoring NAs)", xlab ="Number of steps per day", ylab="Frecuency")
rug(agg$steps)
c(mean = mean(agg$steps),median = median(agg$steps))

ave<-aggregate(steps ~ interval, activity, mean, na.rm=T)
##2
plot(ave$interval, ave$steps, type ="l", main= "Average number of steps by interval each day", xlab = "5-minute interval", ylab= "Average steps by interval per day")
ave[which.max(ave$steps),]

sum(is.na(activity$steps))

act2 <- activity
for (i in act2$interval) {
  act2$steps[which(is.na(act2$steps) & act2$interval == i)] <- round(ave$steps[ave$interval==i])
}
   
agg2<- aggregate(steps~date, act2, sum)
head(agg2)
##3
hist(agg2$steps, main="Total steps per day (imputing NAs)", xlab ="Number of steps per day", ylab="Frecuency")
rug(agg2$steps)
c(mean = mean(agg2$steps),median = median(agg2$steps))

act2$date <- as.Date(act2$date)
act2$weekday <- weekdays(act2$date)
act2$weekend <- ""

 for (i in 1:nrow(act2)) {
   if (act2[i,]$weekday %in% c("Saturday", "Sunday")) {
     act2[i,]$weekend <- "weekend"
   }
   else {
     act2[i,]$weekend <- "weekday"
   }
 }
ave2<-aggregate(steps ~ interval+weekend, act2, mean)

##4
xyplot(ave2$steps~ ave2$interval|ave2$weekend, 
       type = "l", 
       layout=c(1,2),
       main = "Average of steps per interval per type of weekday",
        xlab="Interval",
       ylab="Average of Steps"
)

