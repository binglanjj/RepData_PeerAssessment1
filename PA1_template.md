# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
data=read.csv("activity.csv", header=T)
```

## What is mean total number of steps taken per day?

```r
#1.Make a histogram of the total number of steps taken each day
#calculate the number of steps taken each day
sum=tapply(data$steps, data$date, FUN=sum)
#plot the histogram
hist(sum, main="Total Number of steps taken each day",
     xlab="Total Number of steps taken each day",
     ylab="Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#2.Calculate and report the mean and median total number of steps taken per day
mean(sum,na.rm=T)
```

```
## [1] 10766.19
```

```r
median(sum, na.rm=T)
```

```
## [1] 10765
```
mean is 10766.19

median is 10765


## What is the average daily activity pattern?

```r
#1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
#average number of steps taken on each interval
mean_interval=as.vector(tapply(data$steps, data$interval, FUN=mean, na.rm=T))
#plot a time series plot
library(lattice)
xyplot(mean_interval~data$interval, type="l",xlab="Interval", ylab="Average steps (across all days)", 
       main="Time series plot")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
i=which(mean_interval==max(mean_interval))
data$interval[i]
```

```
## [1] 835
```
the interval contains the maximum number of steps is 835.

## Imputing missing values

```r
#1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
length(which(is.na(data$steps)))
```

```
## [1] 2304
```
The total number of missing values is 2304.


```r
#2.Devise a strategy for filling in all of the missing values in the dataset. 
#strategy--use mean steps across all interval to replace NAs.
#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
newdata<-data
newdata[is.na(newdata)] <- mean(mean_interval)
#4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
#calculate the number of steps taken each day in new dataset
sumnew=tapply(newdata$steps, newdata$date, FUN=sum)
#plot the histogram
hist(sumnew, main="Total number of steps taken each day in new dataset",
     xlab="Total Number of steps taken each day",
     ylab="Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
#mean and median total number of steps taken per day in new dataset
mean(sumnew)
```

```
## [1] 10766.19
```

```r
median(sumnew)
```

```
## [1] 10766.19
```
In new dataset, the mean and median is both 10766.19.

--Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

Yes, the median is larger than in the original dataset.But mean value remains the same.

## Are there differences in activity patterns between weekdays and weekends?

```r
#1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
newdata$newdate<-strptime(as.character(newdata$date),format = "%Y-%m-%d")   
newdata$weekdays<-weekdays(newdata$newdate)
newdata$weekdays[newdata$weekdays %in% c("Monday", "Tuesday", "Wednesday",
                 "Thursday","Friday")]<-"weekday"
newdata$weekdays[newdata$weekdays %in% c("Saturday","Sunday")]<-"weekend"
#2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
par(mfcol = c(2, 1))
par(mar=c(2,2,2,2))
data1<-subset(newdata,newdata$weekdays=="weekend")
data1$mean1=tapply(data1$steps, data1$interval, FUN=mean)
plot(data1$interval, data1$mean1,type="l",main="weekend",xlab="interval",
     ylab="Days")

data2<-subset(newdata,newdata$weekdays=="weekday")
data2$mean2=tapply(data2$steps, data2$interval, FUN=mean)
plot(data2$interval,data2$mean2,type="l",main="weekday",xlab="interval",
     ylab="Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 


