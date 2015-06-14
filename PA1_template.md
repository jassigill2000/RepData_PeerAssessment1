# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The assumption is made that the activity file has been downloaded and the working directory is set to where the data file is saved.
The data is read and the data rows that are not complete are removed.

```r
   activityFile <- "activity.csv"
   if(file.exists(activityFile)) {
     activity <- read.csv(file=activityFile, sep=",", header=TRUE, skipNul=TRUE )
     activity.ignore.na <- activity[complete.cases(activity),]
     str(activity.ignore.na)
     
     require("lattice")
     activity$date <- as.Date(activity$date, "%Y-%m-%d")
   }
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```
## Loading required package: lattice
```

## What is mean total number of steps taken per day?
1.) The total number of steps taken per day is calculated using the aggregate function

```r
   aggActivity <- aggregate(formula = steps ~ date, data = activity.ignore.na, FUN=sum)
```

2.) The histogram uses the the data generated in the above step to plot the steps taken per day


```r
   hist(aggActivity$steps,
        xlab = "Total Steps Per Day",
        ylab = "Number of Days",
        main = "Frequency of Total Steps in a day",
        col = c("red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The mean of the total number of steps taken per day is calculated as follows.

```r
   mean(aggActivity$steps)
```

```
## [1] 10766.19
```

The median of the total number of steps taken per day is calculated as follows.

```r
   median(aggActivity$steps)
```

```
## [1] 10765
```

The mean is 10766.19 and the median is 10765

## What is the average daily activity pattern?

1.) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

First create a dataset that caclulates the average number of steps taken across each day for a given interval.

Then a time series plot is generated.

```r
   steps_per_interval <- aggregate(data=activity.ignore.na, steps ~ interval, FUN=mean)

   plot(x=steps_per_interval$interval,
        y = steps_per_interval$steps, 
        type="l", 
        xlab="5-min interval", 
        ylab="Number of Steps",
        main="Average number of steps taken",
        col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2.) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
   max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]
```
835 is the 5-minute interval and the maximum number of steps taken are approximately 206 steps.

## Imputing missing values
1.) What are the toatl number of rows with NA's. 

```r
   activity_NA <- sum(is.na(activity))
```
The total number of rows with NA's is 2304

2.) We fill the missing values with average number of steps taken across each day for a given interval.


```r
   fillNA <- numeric() 
   for(i in 1:nrow(activity)){
       rowData <- activity[i,]
       if(is.na(rowData$steps)){
         steps <- steps_per_interval[steps_per_interval$interval == rowData$interval, ]$steps
       } else {
         steps <- rowData$steps
       }
    fillNA <- c(fillNA, steps)
   }
```

3.) A new dataset that is equal to the original dataset but with the missing data filled in is created

```r
   new_activity <- activity
   new_activity$steps <- fillNA

   str(new_activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

4.) A histogram of the total number of steps taken each day is drawn.

```r
   aggActivity2 <- aggregate(formula = steps ~ date, data = new_activity, FUN=sum)
   
   hist(aggActivity2$steps,
        xlab = "Total Steps Per Day",
        ylab = "Number of Days",
        main = "Frequency of Total Steps in a day",
        col = c("green"))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

The mean and median of the total number of steps taken per day is calculated as follows.

```r
   mean(aggActivity2$steps)
```

```
## [1] 10766.19
```

```r
   median(aggActivity2$steps) 
```

```
## [1] 10766.19
```
The mean and the median are the same 10766.19 steps.


## Are there differences in activity patterns between weekdays and weekends?
1.) A new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day is created.

To get what day it is we used the weekdays function. And depending if it is is a Saturday or Sunday then we assign the daytype to a Weekend else assign it to a Weekday

```r
   day <- weekdays(new_activity$date)
   daytype <- vector()
   for(i in 1:nrow(new_activity)) {
     if(day[i] == "Saturday"){
       daytype[i] <- "Weekend"
     } else if(day[i] == "Sunday") {
       daytype[i] <- "Weekend"
     } else {
       daytype[i] <- "Weekday"
     }
   }
   weekday_data <- activity
   weekday_data$weekday <- daytype
   weekday_data$weekday <- factor(weekday_data$weekday)
```

A panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). First the data is massaged accordingly. And then an xyplot is created.


```r
   stepsByDay <- aggregate(steps ~ interval + weekday, data = weekday_data, mean)
names(stepsByDay) <- c("interval", "weekday", "steps")

   xyplot(steps ~ interval | weekday, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

Looking at the plots we note that people have more movement over the weekend over the weekdays.
