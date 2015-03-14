---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
library(knitr)
opts_chunk$set (echo = TRUE)
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
withoutNA <- na.omit (data)
```

## What is mean total number of steps taken per day?

* Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
ggplot(withoutNA, aes(date, steps)) + geom_histogram(stat = "identity") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![plot of chunk hist_steps](figure/hist_steps-1.png) 

* Calculate and report the mean and median of the total number of steps taken per day
      * mean of the total number of steps taken per day:

```r
total_steps <- tapply (withoutNA$steps, withoutNA$date, sum)
mean(total_steps)
```

```
## [1] 10766.19
```
      
      * median of the total number of steps taken per day:      

```r
median(total_steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(plyr)
mean_steps <- ddply (withoutNA, .(interval), summarize, avg_steps = mean (steps))
ggplot(mean_steps, aes(interval, avg_steps)) + geom_line() + labs(title = "Average Number of Steps Taken Each 5-Minute Interval", x = "5-minute interval", y = "Average number of steps")
```

![plot of chunk plot_interval](figure/plot_interval-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
mean_steps [mean_steps$avg_steps == max(mean_steps$avg_steps),] [1]
```

```
##     interval
## 104      835
```


## Imputing missing values

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
      * I used mean for the interval
      
* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputed_data <- data 
for (i in 1:nrow(imputed_data)) {
    if (is.na(imputed_data$steps[i])) {
        imputed_data$steps[i] <- mean_steps[which(imputed_data$interval[i] == mean_steps$interval), ]$avg_steps
    }
}
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
ggplot(imputed_data, aes(date, steps)) + geom_histogram(stat = "identity") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![plot of chunk hist_imputed_data](figure/hist_imputed_data-1.png) 

      * Mean of total number of steps after imputing the data


```r
total_imputed <- ddply (imputed_data, .(date), summarize, total_steps = sum (steps))
mean (total_imputed$total_steps)
```

```
## [1] 10766.19
```

      * Median of total number of steps after imputing the data

```r
median (total_imputed$total_steps)
```

```
## [1] 10766.19
```

      * Comparing with the estimates from the first part of the assignment


```r
mean (total_imputed$total_steps) - mean (total_steps)
```

```
## [1] 0
```

```r
median (total_imputed$total_steps) - median (total_steps)
```

```
## [1] 1.188679
```

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
imputed_data$weekdays <- weekdays(imputed_data$date)
imputed_data$weekdays <- mapvalues(imputed_data$weekdays, c("Monday", "Tuesday","Wednesday","Thursday", "Friday"), rep("weekday", 5))
imputed_data$weekdays <- mapvalues(imputed_data$weekdays, c("Saturday", "Sunday"), rep("weekend", 2))
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
mean_steps_weekdays <- ddply (imputed_data, .(interval, weekdays), summarize, avg_steps = mean (steps))

ggplot(mean_steps_weekdays, aes(interval, avg_steps)) + geom_line() + facet_grid(weekdays~.) +labs(x = "Interval", y = "Number of steps")
```

![plot of chunk plot_weekdays](figure/plot_weekdays-1.png) 



