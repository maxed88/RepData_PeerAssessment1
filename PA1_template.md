---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


## Loading and preprocessing the data

Load the data.


```r
if(!(file.exists("activity.csv"))) { 
  archiveFile <- "repdata%2Fdata%2Factivity.zip"
  if(!file.exists(archiveFile)) {
    archiveURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url=archiveURL,destfile=archiveFile,method="curl")
  }  
  unzip(archiveFile) 
}

activity <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
```
Here's a brief structure of data.

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
Here's a code to calculate the total number of steps taken per day.


```r
totalSteps <- aggregate(steps ~ date, activity, na.rm = TRUE, sum)
head(totalSteps, 10)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
```
Below is a histogram of the total number of steps taken each day.

```r
hist(totalSteps$steps, xlab = "Total Steps", main = "Total number of steps taken each day", col = "lightgreen")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Below is the code for the mean of the total number of steps taken per day.

```r
meanSteps <- mean(totalSteps$steps)
meanSteps
```

```
## [1] 10766.19
```
The mean of the total number of steps taken per day is 10766.19.

Below is the code for the median of the total number of steps taken per day.

```r
medianSteps <- median(totalSteps$steps)
medianSteps
```

```
## [1] 10765
```
The median of the total number of steps taken per day is 10765.

## What is the average daily activity pattern?
Here's a time series plot of the 5-minute interval(x-axis) and the average number of steps taken, averaged across all days(y-axis).

```r
stepsInterval <- aggregate(steps ~ interval, activity, na.rm = TRUE, mean)
plot(stepsInterval$interval, stepsInterval$steps, type = "l", xlab = "Interval", ylab = "Number of Steps", main = "Average number of steps taken per day", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Below is a 5-minute interval with the maximum number of steps.

```r
maxStepsInterval <- stepsInterval[which.max(stepsInterval$steps), ]
maxStepsInterval
```

```
##     interval    steps
## 104      835 206.1698
```
The interval of 835 has the maximum number of steps.

## Imputing missing values
Here's a code to find the total number of missing values in the dataset.

```r
totalNA <- sum(!(complete.cases(activity)))
totalNA
```

```
## [1] 2304
```
There are 2304 of missing values(NAs) in the dataset.

Here's a code to fill missing values with the mean number of steps with the same interval.

```r
NAindex <- which(is.na(activity$steps))
imputed <- activity

for (i in 1:totalNA) {
  stepsIndex <- imputed[NAindex[i], ]$interval
  imputed[NAindex[i], ]$steps <- stepsInterval[which(stepsInterval$interval == stepsIndex), ]$steps
}
imputedTotalSteps <- aggregate(steps ~ date, imputed, na.rm = TRUE, sum)
```
To find out if it still contains any missing values inside.

```r
sum(!(complete.cases(imputed)))
```

```
## [1] 0
```
There's none.

Let's compare the mean and the median of both the original and imputed dataset.

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

```r
mean(imputedTotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(imputedTotalSteps$steps)
```

```
## [1] 10766.19
```
Both mean is the same but there's a slight difference on the median.

Here's a histogram to tell the difference more desirably.

```r
hist(imputedTotalSteps$steps, xlab = "Total Steps", main = "Total number of steps taken each day", col = "blue")
hist(totalSteps$steps, xlab = "Total Steps", main = "Total number of steps taken each day", col = "lightgreen", add = TRUE)
legend("topright", c("Imputed Data", "Raw Data"), col = c("blue", "lightgreen"), lwd = 5)
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

It looks like missing values have the impact in between 10000 to 15000 of total number of steps taken per day.

## Are there differences in activity patterns between weekdays and weekends?
Here's a code to create a new factor variable in the dataset with two levels -- "Weekday" and "Weekend" indicating whether a given date is a weekday or a weekend.


```r
imputed$dotw <- as.Date(imputed$date)
imputed$dotw <- ifelse(weekdays(imputed$dotw) %in% c("Satuday", "Sunday"), "Weekend", "Weekday")

imputedTotalSteps <- aggregate(steps ~ interval + dotw, imputed, mean)
```

Below is a panel plot containing a time series plot of the 5-minute interval(x-axis) and the average number of steps taken, averaged all across all weekday or weekend(y-axis).


```r
library(lattice)
xyplot(imputedTotalSteps$steps ~ imputedTotalSteps$interval|imputedTotalSteps$dotw, main = "Average Steps per Day by Interval", xlab = "Interval", ylab = "Steps", layout = c(1, 2), type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
