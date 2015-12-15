---
title: "Peer Assessment 1"
author: "Fernanda Guizar"
date: "December 14, 2015"
output: html_document
---

## Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

The data was taken from the URL specified in the following code, and then converted into a data frame. 


```r
File_URL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(File_URL, destfile="Data.zip", mode="wb", method="curl")
temp <- unzip("Data.zip", unzip="internal")
Raw_data <- read.csv(temp, header=TRUE)
Date1 <- as.Date(as.character(Raw_data$date))
Steps1 <- as.numeric(Raw_data$steps)
Interval1 <- as.numeric(Raw_data$interval)
Rawdata_df <- data.frame(Date1, Interval1, Steps1)
```

## What is the mean total number of steps taken per day?

1. Calculate  the total number of steps taken per day


```r
Step_counts <- aggregate(Steps1 ~ Date1, data= Rawdata_df,sum)
Step_counts
```

```
##         Date1 Steps1
## 1  2012-10-02    126
## 2  2012-10-03  11352
## 3  2012-10-04  12116
## 4  2012-10-05  13294
## 5  2012-10-06  15420
## 6  2012-10-07  11015
## 7  2012-10-09  12811
## 8  2012-10-10   9900
## 9  2012-10-11  10304
## 10 2012-10-12  17382
## 11 2012-10-13  12426
## 12 2012-10-14  15098
## 13 2012-10-15  10139
## 14 2012-10-16  15084
## 15 2012-10-17  13452
## 16 2012-10-18  10056
## 17 2012-10-19  11829
## 18 2012-10-20  10395
## 19 2012-10-21   8821
## 20 2012-10-22  13460
## 21 2012-10-23   8918
## 22 2012-10-24   8355
## 23 2012-10-25   2492
## 24 2012-10-26   6778
## 25 2012-10-27  10119
## 26 2012-10-28  11458
## 27 2012-10-29   5018
## 28 2012-10-30   9819
## 29 2012-10-31  15414
## 30 2012-11-02  10600
## 31 2012-11-03  10571
## 32 2012-11-05  10439
## 33 2012-11-06   8334
## 34 2012-11-07  12883
## 35 2012-11-08   3219
## 36 2012-11-11  12608
## 37 2012-11-12  10765
## 38 2012-11-13   7336
## 39 2012-11-15     41
## 40 2012-11-16   5441
## 41 2012-11-17  14339
## 42 2012-11-18  15110
## 43 2012-11-19   8841
## 44 2012-11-20   4472
## 45 2012-11-21  12787
## 46 2012-11-22  20427
## 47 2012-11-23  21194
## 48 2012-11-24  14478
## 49 2012-11-25  11834
## 50 2012-11-26  11162
## 51 2012-11-27  13646
## 52 2012-11-28  10183
## 53 2012-11-29   7047
```

2. Make a histogram of the total number of steps taken per day


```r
Counts1 <- as.numeric(Step_counts$Steps1)
hist(Counts1,main="Hisogram of total number of steps taken per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day. 

This summary of the data contains the mean and the median of the total number of steps taken per day. 


```r
summary(Counts1)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## What is the average daily activity pattern?

4. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 

The data covers a total of 53 days. We use this information to calculate the average. 



```r
Interval_counts1 <- aggregate(Steps1 ~ Interval1, data= Rawdata_df, sum)
Steps2 <- (as.numeric(Interval_counts1$Steps1))/53
Interval2 <- as.numeric(Interval_counts1$Interval1)
Interval_counts2 <- data.frame(Interval2, Steps2)
plot.ts(Interval2, y= Steps2, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


5. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Interval_counts2[which.max(Interval_counts2$Steps2),]
```

```
##     Interval2   Steps2
## 104       835 206.1698
```


## Imputing missing values

6. Calculate and report the total number of missing values in the dataset.


```r
sum(is.na(Raw_data$steps))
```

```
## [1] 2304
```

7. Devise a strategy for filling in all of the missing values in the dataset.

The strategy is to convert the NAs to 0's, because there is no data. 


```r
Steps3 <- Raw_data$steps
Steps3[is.na(Steps3)] <- 0
```

8. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
Filled_indata_df <- data.frame(Date1, Interval1, Steps3)
```

9. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
Step_counts2 <- aggregate(Steps3 ~ Date1, data= Filled_indata_df,sum)
Counts2 <- as.numeric(Step_counts2$Steps3)
hist(Counts2,main="Hisogram of total number of steps taken per day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
summary(Counts2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

These values differ from the estimates from the first part of the assignment. The impact is that the frequency of the observations with 0 steps per interval, increases; the mean and median total number of steps taken per day, decrease. 


## Are there differences in activity patterns between weekdays and weekends?

10. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
library(timeDate)
Rawdata_df$Week <- isWeekday(Rawdata_df$Date1)
Rawdata_df$Week[Rawdata_df$Week=="TRUE"]<-"Weekday"
Rawdata_df$Week[Rawdata_df$Week=="FALSE"]<-"Weekend"
```

11. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

First divide the data set into Weekdays and Weekends. 

Weekdays


```r
Weekdays_df_1 <- Rawdata_df[which(Rawdata_df$Week=="Weekday"),]
Weekdays_df_2 <- Weekdays_df_1[1:3]
Weekdays_df_2$Steps1 <- as.numeric(Weekdays_df_2$Steps1)
```

Then, calculate the total number of steps per day. 


```r
Weekdays_step_counts <- aggregate(Steps1 ~ Date1, data= Weekdays_df_2,sum)
Weekdays_step_counts
```

```
##         Date1 Steps1
## 1  2012-10-02    126
## 2  2012-10-03  11352
## 3  2012-10-04  12116
## 4  2012-10-05  13294
## 5  2012-10-09  12811
## 6  2012-10-10   9900
## 7  2012-10-11  10304
## 8  2012-10-12  17382
## 9  2012-10-15  10139
## 10 2012-10-16  15084
## 11 2012-10-17  13452
## 12 2012-10-18  10056
## 13 2012-10-19  11829
## 14 2012-10-22  13460
## 15 2012-10-23   8918
## 16 2012-10-24   8355
## 17 2012-10-25   2492
## 18 2012-10-26   6778
## 19 2012-10-29   5018
## 20 2012-10-30   9819
## 21 2012-10-31  15414
## 22 2012-11-02  10600
## 23 2012-11-05  10439
## 24 2012-11-06   8334
## 25 2012-11-07  12883
## 26 2012-11-08   3219
## 27 2012-11-12  10765
## 28 2012-11-13   7336
## 29 2012-11-15     41
## 30 2012-11-16   5441
## 31 2012-11-19   8841
## 32 2012-11-20   4472
## 33 2012-11-21  12787
## 34 2012-11-22  20427
## 35 2012-11-23  21194
## 36 2012-11-26  11162
## 37 2012-11-27  13646
## 38 2012-11-28  10183
## 39 2012-11-29   7047
```

There are 39 weekdays. We use this information to calculate the average step per interval per day. 


```r
Interval_counts_Weekdays <- aggregate(Steps1 ~ Interval1, data= Weekdays_df_2, sum)
Steps_weekdays <- (as.numeric(Interval_counts_Weekdays$Steps1))/39
Interval_weekdays <- as.numeric(Interval_counts_Weekdays$Interval1)
plot.ts(Interval_weekdays, y= Steps_weekdays, main="Weekdays Time Series Plot", type="l")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

And for the Weekends, 


```r
Weekends_df_1 <- Rawdata_df[which(Rawdata_df$Week=="Weekend"),]
Weekends_df_2 <- Weekends_df_1[1:3]
Weekends_df_2$Steps1 <- as.numeric(Weekends_df_2$Steps1)
```

Then, calculate the total number of steps per day. 


```r
Weekends_step_counts <- aggregate(Steps1 ~ Date1, data= Weekends_df_2,sum)
Weekends_step_counts
```

```
##         Date1 Steps1
## 1  2012-10-06  15420
## 2  2012-10-07  11015
## 3  2012-10-13  12426
## 4  2012-10-14  15098
## 5  2012-10-20  10395
## 6  2012-10-21   8821
## 7  2012-10-27  10119
## 8  2012-10-28  11458
## 9  2012-11-03  10571
## 10 2012-11-11  12608
## 11 2012-11-17  14339
## 12 2012-11-18  15110
## 13 2012-11-24  14478
## 14 2012-11-25  11834
```

There are 14 weekends. We use this information to calculate the average step per interval per day. 


```r
Interval_counts_Weekends <- aggregate(Steps1 ~ Interval1, data= Weekends_df_2, sum)
Steps_weekends <- (as.numeric(Interval_counts_Weekends$Steps1))/14
Interval_weekends <- as.numeric(Interval_counts_Weekends$Interval1)
plot.ts(Interval_weekends, y= Steps_weekends, main="Weekends Time Series Plot", type="l")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 


