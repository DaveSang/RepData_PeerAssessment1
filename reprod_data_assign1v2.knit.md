# Peer Assessment 1
DaveS  
18 December 2015  


```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.2.3
```

```r
opts_chunk$set(echo = TRUE, results = 'hold')
```

```r
library(data.table)
library(ggplot2)
library(chron)# we shall use ggplot2 for plotting figures
```

## Loading and preprocessing the data

```r
echo=TRUE
setwd("~/")
tbl<-read.csv("activity.csv", header=TRUE, sep=",", colClasses=c("numeric", "character", "numeric"))
```

Pre-processing/transformation of the data - Conversion of the date field to Date class and interval field to Factor class.

```r
tbl$date <- as.Date(tbl$date, format = "%Y-%m-%d")
tbl$interval <- as.factor(tbl$interval)
```

A check of the form of the data

```r
str(tbl)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```


## What is mean total number of steps taken per day?

There are a lot of NA's present, we are allowed to ignore the missing (NA) values

```r
echo = TRUE
step_per_d <- aggregate(steps ~ date, tbl, sum)
colnames(step_per_d) <- c("date","Steps")
head(step_per_d)
tail(step_per_d)
summary(step_per_d)
```

```
##         date Steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
##          date Steps
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
##       date                Steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

Histogram for total number of steps per day

```r
ggplot(step_per_d, aes(x = Steps)) + 
       geom_histogram(fill = "grey", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Frequency") + theme_bw()
```

![](reprod_data_assign1v2.knit_files/figure-html/unnamed-chunk-7-1.png) 

Now, some accompanying summary statistics

```r
Steps_mean <- mean(step_per_d$Steps, na.rm=TRUE)
Steps_mean
Steps_median <- median(step_per_d$Steps, na.rm=TRUE)
Steps_median
```

```
## [1] 10766.19
## [1] 10765
```
The mean is 10766.19 and the median is 10765.

## What is the average daily activity pattern?
Aggregation of steps by intervals of 5-minutes and convertion of the intervals to integers occurs.

```r
steps_per_interval <- aggregate(tbl$steps, 
                                by = list(interval = tbl$interval),
                                FUN=mean, na.rm=TRUE)
tail(steps_per_interval)
#convert to integers
##this helps in plotting
steps_per_interval$interval <- 
        as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")
```

```
##     interval         x
## 283     2330 2.6037736
## 284     2335 4.6981132
## 285     2340 3.3018868
## 286     2345 0.6415094
## 287     2350 0.2264151
## 288     2355 1.0754717
```

We make the plot with the time series of the average number of steps taken (averaged across all days) versus the 5-minute intervals:

```r
ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="red", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw()
```

![](reprod_data_assign1v2.knit_files/figure-html/unnamed-chunk-10-1.png) 


The maximum 206 steps was seen at the 835th interval.

## Imputing missing values
First, we find the missing values

```r
# Total number of missing values
missing_values <- sum(is.na(tbl$steps))

missing_values
```

```
## [1] 2304
```
There are 2304 missing values.

Next, we fill all 2304 missing values and make a check to see of the data has been replaced.

```r
na_fill <- function(data, pervalue) {
        na_index <- which(is.na(data$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- data$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

tbl_fill <- data.frame(  
        steps = na_fill(tbl, steps_per_interval),  
        date = tbl$date,  
        interval = tbl$interval)
head(tbl_fill)
str(tbl_fill)

sum(is.na(tbl_fill$steps))
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
## [1] 0
```
[1] 0 Means there are no missing values.

A new histogram with the now 'found values'

```r
fill_steps_per_day <- aggregate(steps ~ date, tbl_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")

##plotting the histogram
ggplot(fill_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "pink", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day - no missing values", 
             x = "Number of Steps per Day", y = "Frequency") + theme_bw() 
```

![](reprod_data_assign1v2.knit_files/figure-html/unnamed-chunk-13-1.png) 

And the accompanying summary statistics

```r
steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_mean_fill
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill
```

```
## [1] 10766.19
## [1] 10766.19
```

Median=Mean=10766.19 as opposed to the previous Median = 10766.19 and the median is 10765 and Mean = 10766.19

## Are there differences in activity patterns between weekdays and weekends?
We no longer have missing values.
1. Augment the table with a column that indicates the day of the week
2. Subset the table into two parts - weekends (Saturday and Sunday) and #weekdays (Monday through Friday).#3. Tabulate the average steps per interval for each data set.
4. Plot the two data sets side by side for comparison.

We now compare weekends vs weekdays

```r
library(lattice)
weekdays_steps <- function(data) {
    weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                          FUN=mean, na.rm=T)
    # convert to integers for plotting
    weekdays_steps$interval <- 
            as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
    colnames(weekdays_steps) <- c("interval", "steps")
    weekdays_steps
}

data_by_weekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(data$date)) # weekdays
    weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))

    weekend_steps <- weekdays_steps(weekend_data)
    weekday_steps <- weekdays_steps(weekday_data)

    weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
    weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

    data_by_weekdays <- rbind(weekend_steps, weekday_steps)
    data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
    data_by_weekdays
}

data_weekdays <- data_by_weekdays(tbl_fill)
```

And now we plot to visually compare weekdays vs weekends.

```r
ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="violet") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
```

![](reprod_data_assign1v2.knit_files/figure-html/unnamed-chunk-16-1.png) 

Activity on the weekdays has the greatest peak from all step intervals. The viewer can see that weekend activities has more peaks than weekdays. This is probably related to activities on weekdays being impacted by work related routines - more intense activity in the little a free time available between home, work and then home/social activities. On the weekend we can see a more even distribution of effort along the time axis.
