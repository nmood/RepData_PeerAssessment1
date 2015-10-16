# Reproducible Research: Peer Assessment 1


# Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data
at 5 minute intervals through out the day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and include the number of steps
taken in 5 minute intervals each day.


## Loading and preprocessing the data
Throughout your report make sure you always include the code that you used to generate the output you
present. When writing code chunks in the R markdown document, always use echo = TRUE so that
someone else will be able to read the code.

Therefore, we need to set **echo** equal to **TRUE** and **result** equal to **hold.**


```r
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
```


### Load required library
The library required for this assignment are:


```r
library(data.table)
library(ggplot2) # use ggplot2 for plotting figures
```

### Load required data
To load the Activity data (which is in the current working directory) by using this statement:


```r
rdata <- read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
```
                  
### Preproccess the data
After we load the data, we need to process the data into a format suitable for our analysis.

So, we convert the **date** field to Date class and **interval** field to Factor class.

```r
rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
rdata$interval <- as.factor(rdata$interval)
```

To check the data using this statement:

```r
str(rdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

## What is mean total number of steps taken per day?
To get the mean total number of steps taken per day, we ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day.


```r
steps_per_day <- aggregate(steps ~ date, rdata, sum)
colnames(steps_per_day) <- c("date","steps")
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2.Make a histogram of the total number of steps taken each day.


```r
ggplot(steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "green", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

3.Calculate the **mean** dan **median** of the number of steps taken per day.


```r
steps_mean   <- mean(steps_per_day$steps, na.rm=TRUE)
steps_median <- median(steps_per_day$steps, na.rm=TRUE)
```

The mean is **10766.189** and median is **10765**.


## What is the average daily activity pattern?
To get the average daily activity pattern:

1.Calculate the aggregation of steps by 5-minute interval and convert the intervals as integer and save then in a data frame called **steps_per_interval**.


```r
steps_per_interval <- aggregate(rdata$steps, 
                                by = list(interval = rdata$interval),
                                FUN=mean, na.rm=TRUE)
#convert to integers
##this helps in plotting
steps_per_interval$interval <- 
        as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")
```


2.Make a time series plot of the 5-minute interval(x-axis) dand the average number of steps yaken, averaged across all days (y-axis).


```r
ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="orange", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 


3.Find the 5-minute interval with the containing the maximum number of steps.


```r
max_interval <- steps_per_interval[which.max(  
        steps_per_interval$steps),]
```

The **835****^th^** interval has maximum **206** steps.


## Imputing missing values
There are a number of days/intervals where there are missing values (coded as NA ). The
presence of missing days may introduce bias into some calculations or summaries of the data.


1.calculate and report the total of missing values in the dataset.


```r
missing_vals <- sum(is.na(rdata$steps))
```

The total number of missing values are **2304**.


2.Devise a strategy for filling in all of the missing values in the dataset.


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
```


3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
rdata_fill <- data.frame(  
        steps = na_fill(rdata, steps_per_interval),  
        date = rdata$date,  
        interval = rdata$interval)
str(rdata_fill)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

To check is there any missing values remaining:


```r
sum(is.na(rdata_fill$steps))
```

```
## [1] 0
```

Zero output shows that there is NO MISSING VALUES.


4.Make a histogram of the total number of steps taken each day and Calculate and report the mean
and median total number of steps taken per day. 


```r
fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")

##plotting the histogram
ggplot(fill_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 

```r
steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
```

The mean is **10766.189** and median is **10766.189**.   
  
####**Do these values differ from the estimates from the first part of the assignment?**

The values of the **mean** are **same** but the values of the **median** are **slightly different**.  
  
####**What is the impact of imputing missing data on the estimates of the total daily number of steps?**

From our calculation we can see that the values of median after imputing missing data has shifted to the values of mean. It seems that the impact of imputing missing values has increased our peak, but it's not affect our prediction.


## Are there differences in activity patterns between weekdays and weekends?

To find the differences in activity patterns between weekdays dan weekends, we use the dataset with the filled-in missing values.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating
whether a given date is a weekday or weekend day.

2.Make a panel plot containing a time series plot (i.e. type = "l" ) of the 5???minute interval (x-axis) and
the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
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

data_weekdays <- data_by_weekdays(rdata_fill)


ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="violet") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

From the graph above, we can see there are differences in activity patterns between weekdays and weekends.    





