# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
```

## What is mean total number of steps taken per day?

* Calculate the total number of steps taken per day

```r
steps_data <- data[!is.na(data$steps),]
steps_data <- aggregate(steps ~ date, data = steps_data, FUN = sum)
```

* If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(steps_data$steps, col = "red", xlab = "Steps", main = "Total Steps Taken Each Day")
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

* Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_data$steps)
```

```
## [1] 10766.19
```

```r
median(steps_data$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
int_data <- aggregate(steps~interval, data = data, FUN = mean , na.rm = TRUE )
plot(int_data$interval, int_data$steps, type = "l",ylab="Steps",xlab="Interval")
```

![](PA1_template_files/figure-html/time series-1.png)<!-- -->

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_row <- which.max(int_data$steps)
int_data[max_row, "interval"]
```

```
## [1] 835
```
## Imputing missing values
* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)




```r
num_missing <- sum(is.na(data$steps))
num_missing
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# I will Use the Mean number of steps for the 5 minute 
# interval calculated from above to fill the 
# missing values
```
* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataFilled <- data
for (i in 1:nrow(dataFilled))
{
  if(is.na(dataFilled[i, "steps"]))
  {
    dataFilled[i,"steps"] <- int_data[dataFilled[i, "interval"] == int_data$interval,"steps"]
  }
}
sum(is.na(dataFilled))
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
filled_steps_data <- aggregate(steps ~ date, data = dataFilled, FUN = sum)
hist(filled_steps_data$steps, col = "red", xlab = "Steps", main = "Total Steps Taken Each Day")
```

![](PA1_template_files/figure-html/histogram w filled data-1.png)<!-- -->

```r
mean(filled_steps_data$steps)
```

```
## [1] 10766.19
```

```r
median(filled_steps_data$steps)
```

```
## [1] 10766.19
```


```r
# There is no variation for the mean from the first part of the assignment. 
# The median however increased and is now the same value as the mean. 
# Imputing the missing values increased the frequency of steps between 10,000 and 15,000.
# However, the rest of the frequencies show little to know change.
```

## Are there differences in activity patterns between weekdays and weekends?
* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dataFilled$day <- ifelse(as.POSIXlt(as.Date(dataFilled$date))$wday == 0 | as.POSIXlt(as.Date(dataFilled$date))$wday == 6, "weekend", "weekday")
dataFilled$day <- factor(dataFilled$day, levels = c("weekday", "weekend"))
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
interval_data2 <- aggregate(steps ~ interval + day, dataFilled, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = interval_data2, aspect = 1/2, 
    type = "l")
```

![](PA1_template_files/figure-html/plotting weekday weekend-1.png)<!-- -->
