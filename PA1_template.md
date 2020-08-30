---
title: "Week 2 Reproducible Research RMD format"
author: "Wong Hui Shin"
date: "August 30, 2020"
output:
  pdf_document: default
  html_document: default
---

## R Markdown
This is the Markdown of documentation on the daily activity pattern.  

## Loading and preprocessing the data
1) Code for reading in the dataset and/or processing the data

```{r setup data or load the data}
week2 = read.csv("activity.csv")
str(week2)
library(ggplot2)
```

## What is mean total number of steps taken per day?
2) Histogram of the total number of steps taken each day

```{r plot in base 1, echo=TRUE}
steps_by_day <- aggregate(steps ~ date, week2, sum)
hist(steps_by_day$steps, main = ("Total Number of Steps Each Day"), col="red",xlab="Number of Steps")
```

3) Mean and median number of steps taken each day

```{r code to find mean}
rmean <- mean(steps_by_day$steps)
rmean
```

```{r code to find median}
rmedian <- median(steps_by_day$steps)
rmedian
```

## What is the average daily activity pattern?
4) Time series plot of the average number of steps taken

```{r plot in base 2, echo=TRUE}
steps_by_interval <- aggregate(steps ~ interval, week2, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

5) The 5-minute interval that, on average, contains the maximum number of steps

```{r to find maximun steps during intervals}
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval
```

## Imputing missing values
6) Code to describe and show a strategy for imputing missing data

```{r for a strategy for imputing missing data, Strategy 1 Calculate total number of missing data}
NATotal <- sum(!complete.cases(week2))
NATotal
```


```{r for a strategy for imputing missing data, Strategy 2 Calculate missing data using the mean of the day}
StepsAverage <- aggregate(steps ~ interval, data = week2, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(week2)) {
    obs <- week2[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```


```{r for a strategy for imputing missing data, Strategy 3 New dataset creation which included the missing data}
new_activity <- week2
new_activity$steps <- fillNA
```

7) Histogram of the total number of steps taken each day after missing values are imputed

```{r plot for histogram of total steps taken each day after missing data imputed, echo=TRUE}
StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps")
#Create Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("red", "blue"), lwd=10)
```

7a) Calculate and report the mean of total number of steps taken per day. 

```{r for mean calculation}
rmeantotal <- mean(StepsTotalUnion$steps)
rmeantotal
```

7b) Calculate and report the median of total number of steps taken per day. 

```{r for median calculation}
rmediantotal <- median(StepsTotalUnion$steps)
rmediantotal
```

7c) Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r for diffrences calculation}
rmediandiff <- rmediantotal - rmedian
rmediandiff
```


```{r for the effects after calculation}
rmeandiff <- rmeantotal - rmean
rmeandiff
```




## Are there differences in activity patterns between weekdays and weekends?
8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r plot for activity patterns on weekdays and weekends, echo=TRUE}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, mean)
library(lattice)
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
