---
title: "Reproducible Research Project 1"
author: "Nikhilesh Jha"
date: "3/9/2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
Process/transform the data (if necessary) into a format suitable for your analysis

 Contains code for reading in the dataset and/or processing the data

We start by doing preliminary work such as loading the libraries. Then we read the data and store it in the variable data. Then we convert the date variable to date data type and remove all the NAs from the dataset.

```{R}
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))

dat <- read.csv("/Users/apple/Desktop/Coursera/John Hopkins Stats Course/Reproducible Research/activity.csv")
dat$date <- as.Date(dat$date)
dat <- na.omit(dat)
```

# What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


Contains a histogram for number of steps taken per day.

```{r}
tot <- aggregate(dat$steps,list(dat$date),sum)
qplot(tot$x,xlab="Total Number of Steps",ylab = "Frequency",binwidth=500)
```



Mean and median number of steps taken each day

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
avg <- aggregate(dat$steps, list(dat$date),mean)
avg
```

```{r}
aggregate(dat$steps, list(dat$date),median)
```

# What is the average daily activity pattern?


 Contains the Time series plot of the average number of steps taken

```{r}
five_min_steps <- aggregate(steps ~ interval, data = dat, FUN =mean)
plot1 <- ggplot(data = five_min_steps, aes(x = interval, y = steps)) + 
    geom_line() +
    xlab("Time Intervals") + 
    ylab("Total Number of Steps")
print(plot1)

```

The 5-minute interval that, on average, contains the maximum number of steps


```{r}
max_step <- order(dat$steps,decreasing = TRUE)[1]
(dat$date)[max_step]
```

# Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Contains Code to describe and show a strategy for imputing missing data

```{r}
all_data <- read.csv("/Users/apple/Desktop/Coursera/John Hopkins Stats Course/Reproducible Research/activity.csv")
all_data$date <- as.Date(all_data$date)
all_avg <- aggregate(all_data$steps, list(all_data$date),mean)
replacement <- function(d) {
  ind <- which(all_avg$Group.1==d)
  avg$x[ind]
}
for (i in 1:nrow(all_data))
{
  if (is.na(all_data$steps[i]))
    all_data$steps[i]<-replacement(all_data$date[i])
}

avg_all <- aggregate(all_data$steps, list(all_data$date),mean)
avg_all
median_all <- aggregate(all_data$steps, list(all_data$date),median)
median_all
```

Histogram of the total number of steps taken each day after missing values are imputed


```{r}
tot_all <- aggregate(all_data$steps,list(all_data$date),sum)

qplot(tot_all$x,xlab="Total Number of Steps",ylab = "Frequency",binwidth=500)
```


Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r}

daytype <- function(day) {
  if (day == "Saturday" | day == "Sunday")
    return("Weekday")
  else if (day == "Monday" | day == "Tuesday" | day == "Wednesday" | day == "Thursday" | day == "Friday")
    return("Weekend")
  else
    stop("Incorrect value given")
}

all_data$dayType <- sapply(weekdays(all_data$date),daytype)
all_data$dayType <- as.factor(all_data$dayType)
data_weekday <- all_data[which(all_data$dayType=="Weekday"),]
data_weekend <- all_data[which(all_data$dayType=="Weekend"),]
five_min_steps_wd <- aggregate(steps ~ interval, data = data_weekday, FUN =mean)
five_min_steps_we <- aggregate(steps ~ interval, data = data_weekend, FUN =mean)
plot_wd <- ggplot(data = five_min_steps_wd, aes(x = interval, y = steps)) + 
    geom_line() +
    xlab("Time Intervals") + 
    ylab("Total Number of Steps")
print(plot_wd)
```
```{r}
plot_we <- ggplot(data = five_min_steps_we, aes(x = interval, y = steps)) + 
    geom_line() +
    xlab("Time Intervals") + 
    ylab("Total Number of Steps")
print(plot_we)
```

We can see the variation between the two time series plots.



Additional checking plot for imputed data 

```{r}
all_avg_new <- aggregate(all_data$steps, list(all_data$date),mean)
ggplot(data = all_avg_new,aes(Group.1,x)) + geom_line()

```

We can see that this plot is exactly the same with average of NAs removed. Therefore, imputation was performed succesfully.
