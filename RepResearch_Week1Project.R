# Install downlaoder package
install.packages("downloader")
library(downloader)

# Install dplyr package
install.packages("dplyr")
library(dplyr)

# Install ggplot2 package
install.packages("ggplot2")
library(ggplot2)

# Install other required packages
install.packages("knitr")
library(knitr)

install.packages(
        "https://cran.rstudio.com/bin/windows/contrib/3.3/htmltools_0.3.zip", 
        repos = NULL, type = "source"
)

library(htmltools)

install.packages("caTools")
library(caTools)

install.packages(
        "https://cran.rstudio.com/bin/windows/contrib/3.3/rmarkdown_0.9.2.zip", 
        repos = NULL, type = "source"
)
library(rmarkdown)

# Set Working Directory
# setwd("D:\\Applcation-Docs-Src\\SelfStudy\\Coursera\\RProgramming\\ReproducibleResearch\\Week1")

## 1. Code Below for reading in the dataset and/or processing the data

# Acivity Monitoring Data URL
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# Dowload acitivity monitoring data file into data directory
if (!dir.exists("data")) {
        dir.create("data")
}
download(url, dest=".\\data\\activity.zip", mode="wb") 
unzip (".\\data\\activity.zip", exdir = ".//data")

# Load data into R from .csv
activity <- read.csv(".\\data\\activity.csv", header=TRUE)

# Summaries
names(activity)
dim(activity)
head(activity)
str(activity)

# Add a new variable datetime created using date & interval variables
activity$datetime <- as.POSIXct(paste(activity$date, 
                                      sprintf("%04.0f", activity$interval)), 
                                format="%Y-%m-%d %H%M")

# Transform date variable from factor to date
activity$date <- as.Date(activity$date)

# Add a new variable month; create it from date values.
activity$month <- as.numeric(format(activity$date, "%m"))



## 2. Histogram of the total number of steps taken each day

# Total No of Steps taken per day
by_date <- group_by(activity, date, month)
sum_by_date <- summarise(by_date, totalsteps=sum(steps))

# Plot Histogram of the total number of steps taken each day
ggplot(sum_by_date, aes(date, totalsteps)) + 
        geom_bar(stat = "identity", colour = "blue", fill="red",width = 1) + 
        facet_grid(. ~ month, scales = "free") + 
        labs(title = "Histogram of total number of steps taken each day", 
             x = "Date/Day", y = "Total number of steps")
dev.copy(png,'RepReasearch_Hist1.png')
dev.off()

## 3. Mean and median number of steps taken each day
# Mean
mean(sum_by_date$totalsteps, na.rm=TRUE)

# Median
median(sum_by_date$totalsteps, na.rm=TRUE)

## 4. Time series plot of the average number of steps taken

# Avg No of Steps taken per 5 minute interval
by_interval <- group_by(activity, interval)
avg_by_interval <- summarise(by_interval, avgsteps=mean(steps, na.rm=TRUE))

# Plot Time Series of the avg no of steps taken per 5-minute interval
ggplot(avg_by_interval, aes(interval, avgsteps)) + 
        geom_line(colour = "darkblue") + 
        labs(title = "Time Series - Avg No of Steps Taken per 5-Minute Interval"
             , x = "5-Minute Interval", y = "Avg Steps")
dev.copy(png,'RepReasearch_TimeSeries1.png')
dev.off()

## 5. The 5-minute interval that, on average, contains the maximum number of 
# steps

maxavgstpes <- max(avg_by_interval$avgsteps)
avg_by_interval[avg_by_interval$avgsteps==maxavgstpes, ]$interval

## 6. Code to describe and show a strategy for imputing missing data

# Observations/records with missing steps values in the dataset
#activity[is.na(activity$steps),]

# Count of Observations/records with missing steps values in the dataset
count(activity[is.na(activity$steps),])

# Create a new dataset from original dataset but Imput missing values with avg 
# of that 5-minute interval
activity_no_NAs <- activity
for (i in 1:nrow(activity_no_NAs)) {
        if (is.na(activity_no_NAs[i, ]$steps)) {
                affected_interval <- activity_no_NAs[i, ]$interval
                activity_no_NAs[i, ]$steps <- 
                        avg_by_interval[avg_by_interval$interval==
                                                affected_interval, ]$avgsteps
        }
}

## 7. Histogram of the total number of steps taken each day after missing values
## are imputed

# Total No of Steps taken per day
by_date <- group_by(activity_no_NAs, date, month)
sum_by_date <- summarise(by_date, totalsteps=sum(steps))

# Plot Histogram of the total number of steps taken each day
ggplot(sum_by_date, aes(date, totalsteps)) + 
        geom_bar(stat = "identity", colour = "blue", fill="red",width = 1) + 
        facet_grid(. ~ month, scales = "free") + 
        labs(title = "Histogram of total number of steps taken each day", 
             x = "Date/Day", y = "Total number of steps")
dev.copy(png,'RepReasearch_Hist2.png')
dev.off()

# Mean
mean(sum_by_date$totalsteps, na.rm=TRUE) # [1] 10766.19

# Median
median(sum_by_date$totalsteps, na.rm=TRUE) # [1] 10766.19

## 8. Panel plot comparing the average number of steps taken per 5-minute 
## interval across weekdays and weekends

# Add new variables (day & week) to imputted dataset
activity_no_NAs$day <- weekdays(activity_no_NAs$date)
activity_no_NAs$week <- "weekday"
activity_no_NAs[activity_no_NAs$day == "Saturday" | 
                        activity_no_NAs$day == 'Sunday',]$week = "weekend"

# Avg No of Steps taken per 5 minute interval
by_week <- group_by(activity_no_NAs, week, interval)
avg_by_week <- summarise(by_week, avgsteps=mean(steps, na.rm=TRUE))

# Plot Time Series of the avg no of steps taken per 5-minute interval
ggplot(avg_by_week, aes(interval, avgsteps)) + 
     geom_line(colour = "darkblue") + 
     facet_wrap(~ week, ncol=1, nrow=2) + 
     labs(title = "Time Series - Avg No of Steps Taken per 5-Minute Interval", 
          x = "Interval", y = "Number of steps")
dev.copy(png,'RepReasearch_TimeSeries2.png')
dev.off()