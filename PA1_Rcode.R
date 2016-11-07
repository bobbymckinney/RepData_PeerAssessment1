library(plyr)
library(dplyr)
library(ggplot2)

## clear current workspace
rm(list = ls())

if (!file.exists("./data/activity.csv")){
    ## get the current working directory and define the data folder
    mainDir <- getwd()
    dir.create(file.path(mainDir, "data"), showWarnings = FALSE)
    
    ## unzip the data
    zipfile <- "./activity.zip"
    unzip (zipfile, exdir = "./data")
}
datafile <- paste("./data/",list.files(path = "./data")[1],sep="")

## Calculate memory required and prompt user whether or not to proceed
memory_required <- 3 * 17568 * 8
print(paste("Memory required: ",round(memory_required/2^20,digits = 1)," MB",sep=""))

## Read the data into R
activitydata <- read.csv(datafile)

## Transform date variable to date format
activitydata$date <- as.Date(activitydata$date)

### What is mean total number of steps taken per day?

## Select just dates and steps and filter out NA values
steps_per_day <- filter(select(activitydata, c(date, steps)), !is.na(steps))

## find the total number of steps per day
steps_per_day <- ddply(steps_per_day,c("date"),colwise(sum))

g <- ggplot(aes(x=steps),data=steps_per_day)
g + geom_histogram(bins=20) + 
    labs(x = "Steps", y = "Count", title = "Total Steps per Day") + 
    theme_bw()

mean(steps_per_day$steps)
median(steps_per_day$steps)

### What is the average daily activity pattern?

## Select just intervals and steps and filter out NA values
steps_per_interval <- filter(select(activitydata, c(interval, steps)), !is.na(steps))

## find the average steps per interval
steps_per_interval <- ddply(steps_per_interval,c("interval"),colwise(mean))

g2 <- ggplot(aes(x = interval, y = steps), data = steps_per_interval)
g2 + geom_line() + 
    labs(x = "Interval", y = "Average Steps", title = "Average Steps per Interval") + 
    theme_bw()

max_int_steps <- max(steps_per_interval$steps)
steps_per_interval[steps_per_interval$steps==max_int_steps,]

### Imputing missing values

sum(is.na(activitydata$steps))
imputed_activitydata <- activitydata
na_subs <- which(is.na(imputed_activitydata$steps))

for (i in na_subs) {
    sub_interval <- imputed_activitydata[i,]$interval
    imputed_activitydata[i,]$steps <- steps_per_interval[steps_per_interval$interval==sub_interval,]$steps
}

## Select just dates and steps and filter out NA values
imputed_steps_per_day <- filter(select(imputed_activitydata, c(date, steps)))

## find the total number of steps per day
imputed_steps_per_day <- ddply(imputed_steps_per_day,c("date"),colwise(sum))

g3 <- ggplot(aes(x=date,y=steps),data=imputed_steps_per_day)
g3 + geom_bar(stat="identity") + 
    labs(x = "Date", y = "Total Steps", title = "Total Steps per Day") + 
    theme_bw()

mean(imputed_steps_per_day$steps)
median(imputed_steps_per_day$steps)


### Are there differences in activity patterns between weekdays and weekends?
weekend <- c("Saturday","Sunday")
activitydata <- mutate(activitydata, week = weekdays(date))
activitydata$week <- activitydata$week %in% weekend
activitydata$week <- factor(activitydata$week, labels = c("weekday", "weekend"))

steps_per_interval_day <- filter(select(activitydata, c(interval, steps,week)), !is.na(steps))

## find the average steps per interval
steps_per_interval_day <- ddply(steps_per_interval_day,c("interval","week"),colwise(mean))

g4 <- ggplot(aes(x = interval, y = steps), data = steps_per_interval_day)
g4 + geom_line() + facet_grid(. ~ week) +
    labs(x = "Interval", y = "Average Steps", title = "Average Steps per Interval") + 
    theme_bw()

