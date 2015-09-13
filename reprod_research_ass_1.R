echo = TRUE

setwd("RepData_PeerAssessment1")

library(utils)

## Unzip the activity data if the 'activity_data'
## dir has not been created. It assumes the 
## data is already unzipped if the directory 
## has been created.
if (!dir.exists("activity_data")) {
  dir.create("activity_data")
  unzip("activity.zip",exdir="activity_data")
}


## Load the data.

## read the csv file in the 'activity_data'
## directory.
actData <- read.csv("./activity_data/activity.csv",
                    colClasses=c("numeric",
                                 "character",
                                 "numeric"))


## Preprocess the data
## - Remove any rows with an NA value in a column
actData <- actData[!is.na(actData$steps),]
actData <- actData[!is.na(actData$date),]
actData <- actData[!is.na(actData$interval),]

## - Remove any rows where the number of steps is
##   zero.
actData <- actData[!(actData$steps==0),]

## Calculate the mean total number of steps taken
## per day

## 1. Add up number of steps for each day
library(lubridate)
library(dplyr)
tmp <- data.frame(actData)
tmp <- data.frame(steps=actData$steps,
                  DATE=parse_date_time(actData$date,"%y-%m-%d"))

actData_byDay <- group_by(tmp,DATE)
actData_byDay <- summarise(actData_byDay,
                           totalStepsPerDay = sum(steps),
                           numOfRowsPerDay = n_distinct(steps))
head(actData_byDay)

## 2. Make a histogram of the total number of
##    steps taken per day.
library(ggplot2)
tmp2 <- mutate(actData_byDay,
               numOfRowsPerDay=NULL)
stepsHist <- qplot(tmp2,
                   geom="histogram",
                   x=tmp2$DATE,
                   y=as.numeric(tmp2$totalStepsPerDay),
                   stat="identity",
                   ylab="Total # Steps",
                   xlab="Date")
stepsHist

## 3. Calculate the mean and median of the total
##    number of steps taken per day.
actData_byDay <- group_by(tmp,DATE)
actData_byDay <- summarise(actData_byDay,
                           meanSteps = mean(steps),
                           medianSteps = median(steps))
head(actData_byDay)


## What is the average daily activity pattern?

## 1. Time seris plot of the 5-minute interval
##    (x-axis) and the average number of steps
##    taken, averaged across all days (y-axis).

# type = "l"

# get number of days covered in the data
firstDay = min(levels(factor(actData[,"date"])))
lastDay = max(levels(factor(actData[,"date"])))
numDays <- as.POSIXlt(lastDay) - as.POSIXlt(firstDay)
numDays <- round(as.numeric(numDays) + 1)

# 
actData5minInt <- group_by(actData,interval)
actData5minInt <- summarise(actData5minInt,
                            totalSteps = sum(steps),
                            meanSteps = sum(steps)/numDays)

# plot the time series
fiveMinIntPlot <- plot(x=actData5minInt$interval,
                       y=actData5minInt$meanSteps,
                       type="l",
                       xlab="5-min interval",
                       ylab="Avg. steps in interval",
                       xlim=c(0,2500))
## 2. Get the 5-minute interval, on average across
##    all days in the dataset, with the maximum
##    number of steps.
d <- actData5minInt[actData5minInt$meanSteps>184,]["interval"][1,1]
intervalWithMaxAverage <- d[[1,1]]
