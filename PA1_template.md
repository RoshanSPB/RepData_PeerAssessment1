
---
title: "PA1_template.Rmd"
author: "Roshan"
date: "December 30, 2018"
output: 
  html_document:
    keep_md: TRUE
---




```r
#setwd("./Project")
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
```

```r
#read data from the csv file
originalData = read.csv("activity.csv")

#change the date as Date class and format "%m-%d-%Y"
originalData$date <- as.Date(originalData$date,format = "%m/%d/%Y")
originalData$date <- format.Date(originalData$date,"%m-%d-%Y")

#remove NA rows from the data
originalData<-originalData[!is.na(originalData$steps),]
```

###[1] What is mean total number of steps taken per day?


```r
#read the original data from csv file
 originalData = read.csv("activity.csv")
 
#change the date as Date class and format "%m-%d-%Y"
 originalData$date <- as.Date(originalData$date,format = "%m/%d/%Y")
originalData$date <- format.Date(originalData$date,"%m-%d-%Y")

#select only !NA values
originalData<-originalData[!is.na(originalData$steps),]

#keep seperate calculation for for with NA
originalDataNA = read.csv("activity.csv")

# calculate sum of steps per date
data <- originalData %>%
        group_by(date) %>%
        summarise(totalSteps = sum(steps))%>%
        ungroup()
#construct the plot 
g <- ggplot(data, aes(data$totalSteps)) + 
  geom_histogram(breaks=seq(10, 22000, by=1000),col="grey",fill= "red") +
   ggtitle("Total Steps Counts per Date") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Interval") + 
  ylab("Total Step Counts")

#calculate the mean and mean of total steps
TotStepsMedian <- as.integer(median(data$totalSteps))
TotStepsMean <- as.integer(mean(data$totalSteps))
```


#### histogram of the total number of steps taken each day

![](PA1_template_files/figure-html/plot-1.png)<!-- -->


  Mean total-number of steps taken per day: **10765**

  Median total number of steps taken per day: **10766**


###[2] What is the average daily activity pattern?


```r
#calculate average of steps
aveDailyActivity <- originalData %>% 
                    group_by(interval) %>% 
                    summarise(aveSteps = mean(steps))

#calculate maximum of average steps
maxX <- max(aveDailyActivity$aveSteps)

#calcuate average daily activities with maximum aveSteps
maxY <- aveDailyActivity[aveDailyActivity$aveSteps==maxX,]
 
#set label
labels  = paste("[", paste(as.numeric(maxY[1,1]),
#set all maxY as numeric                           
ceiling(as.numeric(maxY[1,2])),sep=","),"]")

#plot interval vs aveSteps
g2 <- ggplot(aveDailyActivity, aes(aveDailyActivity$interval,aveDailyActivity$aveSteps )) + geom_line() +
  ggtitle("Average Steps Over 5min Intervals") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Time Interval (5min unites)") + ylab("Average Steps") +
  geom_text(aes(as.numeric(maxY[1,1] + 300), as.numeric(maxY[1,2])),label= labels)
```
####The average daily activity pattern

![](PA1_template_files/figure-html/the average daily activity pattern-1.png)<!-- -->

####Maximum number of steps - on average across all the days

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 
                          **835**
                          
###[3]Imputing missing values

####Calculate and report the total number of missing values in the dataset 


```r
#count total NA value rows in original data with NA
countsNA = count(originalDataNA,is.na(originalDataNA$step))

#set count numbers as numeric
totalNA <- as.numeric(countsNA[2,2])
```

The total number of rows with NAs is **2304**


####Devise a strategy for filling in all of the missing values in the dataset


```r
#read dataset again to make sure itis original
originalData = read.csv("activity.csv")

#change the date as Date class and format "%m-%d-%Y"
originalData$date <- as.Date(originalData$date,format = "%m/%d/%Y")
originalData$date <- format.Date(originalData$date,"%m-%d-%Y")

#original Data without NAs
originalDataWithoutNA<-originalData[!is.na(originalData$steps),]

#read data to use with NAs
dataWithNA = read.csv("activity.csv")

#change the date as Date class and format "%m-%d-%Y"
dataWithNA$date <- as.Date(dataWithNA$date,format = "%m/%d/%Y")
dataWithNA$date <- format.Date(dataWithNA$date,"%m-%d-%Y")

#calculate the average steps without NAs over interval
aveDailyActivityWtNA <- originalDataWithoutNA %>% 
                      dplyr::group_by(interval) %>% 
                      dplyr::summarise(aveSteps = mean(steps))

aveDailyActivityWtNA$aveSteps <- ceiling(aveDailyActivityWtNA$aveSteps )

#sum over new aveDailyActivityWtNA set aveSteps
dataNA<- aveDailyActivityWtNA %>%
    dplyr::summarise(totalSteps = sum(aveSteps))

dataNA$totalSteps <- ceiling(dataNA$totalSteps)

#calculate sum over all original data with NA
 data <- dataWithNA %>%
   group_by(date) %>%
   summarise(totalSteps = sum(steps))%>%
   ungroup()
 
 #mutate dataset that is equal to the original dataset but with the missing data
 data <- data %>% mutate(totalSteps = ifelse(is.na(totalSteps), dataNA, totalSteps))
 
 #set totalSteps as integer to match origina type -- NEW Data
 data$totalSteps <- as.integer(data$totalSteps)

  #construct the plot
g3 <- ggplot(data, aes(data$totalSteps)) + 
      geom_histogram(breaks=seq(10, 22000, by=1000),col="grey",fill= "red") +
      labs(title = "                      Total Steps Counts per Date") +
      xlab("Total Steps") + 
      ylab("Total Step Counts")
```


#### Histogram of the total number of steps taken each day with NA replaced

![](PA1_template_files/figure-html/total number of steps taken each day with NA replaced-1.png)<!-- -->

#### Mean and Median total number of steps taken per day with replaced NAs


 Mean total-number of steps taken per day with replaced NAs: **10909**

 Median total number of steps taken per day with replaced NAs: **10784**

#### Difference in Mean and Median total number of steps taken per day w/out replaced NAs
 Mean difference = **144**

 Median difference = **18**

 Percentage of mean diffrence =  **1.3376684**

 Percentage of median diffrence =  **0.167193**
 
 ! Replacing NAs with averaged sum over interals wont impact significatly on data


###Are there differences in activity patterns between weekdays and weekends?



####Plot of average number of steps taken, averaged across all weekday days or weekend days

![](PA1_template_files/figure-html/plot average number of steps taken, averaged across all weekday days or weekend days-1.png)<!-- -->
