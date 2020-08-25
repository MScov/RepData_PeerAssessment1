---
title: "PA1_template"
output: html_document
---

Loading and preprocessing the data

```{r, echo = T}
df <- read.csv("activity.csv")
```

What is mean total number of steps taken per day?

```{r, echo = T}
#sum total steps for each day
stepsday <- aggregate(steps ~ date, df, sum, na.rm = T)
#histogram steps per day
hist(stepsday$steps)
#mean and median steps per day
summary(stepsday$steps)
```

What is the average daily activity pattern?

```{r, echo = T}
dailyavg <- aggregate(steps ~ interval, df, mean, na.rm = T)
#time series plot
plot(dailyavg$interval,dailyavg$steps, type = "l")
#which 5-min inter contains the maximum nuber of steps?
dailyavg[which.max(dailyavg$steps),]$interval

```

Imputing missing values

```{r, echo = T}
# find number of NA
summary(df)
# missing values = 2304

#impute missing data using mice
library(mice)
missing <- mice(df)
nomiss <- complete(missing, 1) #use first iteration 
summary(nomiss)

#histogram of steps per day & means and median
#sum total steps for each day
stepsdaynomiss <- aggregate(steps ~ date, nomiss, sum)

#histogram steps per day (no missing)
hist(stepsdaynomiss$steps)
#histogram steps per day (missing)
hist(stepsday$steps)

### histograms similar

#mean and median steps per day (no missing)
summary(stepsdaynomiss$steps)
#mean and median steps per day (missing)
summary(stepsday$steps)

```

Are there differences in activity patterns between weekdays and weekends?

```{r, echo = T}
#add name of day to each date
nomiss$day <- weekdays(as.Date(nomiss$date))
#recode variables
library(dplyr)
nomiss$weekendday <- recode(nomiss$day, Monday = "Weekday", Tuesday = "Weekday",
                              Wednesday = "Weekday", Thursday = "Weekday",
                              Friday = "Weekday", Saturday = "Weekend",
                             Sunday = "Weekend")

#plot
stepsdayinter <- aggregate(steps ~ interval + weekendday, nomiss, mean)
library(ggplot2)
plot <- ggplot(stepsdayinter, aes(x= interval, y = steps, color = weekendday))+
        geom_line()+
        facet_wrap(~weekendday, ncol = 1, nrow = 2) #separate into two graphs
plot
```