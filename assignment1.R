library("ggplot2")
act <- read.csv("activity.csv")

agg <- aggregate(steps~date, act, sum)

ggplot(agg, aes(steps)) + geom_histogram(binwidth = 1000)

##############
mean(agg$steps)

median(agg$steps)

##############
step_interval <- aggregate(steps ~ interval, act, mean)

ggplot(step_interval, aes(interval, steps)) + geom_line() + xlab("5-minute interval") + 
    ylab("average number of steps taken")

################
step_interval[which.max(step_interval$steps), ]
################
missing <- is.na(act$steps)
table(missing)

################
imputed <- function (step, interval) {
    filled <- NULL
    if (!is.na(step)) {
        filled <- step
    } else {
        filled <- mean(step_interval$steps[step_interval$interval == interval])
    }
    return(filled)
}

filledDate <- act
filledDate$steps <- mapply(imputed, filledDate$steps, filledDate$interval)
stepsByDate <- aggregate(steps~date, filledDate, sum)
hist(stepsByDate$steps, xlab = "steps", ylab = "counts", main = "total number of steps taken each day")

###################
weekdaysOrWeekend <- function(day) {
    day <- weekdays(day)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
        return("weekday")
    } else {
        return("weekend")
    }
}

filledDate$date <- as.Date(filledDate$date)
filledDate$date <- mapply(weekdaysOrWeekend, filledDate$date)

aggByDate <- aggregate(steps~interval + date, filledDate, sum)
ggplot(aggByDate, aes(interval, steps)) + geom_line() + facet_grid(date ~ .) + 
    xlab("5 minutes interval") + ylab("Numbers of Steps")
    
