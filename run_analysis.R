activityFile <- "activity.csv"
zippedFile <- "activity.zip"

if(!file.exists(activityFile)) {
    unzip(zippedFile)
    message(paste("" ,zippedFile, "extracted to", activityFile, "file"))
} else {
    message(paste(activityFile, "file already existed."))   
}

# Load the dataset

activity <- read.csv(activityFile)

# Generate a histogram with daily sums of steps

library(dplyr)

activityDaily <- activity %>%
                    group_by(date) %>% 
                    summarise(stepsSum = sum(steps, na.rm = TRUE)) %>% 
                    as.data.frame()

library(ggplot2)
hist1 <- qplot(activityDaily$stepsSum, geom="histogram", xlab = "Daily sum of steps") 
hist1 <- hist1 + ggtitle("Histogram of daily sums of steps")
hist1

# Calculating daily mean and median number of steps
dailyMean <- mean(activityDaily$stepsSum)
dailyMedian <- median(activityDaily$stepsSum)
dailyMean
dailyMedian

# Average number of steps in 5-minute interval

activityInterval <- activity %>%
    group_by(interval) %>% 
    summarise(stepsAvg = mean(steps, na.rm = TRUE)) %>% 
    as.data.frame()

hist2 <- ggplot(activityInterval, aes(interval, stepsAvg)) + geom_line() 
hist2 <- hist2 + xlab("5-minute intervals") + ylab("Average number of steps")
hist2 <- hist2 + ggtitle("Average number of steps per interval")
hist2

# Interval with the maximum number of steps

intervalMaxAvg <- activityInterval[which.max(activityInterval$stepsAvg),]
print(intervalMaxAvg, row.names = FALSE)

hist2 <- hist2 + geom_point(data = intervalMaxAvg, color = "red")
hist2 + annotate("text",
                 label = paste("max: ", intervalMaxAvg[,1], ", ", round(intervalMaxAvg[,2], digits = 2), sep = ""),
                 x=intervalMaxAvg[,1] + 25,
                 y=intervalMaxAvg[,2],
                 hjust = 0,
                 size = 4)

# Calculate the number of intervals with missing values

nrow(activity[is.na(activity$steps),])

# Impute the missing values with the mean value for a given interval
# Used the mean value for each interval for imputation

activityNotNA <- activity[!is.na(activity$steps),]
activityNA <- activity[is.na(activity$steps),]

fillNA <- function(x) {activityNA$steps[activityNA$interval == x] <<- activityInterval$stepsAvg[activityInterval$interval == x]}

lapply(unique(activityNA$interval), fillNA)

fillNA(900)
activityNA$steps[activityNA$interval == 900] 
activityInterval$stepsAvg[activityInterval$interval == 900]

activityNew <- arrange(rbind(activityNotNA, activityNA),date, interval)

# Generate a histogram with daily sums of steps

activityNewDaily <- activityNew %>%
    group_by(date) %>% 
    summarise(stepsSum = sum(steps)) %>% 
    as.data.frame()

hist3 <- ggplot(activityNewDaily, aes(x = stepsSum)) + geom_histogram()
hist3 <- hist3 + xlab("Daily sum of steps") 
hist3 <- hist3 + ggtitle("Histogram of daily sums of steps - dataset with imputed values")
hist3

# Calculating daily mean and median number of steps for the new dataset

dailyNewMean <- mean(activityNewDaily$stepsSum)
dailyNewMedian <- median(activityNewDaily$stepsSum)
dailyNewMean
dailyNewMedian

meanDiff <- dailyNewMean - dailyMean
medianDiff <- dailyNewMedian - dailyMedian
meanDiff
medianDiff

# data.frame(activityDaily$date,activityDaily$stepsSum,activityNewDaily$stepsSum)

# Average number of steps in 5-minute interval - dataset with imputed values

# install.packages("timeDate")
library(timeDate)

activityNewInterval <- activityNew %>%
    mutate(day = ifelse(isWeekend(date),"weekend", "weekday")) %>%
    group_by(interval, day) %>% 
    summarise(stepsAvg = mean(steps, na.rm = TRUE)) %>% 
    as.data.frame()

hist5 <- ggplot(activityNewInterval, aes(interval, stepsAvg)) + geom_line()
hist5 <- hist5 + facet_grid(day ~ .)
hist5 <- hist5 + xlab("5-minute intervals") + ylab("Average number of steps")
hist5 <- hist5 + ggtitle("Average number of steps per interval - dataset with imputed values")
hist5
