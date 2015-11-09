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

# Generate a histogram

library(dplyr)

activityDaily <- activity %>%
                    group_by(date) %>% 
                    summarise(stepsSum = sum(steps, na.rm = TRUE)) %>% 
                    as.data.frame()

library(ggplot2)
hist1 <- qplot(activityDaily$stepsSum, geom="histogram", xlab = "Daily sum of steps") 
hist1 <- hist + ggtitle("Histogram of daily sums of steps")
hist1

# Calculating daily mean and median number of steps
mean(activityDaily$stepsSum)
median(activityDaily$stepsSum)

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
