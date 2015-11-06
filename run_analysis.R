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

activityDaily <- activity %>%
                    group_by(date) %>% 
                    summarise(stepsSum = sum(steps, na.rm = TRUE)) %>% 
                    as.data.frame()

library(ggplot2)
qplot(activityDaily$stepsSum, geom="histogram") 
# Histogram with a smaller number of bins
qplot(activityDaily$stepsSum, geom="histogram", binwidth = 2000) 
# now add the axis labels

