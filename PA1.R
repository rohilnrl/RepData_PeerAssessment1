#============================
# IMPORTANT: This script contains RAW analysis code and is not to be used for analysis.
# The testing for the code in the markdown was done in this script.
#============================

packages <- c("dplyr", "ggplot2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

# The working directory should be set here
setwd("~/Data Science/RepData_PeerAssessment1")
path = file.path(getwd(), "activity.zip")

if (file.exists("activity.csv")) {
  activity <- read.csv("activity.csv")
} else if (file.exists("activity.zip")) {
  executable <- file.path("C:", "Program Files", "7-Zip", "7z.exe")
  parameters <- "x"
  cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", path, "\""))
  system(cmd)
  activity <- read.csv("activity.csv")
} else {
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url, path)
  executable <- file.path("C:", "Program Files", "7-Zip", "7z.exe")
  parameters <- "x"
  cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", path, "\""))
  system(cmd)
  activity <- read.csv("activity.csv")
}

activity$date <- as.POSIXct(as.character(activity$date))
activity <- tbl_df(activity)
activity <- group_by(activity, date)
activity

totalSteps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))
print(qplot(totalSteps, binwidth = 1000, xlab = "Total number of steps taken per day", fill = names(totalSteps)))
print(mean(totalSteps, na.rm = TRUE))
print(median(totalSteps, na.rm = TRUE))

averages <- tbl_df(data.frame(interval = unique(activity$interval), average = tapply(activity$steps, activity$interval, mean, na.rm = TRUE)))
qplot(interval, average, data = averages, geom = "line")
print(averages[which.max(averages$average), ])

# Imputing
impute <- function(steps, interval) {
  value <- NA
  if (is.na(steps)) {
    value <- averages[averages$interval == interval, "average"]
  } else {
    value <- c(steps)
  }
  
  return (value)
}

activity$imputed <- unlist(mapply(impute, activity$steps, activity$interval))
qplot(totalSteps, binwidth = 1000, fill = names(totalSteps))

checkDay <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Saturday", "Sunday")) {
    return("Weekend")
  } else if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
    return("Weekday")
  } else {
    stop("invalid date")
  }
}

activity$day <- factor(sapply(activity$date, checkDay))
