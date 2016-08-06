## Reproducible Research - Week 02

library(dplyr)
library(ggplot2)
library(Hmisc)


### Loading and preprocessing the data

if(!file.exists('activity.csv')){
  unzip('repdata_data_activity.zip')
}

data_pac <- read.csv("activity.csv") ##data about Personal movement using ACtivity 

########## PART 01 #################################
### What is mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values in the dataset.

##1.	Calculate the total number of steps taken per day
dates <- group_by(data_pac, date)

data_sum_step <- summarise(dates, steps_in_day = sum(steps, na.rm = TRUE))
plot(data_sum_step$date, data_sum_step$steps_in_day, type = "l", xlab="DATE", ylab="Number of steps", main = "Sum of steps in Day")

##2.	If you do not understand the difference between a histogram and a barplot, 
## research the difference between them. 
## Make a histogram of the total number of steps taken each day
hist(data_sum_step$steps_in_day, breaks = 50, xlab = "Number of steps in a Day", ylab = "Frequency", main = "Total Number of Steps") 
#number of breaks is chosen for better distinguishability 


##3.	Calculate and report the mean and median of the total number of steps taken per day
mean_total_no_steps <- mean(data_sum_step$steps_in_day)
median_total_no_steps <- median(data_sum_step$steps_in_day)


########## PART 02 #################################
### What is the average daily activity pattern?

##1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all days (y-axis)

data_intervals <- group_by(data_pac, interval)

data_mean_interval <- summarise(data_intervals, steps_in_interval = mean(steps, na.rm = TRUE))
plot(data_mean_interval$interval, data_mean_interval$steps_in_interval, type = "l", xlab = "Time Interval", ylab = "Mean Number of Steps", main = "Mean Steps in Interval")

##2. Which 5-minute interval, on average across all the days in the dataset, contains 
## the maximum number of steps?

max_steps_interval_index <- which.max(data_mean_interval$steps_in_interval)
max_steps_interval_value <- data_mean_interval$interval[max_steps_interval_index]

########## PART 03 ################################

##1.	Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NAs)
new_DF <- data_pac[rowSums(is.na(data_pac)) > 0,]
number_of_rows_without_NA <- nrow(new_DF)

##2.	Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, you could use the 
## mean/median for that day, or the mean for that 5-minute interval, etc.

##3.	Create a new dataset that is equal to the original dataset but with the missing
## data filled in.

impute_data_pac <- data_pac

impute_data_pac$steps <- impute(data_pac$steps, fun=mean)

# impute_data_pac is the new data

##4.	Make a histogram of the total number of steps taken each day and Calculate and 
## report the mean and median total number of steps taken per day. Do these values 
## differ from the estimates from the first part of the assignment? What is the impact
## of imputing missing data on the estimates of the total daily number of steps?

impute_data_pac_dates <- group_by(impute_data_pac, date)

##
impute_data_sum_step <- summarise(impute_data_pac_dates, steps_in_day = sum(steps, na.rm = TRUE))
hist(impute_data_sum_step$steps_in_day)

mean_total_no_steps_imputed <- mean(impute_data_sum_step$steps_in_day)
median_total_no_steps_imputed <- median(impute_data_sum_step$steps_in_day)
# These value do differ.

########## PART 04 ################################
###################################################
##Are there differences in activity patterns between weekdays and weekends?
##For this part the weekdays() function may be of some help here. Use the dataset 
## with the filled-in missing values for this part.

##1.	Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
## indicating whether a given date is a weekday or weekend day.

pdate <-  as.POSIXlt(impute_data_pac$date)
impute_data_pac$dayType <-  ifelse(pdate$wday %in% c(0,6), "weekday", "weekend")


##2.	Make a panel plot containing a time series plot (i.e. type = "l") of the 
## 5-minute interval (x-axis) and the average number of steps taken, averaged across
## all weekday days or weekend days (y-axis). See the README file in the GitHub
## repository to see an example of what this plot should look like using simulated data.

impute_data_pac_mean <- aggregate(steps ~ interval + dayType, data = impute_data_pac, mean)
ggplot(impute_data_pac_mean, aes(interval, steps)) +
  geom_line() +
  facet_grid(dayType ~ .) +
  xlab("5-minute interval") +  
  ylab("avarage number of steps")