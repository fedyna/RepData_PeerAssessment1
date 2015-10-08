#Reproducible Peer Assessments1
DATAactivity <- read.csv("activity.csv", header = TRUE, sep = ",")
str(DATAactivity)
summary(DATAactivity)

# generate DATAactivity1 without NA
DATAactivity1 <- na.omit(DATAactivity)

####What is mean total number of steps taken per day?
#aggregate total number of steps in a day
DATA_date_steps <- aggregate(steps ~ date, DATAactivity1, sum)

#Make a histogram of the total number of steps taken each day
hist(DATA_date_steps$steps, col="dark green", breaks = 30,
     main="Histogram of total number of steps per day", xlab = "")

#Calculate and report the mean and median of the total number of steps taken per day
mean(DATA_date_steps$steps)          #[1] 10766.19
median(DATA_date_steps$steps)        #[1] 10765

####What is the average daily activity pattern?

DATA_interval_steps <- aggregate(steps ~ interval, DATAactivity1, mean)

#Make a time series plot(i.e. type = “l”) of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis).
plot(DATA_interval_steps$interval, DATA_interval_steps$steps, type = "l", lwd = 2, col = "dark green",
     main = "The average number of steps averaged across all days",
     xlab = "the 5-minute interval",
     ylab = "the average number of steps")

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?

#find row with max value of steps
max_steps_row <- which.max(DATA_interval_steps$steps) #[1] 104

#find interval with this max value
DATA_interval_steps[max_steps_row, ]
#    interval    steps
#104      835 206.1698

####Imputing missing values

#Calculate and report the total number of missing values in the dataset
sum(is.na(DATAactivity)) #[1] 2304

#Create a new dataset that is equal to the original dataset but with the missing data filled in
DATA <- DATAactivity
DATA$steps[is.na(DATA$steps)] <- mean(DATA$steps, na.rm = TRUE)
#colSums(is.na(DATA))        

#Make a histogram of the total number of steps taken each day
DATA_day_steps <- aggregate(steps ~ date, DATA, sum)

hist(DATA_day_steps$steps, col = "dark green", breaks = 30,
     main="Histogram of total number of steps per day (NA filled by mean)", xlab = "")

#Calculate and report the mean and median total number of steps taken per day
mean(DATA_day_steps$steps) #[1] 10766.19
median(DATA_day_steps$steps) #[1] 10766.19
#After imputing missing value with mean value of steps -> Mean = Median

####Are there differences in activity patterns between weekdays and weekends?
DATA$date <- as.Date(DATA$date)
DATA$weekdays <- weekdays(DATA$date)
DATA$weeks[(DATA$weekdays == "Saturday" | DATA$weekdays == "Sunday")] <- "weekend"
DATA$weeks[!(DATA$weekdays == "Saturday" | DATA$weekdays == "Sunday")] <- "weekdays"

DATAweeks <- aggregate(steps ~ interval+weeks, DATA, mean)

g <- ggplot(DATAweeks, aes(interval, steps))
g + facet_grid(weeks ~ .) + geom_line(color="dark red") +
        ggtitle("Average Number of Steps (weekdays vs weekend)")


