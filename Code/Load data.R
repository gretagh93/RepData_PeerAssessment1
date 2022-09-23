
# Loading Data
# ============

# Unzip
unzip("activity.zip")
# Read .rds files
act <- read.csv("activity.csv")
# Check csv file
head(act)


# act_mean <- aggregate(steps ~ interval, data = act_notNan, mean)

aid <- aggregate(steps ~ interval+dayweek, data = act_fill_week, mean)
weekend <- aid[aid$dayweek == "weekend", ]
weekday <- aid[aid$dayweek == "weekday", ]

test1 <- cbind(weekend$interval, weekend$steps, weekday$interval, weekday$steps)
colnames(test1) <- c("end_int", "end_stepm", "day_int", "day_stepm")



#____________________________________________________
library(zoo)

plot(test1,
     yax.flip = TRUE, col = "blue", frame.plot = TRUE,
     main = "", xlab = "Interval", ylab="Number of steps")
 


plot(aid$interval, aid$steps, type="l", col="blue", main="Days Average")




