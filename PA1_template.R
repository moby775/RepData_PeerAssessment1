library(ggplot2)

path <- getwd()
folder<-file.path(path)
filename<-"activity.zip"
f<-file.path(folder,filename)
Data<-read.csv(unzip(f))
head(Data)

#####################################################################
daily_steps<-
    aggregate(formula = steps~date,data = Data,FUN = sum,na.rm=TRUE)


mean_steps <- round(mean(daily_steps$steps), 2)  # Mean
mean_steps
median_steps <- quantile(x = daily_steps$steps, probs = 0.5)  # Median
mean_steps

hist<-qplot(y=steps,
            x=date,
            data=daily_steps,
            geom="bar",
            stat ="identity",
            xlab="Date",
            ylab="Steps")

plot(hist)
hist + theme(axis.text.x = element_text(hjust=0, angle=270))

########################################################################
activity_pattern<-
    aggregate(formula = steps~interval,data = Data,FUN = mean,na.rm=TRUE)

series_plot<-qplot(y=steps,
            x=interval,
            data=activity_pattern,
            geom="smooth",
            stat ="identity",
            xlab="5-minute interval",
            ylab="Average number of steps taken, averaged across all days")
plot(series_plot)

max_steps<-which(activity_pattern$steps == max(activity_pattern$steps))

##5-minute interval that contains the maximum number of steps
max_interval<-activity_pattern[max_steps,1]

#########################################################################

total_NAs <- sum(!complete.cases(Data))

NewData <- Data
ImputedData <- merge(NewData,
                 activity_pattern[,c('interval', 'steps')],
                 by='interval')

ImputedData$steps.x <- ifelse(is.na(ImputedData$steps.x),
                              ImputedData$steps.y,
                              ImputedData$steps.x)

New_daily_steps<-
    aggregate(formula = steps.x~date,data = ImputedData,FUN = sum,na.rm=TRUE)
NewData_mean <- round(mean(New_daily_steps$steps), 2)  # Mean
NewData_median <- quantile(x = New_daily_steps$steps, probs = 0.5)  # Median


hist<-qplot(y=steps.x,
            x=date,
            data=New_daily_steps,
            geom="bar",
            stat ="identity",
            xlab="Date",
            ylab="Steps")

plot(hist)
hist + theme(axis.text.x = element_text(hjust=0, angle=270))

########################################################################

DayType <- data.frame(sapply(X = ImputedData$date, FUN = function(day) {
    if (weekdays(as.Date(day)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                      "Friday")) {
        day <- "weekday"
    } else {
        day <- "weekend"
    }
}))

ImputedData_DayType <- cbind(ImputedData, DayType)
colnames(ImputedData_DayType) <- c("Interval","Steps", "Date", "Mean", "DayType")

DayType_pattern<-aggregate(formula = Steps~Interval+DayType,
                           data = ImputedData_DayType,
                           FUN = mean,
                           na.rm=TRUE)

DayTypeSeries<-qplot(y=Steps,
                   x=Interval,
                   data=DayType_pattern,
                   facets = DayType ~ .,
                   geom="smooth",
                   stat ="identity",
                   xlab="5-minute interval",
                   ylab="Average steps taken, averaged across weekends/weekdays")
plot(DayTypeSeries)


