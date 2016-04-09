        library(dplyr)
        library(ggplot2)
        zfile<-"repdata-data-activity.zip"
##read and unzip file
##store results in dataframe 'act'        
        
        act<-read.csv(unz(zfile,"activity.csv"))
        act$date<-as.POSIXct(act$date)
##calculate daily mean value - remove NAs        
        act_sum<-group_by(act,date)
        act_sum<-summarize(act_sum, day_steps=sum(steps, na.rm =T))
##plot using ggplot - histograp will by colored by count        
        ggplot(data=act_sum, aes(act_sum$day_steps)) +   geom_histogram(aes(y =..count..,fill=..count..), 
                                                                        col="red", 
                                                                        bins=40)+labs(title="Histogram of Daily Steps") +labs(x="Daily Steps", y="Frequency")
        
##Calculate mean and medians across dates       
        mean(act_sum$day_steps)
        
        
        median(act_sum$day_steps)
        
        
##Group original data set by interval
        int_mean<-group_by(act, interval)
##summarize and store results in
        int_mean<-summarise(int_mean, avg_steps=mean(steps, na.rm=T))
##base plot system for time series line graph        
        plot(int_mean, type="l", col ="blue", xlab="5 Minute Intervals", ylab="Step Count",main="Average Interval Step Count")
##find highest interval and the average step value        
        int_mean[which.max(int_mean$avg_steps),]
        
        
        
#create merged data frame that captures a new variable, corresponding to the mean for the interval        
        mrg<-merge(act,int_mean)
#identify which rows are NAs        
        nas<-which(is.na(mrg$steps))
        length(nas)
##replace NAs for the identified rows using the mean value       
         mrg[nas,]$steps<-mrg[nas,]$avg_steps
##reset the data frame to line up like the original
        mrg<-arrange(mrg[,c(2,3,1)],date)
##group data frame by date and calculate the daily mean      
        act_sum_iv<-group_by(mrg,date)
        act_sum_iv<-summarize(act_sum_iv, day_steps=sum(steps))
        ggplot(data=act_sum_iv, aes(act_sum_iv$day_steps)) +   geom_histogram(aes(y =..count..,fill=..count..), 
                                                                        col="red", 
                                                                        bins=40)+labs(title="Histogram of Daily Steps") + labs(x="Daily Steps", y="Frequency")
##Calculate mean and medians across dates       
        mean(act_sum_iv$day_steps)
        
        
        median(act_sum_iv$day_steps)        
        
##Identify if date is weekday or weekend and set new varible to a factor        
        act<-mutate(act,day= ifelse(weekdays(date) %in% c("Saturday","Sunday"),"Weekend","Weekday"))
        act$day<-as.factor(act$day)
##create facet grid of results        
        day_compare <- aggregate(steps ~ interval + day, data = act, mean)
        ggplot(day_compare, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
            xlab("5-minute interval") + ylab("Number of steps")
