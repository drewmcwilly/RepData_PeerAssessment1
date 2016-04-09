library(dplyr)
library(ggplot2)
zfile<-"activity.zip"

act<-read.csv(unz("activity.zip","activity.csv"))
act_sum<-group_by(act,date)
act_sum<-summarize(act_sum, day_steps=sum(steps, na.rm =T))
ggplot(data=act_sum, aes(act_sum$day_steps)) +   geom_histogram(aes(y =..count..,fill=..count..), 
                                                                col="red", 
                                                                 bins=40)+labs(title="Histogram of Daily Steps") +
  labs(x="Daily Steps", y="Frequency")


mean(act_sum$day_steps)


median(act_sum$day_steps)

int_mean<-tapply(act$steps, act$interval,mean, na.rm=T)
plot(int_mean, type="l")