
##Saddle temp data

library(dplyr)
library(plotrix)
library(Rmisc)

Saddletemp<-read.csv("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/sdltdayv.ml.data.infilled.csv",na.strings = c("."))
head(Saddletemp)
tail(Saddletemp)

#Calculate mean daily temperature for each year, make histogram
Saddletemp_yr_mean<-Saddletemp %>% 
  filter(year!=1981) %>% 
  group_by(year,month) %>% 
  summarise(temp=mean(mean_temp)) #%>% print.data.frame()

mean(Saddletemp_yr_mean$temp) #-2.051794
length(Saddletemp_yr_mean$temp)
#If I use 95% CI as definition of extreme, I just take teh lowest and highest year value 33*.95=31
max(Saddletemp_yr_mean$temp)#-0.3008197

std.error(Saddletemp_yr_mean$temp)*1.96
-2.051794+0.3256821=-1.726112
CI(Saddletemp_yr_mean$temp)


#for jane, she wanted saddle temp by month
Saddletemp_yr_mean<-Saddletemp %>% 
  filter(year!=1981) %>% 
  group_by(year,month)
saddletempbymonth<-aggregate.data.frame(Saddletemp_yr_mean$mean_temp,by=list(month=Saddletemp_yr_mean$month,year=Saddletemp_yr_mean$year),mean)
colnames(saddletempbymonth)[3]<-"mean_temp"
write.csv(saddletempbymonth,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/saddletempbymonth.csv")















