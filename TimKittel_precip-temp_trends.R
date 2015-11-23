#Caitlin White#
#INSTAAR, University of Colorado#
#caitlin.t.whit@colorado.edu#
#November 2015#

##Recreation of Tim Kittel's NWT long-term precip and temperature data for NWT LTER Funding Renewal Proposal##

#Dataset compilation#

#Set working directory as appropriate..#
setwd("~/Dropbox/NWT_data")

#load necessary libraries#
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

#Read in .csv files created from Tim's infilled C1 and D1 Excel files#
C1dailytemp<- read.csv("c1_infilled_daily_temp.csv") %>%
    tbl_df()
D1dailytemp <- read.csv("d1_infilled_daily_temp.csv") %>%
    tbl_df()
C1dailyppt <- read.csv("c1_infilled_daily_ppt.csv") %>%
    tbl_df()
D1dailyppt <- read.csv("d1_infilled_daily_ppt.csv") %>%
    tbl_df()

#Subset data and merge daily values for C1 and D1, adding columns for Date and Site#
C1dailytemp.sub <- C1dailytemp[,1:6] %>%
    mutate(Date=ymd((paste(Year, Month, Day))), Month = month(Month, label=TRUE), Site = "C1")
names(C1dailytemp.sub) <- c("Month", "Day", "Year", "Tmax", "Tmin", "Tmean", "Date", "Site")
D1dailytemp.sub <- D1dailytemp[,1:6] %>%
    mutate(Date=ymd(paste(year,month,day)), month = month(month, label=TRUE), Site = "D1") %>%
    rename(Month=month, Day=day, Year=year)
C1dailyppt.sub <- C1dailyppt[,1:4] %>%
    mutate(Date=ymd(paste(Year, Month, Day)), Month = month(Month, label=TRUE), Site = "C1") %>%
    rename(PPT = Precipitation..mm.)
D1dailyppt.sub <- D1dailyppt[,1:4] %>%
    mutate(Date=ymd(paste(Year, Month, Day)), Month = month(Month, label=TRUE), Site = "D1") %>%
    rename(PPT = D1.mm.ppt)

#Compile C1 and D1 precip data in one data frame#
DailyPPT <- rbind(C1dailyppt.sub, D1dailyppt.sub) %>%
    mutate(Site = as.factor(Site))

#Compile C1 and D1 temperature data in one data frame#
DailyTemp <- rbind(C1dailytemp.sub, D1dailytemp.sub) %>%
    mutate(Site = as.factor(Site))


##Summary tables by month and year##
#Total monthly precipitation, by year, for C1 and D1#
MonthlyxYear_PPT <- group_by(DailyPPT, Month, Year, Site) %>%
    summarize(sum(PPT)) %>%
    rename(PPT = `sum(PPT)`)

#Average total monthly precipitation across all years, for C1 and D1#
Monthly_PPT <- group_by(MonthlyxYear_PPT, Month, Site) %>%
    summarize(mean(PPT))

#Total yearly precipitation, for C1 and D1#
Yearly_PPT <- group_by(MonthlyxYear_PPT, Year, Site) %>%
    summarize(sum(PPT)) %>%
    rename(AnnualPPT = `sum(PPT)`)

#Max, min, mean max, mean min, and average temps in each month, by year, for C1 and D1#
MonthlyxYear_Temp <- group_by(DailyTemp, Month, Year, Site) %>%
    summarize(min(Tmin), mean(Tmin), max(Tmax), mean(Tmax), mean(Tmean))
names(MonthlyxYear_Temp) <- c("Month", "Year", "Site",
                              "Tmin", "AvgDailyLo", "Tmax", "AvgDailyHi", "Tmean")

Monthly_Temp <- group_by(DailyTemp, Month, Site) %>%
    summarize(min(Tmin), mean(Tmin), max(Tmax), mean(Tmax), mean(Tmean))
names(Monthly_Temp) <- c("Month", "Site",
                          "Tmin", "AvgDailyLo", "Tmax", "AvgDailyHi", "Tmean")

Yearly_Temp <- group_by(DailyTemp, Year, Site) %>%
    summarize(min(Tmin), mean(Tmin), max(Tmax), mean(Tmax), mean(Tmean))
names(Yearly_Temp) <- c("Year", "Site",
                        "Tmin", "AvgDailyLo", "Tmax", "AvgDailyHi", "Tmean")


#Plot data in panel graphs, Katie wants line graphs for all:
#1) Yearly total precipitation for C1 and D1
#2) Mean monthly total precip for C1 and D1
#3) Max and min temps by year for C1 and D1
#4) Mean monthly max and min temps across all years for C1 and D1

#Yearly total precip
YRLY.PPT <- ggplot(Yearly_PPT, aes(Year, AnnualPPT, color=Site))
a <- YRLY.PPT + geom_line() + geom_point(aes(shape=Site)) + 
  ylab("Precipitation (mm/yr)") + ggtitle("NWT Total Precipitation by Year (1952-2010)") + 
  theme_classic() + stat_smooth(method = lm, se=FALSE)

#Mean monthly total precip#
MNLY.PPT <- ggplot(Monthly_PPT, aes(Month, `mean(PPT)`, color=Site))
MNLY.PPT + geom_bar(position="dodge", stat="identity") + ylab("Precipitation (mm/month)") + theme_classic()
b <- MNLY.PPT + geom_line(aes(group=Site)) + geom_point(aes(shape=Site)) + 
  ylab("Precipitation (mm/month)") + ggtitle("NWT Mean Total Precipitation by Month (1952-2010)") +
  theme_classic()

#Temperature
#Gather temperature in one column to allow plotting max, mean, and min all on one graph, 
#add variable for unique Site_TempType combination,
#filter out mean TMin and mean TMax (for now)
MonthlyxYear_Temp.collapsed <- select(MonthlyxYear_Temp, -AvgDailyLo, -AvgDailyHi) %>%
  gather(TempType,Temp, Tmin:Tmean) %>%
  mutate(Key = paste(Site,TempType, sep="_"))
Monthly_Temp.collapsed <- select(Monthly_Temp, -AvgDailyLo , -AvgDailyHi) %>%
  gather(TempType, Temp, Tmin:Tmean) %>%
  mutate(Key = paste(Site,TempType, sep="_"))
Yearly_Temp.collapsed <- select(Yearly_Temp, -AvgDailyLo, -AvgDailyHi) %>%
  gather(TempType, Temp, Tmin:Tmean) %>%
  mutate(Key = paste(Site,TempType, sep="_"))
  
#Yearly temp trends over time#
YRLY.Temp <- ggplot(Yearly_Temp.collapsed, aes(Year, Temp, group=Key))
c <- YRLY.Temp + geom_line(aes(color=Key)) + geom_point(aes(shape=Site)) + 
  ylab("Temperature (C)") + ggtitle("NWT Max, Mean and Min Temperature by Year (1952-2010)") + 
  theme_classic() + stat_smooth(method = lm, se=FALSE)

#Not using these (for now)
#YRLY.MaxT <- ggplot(Yearly_Temp, aes(Year, `max(Tmax)`, color=Site))
#YRLY.MaxT + geom_line() + geom_point(aes(shape=Site)) + 
#  ylab("Temperature (deg. C)") + theme_classic() + stat_smooth(method = lm)

#YRLY.MinT <- ggplot(Yearly_Temp, aes(Year, `min(Tmin)`, color=Site))
#YRLY.MinT + geom_line() + geom_point(aes(shape=Site)) + 
#  ylab("Temperature (deg. C)") + theme_classic() + stat_smooth(method = lm)


#Monthly temp trends over time#
MNLY.Temp <- ggplot(Monthly_Temp.collapsed, aes(Month, Temp, group=Key))
d <- MNLY.Temp + geom_line(aes(color=Key)) + geom_point(aes(shape=Site)) + 
  ylab("Temperature (C)") + ggtitle("NWT Max, Mean and Min Temperature by Month (1952-2010)") + 
  theme_classic()

#Not using these (for now)
#MNLY.MeanT <- ggplot(Monthly_Temp, aes(Month, `mean(Tmean)`, group=Site))
#MNLY.MeanT + geom_bar(position="dodge", stat="identity") + ylab("Temperature (deg. C)") + theme_classic()
#MNLY.MeanT + geom_line(aes(group=Site)) + geom_point(aes(shape=Site)) + ylab("Temperature (deg. C)") + theme_classic()

#MNLY.MaxT <- ggplot(Monthly_Temp, aes(Month, `max(Tmax)`, fill=Site))
#MNLY.MaxT + geom_bar(position="dodge", stat="identity") + ylab("Temperature (deg. C)") + theme_classic()
#MNLY.MaxT + geom_line(aes(group=Site)) + geom_point(aes(shape=Site)) + ylab("Temperature (deg. C)") + theme_classic()

#MNLY.MinT <- ggplot(Monthly_Temp, aes(Month, `min(Tmin)`, fill=Site))
#MNLY.MinT + geom_bar(position="dodge", stat="identity") + ylab("Temperature (deg. C)") + theme_classic()
#MNLY.MinT + geom_line(aes(group=Site)) + geom_point(aes(shape=Site)) + ylab("Temperature (deg. C)") + theme_classic()

#Print all graphs to 4-panel figure
grid.arrange(a,b,c,d)
