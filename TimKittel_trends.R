#Caitlin White#
#INSTAAR, University of Colorado#
#caitlin.t.whit@colorado.edu#
#November 2015#

##Recreation of Tim Kittel's NWT long-term precip and temperature data for NWT LTER Funding Renewal Proposal##

#Set working directory as appropriate..#
setwd("~/Dropbox/NWT_data")

#load necessary libraries#
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(broom)

#Dataset compilation#
#Read in .csv files created from Tim's infilled C1 and D1 Excel files, plus Emily's Saddle data#
C1dailytemp<- read.csv("c1_infilled_daily_temp.csv") %>%
    tbl_df()
D1dailytemp <- read.csv("d1_infilled_daily_temp.csv") %>%
    tbl_df()
C1dailyppt <- read.csv("c1_infilled_daily_ppt.csv") %>%
    tbl_df()
D1dailyppt <- read.csv("d1_infilled_daily_ppt.csv") %>%
    tbl_df()

  
#Subset data and merge daily values for C1, D1, and Saddle, adding columns for Date and Site#
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
#11/24/15: Added in Saddle data, but Katie decided to exclude because looks suspect.
DailyPPT <- rbind(C1dailyppt.sub, D1dailyppt.sub) %>%
    mutate(Site = as.factor(Site))

#Compile C1, D1, and Saddle temperature data in one data frame#
DailyTemp <- rbind(C1dailytemp.sub, D1dailytemp.sub) %>%
    mutate(Site = as.factor(Site), DiurnalT = Tmax-Tmin)


##SUMMARY TABLES##
#Precipitation data#
#Total monthly precipitation, by year, for C1, D1, and Saddle#
MonthlyxYear_PPT <- group_by(DailyPPT, Month, Year, Site) %>%
    summarize(sum(PPT)) %>%
    rename(PPT = `sum(PPT)`)

#Total yearly precipitation, for C1, D1, and Saddle#
#If want to include Saddle, add: subset(MonthlyxYear_PPT, !(Site=="Saddle" & Year == 1981)) %>%#
Yearly_PPT <- group_by(MonthlyxYear_PPT, Year, Site) %>%
  summarize(sum(PPT)) %>%
  rename(AnnualPPT = `sum(PPT)`)


#Monthly precip trends...
#2-part step, using looping (but only extracts coefficients):
#1) Break out multiple datasets by Site and capture Year coeffs in new table
  
#D1
D1.ppt <- subset(MonthlyxYear_PPT, Site=="D1")
D1.ppt.list <-lapply(split(D1.ppt,D1.ppt$Month), lm, formula = PPT~Year)
D1.monthlyppt.trends <- as.data.frame(t(sapply(D1.ppt.list,coef))) 
D1.monthlyppt.trends <- mutate(D1.monthlyppt.trends, Month = month(1:12, label=TRUE), Site="D1")

#C1
C1.ppt <- subset(MonthlyxYear_PPT, Site=="C1")
C1.ppt.list <-lapply(split(C1.ppt,C1.ppt$Month),lm,formula=PPT~Year)
C1.monthlyppt.trends <- as.data.frame(t(sapply(C1.ppt.list,coef))) 
C1.monthlyppt.trends <- mutate(C1.monthlyppt.trends, Month=month(1:12, label=TRUE), Site="C1")

#2 Combine all sites in one data frame for graphing
MonthlyPPT <- rbind(D1.monthlyppt.trends, C1.monthlyppt.trends) %>%
  rename(Trend=Year) %>%
  mutate(NegTrend = ifelse(Trend <0, T, F), NumMo = as.numeric(Month))

#Temperature
#Max, min, mean max, mean min, and average temps in each month, by year, for C1 and D1#
MonthlyxYear_Temp <- group_by(DailyTemp, Month, Year, Site) %>%
    summarize(min(Tmin), mean(Tmin), max(Tmax), mean(Tmax), mean(Tmean), mean(DiurnalT))
names(MonthlyxYear_Temp) <- c("Month", "Year", "Site",
                              "Tmin", "AvgDailyLo", "Tmax", "AvgDailyHi", "Tmean", "DiurnalMean")

#DiurnalOnly#
Yearly_Diurnal <- group_by(MonthlyxYear_Temp, Year, Site) %>%
  summarize(mean(DiurnalMean)) %>%
  rename(DiurnalMean = `mean(DiurnalMean)`)

#Monthly precip trends...
#2-part step, using looping (but only extracts coefficients):
#1) Break out multiple datasets by Site and capture Year coeffs in new table

#D1
D1.diurnal <- subset(MonthlyxYear_Temp, Site=="D1")
D1.diurnal.list <-lapply(split(D1.diurnal,D1.diurnal$Month), lm, formula = DiurnalMean~Year)
D1.monthlydiurnal.trends <- as.data.frame(t(sapply(D1.diurnal.list,coef))) 
D1.monthlydiurnal.trends <- mutate(D1.monthlydiurnal.trends, Month = month(1:12, label=TRUE), Site="D1")

#C1
C1.diurnal <- subset(MonthlyxYear_Temp, Site=="C1")
C1.diurnal.list <-lapply(split(C1.diurnal,C1.diurnal$Month), lm, formula = DiurnalMean~Year)
C1.monthlydiurnal.trends <- as.data.frame(t(sapply(C1.diurnal.list,coef))) 
C1.monthlydiurnal.trends <- mutate(C1.monthlydiurnal.trends, Month = month(1:12, label=TRUE), Site="C1")

#2 Combine all sites in one data frame for graphing
MonthlyDiurnal <- rbind(D1.monthlydiurnal.trends, C1.monthlydiurnal.trends) %>%
  rename(Trend=Year)

##Not using Monthly below
#Monthly_Temp <- group_by(DailyTemp, Month, Site) %>%
#    summarize(min(Tmin), mean(Tmin), max(Tmax), mean(Tmax), mean(Tmean))
#names(Monthly_Temp) <- c("Month", "Site",
#                          "Tmin", "AvgDailyLo", "Tmax", "AvgDailyHi", "Tmean")

#Yearly_Temp <- group_by(DailyTemp, Year, Site) %>%
#    summarize(min(Tmin), mean(Tmin), max(Tmax), mean(Tmax), mean(Tmean), mean(DiurnalMean))
#names(Yearly_Temp) <- c("Year", "Site",
#                        "Tmin", "AvgDailyLo", "Tmax", "AvgDailyHi", "Tmean", "DiurnalMean")


#Plot data in panel graphs, Katie wants line graphs for all:
#1) Yearly total precipitation for C1, D1, and Saddle
#2) Monthly trends for precip for C1, D1, and Saddle
#3) Max and min temps by year for C1, D1, and Saddle
#4) Monthly trends for max and min temps across all years for C1, D1 and Saddle

#Yearly total precip
YRLY.PPT <- ggplot(Yearly_PPT, aes(Year, AnnualPPT, color=Site))
a <- YRLY.PPT + geom_line() + geom_point(aes(shape=Site), size=3) + 
  ylab(expression(paste("Precipitation (mm yr"^-1,")"))) +
  geom_smooth(colour="black", data=subset(Yearly_PPT, Site=="D1" & Year<1977), method = lm, se=FALSE, lwd=1) +
  geom_smooth(colour="black", data=subset(Yearly_PPT, Site=="D1" & Year>1977), method = lm, se=FALSE, lwd=1) +
  geom_smooth(colour="black", linetype=2,data=subset(Yearly_PPT, Site=="C1" & Year<1977), method = lm, se=FALSE, lwd=1) +
  geom_smooth(colour="black", linetype=2,data=subset(Yearly_PPT, Site=="C1" & Year>1977), method = lm, se=FALSE, lwd=1) +
  geom_vline(x=1977, colour="black", linetype=4) +
  theme_classic(base_size = 14) +
  geom_text(aes(2005, 1500, label = "D1 - High Alpine"), size=5, colour="black") +
  geom_text(aes(2005, 400, label = "C1 - Subalpine"), size=5, colour="black") +
  geom_text(aes(1978, 420, label = "1976/77 PDO \n Regime Shift"), colour="black", hjust=0, family="Courier") +
  theme(legend.position="none", axis.title.x = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(vjust=0.5, size=18))


#Mean monthly total precip#
MNLY.PPT <- ggplot(MonthlyPPT, aes(Month, Trend, color=Site))
b <- MNLY.PPT + geom_line(aes(group=Site)) + geom_point(aes(shape=Site), size=3) +
  geom_hline(yintercept=0) +
  ylab(expression(paste("Precipitation Trend (mm mo"^-1, yr^-1,")"))) + theme_classic(base_size = 14) +
  geom_text(aes(11, 0.7, label = "D1 - High Alpine"), size=5, colour="black") +
  geom_text(aes(11, -0.3, label = "C1 - Subalpine"), size=5, colour="black") +
  geom_text(aes(2, 0.708, label= "*"), colour="black") +
  geom_text(aes(4, 1.231, label= "**"), colour="black") +
  geom_text(aes(10, 1.314, label= "***"), colour="black") +
  geom_text(aes(11, 0.873, label= "*"), colour="black") +
  geom_text(aes(12, 1.163, label= "**"), colour="black") +            
  theme(legend.position="none", axis.title.x = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(vjust=0.5, size=18))

#Temperature - Diurnal only
YRLY.DIURNAL <- ggplot(Yearly_Diurnal, aes(Year, DiurnalMean, color=Site))
c <- YRLY.DIURNAL + geom_line() + geom_point(aes(shape=Site), size=3) + 
  ylab(expression(paste("Diurnal Temp. Range Trend ("*degree*C,")"))) + 
  geom_smooth(colour="black", data=subset(Yearly_Diurnal, Site=="D1"), method = lm, se=FALSE, lwd=1) +
  geom_smooth(colour="black", data=subset(Yearly_Diurnal, Site=="C1"), method = lm, se=FALSE, lwd=1) +
  theme_classic(base_size = 14) +
  geom_text(aes(2000, 8.5, label = "D1 - High Alpine"), size=5, colour="black") +
  geom_text(aes(2000, 11.5, label = "C1 - Subalpine"), size=5, colour="black") +
  theme(legend.position="none",
        axis.text = element_text(size = 16),
        axis.title.x = element_text(vjust=-0.5, size =18),
        axis.title.y = element_text(vjust=0.5, size=18))


#NOT USING ANY TEMP GRAPHS BELOW HERE#

#Gather temperature in one column to allow plotting max, mean, and min all on one graph, 
#add variable for unique Site_TempType combination,
#filter out mean TMin and mean TMax (for now)
#MonthlyxYear_Temp.collapsed <- select(MonthlyxYear_Temp, -AvgDailyLo, -AvgDailyHi) %>%
#  gather(TempType,Temp, Tmin:Tmean) %>%
#  mutate(Key = paste(Site,TempType, sep="_"))
#Monthly_Temp.collapsed <- select(Monthly_Temp, -AvgDailyLo , -AvgDailyHi) %>%
#  gather(TempType, Temp, Tmin:Tmean) %>%
#  mutate(Key = paste(Site,TempType, sep="_"))
#Yearly_Temp.collapsed <- select(Yearly_Temp, -AvgDailyLo, -AvgDailyHi) %>%
#  gather(TempType, Temp, Tmin:Tmean) %>%
#  mutate(Key = paste(Site,TempType, sep="_"))
  
#Yearly temp trends over time#
#YRLY.Temp <- ggplot(Yearly_Temp.collapsed, aes(Year, Temp, group=Key))
#z <- YRLY.Temp + geom_line(aes(color=Key)) + geom_point(aes(shape=Site)) + 
#  ylab("Temperature (C)") + ggtitle("NWT Max, Mean and Min Temperature by Year (1952-2010)") + 
#  theme_classic() + stat_smooth(method = lm, se=FALSE)

#Not using these (for now)
#YRLY.MaxT <- ggplot(Yearly_Temp, aes(Year, `max(Tmax)`, color=Site))
#YRLY.MaxT + geom_line() + geom_point(aes(shape=Site)) + 
#  ylab("Temperature (deg. C)") + theme_classic() + stat_smooth(method = lm)

#YRLY.MinT <- ggplot(Yearly_Temp, aes(Year, `min(Tmin)`, color=Site))
#YRLY.MinT + geom_line() + geom_point(aes(shape=Site)) + 
#  ylab("Temperature (deg. C)") + theme_classic() + stat_smooth(method = lm)


#Monthly temp trends over time#
MNLY.Temp <- ggplot(MonthlyDiurnal, aes(Month, Trend, color=Site))
d <- MNLY.Temp + geom_line(aes(group=Site)) + geom_point(aes(shape=Site), size=3) + 
  ylab(expression(paste("Diurnal Temp. Range Trend ("*degree*C," ", yr^-1,")"))) + theme_classic(base_size = 14) + 
  geom_text(aes(10, 0.02, label = "D1 - High Alpine"), size=5, colour="black", hjust=0) +
  geom_text(aes(10, 0.055, label = "C1 - Subalpine"), size=5, colour="black", hjust=0) +
  geom_text(aes(1, 0.045, label= "***"), colour="black") +
  geom_text(aes(2, 0.056, label= "***"), colour="black") +
  geom_text(aes(3, 0.057, label= "***"), colour="black") +
  geom_text(aes(4, 0.06, label= "***"), colour="black") +
  geom_text(aes(5, 0.059, label= "***"), colour="black") + 
  geom_text(aes(6, 0.053, label= "***"), colour="black") +
  geom_text(aes(7, 0.048, label= "***"), colour="black") +
  geom_text(aes(8, 0.036, label= "***"), colour="black") +
  geom_text(aes(9, 0.053, label= "***"), colour="black") +
  geom_text(aes(10, 0.036, label= "**"), colour="black") +
  geom_text(aes(11, 0.047, label= "***"), colour="black") +
  geom_text(aes(12, 0.045, label= "***"), colour="black") +
  geom_text(aes(5, 0.026, label= "***"), colour="black") +
  geom_text(aes(6, 0.027, label= "***"), colour="black") +
  geom_text(aes(7, 0.015, label= "**"), colour="black") +
  geom_text(aes(8, 0.012, label= "*"), colour="black") +
  geom_text(aes(9, 0.02, label= "**"), colour="black") +
  theme(legend.position="none",
        axis.text = element_text(size = 16),
        axis.title.x = element_text(vjust=-0.5, size =18),
        axis.title.y = element_text(vjust=0.5, size=18))

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
