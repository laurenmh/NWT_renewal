#Caitlin White#
#INSTAAR, University of Colorado#
#caitlin.t.whit@colorado.edu#
#December 2015#

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
C1dailytemp.sub <- C1dailytemp[,1:7] %>%
    mutate(Date=ymd((paste(Year, Month, Day))), Month = month(Month, label=TRUE), Site = "C1")
names(C1dailytemp.sub) <- c("Month", "Day", "Year", "Tmax", "Tmin", "Tmean", "DTR", "Date", "Site")

D1dailytemp.sub <- D1dailytemp[,1:7] %>%
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
    mutate(Site = as.factor(Site))


##SUMMARY TABLES##
#Precipitation data#
#Total monthly precipitation, by year, for C1, D1, and Saddle#
MonthlyxYear_PPT <- group_by(DailyPPT, Month, Year, Site) %>%
    summarize(PPT = sum(PPT))

#Total yearly precipitation, for C1, D1, and Saddle#
#If want to include Saddle, add: subset(MonthlyxYear_PPT, !(Site=="Saddle" & Year == 1981)) %>%#
Yearly_PPT <- group_by(MonthlyxYear_PPT, Year, Site) %>%
  summarize(PPT = sum(PPT))


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
        mutate(NegTrend = ifelse(Trend <0, T, F), NumMo = as.numeric(Month)) %>%
        select(Trend:NumMo)


#Temperature
#Max, min, mean max, mean min, and average temps in each month, by year, for C1 and D1#
MonthlyxYear_Temp <- group_by(DailyTemp, Month, Year, Site) %>%
    summarize(Tmin = min(Tmin), Tmax= max(Tmax), Tmean = mean(Tmean), DTRmean = mean(DTR))

#DiurnalOnly# -- DRAFT 2: Not using DTR anymore
#Yearly_Diurnal <- group_by(MonthlyxYear_Temp, Year, Site) %>%
#  summarize(mean(DiurnalMean)) %>%
#  rename(DiurnalMean = `mean(DiurnalMean)`)

#Tmax and Tmin only
Yearly_T.extremes <- group_by(MonthlyxYear_Temp, Year, Site) %>%
        summarize(Tmax = max(Tmax), Tmin=min(Tmin))

#check for trend significance of year on yearly Tmax
D1.Tmax <- lm(Tmax~Year,Yearly_T.extremes, Site=="D1")
summary(D1.Tmax) #signif at p<.0001

C1.Tmax <- lm(Tmax~Year,Yearly_T.extremes, Site=="C1")
summary(C1.Tmax) #signif at p<.0001

#Monthly precip trends...
#2-part step, using looping (but only extracts coefficients):
#1) Break out multiple datasets by Site and capture Year coeffs in new table

#D1
D1.Text <- subset(MonthlyxYear_Temp, Site=="D1")
D1.Tmax.list <-lapply(split(D1.Text,D1.Text$Month), lm, formula = Tmax~Year)
D1.monthlyTmax.trends <- as.data.frame(t(sapply(D1.Tmax.list,coef))) 
D1.monthlyTmax.trends <- mutate(D1.monthlyTmax.trends, Month = month(1:12, label=TRUE), Site="D1")

#my own extractions
D1.test <- as.data.frame(t(sapply(D1.Tmax.list,summary)))
D1.test2 <- D1.test[["coefficients"]]
D1.Tmaxcoeff <- sapply(D1.test2, "[", i = 2) #extract coeff
D1.Tmaxpval <- sapply(D1.test2, "[", i = 8) #extract coeff/model signif
#combine coeff and pval in data frame, add month
Month <- as.numeric(seq(1:12))
D1.monthlyTmax.trends2 <- as.data.frame(cbind(D1.Tmaxcoeff, D1.Tmaxpval))
D1.monthlyTmax.trends2 <- mutate(D1.monthlyTmax.trends2, Month = month(1:12, label=TRUE), Site="D1")
names(D1.monthlyTmax.trends2) <- c("Trend", "Pval", "Month", "Site")


#C1
C1.Text <- subset(MonthlyxYear_Temp, Site=="C1")
C1.Tmax.list <-lapply(split(C1.Text, C1.Text$Month), lm, formula = Tmax~Year)
C1.monthlyTmax.trends <- as.data.frame(t(sapply(C1.Tmax.list,coef))) 
C1.monthlyTmax.trends <- mutate(C1.monthlyTmax.trends, Month = month(1:12, label=TRUE), Site="C1")

#my own extractions
C1.test <- as.data.frame(t(sapply(C1.Tmax.list,summary)))
C1.test2 <- C1.test[["coefficients"]]
C1.Tmaxcoeff <- sapply(C1.test2, "[", i = 2) #extract coeff
C1.Tmaxpval <- sapply(C1.test2, "[", i = 8) #extract coeff/model signif
#combine coeff and pval in data frame, add month
C1.monthlyTmax.trends2 <- as.data.frame(cbind(C1.Tmaxcoeff, C1.Tmaxpval))
C1.monthlyTmax.trends2 <- mutate(C1.monthlyTmax.trends2, Month = month(1:12, label=TRUE), Site="C1")
names(C1.monthlyTmax.trends2) <- c("Trend", "Pval", "Month", "Site")

#2 Combine all sites in one data frame for graphing
MonthlyTmax <- rbind(D1.monthlyTmax.trends, C1.monthlyTmax.trends) %>%
        rename(Trend=Year) %>%
        mutate(NegTrend = ifelse(Trend <0, T, F), NumMo = as.numeric(Month)) %>%
        select(Trend:NumMo)

MonthlyTmax2 <- rbind(D1.monthlyTmax.trends2, C1.monthlyTmax.trends2)
#add plotting point for significant
MonthlyTmax2$Point <- as.numeric(MonthlyTmax2$Trend + 0.005)

##Not using Monthly below
#Monthly_Temp <- group_by(DailyTemp, Month, Site) %>%
#    summarize(min(Tmin), mean(Tmin), max(Tmax), mean(Tmax), mean(Tmean))
#names(Monthly_Temp) <- c("Month", "Site",
#                          "Tmin", "AvgDailyLo", "Tmax", "AvgDailyHi", "Tmean")

#Yearly_Temp <- group_by(DailyTemp, Year, Site) %>%
#    summarize(min(Tmin), mean(Tmin), max(Tmax), mean(Tmax), mean(Tmean), mean(DiurnalMean))
#names(Yearly_Temp) <- c("Year", "Site",
#                        "Tmin", "AvgDailyLo", "Tmax", "AvgDailyHi", "Tmean", "DiurnalMean")


#########################################
##### Add in 2011-2014 Chart data #######
#########################################

#Read in and rbind chart temperature data for C1 and D1, dates 2011 Jan 1 - 2014 Dec 31
#These values do not have any QA flags for max temp values (yay!)
Chart.C1dailytemp <- read.csv("c-1tdayv.ml.data.csv")
Chart.D1dailytemp <- read.csv("d-1tdayv.ml.data.csv")

#Precip data
Chart.C1dailyppt <- read.csv("c-1pdayv.ml.data.csv")
Chart.D1dailyppt <- read.csv("d-1pdayv.ml.data.csv")


#subset both chart datasets for dates of interest (need to first convert date to Date), reformat to be consistent with Kittel datasets
Chart.C1dailytemp.11to14 <- mutate(Chart.C1dailytemp, date=as.Date(date)) %>%
        subset(date>"2010-12-31") %>%
        mutate(Month=month(date, label=TRUE), Day=day(date), Year=year(date), Site="C1") %>%
        rename(Tmax = max_temp, Tmin=min_temp, Tmean=mean_temp, Date=date) %>%
        select(Month, Day, Year, Tmax, Tmin, Tmean, Date, Site)

Chart.D1dailytemp.11to14 <- mutate(Chart.D1dailytemp, date=as.Date(date)) %>%
        subset(date>"2010-12-31") %>%
        mutate(Month=month(date, label=TRUE), Day=day(date), Year=year(date), Site="D1") %>%
        rename(Tmax = max_temp, Tmin=min_temp, Tmean=mean_temp, Date=date) %>%
        select(Month, Day, Year, Tmax, Tmin, Tmean, Date, Site)

Chart.C1dailyppt.11to14 <- Chart.C1dailyppt[,1:2] %>%
        rename(Date=date, PPT=precip) %>%
        mutate(Date=as.Date(Date), Month=month(Date, label=TRUE), Day= day(Date), Year=year(Date), Site="C1") %>%
        subset(Date>"2010-12-31")

Chart.D1dailyppt.11to14 <- Chart.D1dailyppt[,1:2] %>%
        rename(Date=date, PPT=precip) %>%
        mutate(Date=as.Date(Date), Month=month(Date, label=TRUE), Day=day(Date), Year=year(Date), Site="D1") %>%
        subset(Date>"2010-12-31")


#combine temp data in one DF, and ppt data in another
Chart.temp.11to14 <- rbind(Chart.C1dailytemp.11to14, Chart.D1dailytemp.11to14)
Chart.ppt.11to14 <- rbind(Chart.C1dailyppt.11to14, Chart.D1dailyppt.11to14)


#Summarize yearly precip, tmin, and tmax to add to yearly figures
#Remove NA values so will summarize
Chart.temp_Yrly <- na.omit(Chart.temp.11to14) %>%
        group_by(Year, Site) %>%
        summarise(Tmax = max(Tmax), Tmin=min(Tmin))

Chart.ppt_Yrly <- na.omit(Chart.ppt.11to14) %>%
        group_by(Year, Site) %>%
        summarise(PPT = sum(PPT))

#Add chart summaries to Kittel yearly summaries
Yearly_PPT_all <- rbind(Yearly_PPT, Chart.ppt_Yrly)

Yearly_Temp_all <- rbind(Yearly_T.extremes, Chart.temp_Yrly)
##########################################
####### Figures ##########################
##########################################

#Plot data in panel graphs, Katie wants line graphs for all:
#1) Yearly total precipitation for C1, D1, and Saddle
#2) Monthly trends for precip for C1, D1, and Saddle
#3) Max and min temps by year for C1, D1, and Saddle
#4) Monthly trends for max and min temps across all years for C1, D1 and Saddle

#Yearly total precip
YRLY.PPT <- ggplot(Yearly_PPT_all, aes(Year, PPT, color=Site))
a <- YRLY.PPT + geom_line(size=1) + 
        geom_point(aes(shape=Site), size=4, color="black") + 
        geom_point(aes(shape=Site), size=3) + 
        ylab(expression(paste("Precipitation (mm yr"^-1,")"))) +
        #geom_smooth(colour="black", data=subset(Yearly_PPT, Site=="D1" & Year<1977), method = lm, se=FALSE, lwd=1) +
        #geom_smooth(colour="black", data=subset(Yearly_PPT, Site=="D1" & Year>1977), method = lm, se=FALSE, lwd=1) +
        geom_smooth(colour="black", linetype=2,data=subset(Yearly_PPT, Site=="C1" & Year<1977), method = lm, se=FALSE, lwd=1) +
        geom_smooth(colour="black", linetype=2,data=subset(Yearly_PPT, Site=="C1" & Year>1977 & Year<2011), method = lm, se=FALSE, lwd=1) +
        geom_vline(x=1977, colour="black", linetype=4, lwd=1) +
        geom_segment(y=990, yend=990, x=1952, xend=1976, color="black", lwd=1) +
        geom_segment(y=1156, yend=1156, x=1978, xend= 2010, color="black", lwd=1) + 
        theme_classic(base_size = 16) +
        #geom_text(aes(2005, 1500, label = "D1 - High Alpine"), size=5, colour="black") +
        #geom_text(aes(2005, 400, label = "C1 - Subalpine"), size=5, colour="black") +
        #geom_text(aes(1978, 420, label = "1976/77 PDO \n Regime Shift"), colour="black", hjust=0, family="Courier") +
        geom_point(aes(1954, 10), color="white") +
        geom_point(aes(1954, 1799), color="white") +
        scale_y_continuous(breaks= c(seq(from=0, to=1800, by=200))) + 
        scale_x_continuous(breaks= c(seq(from=1950, to=2015, by=10))) + 
        theme(legend.position="none", axis.title.x = element_blank(),
                axis.text = element_text(size = 16),
                axis.title.y = element_text(vjust=1, size=18))

tiff("YearlyPrecip2.tiff", width=630, height=400)
a
dev.off()

#Mean monthly total precip#
MNLY.PPT <- ggplot(MonthlyPPT, aes(Month, Trend, color=Site))
b <- MNLY.PPT + geom_line(aes(group=Site), size=1) + 
        geom_point(aes(shape=Site), size=4, color="black") +
        geom_point(aes(shape=Site), size=3) +
        geom_hline(yintercept=0) +
        ylab(expression(paste("Precipitation Trend (mm mo"^-1, yr^-1,")"))) +
        #geom_text(aes(11, 0.7, label = "D1 - High Alpine"), size=5, colour="black") +
        #geom_text(aes(11, -0.3, label = "C1 - Subalpine"), size=5, colour="black") +
        geom_text(aes(2, 0.708, label= "*"), colour="black") +
        geom_text(aes(4, 1.231, label= "**"), colour="black") +
        geom_text(aes(10, 1.314, label= "***"), colour="black") +
        geom_text(aes(11, 0.873, label= "*"), colour="black") +
        geom_text(aes(12, 1.163, label= "**"), colour="black") +
        scale_y_continuous(breaks=c(seq(from=-1, to=1.5, by=0.5))) +
        geom_point(aes(1,-1), color="white") +
        geom_point(aes(1, 1.5), color="white") +
        theme_classic(base_size = 16) +
        theme(legend.position="none", axis.title.x = element_blank(),
                axis.text = element_text(size = 16),
                axis.title.y = element_text(vjust=1, size=18))

tiff("MonthlyPrecip.tiff", width=600, height=400)
b
dev.off()

#Temperature - Diurnal only
#YRLY.DIURNAL <- ggplot(Yearly_Diurnal, aes(Year, DiurnalMean, color=Site))
#c <- YRLY.DIURNAL + geom_line() + geom_point(aes(shape=Site), size=3) + 
#  ylab(expression(paste("Diurnal Temp. Range Trend ("*degree*C,")"))) + 
#  geom_smooth(colour="black", data=subset(Yearly_Diurnal, Site=="D1"), method = lm, se=FALSE, lwd=1) +
#  geom_smooth(colour="black", data=subset(Yearly_Diurnal, Site=="C1"), method = lm, se=FALSE, lwd=1) +
#  theme_classic(base_size = 14) +
#  geom_text(aes(2000, 8.5, label = "D1 - High Alpine"), size=5, colour="black") +
#  geom_text(aes(2000, 11.5, label = "C1 - Subalpine"), size=5, colour="black") +
#  theme(legend.position="none",
#        axis.text = element_text(size = 16),
#        axis.title.x = element_text(vjust=-0.5, size =18),
#        axis.title.y = element_text(vjust=0.5, size=18))

#MaxTemp Yearly and Monthly trends
YRLY.TMAX <- ggplot(Yearly_Temp_all, aes(Year, Tmax, color=Site))

#add significance points

c <- YRLY.TMAX + geom_line() +
        geom_point(aes(shape=Site), size=4, color="black") +
        geom_point(aes(shape=Site), size=3) + 
        ylab(expression(paste("Maximum Temperature ("*degree*C,")"))) + 
        geom_smooth(colour="black", data=subset(Yearly_Temp_all, Site=="D1" & Year<2011), method = lm, se=FALSE, lwd=1) +
        geom_smooth(colour="black", data=subset(Yearly_Temp_all, Site=="C1" & Year<2011), method = lm, se=FALSE, lwd=1) +
        theme_classic(base_size = 16) +
        #geom_text(aes(2000, 8.5, label = "D1 - High Alpine"), size=5, colour="black") +
        #geom_text(aes(2000, 11.5, label = "C1 - Subalpine"), size=5, colour="black") +
        scale_y_continuous(breaks=c(seq(from=0, to=40, by=5))) +
        scale_x_continuous(breaks= c(seq(from=1950, to=2015, by=10))) +
        geom_point(aes(1954, 11), color="white") +
        geom_point(aes(1954, 39), color="white") +
        theme(legend.position="none",
              axis.text = element_text(size = 16),
              axis.title.x = element_blank(),
              axis.title.y = element_text(vjust=1, size=18))


tiff("YearlyTmax.tiff", width=600, height=400)
c
dev.off()

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
MNLY.Temp <- ggplot(MonthlyTmax, aes(Month, Trend, color=Site))
d <- MNLY.Temp + geom_line(aes(group=Site), lwd=1) + 
        geom_point(aes(shape=Site), size=4, color="black") +
        geom_point(aes(shape=Site), size=3) +
        ylab(expression(paste("Maximum Temperature Trend ("*degree*C," ", yr^-1,")"))) +
        geom_hline(yintercept=0) +
        scale_y_continuous(breaks=c(seq(from=-0.04, to=0.10, by=0.02))) +
        geom_point(aes(1,-0.02), color="white") +
        theme_classic(base_size = 16) +
        theme(legend.position="none",
        axis.text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust=1, size=18)) + 
        geom_text(data=subset(MonthlyTmax2, Pval<0.05 & Pval>0.01), aes(Month, Point), label="*", color="black", size=6) +
        geom_text(data=subset(MonthlyTmax2, Pval<0.01 & Pval>0.001), aes(Month, Point), label="**", color="black", size=6) +
        geom_text(data=subset(MonthlyTmax2, Pval<0.001), aes(Month, Point), label="***", color="black", size=6)

tiff("MonthlyTMax.tiff", width=600, height=400)
d
dev.off()

#Not using these (for now)

#annotation from monthly diurnal temp (not being used for draft 2)
#geom_text(aes(10, 0.02, label = "D1 - High Alpine"), size=5, colour="black", hjust=0) +
#        geom_text(aes(10, 0.055, label = "C1 - Subalpine"), size=5, colour="black", hjust=0) +
#        geom_text(aes(1, 0.045, label= "***"), colour="black") +
#        geom_text(aes(2, 0.056, label= "***"), colour="black") +
#        geom_text(aes(3, 0.057, label= "***"), colour="black") +
#        geom_text(aes(4, 0.06, label= "***"), colour="black") +
#        geom_text(aes(5, 0.059, label= "***"), colour="black") + 
#        geom_text(aes(6, 0.053, label= "***"), colour="black") +
#        geom_text(aes(7, 0.048, label= "***"), colour="black") +
#        geom_text(aes(8, 0.036, label= "***"), colour="black") +
#        geom_text(aes(9, 0.053, label= "***"), colour="black") +
#        geom_text(aes(10, 0.036, label= "**"), colour="black") +
#        geom_text(aes(11, 0.047, label= "***"), colour="black") +
#        geom_text(aes(12, 0.045, label= "***"), colour="black") +
#        geom_text(aes(5, 0.026, label= "***"), colour="black") +
#        geom_text(aes(6, 0.027, label= "***"), colour="black") +
#        geom_text(aes(7, 0.015, label= "**"), colour="black") +
#        geom_text(aes(8, 0.012, label= "*"), colour="black") +
#        geom_text(aes(9, 0.02, label= "**"), colour="black") +


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


#QA check on chart data
tapply(Chart.D1dailytemp.11to14$Tmax, list(Chart.D1dailytemp.11to14$Year,Chart.D1dailytemp.11to14$Month), function(x) sum(is.na(x)))
tapply(Chart.C1dailytemp.11to14$Tmax, list(Chart.C1dailytemp.11to14$Year,Chart.C1dailytemp.11to14$Month), function(x) sum(is.na(x)))
tapply(Chart.C1dailyppt.11to14$PPT, list(Chart.C1dailyppt.11to14$Year,Chart.C1dailyppt.11to14$Month), function(x) sum(is.na(x)))
tapply(Chart.D1dailyppt.11to14$PPT, list(Chart.D1dailyppt.11to14$Year,Chart.D1dailyppt.11to14$Month), function(x) sum(is.na(x)))