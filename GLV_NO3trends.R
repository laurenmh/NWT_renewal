#Caitlin White#
#INSTAAR, University of Colorado#
#caitlin.t.whit@colorado.edu#
#November 2015#

#Compile N03 data from Nel Caine's sampling in GLV, 1982-recent (years 2013 and 2014 not avaiable yet)
#Summarize trends for NWT proposal

#set working directory as appropriate..
setwd("~/Documents/Suding Lab/NWT LTER/GLV_BioandChem/Data")

#load needed libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

#read in .csv files from 9 sites monitored
Arikaree <- read.csv("ariksolu.nc.data.csv") %>%
  tbl_df()
Albion <- read.csv("albisolu.nc.data.csv") %>%
  tbl_df()
Inlet <- read.csv("inlesolu.nc.data.csv") %>%
  tbl_df()
Martinelli <- read.csv("martsolu.nc.data.csv") %>%
  tbl_df()
Navajo <- read.csv("navasolu.nc.data.csv") %>%
  tbl_df()
Spill <- read.csv("spilsolu.nc.data.csv") %>%
  tbl_df()
GL1<- read.csv("gre1solu.nc.data.csv") %>%
  tbl_df()
GL4 <- read.csv("gre4solu.nc.data.csv") %>%
  tbl_df()
GL5 <- read.csv("gre5solu.nc.data.csv") %>%
  tbl_df()

#subset data just for date, location and NO3#
Albion.NO3 <- Albion[,c(1,2,16)] 
Arikaree.NO3 <- Arikaree[,c(1,2,16)] 
Inlet.NO3 <- Inlet[,c(1,2,16)]
Martinelli.NO3 <- Martinelli[,c(1,2,16)]
Navajo.NO3 <- Navajo[,c(1,2,16)]
Spill.NO3 <- Spill[,c(1,2,16)]
GL1.NO3 <- GL1[,c(1,2,16)]
GL4.NO3 <- GL4[,c(1,2,16)]
GL5.NO3 <- GL5[,c(1,2,16)]

#Individual dataset cleaning..
#Clean Albion dates manually (lubridate converts one value from 1993 to "NA")
Albion.NO3 <- mutate(Albion.NO3, Date=as.Date(date.collected..yyyy.mm.dd.))
Albion.NO3[264,4] <- ymd("1993-06-31")
names(Albion.NO3) <- c("Site", "rawDate", "NO3", "Date")
Albion.NO3 <- select(Albion.NO3, Site, Date, NO3)

#Apply uniform name to Martinelli data (unique() shows there are two character variations of "Martinelli".. maybe extra space char?)
unique(Martinelli.NO3$sample.location)
Martinelli.NO3$sample.location <- "MARTINELLI"

#Remove tunnel and waterfall observations from GL4 (2 obs of 737), and remove as factors
GL4.NO3 <- subset(GL4.NO3, sample.location=="GREEN LAKE 4")
GL4.NO3$sample.location <- "GREEN LAKE 4"
  
#Reformat GL5.NO3 date variable to yyyy-mm-dd to match others, and remove Green Lake 5 Rock Glacier obs (31 of 511)
GL5.NO3 <- mutate(GL5.NO3, rawDate=mdy(date.collected..yyyy.mm.dd.),
                  Day=day(rawDate),
                  Month=month(rawDate),
                  Year=as.numeric(year(rawDate)),
                  NewYear = ifelse(Year<2015, Year,Year-100),
                  Date=ymd((paste(NewYear, Month, Day)))) %>%
  rename(Site=sample.location, NO3 = NO3...ueq.L.) %>%
  select(Site, Date, NO3) %>%
  subset(Site=="GREEN LAKE 5")
GL5.NO3$Site <- "GREEN LAKE 5" #Removes "Green Lake 5 Rock Glacier" as factor

#combine all in one master dataset for cleaning, starting with all but Albion and GL5 first
AllSites.NO3 <- rbind(Arikaree.NO3,
                      Inlet.NO3,
                      Martinelli.NO3,
                      Navajo.NO3,
                      Spill.NO3,
                      GL1.NO3,
                      GL4.NO3) %>%
  rename(Site=sample.location, NO3 = NO3...ueq.L.) %>%
  mutate(Date=as.Date(date.collected..yyyy.mm.dd.)) %>%
  select(Site, Date, NO3)

#Add GL5 data to AllSites and add column for Month and Year, and column for numeric-only NO3 values
#Metatadata says "NP" value means "not performed#
AllSites.NO3 <- rbind(AllSites.NO3, Albion.NO3,GL5.NO3) %>%
  mutate(Month = month(Date, label=TRUE), Year = year(Date),
         NumNO3 = extract_numeric(NO3))

sum(!is.na(AllSites.NO3$NumNO3)) #4247 NO3 observations have numeric data
sum(is.na(AllSites.NO3$NumNO3)) #250 NA NO3 values in raw data

CleanNO3 <- na.omit(AllSites.NO3) #4247 observations

sum(CleanNO3$NumNO3<0.09) #Metadata says detection limit for NO3 is 0.09 ueq/L. 
#There are 29 observations under 0.09. One is 0.08. Decide to remove values less than 0.08.
CleanNO3 <- subset(CleanNO3, NumNO3>0.07)

#read in lakes elevation data and merge to summarize by elevation type
setwd("~/Dropbox/NWT_data")
GLV.elevations <- read.csv("GLV_elevations.csv") %>%
  tbl_df()
MergedNO3 <- merge(CleanNO3,GLV.elevations, by="Site")


##EDIT (12/17/15): Modeling
#Read in and merge sumallPCA scores to cleaned data, before summarizing#
setwd("~/Dropbox/NWT_data")
PCAscores <- read.csv("NWT_Climate_summerPCscores_20151123.csv")
MergedNO3 <- merge(MergedNO3, PCAscores, by="Year")
write.csv(MergedNO3, file="MergedNO3.csv")


#Summary tables for NO3
#1) Max, min and mean NO3 at all lakes over all years
SumNO3_SitexMoxYr <- group_by(MergedNO3, Site, Elev_Type, Group, Month, Year) %>%
  summarize(min(NumNO3), mean(NumNO3), max(NumNO3), length(NumNO3)) %>%
  rename(MinNO3 = `min(NumNO3)`, MeanNO3 = `mean(NumNO3)`, MaxNO3 = `max(NumNO3)`, No.Obs = `length(NumNO3)`) 

ggplot(SumNO3_SitexMoxYr, aes(Year, MeanNO3, color=Month)) + geom_line() + facet_grid(~Site) + stat_smooth(method="lm", se=F, colour="black")
ggplot(SumNO3_SitexMoxYr, aes(Year, MeanNO3, color=Month)) + geom_line() + facet_grid(~Elev_Type) + stat_smooth(method="lm", se=F, colour="black")
#decide to select only for Months May - October because not all lakes have data for non-summer months

#2) Filter for summer months only and calculate min, max and mean by year by lake site
SummerNO3_SitexYear <- subset(SumNO3_SitexMoxYr, Month == "May" | Month =="Jun" | Month =="Jul" | Month =="Aug" | Month =="Oct") %>% 
  group_by(Site, Elev_Type, Group, Year) %>%
  summarize(min(MinNO3), max(MaxNO3), mean(MeanNO3), length(MeanNO3)) %>%
  rename(MinNO3 = `min(MinNO3)`, MeanNO3 = `mean(MeanNO3)`, MaxNO3 = `max(MaxNO3)`, No.Obs = `length(MeanNO3)`) 

ggplot(SummerNO3_SitexYear, aes(Year, MeanNO3, color=Elev_Type)) + geom_line() + facet_grid(~Site) + stat_smooth(method="lm", se=F, colour="black")
ggplot(SummerNO3_SitexYear, aes(Year, MeanNO3)) + geom_line() + facet_grid(~Elev_Type) + stat_smooth(method="lm", se=F, colour="black")
ggplot(SummerNO3_SitexYear, aes(Year, MinNO3, color=Elev_Type)) + geom_line() + facet_grid(~Site) + stat_smooth(method="lm", se=F, colour="black")
ggplot(SummerNO3_SitexYear, aes(Year, MaxNO3, color=Elev_Type)) + geom_line() + facet_grid(~Site) + stat_smooth(method="lm", se=F, colour="black")
#might want to re-class elevation types. GL4 behaving more like "high" elevation sites
#Upon reviewing, Katie says to move GL4 to "High" and group remaining "Middle" sites into "low". Created additional "Group" column in elevation .csv
#Re-check using Group instead of Elev_Type

ggplot(SummerNO3_SitexYear, aes(Year, MeanNO3, color=Group)) + geom_line() + facet_grid(~Site) + stat_smooth(method="lm", se=F, colour="black")
ggplot(SummerNO3_SitexYear, aes(Year, MeanNO3)) + geom_line() + facet_grid(~Group) + stat_smooth(method="lm", se=F, colour="black")
ggplot(SummerNO3_SitexYear, aes(Year, MinNO3, color=Group)) + geom_line() + facet_grid(~Site) + stat_smooth(method="lm", se=F, colour="black")
ggplot(SummerNO3_SitexYear, aes(Year, MaxNO3, color=Group)) + geom_line() + facet_grid(~Site) + stat_smooth(method="lm", se=F, colour="black")

#Sum by Group for plotting against PC1 scores
SummerNO3_GroupxYear <- group_by(SummerNO3_SitexYear, Group, Year) %>%
  summarize(min(MinNO3), mean(MeanNO3), max(MaxNO3), length(MeanNO3)) %>%
  rename(MinNO3 = `min(MinNO3)`, MeanNO3 = `mean(MeanNO3)`, MaxNO3 = `max(MaxNO3)`, No.Obs = `length(MeanNO3)`) 

ggplot(SummerNO3_GroupxYear, aes(Year, MeanNO3, color=Group, group=Group)) + geom_line() + stat_smooth(aes(group=Group), method="lm", se=F, colour="black")
ggplot(SummerNO3_GroupxYear, aes(Year, MinNO3, color=Group)) + geom_line() + stat_smooth(aes(group=Group), method="lm", se=F, colour="black")
ggplot(SummerNO3_GroupxYear, aes(Year, MaxNO3, color=Group)) + geom_line() + stat_smooth(aes(group=Group), method="lm", se=F, colour="black")


#Final NO3 figure for renewal proposal
#Merge with PCA1 scores (uses pcouts2 from climate_PCA.R)

SummerNO3_withPCA <- rename(SummerNO3_GroupxYear, year=Year)
SummerNO3_withPCA <- merge(SummerNO3_withPCA, pcouts2, by ="year")

summallPCA1xMeanNO3 <- ggplot(SummerNO3_withPCA, aes(sumallPCA, MeanNO3, color=Group))
summallPCA1xMeanNO3 + geom_line() + 