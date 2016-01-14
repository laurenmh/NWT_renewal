#load needed libraries#
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

#Set working directory as appropriate...#
datpath = "~/Dropbox/NWT_data/NWT_GreenLakes_data" 

#[1] Read in .csv file from Kathi Hell containing chl-a and NO3 data from 2000 to 2014#
Kathi_chlA <- read.csv(file.path(datpath, "GL4_chlA_NO3_2000to2014.csv")) %>%
    tbl_df() %>%
    rename(Depth = Depth_m.Location, NO3 = NO3_ueq.L, chlA = Chl.a_ug.L) 

#Convert NO3 and chlA fields to numeric, and change non-numeric values to NA for summarizing#
ChlA_cleaned <- mutate(Kathi_chlA, NO3=extract_numeric(NO3), chlA=extract_numeric(chlA), year=Year)

#merge in with the summer PCA
#pcouts2 comes from the climate_PCA file
ChlA_cleaned2 <-merge(ChlA_cleaned, pcouts2) %>%
  tbl_df()

#quick visuals
ggplot(ChlA_cleaned2, aes(y=chlA, x=sumallPC1)) + geom_point() + facet_wrap(~Depth) + geom_smooth(method="lm")
ggplot(ChlA_cleaned2, aes(y=chlA, x=year)) + geom_point() + facet_wrap(~Depth) + geom_smooth(method="lm")


#Group by Year and Depth, Summarize NO3 and ChlA for mean and max values#
GL4_trends_00to14 <- group_by(ChlA_cleaned, Year, Depth) %>%
    summarise(mean(NO3, na.rm=TRUE), max(NO3, na.rm=TRUE), mean(chlA, na.rm=TRUE), max(chlA, na.rm=TRUE))
names(GL4_trends_00to14) <- c("Year", "Depth", "AvgNO3", "MaxNO3", "AvgChlA", "MaxChlA")

#Visualize data trends#
ggplot(GL4_trends_00to14, aes(Year, AvgNO3, color=Depth)) + geom_line()
ggplot(GL4_trends_00to14, aes(Year, MaxNO3, color=Depth)) + geom_line()
ggplot(GL4_trends_00to14, aes(Year, AvgChlA, color=Depth)) + geom_line()
ggplot(GL4_trends_00to14, aes(Year, MaxChlA, color=Depth)) + geom_line()

#merge with the PCA data
GL4_trends_00to14$year <-GL4_trends_00to14$Year
GL4_clim <-merge(GL4_trends_00to14, pcouts2) %>%
  tbl_df()

#quick general visual
ggplot((GL4_clim), aes(x=sumallPC1, y=MaxChlA)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~Depth)

#Nice visual of just the lake depths (which have more data than the inlet and outlet)
#includes the high values (which I think are artifacts of high inputs, need to check with Kathi)
ChlA_panel_withoutlier<- ggplot(subset(GL4_clim, Depth !="Inlet" & Depth!="Outlet"), aes(x=sumallPC1, y=MaxChlA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Depth, ncol=1, scale="free_x")  +
  theme_classic() + theme(strip.background = element_blank(), text=element_text(size=16)) +
  labs(x="PC1 (Length of summer)", y = "Max Chlorophyll A")

#  tiff("ChlA_panel_withoutlier.tiff", height=900, width=400)
#  ChlA_panel_withoutlier
#  dev.off()

#quick regressions
l<-lm(MaxChlA~sumallPC1, subset(GL4_clim, Depth==9 & MaxChlA<10))
summary(l)

l<-lm(MaxChlA~sumallPC1, subset(GL4_clim, MaxChlA<10))
summary(l)


#Nice visual of just the lake depths (which have more data than the inlet and outlet)
#Removed the really high ChL because it looks like that was a product of a high inlet value
 ChlA_panel_nooutlier <- ggplot(subset(GL4_clim, Depth !="Inlet" & Depth!="Outlet"& MaxChlA<10), aes(x=sumallPC1, y=MaxChlA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Depth, ncol=1, scale="free_x")  +
  theme_classic() + theme(strip.background = element_blank(), text=element_text(size=16)) +
  labs(x="PC1 (Length of summer)", y = "Max Chlorophyll A")

#  tiff("ChlA_panel_nooutlier.tiff", height=900, width=400)
#  ChlA_panel_nooutlier
#  dev.off()

#here is with all the levels, graphs by value
##check for significance in lm trendline by depth
MaxChla.0<-lm(MaxChlA~sumallPC1, subset(GL4_clim, Depth==0 & MaxChlA<10))
summary(MaxChla.0) #not signif
MaxChla.3<-lm(MaxChlA~sumallPC1, subset(GL4_clim, Depth==3 & MaxChlA<10))
summary(MaxChla.3) #not signif
MaxChla.9<-lm(MaxChlA~sumallPC1, subset(GL4_clim, Depth==9 & MaxChlA<10))
summary(MaxChla.9) #signif at p<0.05


#Caitlin uses this graph for renewal figure
## [1] add in Eric Sokol's theme
# -- plotting parameters and themes
text.size<-16
margins.plot<-unit(c(0.5,0.5,0.5,2.5), 'lines') #top, right, bottom, left
margins.axes<-unit(.25,'lines')
margins.panel<-unit(3,'lines')

plottheme<-theme(plot.margin = margins.plot,
                 axis.ticks.margin = margins.axes,
                 axis.title = element_text(face='plain'))

## [2] remake Lauren's max chl-a figure
#need to add white/blank point at (-1.5, 0) to make x axis same scale as Pika figure
#Note 12/29: Katie wants fit lines to be black
singlepanel_ChlA <- ggplot(subset(GL4_clim, Depth !="Inlet" & Depth!="Outlet" & MaxChlA<10), aes(x=sumallPC1, y=MaxChlA, color=Depth)) + 
        geom_point(aes(shape=Depth), size=4, color="black") +
        geom_point(aes(shape=Depth), size=3) +
        geom_smooth(data=subset(GL4_clim, Depth==0), method="lm", se=F, linetype=2, lwd=1, color="black") + 
        #geom_smooth(data=subset(GL4_clim, Depth==0), method="lm", se=F, linetype=5, lwd=.8) + 
        geom_smooth(data=subset(GL4_clim, Depth==3), method="lm", se=F, linetype=2, lwd=1, color="black") +
        #geom_smooth(data=subset(GL4_clim, Depth==3), method="lm", se=F, linetype=5, lwd=.8) + 
        geom_smooth(data=subset(GL4_clim, Depth==9), method="lm", se=F, lwd=1, color="black") +
        #geom_smooth(data=subset(GL4_clim, Depth==9), method="lm", se=F, lwd=.8) + 
        #xlab("PC1 (Length of summer)") +
        ylab(expression(paste("Max Chlorophyll ", italic("a")," (",mu,"g ", L^-1,")"))) +
        theme_classic() + 
        theme(text=element_text(size=text.size),
              axis.text.x=element_text(size=text.size),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=text.size),
              axis.title.y=element_text(vjust=1),
              legend.position="none",
              aspect.ratio=1) +
        geom_point(aes(-1.36,0), color="white") +
        scale_x_continuous(breaks= c(-1.0, -0.5, 0, 0.5, 1.0, 1.5)) +
        plottheme
    

tiff("singlepanel_ChlA3.tiff", height=400, width=500)
singlepanel_ChlA
dev.off()

#plot Avg Chl-a just to see.. model not significant at any depth
ggplot(subset(GL4_clim, Depth !="Inlet" & Depth!="Outlet"), aes(x=sumallPC1, y=AvgChlA, color=Depth)) + 
        geom_point(aes(shape=Depth), size=4, color="black") +
        geom_point(aes(shape=Depth), size=3) + 
        geom_smooth(method="lm", se=F)

#lm summaries for Avg Chl-a
AvgChla.0<-lm(AvgChlA~sumallPC1, subset(GL4_clim, Depth==0))
summary(AvgChla.0) #not signif
AvgChla.3<-lm(AvgChlA~sumallPC1, subset(GL4_clim, Depth==3))
summary(AvgChla.3) #not signif
AvgChla.9<-lm(AvgChlA~sumallPC1, subset(GL4_clim, Depth==9))
summary(AvgChla.9) #not signif (p=0.1085)



#and with all levels, graphed by value, then with fit across depths
singlepanel_ChlA_2<- ggplot(subset(GL4_clim, Depth !="Inlet" & Depth!="Outlet" & MaxChlA<10), aes(x=sumallPC1, y=MaxChlA)) + 
  geom_point(aes(color=Depth), size=4) + geom_smooth(method="lm", se=F, color="black", lwd=2) + 
  theme_classic() + theme(strip.background = element_blank(), text=element_text(size=16)) +
  labs(x="PC1 (Length of summer)", y = "Max Chlorophyll A")
# 
# tiff("singlepanel_ChlA_2.tiff", height=400, width=400)
# singlepanel_ChlA_2
# dev.off()

#Just the output value (which is what the landscape sees; unfortunately not collected in many high summer years)
output_ChlA <- ggplot(subset(GL4_clim,Depth=="Outlet"), aes(x=sumallPC1, y=MaxChlA)) + 
  geom_point(size=4) + geom_smooth(method="lm", lwd=2) + 
  theme_classic() + theme( text=element_text(size=16)) +
  labs(x="PC1 (Length of summer)", y = "Max Chlorophyll A")

# tiff("output_ChlA.tiff", height=400, width=400)
# output_ChlA
# dev.off()


#[2] Read in earlier water chemistry data from Niwot website -- only NO3, no ChlA data available#
#[2a] 1982-2011 dataset#
GLVDat_1982to2011 <- read.csv(file.path(datpath, "gre4solu.nc.data.csv")) %>%
    tbl_df()

#Filter and clean data, selecting only for GL4 and NO3 data#
GL4_82to11_cleaned <- select(GLVDat_1982to2011, sample.location, date.collected..yyyy.mm.dd., NO3...ueq.L.) %>%
    rename(location = sample.location, date = date.collected..yyyy.mm.dd., NO3 = NO3...ueq.L.) %>%
    mutate(NO3=extract_numeric(NO3), Year = year(date), Month = month(date)) %>%
    filter(location == "GREEN LAKE 4")

#Further filter to only select summer months (June, July, August) to compare with Kathi's data#
GL4_82to11_summerdat <- filter(GL4_82to11_cleaned, Month>5 & Month<9) %>%
    group_by(Year) %>%
    summarise(mean(NO3, na.rm=TRUE), max(NO3, na.rm=TRUE)) %>%
    mutate(Depth = "Outlet")
names(GL4_82to11_summerdat) <- c("Year", "AvgNO3", "MaxNO3", "Depth")

#Visualize data trends#
ggplot(GL4_82to11_summerdat, aes(Year, AvgNO3)) + geom_line()
ggplot(GL4_82to11_summerdat, aes(Year, MaxNO3)) + geom_line()



#[2b] Read in 1998 to 2012 dataset#
GLVDat_1998to2012 <- read.csv(file.path(datpath, "water_chemistry_GL.dm.data.csv")) %>%
    tbl_df()

#Filter and clean data, selecting only for GL4 and NO3 data#
GL4_98to12_cleaned <- select(GLVDat_1998to2012, site, depth.loc, date, NO3.) %>%
    rename(Depth=depth.loc, NO3 = NO3.) %>%
    mutate(NO3=extract_numeric(NO3), Year = year(date), Month = month(date)) %>%
    filter(site == "GL4", Depth == "3m" | Depth == "9m" | Depth =="Inlet" | Depth =="Outlet" |Depth == "Surface" | Depth == "0m" | Depth == "3 A" | Depth == "3 B" | Depth == "3m.a" | Depth == "3m.b" | Depth == "3m.c") %>%
    mutate(Depth_m = extract_numeric(Depth), 
           newDepth = ifelse(Depth=="Surface", "0", 
                             ifelse(Depth=="Inlet", "Inlet", 
                                    ifelse(Depth=="Outlet", "Outlet", Depth_m)))) %>%
    filter(Month > 5 & Month <9)

#Group and summarize data#
GL4_trends_98to12 <- group_by(GL4_98to12_cleaned, Year, newDepth) %>%
    summarise(mean(NO3, na.rm=TRUE), max(NO3, na.rm=TRUE))
names(GL4_trends_98to12) <- c("Year", "Depth", "AvgNO3", "MaxNO3")

#Visualize data#
ggplot(GL4_trends_98to12, aes(Year, AvgNO3, color=Depth)) + geom_line()
ggplot(GL4_trends_98to12, aes(Year, MaxNO3, color=Depth)) + geom_line()


#Combine all three sets, selecting only for depth = outlet, and adding file source column to compare values#
Copy_GL4_trends_98to12 <- subset(GL4_trends_98to12, Depth == "Outlet") %>%
    mutate(Source = "water_chemistry_GL_dm")
Copy_GL4_82to11_summerdat <- mutate(GL4_82to11_summerdat, Source = "gre4solu.nc.data") %>%
    select(Year, Depth, AvgNO3, MaxNO3, Source)
Copy_GL4_trends_00to14 <- subset(GL4_trends_00to14, Depth == "Outlet") %>%
    mutate(Source = "Kathi Hell") %>%
    select(Year, Depth, AvgNO3, MaxNO3, Source)
Combined_GL4_outlet <- rbind(Copy_GL4_82to11_summerdat, Copy_GL4_trends_98to12, Copy_GL4_trends_00to14)

#Visualize to compare Avg NO3 and Max NO3 where datasets overlap#
ggplot(Combined_GL4_outlet, aes(Year, AvgNO3, color=Source)) + geom_line() + theme_bw()
ggplot(Combined_GL4_outlet, aes(Year, MaxNO3, color=Source)) + geom_line() + theme_bw()

#Try combining Nel's 1982 to 2011 data an Kathi's 2000 to 2014 data points, for depth = outlet and NO3 only, to see how individual daily measurements compare.#
#Difference in trends graph may be a function of number of samples taken by each group over the summer and timing of samples (Nel's group started earlier in June), and number of observations per summer to average#
Kathi_outlet_NO3 <- filter(ChlA_cleaned, Depth == "Outlet") %>%
    select(Date, Year, NO3) %>%
    mutate(Source = "Kathi Hell", Date = as.Date(Date, "%m/%d/%y"), Year = year(Date))

Nel_outlet_NO3 <- filter(GL4_82to11_cleaned, Month>5 & Month<9) %>%
    select(date, Year, NO3) %>%
    rename(Date = date) %>%
    mutate(Source = "gre4solu.nc.data.csv", Date = as.Date(Date))

Combined_Kathi_Nel <- rbind(Kathi_outlet_NO3, Nel_outlet_NO3)

#Visualize data for comparison#
ggplot(Combined_Kathi_Nel, aes(Date, NO3, color=Source)) + geom_point(alpha=0.5) + theme_bw()

#Overlap values are fairly close when compare day to day in table. Decide to average readings between sets for overlapping days, then average by year for final figure#
Combined_Kathi_Nel_AvgdxDay <- group_by(Combined_Kathi_Nel, Date, Year) %>%
    subset(!is.na(NO3)) %>%
    summarise(mean(NO3, na.rm=TRUE))
names(Combined_Kathi_Nel_AvgdxDay) <- c("Date", "Year", "NO3")

plot(Combined_Kathi_Nel_AvgdxDay$Date, Combined_Kathi_Nel_AvgdxDay$NO3)

#Group_by Year and summarize mean and max NO3#
Outlet_NO3_final <- group_by(Combined_Kathi_Nel_AvgdxDay, Year) %>%
    summarise(mean(NO3), max(NO3), sd(NO3), length(Year)) %>%
    mutate(seNO3 = `sd(NO3)`/ sqrt(`length(Year)`))
names(Outlet_NO3_final) <- c("Year", "AvgNO3", "MaxNO3", "sdNO3","n", "seNO3")

#Final long-term trends in GL4 Outlet Summer NO3 figures#
ggplot(Outlet_NO3_final, aes(Year, AvgNO3)) + 
    geom_errorbar(aes(ymin=AvgNO3-seNO3, ymax=AvgNO3+seNO3), width=.1, color="red") + 
    geom_line() + 
    ylab("NO3- (ueq/L)") + 
    ggtitle("Average summer (June-August) NO3- in Green Lake 4 outlet, 1985-2014") + 
    theme_bw(base_size = 15)
ggplot(Outlet_NO3_final, aes(Year, MaxNO3)) + 
    geom_line() +
    ylab("NO3- (ueq/L)") + 
    ggtitle("Maximum summer (June-August) NO3- in Green Lake 4 outlet, 1985-2014") + 
    theme_bw(base_size = 15)
