##Caitlin White##
##INSTAAR, University of Colorado##
##caitlintwhite@colorado.edu##
##January 2016##

#R code to plot Katherine Wentz's Saddle limitation model outputs for years 2002, 2004, and 2005

#set wd as appropriate
setwd("~/Documents/Suding Lab/NWT LTER/Katherine_Wentz")


# -- plotting parameters and themes (borrowed from Eric Sokol's graphs)
text.size<-8
limits.x<-c(0, 150)
margins.plot<-unit(c(0.25,0.4,0.25,.25), 'lines') #top, right, bottom, left
margins.axes<-unit(.25,'lines')
margins.panel<-unit(3,'lines')

plottheme<-theme(plot.margin = margins.plot,
                 axis.title = element_text(
                         face='plain')
)


# -- read in .csv files for years 2002, 2004 and 2005
Dat2002 <- read.csv("Limitations_2002.csv")
Dat2004 <- read.csv("Limitations_2004.csv")
Dat2005 <- read.csv("Limitations_2005.csv")

#check files to verify field types as numeric
str(Dat2002)
str(Dat2004)
str(Dat2005)

#check NAs -- Katherine wrote she has missing values on days where Saddle radiation/air temp data not available
summary(Dat2002) #20 rows with NAs
summary(Dat2004) #38 rows with NAs
summary(Dat2005) #20 rows with NAs


# -- load ggplot, dplyr and tidyr for tidying data and plotting 3 years in one panel
library(ggplot2)
library(dplyr)
library(tidyr)

#rename variables for legend, and gather model output into one column for plotting
Dat2002.collapsed <- rename(Dat2002, Temperature = FGPPTA, Soil_Moisture = GPPswc, Nitrogen = FNV, Phosphorus = FPV) %>%
        #mutate(Total = Temperature + Soil_Moisture + Nitrogen, pTemp = Temperature/Total, pSoil_Moisture = Soil_Moisture/Total, pNitrogen = Nitrogen/Total) %>%
        mutate(AltSoilMoisture = ifelse(Soil_Moisture=="NA", "NA", 
                                        ifelse(DOY<137 | DOY>276, 0, Soil_Moisture))) %>%
        gather(model, output, -DOY)
Dat2004.collapsed <- rename(Dat2004, Temperature = FGPPTA, Soil_Moisture = GPPswc, Nitrogen = FNV, Phosphorus = FPV) %>% 
        mutate(AltSoilMoisture = ifelse(Soil_Moisture=="NA", "NA", 
                                        ifelse(DOY<141 | DOY>285, 0, Soil_Moisture))) %>%
        gather(model, output, -DOY)
Dat2005.collapsed <- rename(Dat2005, Temperature = FGPPTA, Soil_Moisture = GPPswc, Nitrogen = FNV, Phosphorus = FPV) %>%
        mutate(AltSoilMoisture = ifelse(Soil_Moisture=="NA", "NA", 
                                        ifelse(DOY<145, 0, Soil_Moisture))) %>%
        gather(model, output, -DOY)

GS2002 <- subset(Dat2002.collapsed, DOY > 91 & DOY < 300) #April 2 - Oct 26 
GS2004 <- subset(Dat2004.collapsed, DOY > 92 & DOY < 301) #April 2 - Oct 26
GS2005 <- subset(Dat2005.collapsed, DOY > 91 & DOY < 300) #April 2 - Oct 26


#load libraries for exracting spline y values

library(ggplot2)
library(splines)

### -- 2002##
#extract predicted y values from smoothing splines 
GSLimit2002 <- ggplot(data=subset(GS2002, model !="Phosphorus"), aes(DOY, output, color=model))
GSLimit2002 + stat_smooth(aes(outfit=fit<<-..y..), method="lm", fullrange = TRUE, formula=y ~ ns(x,10), na.rm=T, se=F, n=208)
#order of predicted y values from splines is: Temp, Soil_Moisture, Nitrogen, AltSoilMoisture

#create vectors of model names and DOY to bind with y values
Model <- c(rep_len("Temperature", 208), rep_len("Soil_Moisture", 208), rep_len("Nitrogen", 208), rep_len("AltSoilMoisture", 208))
DOY <- rep(seq(from=92, to=299), 4)

#bind all and plot to verify captured correctly
my.df.2002 <- data.frame(DOY, Model, fit)
ggplot(my.df.2002, aes(DOY, fit, color=Model)) + geom_point() + geom_line() #looks correct compared to ggplot with raw data and stat_smooth called

#manipulate values so y value boundaries are ymin=0, ymax=1
my.df.2002.limited <- my.df.2002 %>%
        mutate(CorrectedFit = ifelse(fit>1, 1,
                                     ifelse(fit<0, 0, fit))) %>%
        mutate(CorrectedFit2= ifelse(Model=="Nitrogen" & DOY<118, 0, 
                                  ifelse(Model=="Nitrogen" & DOY>250, 0, 
                                         ifelse(Model=="AltSoilMoisture" & DOY < 100, 0, CorrectedFit))))

my.df.2002.limited.subset <- subset(my.df.2002.limited, Model != "Soil_Moisture") %>%
        mutate(DaysAfterMay1 =DOY-121)
levels(my.df.2002.limited.subset$Model)[levels(my.df.2002.limited.subset$Model)=="AltSoilMoisture"] <- "Soil Moisture"

#plot
Bottom <- ggplot(my.df.2002.limited.subset, 
       aes(DaysAfterMay1, CorrectedFit2, color=Model)) + 
        #geom_point() + 
        geom_line(lwd=1) +
        geom_ribbon(aes(ymin=0, ymax=my.df.2002.limited.subset$CorrectedFit2, fill=Model), alpha=0.2) +
        theme_classic() +
        labs(x=NULL,y=NULL) +
        #labs(x = "Days from May 1", y=NULL) +
        scale_y_continuous(breaks=seq(from=0, to=1, by=0.25),
                           limits=c(0,1),
                           expand=c(0,0)) +
        scale_x_continuous(limits=c(0,151),
                           expand=c(0,0)) +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size, margin=margin(3,5,5,5,"pt")),
              #axis.text.y = element_text(size=text.size, margin=margin(5,3,5,5,"pt")),
              axis.text.y=element_blank(),
              legend.position = "none") +
        plottheme
        #scale_fill_brewer(palette="Set2") +
        #scale_color_brewer(palette="Set2")



### -- 2004
#extract predicted y values from smoothing splines 
GSLimit2004 <- ggplot(data=subset(GS2004, model !="Phosphorus"), aes(DOY, output, color=model))
GSLimit2004 + geom_point() + stat_smooth(aes(outfit=fit04<<-..y..), method="lm", fullrange = TRUE, formula=y ~ ns(x,10), na.rm=T, se=F, n=208)
#order of predicted y values from splines is: Temp, Soil_Moisture, Nitrogen, AltSoilMoisture

#create vectors of model names to and DOY to bind with y values
DOY04 <- rep(seq(from=93, to=300), 4)

#bind all and plot to verify captured correctly
my.df.2004 <- data.frame(DOY04, Model, fit04)
ggplot(my.df.2004, aes(DOY04, fit04, color=Model)) + geom_point() + geom_line() #looks correct


#manipulate values so y value boundaries are ymin=0, ymax=1
my.df.2004.limited <- my.df.2004 %>%
        mutate(CorrectedFit = ifelse(fit04>1, 1,
                                     ifelse(fit04<0, 0, fit04))) %>%
        mutate(CorrectedFit2= ifelse(Model=="Nitrogen" & DOY04<120, 0, 
                                     ifelse(Model=="Nitrogen" & DOY04>294, 0, 
                                            ifelse(Model=="AltSoilMoisture" & DOY04 < 120, 0, CorrectedFit))))

my.df.2004.limited.subset <- subset(my.df.2004.limited, Model != "Soil_Moisture") %>%
        mutate(DaysAfterMay1 =DOY04-122)
levels(my.df.2004.limited.subset$Model)[levels(my.df.2004.limited.subset$Model)=="AltSoilMoisture"] <- "Soil Moisture"

#plot
Top <- ggplot(my.df.2004.limited.subset, 
              aes(DaysAfterMay1, CorrectedFit2, color=Model)) + 
        #geom_point() + 
        geom_line(lwd=1) +
        geom_ribbon(aes(ymin=0, ymax=my.df.2004.limited.subset$CorrectedFit2, fill=Model), alpha=0.2) +
        theme_classic() +
        labs(x = NULL, y=NULL) +
        scale_y_continuous(breaks=seq(from=0, to=1, by=0.25),
                           limits=c(0,1),
                           expand=c(0,0)) +
        scale_x_continuous(limits=c(0,151),
                           expand=c(0,0)) +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size, margin=margin(3,5,5,5,"pt")),
              axis.text.y = element_text(size=text.size, margin=margin(5,3,5,5, "pt")),
              legend.position = "none",
              legend.title=element_blank()) +
        plottheme



## -- 2005
#extract predicted y values from smoothing splines 
GSLimit2005 <- ggplot(data=subset(GS2005, model !="Phosphorus"), aes(DOY, output, color=model))
GSLimit2005 + geom_point() + geom_line() + stat_smooth(aes(outfit=fit05<<-..y..), method="lm", fullrange = TRUE, formula=y ~ ns(x,10), na.rm=T, se=F, n=208)
#order of predicted y values from splines is: Temp, Soil_Moisture, Nitrogen, AltSoilMoisture

#create vectors of model names to and DOY to bind with y values
#Model <- c(rep_len("Temperature", 208), rep_len("Soil_Moisture", 208), rep_len("Nitrogen", 208), rep_len("AltSoilMoisture", 208))
#DOY <- rep(seq(from=92, to=299), 4)

#bind all and plot to verify captured correctly
my.df.2005 <- data.frame(DOY, Model, fit05)
ggplot(my.df.2005, aes(DOY, fit05, color=Model)) + geom_point() + geom_line() #looks correct compared to ggplot with raw data and stat_smooth called

#manipulate values so y value boundaries are ymin=0, ymax=1
my.df.2005.limited <- my.df.2005 %>%
        mutate(CorrectedFit = ifelse(fit05>1, 1,
                                     ifelse(fit05<0, 0, fit05))) %>%
        mutate(CorrectedFit2= ifelse(Model=="Nitrogen" & DOY<100, 0,
                                     ifelse(Model=="Nitrogen" & DOY>266, 0,
                                     ifelse(Model=="AltSoilMoisture" & DOY < 120, 0, CorrectedFit))))

my.df.2005.limited.subset <- subset(my.df.2005.limited, Model != "Soil_Moisture") %>%
        mutate(DaysAfterMay1 =DOY-121)
levels(my.df.2005.limited.subset$Model)[levels(my.df.2005.limited.subset$Model)=="AltSoilMoisture"] <- "Soil Moisture"

#plot
Middle<- ggplot(my.df.2005.limited.subset, 
              aes(DaysAfterMay1, CorrectedFit2, color=Model)) + 
        #geom_point() + 
        geom_line(lwd=1) +
        geom_ribbon(aes(ymin=0, ymax=my.df.2005.limited.subset$CorrectedFit2, fill=Model), alpha=0.2) +
        theme_classic() +
        labs(x = NULL, y=NULL) +
        scale_y_continuous(breaks=seq(from=0, to=1, by=0.25),
                           limits=c(0,1),
                           expand=c(0,0)) +
        scale_x_continuous(limits=c(0,151),
                           expand=c(0,0)) +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size, margin=margin(3,5,5,5,"pt")),
              #axis.text.y = element_text(size=text.size, margin=margin(5,3,5,5,"pt")),
              axis.text.y=element_blank(),
              legend.position = "none") +
        plottheme
#scale_fill_brewer(palette="Set2") +
#scale_color_brewer(palette="Set2")


##old plots
#GSLimit2002 <- ggplot(data=subset(GS2002, model !="Phosphorus"), aes(Days.From.May.30, output, color=model))
#top <- GSLimit2002 + 
#        geom_line() +
#        geom_point() + 
#        theme_classic() +
        #stat_density(aes(x=output, y="Days.From.May.30"), subset(GS2002, model =="pTemp")) +
        #geom_line() +
#        stat_smooth(aes(outfit=fit<<-..y..), method="lm", fullrange = TRUE, formula=y ~ ns(x,10), na.rm=T, se=F, n=208) +
        #stat_smooth(method="lm", formula=y ~ ns(x,10), fullrange=T, na.rm=T, se=F) +
#        ylim(c(0,1)) +
#        xlim(c(0,100)) +
#        labs(x = NULL, y=NULL) +
#        theme(text = element_text(size = text.size), 
#              axis.text.x = element_blank(),
#              axis.text.y = element_text(size=text.size)) +
#        guide_legend(title=NULL) +
#        plottheme

#Limit2002 <- ggplot(data=subset(Dat2002.collapsed, model =="Temperature" | model =="Nitrogen" | model=="Soil_Moisture"), aes(DOY, output, color=model))
#g <- Limit2002 + 
#        geom_point() +
#        theme_bw() + 
        #geom_line() +
#        stat_smooth(aes(outfit=fullfit<<-..y..), method="lm", formula=y ~ ns(x,10), na.rm=T, se=F, n=366) +
        #stat_smooth(fullrange=T, na.rm=T, se=F) +
#        ylim(c(0,1))

#GSLimit2004 <- ggplot(GS2004, aes(Days.After.April.1, output, color=model))
#middle <- GSLimit2004 +
#        theme_classic() +
#        stat_smooth(method="lm", formula = y~ ns(x,10), na.rm=T, se=F)+
#        ylim(c(0,1)) +
#        labs(x = NULL, y=NULL) +
#        theme(text = element_text(size = text.size), 
#              axis.text.x = element_blank(),
#              axis.text.y = element_text(size=text.size),
#              legened.position = "none") +
#        guide_legend(NULL) +
#        plottheme


#GSLimit2005 <- ggplot(GS2005, aes(Days.After.April.1, output, color=model))
#bottom <- GSLimit2005 +
#        theme_classic() +
#        stat_smooth(method="lm", formula = y ~ ns(x,10), na.rm=T, se=F) +
#        ylim(c(0,1)) +
#        labs(x = "Days from April 1", y=NULL) +
#        theme(text = element_text(size=text.size), 
#             axis.text.x = element_text(size=text.size), 
#             axis.text.y = element_text(size=text.size),
#             legend.position = "none") +
#        plottheme


#print
require(cowplot)
require(gridExtra)
#graphics.off

#multiplot_wGS <- plot_grid(top, middle, bottom, ncol =1, nrow=3, align="v")
#multiplot <- plot_grid(top, middle, bottom, ncol=1, nrow=3, align="v")
multiplot_summer <- plot_grid(Top, Middle, Bottom, 
                              ncol=3, 
                              nrow=1, 
                              rel_widths = c(1.2,1,1),
                              align="h")

save_plot("Fig9_revised_20160217_1.pdf", multiplot_summer,
          base_height = 2.45,
          base_aspect_ratio = .55)


save_plot("Fig13_revised_toppanel.pdf", multiplot_summer,
          base_height = 1.5,
          base_aspect_ratio = 3.46)