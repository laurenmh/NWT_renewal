#Caitlin White#
#University of Colorado - INSTAAR#
#caitlin.t.white@colorado.edu#
#February 2016#

#Re-creation of panels in Figure 10 for NWT LTER Renewal proposal
#Uses Will Weider's CLM data, and Katie Suding's 10a and 10b figures from draft 1 of NWT Renewal Proposal


#--Load needed libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)

#--set working directory as appropriate
setwd("~/Documents/Suding Lab/NWT LTER/CLM")

#--read in datasets and review content
CLMdat <- read.csv("~/Dropbox/NWT_data/clm_NWT_TMS.csv")
str(CLMdat)
summary(CLMdat) #reorder factor levels to reflect habitats driest to wettest
#reorder factor levels to match panels 1 and 2
CLMdat$VEG = factor(CLMdat$VEG,levels(CLMdat$VEG)[c(2,1,3,5,4)])

#Emily's ANPP data -- tried using for panel 10.b but did not match Katie's original figure in draft 1
NWTspdat <- read.csv("~/Dropbox/NWT_data/NWT_SnowXProdCorrected.csv")
str(NWTspdat)
#summary(NWTspdat) #in good shape, 333 NAs in ANPP response variable

#ANPP data exploration
lapply(split(NWTspdat$anpp, NWTspdat$year), NROW) #1997 only has 45 obs; all others have around 70
NWTspQA <- group_by(NWTspdat, year, class_3) %>%
        summarise(countANPP = NROW(anpp), notNA.anpp=sum(!is.na(anpp), isNA.anpp=sum(is.na(anpp))))
#no NAs present; DM and FF have fewer observations in 1997
ggplot(NWTspQA, aes(class_3, countANPP)) + geom_boxplot() + theme_bw()
ggplot(NWTspQA, aes(year, countANPP, color=class_3)) + geom_point(size=5) + theme_bw()
#1993 and 1997 are weird years with lower observations

ggplot(NWTspdat, aes(year, anpp, color=class_3)) + geom_point() + theme_bw() + facet_wrap(~class_3)
ggplot(NWTspdat, aes(class_3, anpp, color=class_3)) + geom_boxplot() + theme_bw() + facet_wrap(~year)


#Approximate data points and error bars values, eyeballed from draft 1
#Katie Suding has original data and R script
Fig10a.dat <- read.csv("~/Documents/Suding Lab/NWT LTER/Fig10A.csv")
levels(Fig10a.dat$Habitat)
#reorder factor levels from driest to wettest
Fig10a.dat$Habitat = factor(Fig10a.dat$Habitat,levels(Fig10a.dat$Habitat)[c(2,1,3,5,4)])

#Approximate data points and error bars values, eyeballed from draft 1
#Katie Suding has original data and R script
Fig10b.dat <- read.csv("~/Documents/Suding Lab/NWT LTER/Fig10b.csv")
levels(Fig10b.dat$Habitat)
#reorder factor levels from driest to wettest
Fig10b.dat$Habitat = factor(Fig10b.dat$Habitat,levels(Fig10b.dat$Habitat)[c(2,1,3,5,4)])



##--Data analysis
##--Ignore code below: Tried using Emily's Farrer's ANPP data to recreate 10b but didn't match KS's original figure
#Just for 2012, summarize average ANPP by habitat type (using class_3 grouping), for joinin with all other years dataset
ANPP2012_byHabitat <- subset(NWTspdat, year==2012) %>%
        group_by(class_3) %>%
        summarize(Avg2012ANPP = mean(anpp, na.rm=T))
        #summarise(AvgANPP = mean(anpp, na.rm=T), AvgResponse = mean(anpp_rspns, na.rm=T),
        #          SE=sd(anpp)/sqrt(length(anpp)),
        #          Count=NROW(anpp))

#Note, 2016 Jan 19: KS says try comparing 2012 v. all other years average (remove 2012 from long term average)
ANPP_byHabitat_byyear <- subset(NWTspdat, year!= 2012) %>%
#ANPP_byHabitat_byyear <- group_by(NWTspdat, year, class_3) %>%
        group_by(year, class_3) %>%
        summarise(AvgANPP= mean(anpp), AvgResponse=mean(anpp_rspns, na.rm=T),
                  count=NROW(anpp), SE=sd(anpp)/sqrt(count))

ggplot(ANPP_byHabitat_byyear, aes(year, AvgANPP, color=class_3)) + geom_point(size=5) + theme_bw()

AvgANPP_2012_Difference <- left_join(ANPP_byHabitat_byyear, ANPP2012_byHabitat, by="class_3") %>%
        mutate(Percent.Difference=((Avg2012ANPP-AvgANPP)/AvgANPP)*100)

ANPP_AvgDifference <- group_by(AvgANPP_2012_Difference, class_3) %>%
        summarise(AvgDiff = mean(Percent.Difference),
                  SE.diff=sd(Percent.Difference)/sqrt(length(Percent.Difference)))
#reorder factor levels from driest to wettest
ANPP_AvgDifference$class_3 = factor(ANPP_AvgDifference$class_3,levels(ANPP_AvgDifference$class_3)[c(2,1,3,5,4)])



ANPP_byHabitat_byyear_averaged <- group_by(ANPP_byHabitat_byyear, class_3) %>%
        summarise(AvgANPP.mean.yearlymeans = mean(AvgANPP),
                  #AvgResponse.mean.yearlymeans = mean(AvgResponse, na.rm=T),
                  SE.yearlymeans=sd(AvgANPP)/sqrt(length(AvgANPP)))

ANPP.Avg.YrlyMeans <- select(ANPP_byHabitat_byyear_averaged, class_3, 
                             AvgANPP.mean.yearlymeans, SE.yearlymeans) %>%
        rename(AvgANPP = AvgANPP.mean.yearlymeans, SE = SE.yearlymeans) %>%
        mutate(method="mean(yearlymeans)")

#ANPP_byHabitat_allyears <- subset(NWTspdat, year != 2012) %>%
ANPP_byHabitat_allyears <- group_by(NWTspdat, class_3) %>%
        summarise(AvgANPP.pooled = mean(anpp), AvgResponse.pooled=mean(anpp_rspns, na.rm=T), 
                  count=NROW(anpp), SE.pooled=sd(anpp)/sqrt(count))

ANPP.Avg.Pooled <- select(ANPP_byHabitat_allyears, class_3,
                          AvgANPP.pooled, SE.pooled) %>%
        rename(AvgANPP =AvgANPP.pooled, SE=SE.pooled) %>%
        mutate(method="pooled mean")

ANPP.means.comparison <- merge(ANPP_byHabitat_allyears, ANPP_byHabitat_byyear_averaged, by="class_3")
ggplot(ANPP.means.comparison, aes(AvgANPP.pooled, AvgANPP.mean.yearlymeans, color=class_3)) + 
        geom_point(size=5) + 
        geom_abline(aes(intercept=0, slope=1), color="black", linetype=2, lwd=1) +
        labs(y="AvgANPP - mean of yearly means", x="Avg ANPP - all years pooled mean") +
        theme_bw()

NWTspdat.2012 = subset(NWTspdat, year==2012) %>%
        left_join(y=ANPP.means.comparison, by="class_3") %>%
        mutate(dev.meansyearlymeans = (anpp-AvgANPP.mean.yearlymeans)/AvgANPP.mean.yearlymeans, 
               dev.pooled=(anpp-AvgANPP.pooled)/AvgANPP.pooled)

ggplot(NWTspdat.2012, aes(dev.meansyearlymeans, dev.pooled, color=class_3)) + geom_point(size=4) + theme_bw()

NWT_ANPP_10b <- group_by(NWTspdat.2012, class_3) %>%
        summarise(AvgDev.yrlymeans =mean(dev.meansyearlymeans), AvgDev.pooled=mean(dev.pooled),
                  se.yrlymeans = sd(dev.meansyearlymeans)/sqrt(length(dev.meansyearlymeans)),
                  se.pooled = sd(dev.pooled/sqrt(length(dev.pooled))))

NWT_AvgANPP10b <- select(NWT_ANPP_10b,1:3) %>%
        rename(YearlyMeans.Mean = AvgDev.yrlymeans, Pooled.Mean = AvgDev.pooled) %>%
        gather(Method, Deviation, -class_3)


NWT_seANPP10b <- select(NWT_ANPP_10b,c(1,4,5)) %>%
        rename(YearlyMeans.Mean = se.yrlymeans, Pooled.Mean = se.pooled) %>%
        gather(Method, SE, -class_3)

NWT_ANPP_2012 <- merge(NWT_AvgANPP10b, NWT_seANPP10b, by=c("class_3", "Method"))


CLMdat.2012 <- subset(CLMdat, year_out==2012) %>%
        select(pVEGC, TX, VEG) %>%
        rename(Deviation=pVEGC, Method=TX, class_3=VEG) %>%
        mutate(SE=NA, Method="CLM")


Fig10b.dat.comparison <- mutate(Fig10b.dat, Deviation=ANPP_deviation/100, se = SE/100, Method="KS, draft 1") %>%
        select(Habitat, Deviation, se, Method) %>%
        rename(class_3=Habitat, SE=se)

NWT_ANPP_2012 <- rbind(NWT_ANPP_2012, CLMdat.2012, Fig10b.dat.comparison)
NWT_ANPP_2012$class_3 = factor(NWT_ANPP_2012$class_3,levels(NWT_ANPP_2012$class_3)[c(2,1,3,5,4)])
NWT_ANPP_2012 <- mutate(NWT_ANPP_2012, xpos=as.numeric(class_3))

rects2 <- data.frame(xstart = c(0,2.5,4.5), 
                    xend = seq(1.5,3.5, 5.5))

ggplot(NWT_ANPP_2012, aes(xpos, Deviation, color=Method)) + 
#ggplot() +
        #geom_rect(data = rects2, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = "grey"), alpha = 0.5) +
        #geom_bar(stat="identity", position="dodge") +
        geom_errorbar(data=NWT_ANPP_2012, aes(x=xpos, ymin=Deviation-SE, ymax=Deviation+SE), width=0.25, position=position_dodge(width=1)) +
        geom_point(data=NWT_ANPP_2012, aes(x=xpos, y=Deviation, color=Method, shape=Method), size=5, position=position_dodge(width=1)) +
        geom_hline(aes(yintercept=0), lwd=1) +
        labs(x="Habitat (using class_3)", y="2012 AVG ANPP Deviation\n(relative change from long-term average)") +
        theme_bw(base_size=16) +
        scale_color_discrete(name="Method") +
        scale_shape_discrete(name="Method") +
        geom_vline(aes(xintercept=1.5), color="grey", linetype=2, lwd=1) +
        geom_vline(aes(xintercept=2.5), color="grey", linetype=2, lwd=1) +
        geom_vline(aes(xintercept=3.5), color="grey", linetype=2, lwd=1) +
        geom_vline(aes(xintercept=4.5), color="grey", linetype=2, lwd=1) +
        scale_x_continuous(breaks=(seq(from=1, to=5, by=1)), expand=c(0,0), 
                           labels=c("FF","DM", "MM", "WM", "SB"))

#KS wants to compare avg 2012 value to to model average -- NOTE: only modelled for years 2008-2012
CLMdat.averages.allyears <- group_by(CLMdat, VEG) %>%
        summarise(Avg.dVEGC = mean(dVEGC), Avg.pVEGC=mean(pVEGC), Avg.maxSNOW=mean(maxSNOW))

#ANPP2012_byHabitat_class2 <- subset(NWTspdat, year==2012) %>%
#        group_by(class_2) %>%
#        summarise(AvgANPP = mean(anpp, na.rm=T), AvgResponse = mean(anpp_rspns, na.rm=T))

#NWTspdat_no07 <- subset(NWTspdat, year<2007 | year>2007) #not removing 1997 ('year!=2007' doesn't work either)..not sure why
#AcrossYrsMean_no07 <- group_by(prodmeanse, class_3) %>%
#        summarise(AllYrsAvg = mean(mean))

#test_df <- merge(ANPP2012_byHabitat, AcrossYrsMean_no07, by="class_3") %>%
#        mutate(deviation = (AvgANPP/AllYrsAvg) - 1)

#ggplot(test_df, aes(class_3, deviation)) + geom_point() + geom_hline(aes(yintercept=0))

#class_3 <- as.vector(names(AcrossYrsMean))
#AllYrsMeanANPP <- as.vector(unlist(AcrossYrsMean))
#df <- as.data.frame(cbind(class_3,AllYrsMeanANPP))

#all <- merge(ANPP2012_byHabitat, df, by="class_3") %>%
#        mutate(AllYrsMeanANPP =extract_numeric(AllYrsMeanANPP), 
#               dev = ((AvgANPP/AllYrsMeanANPP) -1)*100)

#ggplot(all, aes(class_3, dev)) + geom_point()

#--Update, 2016-02-02, try plotting deviation from 2012 as scatterplot, by habitat type
# (1) Isolate 2012 data and join to NWTsp dataset, pairing by habitat and plot
NWTspdat.2012.raw <- subset(NWTspdat, year==2012) %>%
        select(plot, class_3, anpp_rspns, anpp)
# (2) Left inner join on plot and class_3
NWTspdat.deviations <- left_join(NWTspdat, NWTspdat.2012.raw, by = "plot") %>%
        mutate(Difference = anpp.y - anpp.x, Percent.Change = ((anpp.y-anpp.x)/anpp.x)*100) %>% #difference of 2012 ANPP from other year
        rename(Year=year)

ggplot(NWTspdat.deviations, aes(Year, Difference, color=class_3.x)) +
        geom_point() +
        facet_grid(class_3.x ~.) +
        geom_hline(aes(yintercept=0)) +
        theme_classic()

# (3) read in and merge in PCA scores to plot by PC1
PCAscores <- read.csv("~/Dropbox/NWT_data/NWT_Climate_summerPCscores_20151123.csv")
colnames(PCAscores[1]) <- c("year")
NWTspdat.deviations <- left_join(NWTspdat.deviations, PCAscores, by="Year")

#re-order factor levels for class_3.x from driest to wettest
NWTspdat.deviations$class_3.x = factor(NWTspdat.deviations$class_3.x,levels(NWTspdat.deviations$class_3.x)[c(2,1,3,5,4)])


ggplot(NWTspdat.deviations, aes(factor(sumallPC1), Difference, fill=class_3.x)) +
        geom_boxplot(position="dodge") +
        #facet_grid(class_3.x ~.) +
        geom_hline(aes(yintercept=0)) +
        theme_classic()
#I don't see a clear pattern in habitat response to PC1 scores..


NWTanppDev.lm <- lm(Difference ~ class_3.x, data=subset(NWTspdat.deviations, Year !=2012))
summary(NWTanppDev.lm) #signif for DM and MM
plot(fitted(NWTanppDev.lm), resid(NWTanppDev.lm))

# (4) Summarize average deviation by habitat and by year/PC1. Remove 2012.
NWT.deviations.average <- group_by(NWTspdat.deviations, class_3.x, Year, sumallPC1) %>%
        summarise(AvgANPP = mean(anpp.x), AvgDev=mean(Difference), AvgPercentDev = mean(Percent.Change), AvgSnow=mean(max_snow),
                  StDev.ANPP=sd(anpp.x)/sqrt(length(anpp.x)),
                  StDev.Deviation =sd(Difference)/sqrt(length(Difference)),
                  StDev.PercentDev = sd(Percent.Change)/sqrt(length(Percent.Change)),
                  Count=NROW(Difference))

NWT2012Dev.aov <- anova(lm(AvgDev ~ class_3.x + class_3.x*sumallPC1, data=subset(NWT.deviations.average, Year !=2012)))
NWT2012Dev.aov #overall, class_3.x very significant (p=4.29e-05)

NWT2012Dev.lm <- lm(AvgDev ~ class_3.x, data=subset(NWT.deviations.average, Year !=2012))
summary(NWT2012Dev.lm) #signif for DM only (very, p=000343); model signif (same as anova), R-squared=0.424

# (5) Avg by habitat only to compare results with Katie's original figure from Draft 1
NWT.deviations.average.collapsed <- subset(NWTspdat.deviations, Year !=2012) %>%
        group_by(class_3.x) %>%
        summarise(AvgANPP = mean(anpp.x), AvgDev=mean(Difference), AvgPercentDev = mean(Percent.Change), AvgSnow=mean(max_snow),
                  StDev.ANPP=sd(anpp.x)/sqrt(length(anpp.x)),
                  StDev.Deviation =sd(Difference)/sqrt(length(Difference)),
                  StDev.PercentDev = sd(Percent.Change)/sqrt(length(Percent.Change)),
                  Count=NROW(Difference))

NWT.deviations.average.yearlymeans <- subset(NWT.deviations.average, Year !=2012) %>%
        group_by(class_3.x) %>%
        summarise(MeanANPP = mean(AvgANPP), MeanDev=mean(AvgDev), MeanRelDev = mean(AvgPercentDev),
                  StDev.ANPP=sd(AvgANPP)/sqrt(length(AvgANPP)),
                  StDev.Deviation =sd(AvgDev)/sqrt(length(AvgDev)),
                  StDev.PercentDev = sd(AvgPercentDev)/sqrt(length(AvgPercentDev)),
                  Count=NROW(AvgDev))


##Update: Feb 11, 2016. KS wants avg ANPP by habitat with error bars showing variability in space and time (not sure how to show that last part yet)

ANPP_plotavg <- group_by(NWTspdat, class_3, plot) %>%
        summarize(AvgANPP = mean(anpp), 
                  se = sd(anpp)/sqrt(length(anpp)),
                  Count=NROW(anpp))

#re-order habitat factor levels by dry to wet
#add numeric score to ANPP_plotavg based on class_3 to make x-axis continuous
ANPP_plotavg$class_3 = factor(ANPP_plotavg$class_3,levels(ANPP_plotavg$class_3)[c(2,1,3,5,4)])
ANPP_plotavg$class3_order <- as.numeric(ANPP_plotavg$class_3)

ggplot(ANPP_plotavg, aes(class3_order, AvgANPP, color=class_3)) + geom_point() +
        geom_errorbarh(aes(xmin=class3_order-se, xmax=class3_order+se)) +
        facet_grid(.~class_3) +
        theme(legend.position="none")

ggplot(ANPP_plotavg, aes(x=1, AvgANPP, color=class_3))+
        geom_point(aes(group=plot), size=3, position=position_dodge(.5)) +
        geom_errorbar(aes(ymin=AvgANPP-se, ymax=AvgANPP+se, group=plot), width=0.25, position=position_dodge(.5)) +
        facet_grid(.~class_3, switch="x") +
        theme(text=element_text(size=16),
                legend.position="none",
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=16),
              axis.ticks.x=element_blank(),
              strip.background=element_blank())

##update Feb 22, 2016: Combine ANPP by plot average and percent deviation from 2012
# (1) Repeat above summary, but remove 2012 from plot long term average
ANPP_plotavg_no2012 <- group_by(subset(NWTspdat, year != 2012), class_3, plot) %>%
        summarize(AvgANPP = mean(anpp), 
                  se = sd(anpp)/sqrt(length(anpp)),
                  Count=NROW(anpp))

#re-order habitat factor levels by dry to wet
#add numeric score to ANPP_plotavg based on class_3 to make x-axis continuous
ANPP_plotavg_no2012$class_3 = factor(ANPP_plotavg_no2012$class_3,levels(ANPP_plotavg_no2012$class_3)[c(2,1,3,5,4)])

#quick visual
ggplot(ANPP_plotavg_no2012, aes(class_3, AvgANPP, color=class_3)) +
        geom_point(aes(group=plot), position=position_dodge(.5)) +
        geom_point(ANPP_plotavg, aes(class_3, AvgANPP, color=class_3, group=plot))

# (2) isolate 2012 ANPP and take average with SE to plot over long-term data
ANPP_2012only <- group_by(NWTspdat.2012.raw, class_3) %>%
        summarise(AvgANPP = mean(anpp, na.rm=T), 
                  SE = sd(anpp)/sqrt(length(anpp)),
                  Count = NROW(anpp))

##--Plotting
###---Make 3 panels for Figure 10###
#set plot margin values (for calling later in line)
text.size = 8
margins.plot<-unit(c(0.25,.5,.25,1), 'lines') #top, right, bottom, left

plottheme<-theme(plot.margin = margins.plot,
                 #axis.ticks.margin = margins.axes,
                 axis.title = element_text(
                         face='plain')
)

#---Panel 1
#make fake data for background panel colors by habitat type
rects <- data.frame(xstart = seq(0,4,1), 
                    xend = seq(1,5,1), col = as.factor(seq(1:5)))


Fig10a.dat$Habitat <- factor(Fig10a.dat$Habitat,
                    levels = seq(1:5),
                    labels = c("FF", "DM", "MM", "WM", "SB")) 

Fig10.a <- ggplot() + 
        geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.5) +
        geom_point(data=Fig10a.dat, aes(xpos, Value, group=Limitation_Type), size=2, color="black", position= position_dodge(width=1)) +
        geom_point(data=Fig10a.dat, aes(xpos,Value, color=Limitation_Type), size=1, position= position_dodge(width=1)) +
        geom_line(data=Fig10a.dat, aes(xpos,Value, color=Limitation_Type), position=position_dodge(width=1), lwd=1) +
        geom_errorbar(data=Fig10a.dat, aes(x=xpos, ymin=SE.ymin, ymax=SE.ymax, group=Limitation_Type), width=0.25, position=position_dodge(width=1)) +
        scale_y_continuous(breaks=(seq(from=0.0, to=0.6, by=0.2)), expand=c(0.005,0.01), limits=c(0,0.6)) + 
        scale_x_continuous(breaks=(seq(from=0.5, to=4.5, by=1)), expand=c(0,0), 
                           labels=c("FF","DM", "MM", "WM", "SB"), limits=c(0,5.1)) +
        labs(y="Production Limiting Effect\n (cumulative, unitless)", x=NULL) +
        theme_classic(base_size = 18) +
        theme(axis.text = element_text(size=16),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size=18, face="bold"),
              legend.position="none",
              axis.text.x=element_text(angle=45, vjust=0.5, face="bold"),
              plot.margin=margins.plot)


#load libraries for exracting spline y values

library(ggplot2)
library(splines)

### -- 2002##
#extract predicted y values from smoothing splines 
test <- ggplot(data=Fig10a.dat, aes(xpos, Value, color=Limitation_Type))
test + stat_smooth(aes(outfit=fit.CLM<<-..y..), method="loess", fullrange = TRUE, formula=y ~ x, na.rm=T, se=F) #generates 80 points per Limitation_Type by default
#order of predicted y values from splines is: GSL, (Soil) Moisture, Nutrient

#create vectors of model names and DOY to bind with y values
Limitation_Type <- c(rep_len("GSL", 80), rep_len("Moisture", 80), rep_len("Nutrient", 80))
Index <- rep(seq(from=0.5, to=4.45, by=0.05), 3)

#bind all and plot to verify captured correctly
my.test.df <- data.frame(Index, Limitation_Type, fit.CLM)

#add in values at x= 0 and x=4.5 to close the polygon, setting y values to FF and SB respectively
#extract those values from main dataset, then rbind to smoothing spline dataset
Fig10a.subset <- subset(Fig10a.dat,Habitat == "FF" | Habitat == "SB") %>%
        select(xpos, Limitation_Type, Value) %>%
        rename(Index=xpos, fit.CLM=Value)
Fig10a.subset$Index <- ifelse(Fig10a.subset$Index==0.5, 0, 5)

#rbind to smooth spline dataframe
my.test.df <- rbind(my.test.df, Fig10a.subset)

#reassign factor levels to match Figure 
my.test.df$Limitation_Type <- factor(my.test.df$Limitation_Type,levels(my.test.df$Limitation_Type)[c(2,3,1)])

#quick visual
ggplot(data=Fig10a.dat, aes(x=xpos, y=Value, color=Limitation_Type, group=Limitation_Type)) +
        geom_line()+
        stat_smooth(data=subset(Fig10a.dat, Limitation_Type=="GSL"), 
                    method="loess", formula=y ~ x, se=F) +
        stat_smooth(data=subset(Fig10a.dat, Limitation_Type=="Moisture"), 
                    method="loess", formula=y ~ x, se=F) +
        stat_smooth(data=subset(Fig10a.dat, Limitation_Type=="Nutrient"), 
                    method="loess", formula=y ~ x, se=F) +
        #scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))
#stat_smooth(data=subset(Fig10a.dat, Limitation_Type=="GSL"), 
#            aes(x = seq(length(unique(Habitat)))), # continuous x-axis
#            se = F, method = "lm", formula = y ~ poly(x, 4),
#            position=position_dodge(.5)) +
#        stat_smooth(data=subset(Fig10a.dat, Limitation_Type=="Nutrient"), 
#                    aes(x = seq(length(unique(Habitat)))), # continuous x-axis
#                    se = F, method = "lm", formula = y ~ poly(x, 4),
#                    position=position_dodge(1)) +
#        stat_smooth(data=subset(Fig10a.dat, Limitation_Type=="Moisture"), 
#                    aes(x = seq(length(unique(Habitat)))), # continuous x-axis
#                    se = F, method = "lm", formula = y ~ poly(x, 3))




##do above again with SE value added to mean obs to capture simulated "TMS" year
#extract predicted y values from smoothing splines 
CLM_upperSE <- ggplot(data=Fig10a.dat, aes(xpos, Value+(SE*1.96), color=Limitation_Type))
CLM_upperSE + stat_smooth(aes(outfit=fit.upperSE<<-..y..), method="loess", fullrange = TRUE, formula=y ~ x, na.rm=T, se=F) #generates 80 points per Limitation_Type by default

CLM_lowerSE <- ggplot(data=Fig10a.dat, aes(xpos, Value-(SE*1.96), color=Limitation_Type))
CLM_lowerSE + stat_smooth(aes(outfit=fit.lowerSE<<-..y..), method="loess", fullrange = TRUE, formula=y ~ x, na.rm=T, se=F) #generates 80 points per Limitation_Type by default

#order of predicted y values from splines is: GSL, (Soil) Moisture, Nutrient

#create vectors of model names and DOY to bind with y values
Limitation_Type <- c(rep_len("GSL", 80), rep_len("Moisture", 80), rep_len("Nutrient", 80))
Index <- rep(seq(from=0.5, to=4.45, by=0.05), 3)

#bind all and plot to verify captured correctly
my.test_wSE.df <- data.frame(Index, Limitation_Type, fit.upperSE, fit.lowerSE)

#add in values at x= 0 and x=4.5 to close the polygon, setting y values to FF and SB respectively
#extract those values from main dataset, then rbind to smoothing spline dataset
Fig10a.SE.subset <- subset(Fig10a.dat,Habitat == "FF" | Habitat == "SB") %>%
        select(xpos, Limitation_Type, Value, SE) %>%
        rename(Index=xpos) %>%
        mutate(fit.upperSE = Value+(SE*1.96), fit.lowerSE=Value-(SE*1.96)) %>%
        select(Index,Limitation_Type, fit.upperSE, fit.lowerSE)
Fig10a.SE.subset$Index <- ifelse(Fig10a.SE.subset$Index==0.5, 0, 5)

#rbind to smooth spline dataframe
my.test_wSE.df <- rbind(my.test_wSE.df, Fig10a.SE.subset)

#reassign factor levels to match Figure 
my.test_wSE.df$Limitation_Type <- factor(my.test_wSE.df$Limitation_Type,levels(my.test_wSE.df$Limitation_Type)[c(2,3,1)])

#create duplicate field for 95%CI interval that forces negative values to 0 for plotting purposes (CLM index results cannot be less than 0)
my.test_wSE.df$fit2.upperSE <- with(my.test_wSE.df, ifelse(fit.upperSE>0, fit.upperSE, 0))
my.test_wSE.df$fit2.lowerSE <- with(my.test_wSE.df, ifelse(fit.lowerSE>0, fit.lowerSE, 0))


##Final plot
CLM.model <- ggplot(my.test.df, aes(Index, fit.CLM, color=Limitation_Type)) + 
        geom_ribbon(aes(ymin=my.test_wSE.df$fit2.lowerSE, ymax=my.test_wSE.df$fit2.upperSE, fill=Limitation_Type), alpha=0.2) +
        geom_line(lwd=1) + 
        #geom_ribbon(aes(ymin=0, ymax=my.test.df$fit, fill=Limitation_Type), alpha=0.2) +
        #geom_point(data=Fig10a.dat, aes(xpos, Value, color=Limitation_Type), size=1) + #looks correct, lines run through point
        #geom_line(data=my.test_wSE.df, aes(Index,fit2.upperSE, color=Limitation_Type), linetype=2, lwd=0.5) +
        #geom_point(data=Fig10a.dat, aes(xpos, SE.ymax, color=Limitation_Type), size=1, pch=1) +
        scale_x_continuous(breaks=seq(from=0.5, to=4.5, by=1),
                           labels=c("FF", "DM", "MM", "WM", "SB"),
                           limits=c(0,5),
                           expand=c(0,0)) +
        scale_y_continuous(breaks=seq(from=0.0, to=0.6, by=.1),
                           limits=c(0,0.6),
                           expand=c(0,0)) +
        #scale_color_manual(values = c("#F8766D", "#00B6EB", "#53B400")) +
        #scale_fill_manual(values = c("blue", "red", "green")) +
        labs(x=NULL, y=NULL) +
        theme_classic() +
        theme(text=element_text(size=text.size),
              axis.text.x=element_text(size=text.size, margin=margin(3,5,5,5,"pt")),
              axis.text.y=element_text(size=text.size, margin=margin(5,3,5,5,"pt")), 
              legend.position="none")


Fig10a.nocolorblocking <- ggplot() + 
        #geom_rect(aes(xmin=1, xmax=2, ymin=0, ymax=0.6), color="gray89", alpha=0.2) +
        #geom_rect(aes(xmin=3, xmax=4, ymin=0, ymax=0.6), color="gray89", alpha=0.2) +
        geom_line(data=Fig10a.dat, aes(Habitat,Value, group=Limitation_Type, color=Limitation_Type), position=position_dodge(width=1), lwd=.5) +
        geom_errorbar(data=Fig10a.dat, aes(x=Habitat, ymin=SE.ymin, ymax=SE.ymax, group=Limitation_Type), width=0.25, position=position_dodge(width=1)) +
        geom_point(data=Fig10a.dat, aes(Habitat, Value, shape=Limitation_Type), size=2, color="black", position= position_dodge(width=1)) +
        geom_point(data=Fig10a.dat, aes(Habitat,Value, shape=Limitation_Type, color=Limitation_Type), size=1, position= position_dodge(width=1)) +
        #scale_color_manual(values=c("grey", "white", "black")) +
        scale_color_manual(values=c("dark turquoise", "goldenrod2", "dodger blue4")) +
        scale_y_continuous(breaks=(seq(from=0.0, to=0.6, by=0.2)), 
                           expand=c(0,0), 
                           limits=c(0,0.6)) + 
        #scale_x_continuous(breaks=(seq(from=0.5, to=4.5, by=1)), 
        #                   expand=c(0,0), 
        #                   labels=c("FF","DM", "MM", "WM", "SB"), 
        #                   limits=c(0,5.1)) +
        #labs(y="Production Limiting Effect\n (cumulative, unitless)", x=NULL) +
        labs(x=NULL, y=NULL) +
        theme_classic() +
        theme(text = element_text(size=text.size),
              axis.title.x=element_blank(),
              #axis.title.y=element_text(size=text.size, margin=margin(5,13,5,5,"pt")),
              axis.text.x=element_text(size=text.size, margin=margin(3,5,5,5,"pt")),
              axis.text.y=element_text(size=text.size, margin=margin(5,3,5,5,"pt")), 
              legend.position="none") +
              #legend.title=element_blank(),
              #legend.position=c(0.1,0.9),
              #legend.key.height=unit(.25, "lines"),
              #legend.key.width=unit(.5, "cm"),
              #legend.margin=unit(0, "cm"),
              #legend.text = element_text(size=text.size),
              #legend.background =element_rect(colour="black")) +
        geom_vline(xintercept=1.5, linetype=2, color="gray30") +
        geom_vline(xintercept=2.5, linetype=2, color="gray30") +
        geom_vline(xintercept=3.5, linetype=2, color="gray30") +
        geom_vline(xintercept=4.5, linetype=2, color="gray30") +
        guides(col=guide_legend(ncol=3)) +
        plottheme


#Panel1.alt <- ggplot(ANPP_plotavg, aes(x=1, AvgANPP, color=class_3))+
Panel1.alt <- ggplot(ANPP_plotavg, aes(class_3, AvgANPP, color=class_3)) +
        geom_errorbar(aes(ymin=AvgANPP-se, ymax=AvgANPP+se, group=plot), color="black", width=0, position=position_dodge(.5)) +
        geom_point(aes(group=plot), size=1, color="black", position=position_dodge(.5)) +
        geom_point(aes(group=plot), size=.5, position=position_dodge(.5)) +
        geom_segment(data=subset(ANPP_plotavg, class_3=="FF"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=0.6, xend=1.4), color="black", linetype=1, lwd=.5) +
        geom_segment(data=subset(ANPP_plotavg, class_3=="DM"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=1.6, xend=2.4), color="black", linetype=1, lwd=.5) +
        geom_segment(data=subset(ANPP_plotavg, class_3=="MM"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=2.6, xend=3.4), color="black", linetype=1, lwd=.5) +
        geom_segment(data=subset(ANPP_plotavg, class_3=="WM"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=3.6, xend=4.4), color="black", linetype=1, lwd=.5) +
        geom_segment(data=subset(ANPP_plotavg, class_3=="SB"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=4.6, xend=5.4), color="black", linetype=1, lwd=.5) +
        geom_vline(xintercept=1.5, linetype=2, color="gray30") +
        geom_vline(xintercept=2.5, linetype=2, color="gray30") +
        geom_vline(xintercept=3.5, linetype=2, color="gray30") +
        geom_vline(xintercept=4.5, linetype=2, color="gray30") +
        #labs(y=expression(paste("Average ANPP (g ", m^-2, ")")), x=NULL) +
        labs(x=NULL, y=NULL) +
        #facet_grid(.~class_3, switch="x") +
        scale_color_brewer(type="div") +
        theme(text=element_text(size=text.size),
              legend.position="none",
              axis.text.x=element_text(size=text.size, margin=margin(3,5,5,5,"pt")),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=text.size, margin=margin(5,3,5,5,"pt"))) +
              #axis.title.y=element_text(size=text.size, margin=margin(5,13,5,5,"pt")),
              #axis.ticks.x=element_blank(),
              #strip.background=element_blank(),
              #strip.text=element_text(size=text.size))
        plottheme

#t.test long term plot means v. 2012 plot ANPP to see if Avg ANPP is significantly different, by community type
#create DF with columns: plot, habitat, ANPP.LT, ANPP.2012. Then t.test ANPP.LT vs. ANPP.2012, removing any NAs
t.test.df <- left_join(ANPP_plotavg_no2012, NWTspdat.2012.raw, by=c("class_3", "plot"))

with(t.test.df, lapply(split(t.test.df, class_3), function(x) t.test(AvgANPP, anpp, paired=T))) #splits but returns same stat for each habitat. do individually
test.aov <- with(t.test.df, aov(AvgANPP~anpp + class_3 + anpp:class_3))
summary(test.aov)
plot(test.aov)

with(t.test.df[t.test.df$class_3=="FF",], t.test(AvgANPP, anpp, var.equal = F)) #not signif, changing var.equal from T to F doesn't change results
with(t.test.df[t.test.df$class_3=="DM",], t.test(AvgANPP, anpp, var.equal=F)) #signif at p<.001
with(t.test.df[t.test.df$class_3=="MM",], t.test(AvgANPP, anpp, var.equal=F)) #not signif
with(t.test.df[t.test.df$class_3=="WM",], t.test(AvgANPP, anpp, var.equal=F)) #not signif
with(t.test.df[t.test.df$class_3=="SB",], t.test(AvgANPP, anpp, var.equal=F)) #not signif

#reorder class_3 factor levels in NWTspdat.2012.raw, since those points are plotted first
NWTspdat.2012.raw$class_3 <- factor(NWTspdat.2012.raw$class_3,levels(NWTspdat.2012.raw$class_3)[c(2,1,3,5,4)])
        
Panel1.alt.2012isolated <- ggplot(ANPP_plotavg_no2012, aes(class_3, AvgANPP, color=class_3)) +
        geom_point(data=NWTspdat.2012.raw, aes(class_3, anpp, group=plot), color="darkgrey", size=1, position=position_dodge(.5)) +
        geom_errorbar(aes(ymin=AvgANPP-se, ymax=AvgANPP+se, group=plot), color="black", width=0, position=position_dodge(.5)) +
        geom_point(aes(group=plot), size=1, color="black", position=position_dodge(.5)) +
        geom_point(aes(group=plot), size=.5, position=position_dodge(.5)) +
        geom_segment(data=subset(ANPP_plotavg_no2012, class_3=="FF"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=0.6, xend=1.4), color="black", linetype=1, lwd=.5) +
        geom_segment(data=subset(ANPP_plotavg_no2012, class_3=="DM"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=1.6, xend=2.4), color="black", linetype=1, lwd=.5) +
        geom_segment(data=subset(ANPP_plotavg_no2012, class_3=="MM"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=2.6, xend=3.4), color="black", linetype=1, lwd=.5) +
        geom_segment(data=subset(ANPP_plotavg_no2012, class_3=="WM"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=3.6, xend=4.4), color="black", linetype=1, lwd=.5) +
        geom_segment(data=subset(ANPP_plotavg_no2012, class_3=="SB"), aes(y=mean(AvgANPP), yend=mean(AvgANPP), x=4.6, xend=5.4), color="black", linetype=1, lwd=.5) +
        #geom_point(data=ANPP_2012only, aes(class_3, AvgANPP, group=class_3), color="red", size=1) +
        #geom_errorbar(data=ANPP_2012only, aes(ymin=AvgANPP-SE, ymax=AvgANPP+SE, group=class_3), color="red", width=0) +
        geom_segment(data=subset(NWTspdat.2012.raw, class_3=="FF"), aes(y=mean(anpp), yend=mean(anpp), x=0.6, xend=1.4), color="red", linetype=1, lwd=.5) +
        geom_segment(data=subset(NWTspdat.2012.raw, class_3=="DM"), aes(y=mean(anpp), yend=mean(anpp), x=1.6, xend=2.4), color="red", linetype=1, lwd=.5) +
        geom_segment(data=subset(NWTspdat.2012.raw, class_3=="MM"), aes(y=mean(anpp), yend=mean(anpp), x=2.6, xend=3.4), color="red", linetype=1, lwd=.5) +
        geom_segment(data=subset(NWTspdat.2012.raw, class_3=="WM"), aes(y=mean(anpp), yend=mean(anpp), x=3.6, xend=4.4), color="red", linetype=1, lwd=.5) +
        geom_segment(data=subset(NWTspdat.2012.raw, class_3=="SB"), aes(y=mean(anpp), yend=mean(anpp), x=4.6, xend=5.4), color="red", linetype=1, lwd=.5) +
        geom_vline(xintercept=1.5, linetype=2, color="gray30") +
        geom_vline(xintercept=2.5, linetype=2, color="gray30") +
        geom_vline(xintercept=3.5, linetype=2, color="gray30") +
        geom_vline(xintercept=4.5, linetype=2, color="gray30") +
        #labs(y=expression(paste("Average ANPP (g ", m^-2, ")")), x=NULL) +
        labs(x=NULL, y=NULL) +
        #facet_grid(.~class_3, switch="x") +
        scale_color_brewer(type="div") +
        scale_y_continuous(limits=c(0,400),
                           expand=c(0,0)) +
        theme(text=element_text(size=text.size),
              legend.position="none",
              axis.text.x=element_text(size=text.size, margin=margin(3,5,5,5,"pt")),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=text.size, margin=margin(5,3,5,5,"pt"))) +
        #axis.title.y=element_text(size=text.size, margin=margin(5,13,5,5,"pt")),
        #axis.ticks.x=element_blank(),
        #strip.background=element_blank(),
        #strip.text=element_text(size=text.size))
        plottheme

#---Panel 2
Fig10.b <- ggplot(Fig10b.dat, aes(Habitat,ANPP_deviation, fill=Habitat)) + 
        geom_bar(stat="identity", color="black", width=0.5) +
        geom_errorbar(aes(ymin=ANPP_deviation-SE, ymax=ANPP_deviation+SE), width=0.25) +
        geom_hline(aes(yintercept=0)) + 
        scale_y_continuous(breaks=(seq(from=-40, to=60, by=20)), limits=(c(-50,60))) + 
        #labs(y="ANPP Sensitivity 2012\n (% deviation from average)", x=NULL) +
        labs(x=NULL, y=NULL) +
        theme_classic(base_size = 18) +
        theme(axis.text = element_text(size=text.size),
              axis.title.x=element_blank(),
              #axis.title.y=element_text(size=text.size, face="bold"),
              legend.position="none",
              axis.text.x=element_text(angle=45, vjust=0.5, face="bold"),
              plot.margin=margins.plot)


Fig10b.DevbyPC1 <- ggplot(data=subset(NWT.deviations.average, Year!=2012), aes(sumallPC1, AvgDev, color=class_3.x)) +
        geom_point(size=5, color="black") +
        geom_point(size=4) +
        geom_hline(aes(yintercept=0)) +
        geom_smooth(method=lm, se=F) +
        theme_classic()

DM.dev <- lm(AvgDev~sumallPC1, NWT.deviations.average, class_3.x=="DM" & Year !=2012)
FF.dev <- lm(AvgDev~sumallPC1, NWT.deviations.average, class_3.x=="FF" & Year !=2012)
MM.dev <- lm(AvgDev~sumallPC1, NWT.deviations.average, class_3.x=="MM" & Year !=2012)
SB.dev <- lm(AvgDev~sumallPC1, NWT.deviations.average, class_3.x=="SB" & Year !=2012)
WM.dev <- lm(AvgDev~sumallPC1, NWT.deviations.average, class_3.x=="WM" & Year !=2012)

summary(DM.dev) #not signif
summary(FF.dev) #not signif
summary(MM.dev) #not signif
summary(SB.dev) #not signif
summary(WM.dev) #not signif


#try scatterplot of avg ANPP for all years, 2012 included
ggplot(NWT.deviations.average, aes(sumallPC1, AvgANPP, color=class_3.x)) +
        geom_point(size=5, color="black") +
        geom_point(size=4) +
        geom_smooth(method=lm, se=F) +
        theme_classic()

DM.anpp <- lm(AvgANPP~sumallPC1, NWT.deviations.average, class_3.x=="DM")
FF.anpp <- lm(AvgANPP~sumallPC1, NWT.deviations.average, class_3.x=="FF")
MM.anpp <- lm(AvgDev~sumallPC1, NWT.deviations.average, class_3.x=="MM")
SB.anpp <- lm(AvgDev~sumallPC1, NWT.deviations.average, class_3.x=="SB")
WM.anpp <- lm(AvgDev~sumallPC1, NWT.deviations.average, class_3.x=="WM")

summary(DM.anpp)
summary(FF.anpp)
summary(MM.anpp)
summary(SB.anpp)
summary(WM.anpp)

#try scatterplot of avg AvgDev, to see how compares to Katie's original figure
Figure10.B.devs <- ggplot(data=subset(NWT.deviations.average.collapsed), aes(class_3.x, AvgDev, group=class_3.x)) +
        geom_bar(aes(fill=class_3.x), stat="identity", color="black") +
        geom_errorbar(aes(ymin=AvgDev-StDev.Deviation, ymax=AvgDev+StDev.Deviation), width=0.5, color="black") +
        geom_hline(aes(yintercept=0)) +
        scale_fill_brewer(type="div") +
        labs(y="ANPP Sensitivity 2012\n (Average deviation from other years)", x=NULL) +
        theme_classic() +
        theme(legend.position="none",
              axis.text.x = element_text(size=12, margin=margin(10,5,5,5,"pt")),
              axis.text.y = element_text(size=12, margin=margin(5,10,5,5,"pt")),
              axis.title.y = element_text(size=14, margin=margin(5,13,5,5,"pt"))) +
        plottheme


ggplot(data=subset(NWT.deviations.average.yearlymeans), aes(class_3.x, MeanDev, color=class_3.x)) +
        geom_errorbar(aes(ymin=MeanDev-StDev.Deviation, ymax=MeanDev+StDev.Deviation)) +
        geom_boxplot() +
        geom_hline(aes(yintercept=0)) +
        theme_classic()

Fig10B.Avg2012Diff <- ggplot(ANPP_AvgDifference, aes(class_3, AvgDiff)) +
        geom_bar(aes(fill=class_3), stat="identity", color="black") +
        geom_errorbar(aes(ymin=AvgDiff-(1.96*SE.diff), ymax=AvgDiff+(1.96*SE.diff)), width=0.5) +
        geom_hline(aes(yintercept=0), lwd=0.5) +
        scale_fill_brewer(type="div") +
        #labs(y="ANPP Sensitivity 2012\n (% deviation from average)", x=NULL) +
        labs(x=NULL, y=NULL) +
        theme_classic()+
        theme(legend.position="none",
              axis.text.x = element_text(size=text.size, margin=margin(3,5,5,5,"pt")),
              axis.text.y = element_text(size=text.size, margin=margin(5,3,5,5,"pt"))) +
              #axis.title.y = element_text(size=text.size, margin=margin(5,13,5,5,"pt"))) +
        plottheme
        
        
#---Panel 3 - Peak snow depth vs. relative sensitivity to TMS, by habitat type
Fig10.c <- ggplot(CLMdat, aes(maxSNOW*100, pVEGC*1000, color=VEG)) +
        geom_point(color="black", size=5) +
        geom_point(size=4) +
        geom_hline(aes(yintercept=0), lwd=1) +
        scale_y_continuous(breaks=seq(from=-40, to=60, by=20), limits=c(-50,60)) +
        scale_x_continuous(breaks=seq(from=0,to=500,by=100), limits=c(0,500)) +
        ylab("Extended summer simulation\n(%GPP deviation from control)") + 
        xlab("Average max snowdepth (cm)") +
        scale_color_brewer(type="div") +
        theme_classic() +
        theme(text = element_text(size=text.size),
                axis.text.x = element_text(size=text.size, margin=margin(10,5,5,5,"pt")),
              axis.text.y = element_text(size=text.size, margin=margin(5,10,5,5,"pt")),
              axis.title.x = element_text(size=text.size, margin=margin(13,5,5,5,"pt")), 
              axis.title.y = element_text(size=text.size, margin=margin(5,13,5,5,"pt")),
              legend.title=element_blank(),
              legend.position=c(0.85,0.22), 
              legend.text = element_text(size=text.size),
              legend.background=element_rect(color="black")) +
        plottheme


#---Create final figure, 1 row by 3 columns
Fig10_multiplot<-plot_grid(Fig10.a,Fig10.b,Fig10.c, 
                           ncol=3, nrow=1,
                           rel_widths = c(1,1,1),
                           align='h')

Fig10_revised<-plot_grid(Fig10a.nocolorblocking,Fig10B.Avg2012Diff,Fig10.c, 
                           ncol=3, nrow=1,
                           rel_widths = c(1,1,1),
                           align='h')

save_plot('Fig_10_revised_test.pdf', Fig10_revised,
          base_height = 6,
          base_aspect_ratio = 3.5)

Fig10_alt <- plot_grid(Panel1.alt, Fig10a.nocolorblocking,Fig10B.Avg2012Diff, 
              ncol=3, nrow=1,
              #rel_heights = c(1,1,1),
              align='h')

save_plot('Fig_10_alt_20160222.pdf', Fig10_alt,
          base_height = 1.6,
          base_aspect_ratio = 3.5)

Fig10_2panel <- plot_grid(CLM.model, Panel1.alt.2012isolated,
                          ncol=2,
                          rel_widths = c(1,1.1),
                          align="h")

save_plot("Fig10_2panel_alt.pdf", Fig10_2panel,
         base_height=1.6,
         base_aspect_ratio=3.25)
         #base_height=4,
         #base_aspect_ratio=2)
