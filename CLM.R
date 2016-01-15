#Caitlin White#
#University of Colorado - INSTAAR#
#caitlin.t.white@colorado.edu#
#January 2016#

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
CLMdat <- read.csv("clm_NWT_TMS.csv")
str(CLMdat)
summary(CLMdat) #reorder factor levels to reflect habitats driest to wettest
#reorder factor levels to match panels 1 and 2
CLMdat$VEG = factor(CLMdat$VEG,levels(CLMdat$VEG)[c(2,1,3,5,4)])

#Emily's ANPP data -- tried using for panel 10.b but did not match Katie's original figure in draft 1
#NWTspdat <- read.csv("~/Dropbox/NWT_data/NWT_SnowXProdCorrected.csv")
#str(NWTspdat)
#summary(NWTspdat) #in good shape, 333 NAs in ANPP response variable

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


###---Make 3 panels for Figure 10###
#set plot margin values (for calling later in line)
margins.plot<-unit(c(0.5,0,0.5,2.5), 'lines') #top, right, bottom, left


#---Panel 1
#make fake data for background panel colors by habitat type
rects <- data.frame(xstart = seq(0,4,1), 
                    xend = seq(1,5,1), col = as.factor(seq(1:5)))


Fig10a.dat$Habitat <- factor(Fig10a.dat$Habitat,
                    levels = seq(1:5),
                    labels = c("FF", "DM", "MM", "WM", "SB")) 

Fig10.a <- ggplot() + 
        geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.5) +
        geom_point(data=Fig10a.dat, aes(xpos, Value, group=Limitation_Type), size=7, color="black", position= position_dodge(width=1)) +
        geom_point(data=Fig10a.dat, aes(xpos,Value, color=Limitation_Type), size=6, position= position_dodge(width=1)) +
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

#---Panel 2
Fig10.b <- ggplot(Fig10b.dat, aes(Habitat,ANPP_deviation, fill=Habitat)) + 
        geom_bar(stat="identity", color="black", width=0.5) +
        geom_errorbar(aes(ymin=ANPP_deviation-SE, ymax=ANPP_deviation+SE), width=0.25) +
        geom_hline(aes(yintercept=0)) + 
        scale_y_continuous(breaks=(seq(from=-40, to=60, by=20)), limits=(c(-50,60))) + 
        labs(y="ANPP Sensitivity 2012\n (% deviation from average)", x=NULL) +
        theme_classic(base_size = 18) +
        theme(axis.text = element_text(size=16),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size=18, face="bold"),
              legend.position="none",
              axis.text.x=element_text(angle=45, vjust=0.5, face="bold"),
              plot.margin=margins.plot)


##--Ignore code below: Tried using Emily's Farrer's ANPP data to recreate 10b but didn't match KS's original figure
#Just for 2012, summarize average ANPP by habitat type (using class_3 grouping), then plot deviation from average
#ANPP2012_byHabitat <- subset(NWTspdat, year==2012) %>%
#        group_by(class_3) %>%
#        summarise(AvgANPP = mean(anpp, na.rm=T), AvgResponse = mean(anpp_rspns, na.rm=T))

#AcrossYrsMean <- lapply(split(NWTspdat$anpp, NWTspdat$class_3), mean)


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


#---Panel 3 - Peak snow depth vs. relative sensitivity to TMS, by habitat type
Fig10.c <- ggplot(CLMdat, aes(maxSNOW*100, pVEGC*1000, color=VEG)) +
        geom_point(color="black", size=6) +
        geom_point(size=5) +
        geom_hline(aes(yintercept=0), lwd=1) +
        scale_y_continuous(breaks=seq(from=-40, to=60, by=20), limits=c(-50,60)) +
        scale_x_continuous(breaks=seq(from=0,to=500,by=100), limits=c(0,500)) +
        ylab("Extended summer simulation\n(%GPP deviation from control)") + 
        xlab("Average max snowdepth (cm)") +
        theme_classic(base_size=18) +
        theme(axis.text = element_text(size=16),
              axis.title.x = element_text(size=18, face="bold"), 
              axis.title.y = element_text(size=18, face="bold", lineheight = 1.1, hjust=0.5),
              legend.position="none",
              plot.margin = margins.plot)


#---Create final figure, 1 row by 3 columns
Fig10_multiplot<-plot_grid(Fig10.a,Fig10.b,Fig10.c, 
                           ncol=3, nrow=1,
                           rel_widths = c(1,1,1),
                           align='h')

save_plot('Fig_10.pdf', Fig10_multiplot,
          base_height = 6,
          base_aspect_ratio = 3)