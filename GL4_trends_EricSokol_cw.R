require(ggplot2)
require(grid)

#set wd to Er
setwd("~/Documents/Suding Lab/NWT LTER/Eric_Sokol_Lakes")

# -- read data
GL4_long <- read.csv("ice, climate and flow_D1_Dan.csv")
GL4 <-read.csv("long term data_Dan_fixed.csv")

# -- plotting parameters and themes
text.size<-10
limits.x<-c(65, 130)
margins.plot<-unit(c(0,0,0.5,2.5), 'lines') #top, right, bottom, left
margins.axes<-unit(.25,'lines')
margins.panel<-unit(3,'lines')

plottheme<-theme(plot.margin = margins.plot,
      axis.ticks.margin = margins.axes,
      axis.title = element_text(
        face='plain')
)

#------flushing rate
limits <- aes(ymax = flushing + flushing._se, ymin = flushing - flushing._se)
figa <- ggplot(GL4_long, aes(gl4,flushing)) + 
        geom_point(size = 4, color="black") +
        geom_point(size = 3, color="cyan 3") +
        theme_classic() +
        theme(text = element_text(size = text.size), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=text.size)) + 
        stat_smooth(method = "lm", colour = "black") +
        geom_errorbar(limits, width = 0.5, size = 0.3) +
        labs(x = NULL, y="\nFlushing (days)\n") +
        scale_x_continuous(limits = limits.x) +
        plottheme
# figa


## CW edit: compare flushing rate (days) against PCA1 scores
#(1)read in PCA scores
library(tidry)
library(dplyr)
PCAscores <- read.csv("~/Dropbox/NWT_data/NWT_Climate_summerPCscores_20151123.csv")
PCAscores <- rename(PCAscores, year=Year)

#(2) merge GL4_long dataset with PCA scores (by="year")
GL4_long <- merge(GL4_long, PCAscores, by="year")

#(3) test relationships
library(lme4)
flushing.lm <- lm(flushing ~ sumallPC1, GL4_long)
summary(flushing.lm) #very significant! model p=0.0009645, R-square=0.3835, coeff = 21.626, se=5.718

flushing.iceout.lm <- lm(flushing ~ gl4, GL4_long) #for comparison against 
summary(flushing.iceout.lm) #ice out date accounts for more of the variation in flushing rate, has higher significance

#(4) plot
figa_wPCA1 <- ggplot(GL4_long, aes(sumallPC1, flushing)) + 
        geom_point(size = 3) + 
        stat_smooth(method = "lm", colour = "black", se=F, lwd=1) +
        geom_errorbar(limits) +
        labs(x = NULL, y="\nFlushing (days)\n") +
        scale_x_continuous(breaks=c(seq(from = -1.0, to=1.5, by=0.5))) +
        scale_y_continuous(breaks=c(seq(from = 90, to=210, by=30))) +
        theme_classic() +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size), 
              axis.text.y = element_text(size=text.size)) +
        plottheme +
        #theme(legend.position="none", 
         #     axis.title.x = element_blank(),
         #     axis.text.x = element_blank(),
         #     axis.text = element_text(),
         #     axis.title.y = element_text(vjust=1, size=18))

#figa_wPCA1

#-------stratification
limits <- aes(ymax = strat + strat_se, ymin = strat - strat_se)
figb <- ggplot(GL4, aes(ice,strat)) + 
        geom_point(size = 4, color="black") +
        geom_point(size = 3, color = "cyan 3") + 
        theme_classic() +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_blank(), 
                axis.text.y = element_text(size=text.size)) + 
        stat_smooth(method = "lm", colour = "black") +
        geom_errorbar(limits, width = 0.5, size = 0.3) +
        labs(x = NULL, y=expression(paste("Stratification (",degree~C,")"))) +
        scale_x_continuous(limits = limits.x) +
        plottheme


## CW edit: compare lake stratification temp against PCA1 scores
#(1) merge GL4 dataset with PCA scores (by="year")
GL4 <- merge(GL4, PCAscores, by="year")

#(2) test relationships
stratification.lm <- lm(strat ~ sumallPC1, GL4)
summary(stratification.lm) #signif at p<0.1.. p=0.059225, R-square=0.3117, coeff = 0.6775, se=.3184

stratification.iceout.lm <- lm(strat ~ ice, GL4) #for comparison against sumallPC1
summary(stratification.iceout.lm) #ice out date accounts for more of the variation in flushing rate, has higher significance
#model signif is p=0.0013538, R-square=0.6583, coeff=-0.04474, se=0.01019

#(3) plot
figb_wPCA1 <- ggplot(GL4, aes(sumallPC1, strat)) + 
        geom_point(size = 3) + 
        stat_smooth(method = "lm", colour = "black", se=F, lwd=1) +
        geom_errorbar(limits) +
        labs(x = NULL, y=expression(paste("Stratification (",degree~C,")"))) +
        scale_x_continuous(breaks=c(seq(from = -1.0, to=1.5, by=0.5))) +
        geom_point(aes(x=-1.05, y=1), color="white") +
        theme_classic() +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size), 
              axis.text.y = element_text(size=text.size)) +
        plottheme
        #theme(legend.position="none", 
        #      axis.title.x = element_blank(),
        #      axis.text.x = element_blank(),
        #      axis.text = element_text(size=16),
        #      axis.title.y = element_text(vjust=1, size=18))
#figb_wPCA1


# #-------nitrate
# limits <- aes(ymax = log(no3) + se_log_no3, ymin = log(no3) - se_log_no3)
# figk <- ggplot(GL4, aes(ice,log(no3))) + 
#   geom_point(size = 3) + theme_classic() +
#   theme(text = element_text(size = text.size), 
#         axis.text.x = element_text(size=text.size), 
#         axis.text.y = element_text(size=text.size)) + 
#   geom_errorbar(limits, width = 0.5, size = 0.3) +
#   labs(x = NULL, 
#        y = expression(paste(Log(NO[3]^"-")," ",(uEQ/L), sep=""))) +
#   scale_x_continuous(limits = limits.x) +
#   scale_y_continuous(limits = c(1.8,3.2), breaks = seq(1.8,3.2, by =0.4)) +
#   plottheme

#-------DOC
limits <- aes(ymax = doc + se_doc, ymin = doc - se_doc)
figm <- ggplot(GL4, aes(ice,doc)) + 
        geom_point(size = 4) +
        geom_point(size = 3, color = "cyan 3") + 
        theme_classic() +
         theme(text = element_text(size = text.size), 
                axis.text.x = element_blank(), 
                axis.text.y = element_text(size=text.size)) + 
        geom_errorbar(limits, width = 0.5, size = 0.3) +
        stat_smooth(method = "lm", colour = "black", linetype=2, se=F) + 
        labs(x = NULL, y = expression(paste("DOC (mg ",L^-1,")"))) +
        scale_x_continuous(limits = limits.x) +
        plottheme
        # figm


## CW edit: compare DOC against PCA1 scores
#(1) test relationships
DOC.lm <- lm(doc ~ sumallPC1, GL4)
summary(DOC.lm) #not significant (model p val=0.1706, R-square=0.22)

DOC.iceout.lm <- lm(doc ~ ice, GL4) #for comparison against sumallPC1
summary(DOC.iceout.lm) #not significant either, but has lower pval and higher Rsquare
#model signif is p=0.1199 , R-square=0.2747

#(3) plot
figm_wPCA1 <- ggplot(GL4, aes(sumallPC1, doc)) + 
        geom_point(size = 3) + 
        stat_smooth(method = "lm", colour = "black", se=F, lwd=1, linetype=2) +
        geom_errorbar(limits) +
        labs(x = NULL, y=expression(paste("DOC (mg ",L^-1,")"))) +
        scale_x_continuous(breaks=c(seq(from = -1.0, to=1.5, by=0.5))) +
        geom_point(aes(x=-1.05, y=1), color="white") +
        theme_classic() +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size), 
              axis.text.y = element_text(size=text.size)) + 
        plottheme
        #theme(legend.position="none",
        #      axis.text.x = element_blank(),
        #      axis.title.x = element_blank(),
        #      axis.text = element_text(size=16),
        #      axis.title.y = element_text(vjust=1, size=18))
#figm_wPCA1

#---------------------------Chlorophyll-a
limits <- aes(ymax = chl_new + chl_new_se, ymin = chl_new - chl_new_se) 
figv <- ggplot(GL4, aes(ice,chl_new)) + 
        geom_point(size=4) +
        geom_point(size = 3, color="cyan 3") + 
        theme_classic() +
        theme(text = element_text(size=text.size), 
                axis.text.x = element_blank(), 
                axis.text.y = element_text(size=text.size)) + 
        stat_smooth(method = "lm", colour = "black") +
        geom_errorbar(limits, width = 0.5, size = 0.3) +
        labs(x = NULL , y = expression(paste("Chl-", italic("a")," (",mu,"g ", L^-1,")"))) +
        scale_x_continuous(limits = limits.x) +
        plottheme


## CW edit: compare chl-a against PCA1 scores
#(1) test relationships
chla.lm <- lm(chl_new ~ sumallPC1, GL4)
summary(chla.lm) #not significant (model p val=0.171, R-square=0.1632)

chla.iceout.lm <- lm(chl_new ~ ice, GL4) #for comparison against sumallPC1
summary(chla.iceout.lm) #significant! with higher Rsquare
#model signif is p=0.05628 , R-square=0.2926, coeff=-0.04034, se=0.01891

#(2) plot
figv_wPCA1 <- ggplot(GL4, aes(sumallPC1, chl_new)) + 
        geom_point(size = 4) + 
        stat_smooth(method = "lm", colour = "black", se=F, lwd=1, linetype=2) +
        geom_errorbar(limits) +
        labs(x = NULL, y=expression(paste("Chl-", italic("a")," (",mu,"g ", L^-1,")"))) +
        scale_x_continuous(breaks=c(seq(from = -1.0, to=1.5, by=0.5))) +
        geom_point(aes(x=-1.05, y=1), color="white") +
        theme_classic(base_size = 16) +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size), 
              axis.text.y = element_text(size=text.size)) +
        plottheme
       # theme(legend.position="none",
        #      axis.text.x = element_blank(),
        #      axis.title.x = element_blank(),
        #      axis.text = element_text(size=16),
        #      axis.title.y = element_text(vjust=1, size=18))
#figv_wPCA1

#-------Daphnia

figt <- ggplot(GL4, aes(ice,dapul)) + 
        geom_point(size = 4) + 
        geom_point(size = 3, color = "cyan 3") +
        theme_classic() +
        theme(text = element_text(size=text.size), 
                axis.text.x = element_text(size=text.size), 
                axis.text.y = element_text(size=text.size)) + 
         stat_smooth(method = "lm", colour = "black") +
        labs(x = "\nIce Out Date (days from April 1)", 
        y =expression(paste(italic("Daphnia")," (indiv. ", L^-1,")"))) +
        scale_x_continuous(limits = limits.x) +
        scale_y_continuous(limits = c(-1.5,4), breaks = seq(0,4, by =1)) +
        plottheme

## CW edit: compare Daphnia density against PCA1 scores
#(1) test relationships
daphnia.lm <- lm(dapul ~ sumallPC1, GL4)
summary(daphnia.lm) #significant at p>0.1;
#model p=0.09744, R square=0.6541, coeff=1.10211, se=0.4278

daphnia.iceout.lm <- lm(dapul ~ ice, GL4) #for comparison against sumallPC1
summary(daphnia.iceout.lm) #higher significance, with higher Rsquare
#model signif is p=0.02858 , R-square=0.8401, coeff=-0.05050, se=0.01272

#(2) plot
figt_wPCA1 <- ggplot(GL4, aes(sumallPC1, dapul)) + 
        geom_point(size = 3) + 
        stat_smooth(method = "lm", colour = "black", se=F, lwd=1) +
        labs(x = NULL, y=expression(paste(italic("Daphnia")," (indiv. ", L^-1,")"))) +
        scale_x_continuous(breaks=c(seq(from = -1.0, to=1.5, by=0.5))) +
        scale_y_continuous(breaks=c(seq(from = 1, to=4, by=1))) + 
        geom_point(aes(x=-1.05, y=4), color="white") +
        theme_classic(base_size = 16) +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size), 
              axis.text.y = element_text(size=text.size)) +
        plottheme
        #theme(legend.position="none",
        #      #axis.text.x = element_blank(),
        #      axis.title.x = element_blank(),
        #      axis.text = element_text(size=16),
        #      axis.title.y = element_text(vjust=1, size=18))

# ---------------------------------------------------------
# -- multipanel plot

require(cowplot)
require(gridExtra)
graphics.off()
# windows(4,8)
multiplot<-plot_grid(figa,figb,figm,figv,figt, 
                     labels=c('(A)','(B)','(C)','(D)','(E)'), 
                     ncol=1, nrow=5,
                     rel_heights = c(1,1,1,1,1.2),
                     align='v')
# multiplot

# ggsave("Fig_plt.pdf", width = 16, height = 9, dpi = 120)
save_plot('Fig_plot.pdf', multiplot,
          base_height = 8,
          base_aspect_ratio = .5)

##CW edit: multipanel PC1 score plots
multiplot_wPCA1<-plot_grid(figa_wPCA1,figb_wPCA1,figm_wPCA1,figv_wPCA1,figt_wPCA1, 
                     labels=c('(A)','(B)','(C)','(D)','(E)'), 
                     ncol=1, nrow=5,
                     rel_heights = c(1,1,1,1,1.2),
                     align='v')

save_plot('Fig_plot_wPCA.pdf', multiplot_wPCA1,
          base_height = 8,
          base_aspect_ratio = .5)