#Caitlin White#
#INSTAAR, University of Colorado#
#caitlin.t.whit@colorado.edu#
#November 2015#

##Marko's dataset of species response to N
#Four species, Kobmyo, Carex rup, Des and Geum, in each block in each experiment in each year with their lnRR and PC1 and PC2 scores. 
#Marko transforming PC1, since it isn’t very normal.

#Set working directory as appropriate..#
setwd("~/Dropbox/NWT_data")

#load necessary libraries#
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#Read in Marko's N response dataset
Ndat <- read.csv("NWT_SppResponse_AllExp_20151125.csv") %>%
  tbl_df()

#Test linear relationships for significance
#Subset data by species
Carrup <- subset.data.frame(Ndat, Spp=="CARRUP")
Descae <- subset.data.frame(Ndat, Spp=="DESCAE")
Geuros <- subset.data.frame(Ndat, Spp=="GEUROS")
Kobmyo <- subset.data.frame(Ndat, Spp=="KOBMYO")

c.lm <- lm(lnRR~sumallPC1, data=Carrup)
d.lm <- lm(lnRR~sumallPC1, data=Descae)
g.lm <- lm(lnRR~sumallPC1, data=Geuros)
k.lm <- lm(lnRR~sumallPC1, data=Kobmyo)

summary(c.lm)
summary(d.lm)
summary(g.lm) #only significant trend, at p = 0.094
summary(k.lm)

#Visualize
Ndat.lm <- ggplot(Ndat, aes(sumallPC1, lnRR, color=Spp))
Ndat.lm + geom_point(aes(size=2)) + facet_wrap(~Spp) + 
  geom_smooth(method="lm", se=F, data=Geuros, color="black", lwd=1) +
  geom_smooth(method="lm", se=F, data=subset(Ndat, Spp!="GEUROS"), color="black", linetype=2, lwd=1) +
  theme_classic() +
  theme(
    legend.position="none", 
    axis.text = element_text(size = 16),
    axis.title.x = element_text(vjust=-0.5, size =18),
    axis.title.y = element_text(vjust=0.5, size=18),
    strip.text.x = element_text(size = 14, face="bold"))
