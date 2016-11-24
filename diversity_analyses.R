#Diversity analysis (Emily, Nov 2016)

library(tidyr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(vegan)


#This file has top and bottom hits
dat<-read.csv("~/Dropbox/NWT_data/saddle_plantcomp_LMH.csv")
dat<-dat[,-1]


#spread (and maybe delete lichen and moss)
dat2<-dat%>%
  select(plot,year,abund,class_3,USDA_code)%>%
  spread(USDA_code,abund,fill=0) #%>%
  #select(-contains("LICHN"),-contains("MOSS"))

dat2[1:20,1:10]
dim(dat2)


#calculate relative abundance and replace if you want
spe<-dat2[,4:110]
sperel<-spe/rowSums(spe)

dat2[,4:110]<-sperel
#dat2[,4:110]<-spe


#calculate div and sp richness
dat2$div<-diversity(dat2[,4:110])
dat2$rich<-specnumber(dat2[,4:110])

dat3<-dat2%>%
  group_by(year,class_3)%>%
  summarise(meandiv=mean(div),sediv=std.error(div),meanrich=mean(rich),serich=std.error(rich))%>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")

ggplot(dat3,aes(x=year,y=meanrich,col=class_3)) +
  geom_point(stat="identity") +
  geom_line()+
  geom_errorbar(aes(ymax=meanrich+serich,ymin=meanrich-serich),width=.25)

ggplot(dat3,aes(x=year,y=meandiv,col=class_3)) +
  geom_point(stat="identity") +
  geom_line()+
  geom_errorbar(aes(ymax=meandiv+sediv,ymin=meandiv-sediv),width=.25)


#GEROT,DECE,KOMY,CARUD,PRPA,SIPR,JUDR
dat3<-dat2%>%
  group_by(year,class_3)%>%
  summarise(mean=mean(GEROT),se=std.error(GEROT))%>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")

ggplot(dat3,aes(x=year,y=mean,col=class_3)) +
  geom_point(stat="identity") +
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)



#Pull in snowdepth at the plot level data

snow<-read.csv("~/Dropbox/NWT_data/NWT_SaddleData_snowdepth.csv")
snow<-snow[,-1]
head(snow)

dat4<-merge(dat2,snow,by=c("plot","year"))

dat5<-dat4%>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")

ggplot(dat5,aes(x=max_snow,y=GEROT,col=class_3)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "loess",size=.8,aes(group=class_3))
  
  