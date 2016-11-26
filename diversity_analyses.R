#Diversity analysis (Emily, Nov 2016)
#I'm just trying a million different things, so the code isn't pretty!

library(tidyr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(vegan)


#This file has top and bottom hits
dat<-read.csv("~/Dropbox/NWT_data/saddle_plantcomp_LMH.csv")
dat<-dat[,-1]

#change 2LICHN to LICHN because you can't select a column that starts with a number
dat$USDA_code<-as.character(dat$USDA_code)
dat$USDA_code[which(dat$USDA_code=="2LICHN")]<-"LICHN"
dat$USDA_code[which(dat$USDA_code=="2COMP")]<-"COMP"
dat$USDA_code[which(dat$USDA_code=="2FORB")]<-"FORB"
dat$USDA_code[which(dat$USDA_code=="2GENT")]<-"GENT"
dat$USDA_code[which(dat$USDA_code=="2GRAM")]<-"GRAM"
dat$USDA_code[which(dat$USDA_code=="2MOSS")]<-"MOSS"
dat$USDA_code[which(dat$USDA_code=="2UNK")]<-"UNK"
dat$USDA_code<-as.factor(dat$USDA_code)

#spread (and maybe delete lichen and moss)
dat2<-dat%>%
  select(plot,year,abund,class_3,USDA_code)%>%
  spread(USDA_code,abund,fill=0) #%>%
  #select(-contains("LICHN"),-contains("MOSS"))

dat2[1:20,1:10]
dim(dat2)


#calculate relative abundance and replace if you want
spe<-dat2[,4:dim(dat2)[2]]
sperel<-spe/rowSums(spe)

#dat2[,4:dim(dat2)[2]]<-sperel
dat2[,4:dim(dat2)[2]]<-spe


#calculate div and sp richness
dat2$div<-diversity(dat2[,4:dim(dat2)[2]])
dat2$rich<-specnumber(dat2[,4:dim(dat2)[2]])


#Pull in too much summer PCA axis
tmspca<-read.csv("~/Dropbox/NWT_data/sumallyrsOutput.csv")
names(tmspca)[3]<-"year"
dat3<-merge(dat2,tmspca)


dat4<-dat3%>%
  group_by(year,class_3,sumallPC1)%>%
  summarise(meandiv=mean(div),sediv=std.error(div),meanrich=mean(rich),serich=std.error(rich))%>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")

ggplot(dat4,aes(x=year,y=meanrich,col=class_3)) +
  geom_point(stat="identity") +
  geom_line()+
  geom_errorbar(aes(ymax=meanrich+serich,ymin=meanrich-serich),width=.25)

ggplot(dat4,aes(x=year,y=meandiv,col=class_3)) +
  geom_point(stat="identity") +
  geom_line()+
  geom_errorbar(aes(ymax=meandiv+sediv,ymin=meandiv-sediv),width=.25)

ggplot(dat4,aes(x=sumallPC1,y=meanrich,col=class_3)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(group=class_3))
  


#Plotting all data points for individual species
dat6<-dat3 %>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")

ggplot(dat6,aes(x=sumallPC1,y=DECE,col=class_3)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(group=class_3))

summary(lm(DECE~sumallPC1,data=subset(dat6,class_3=="MM")))




#Selecting only abundance species or doing things on multiple species

#ind<-which(colSums(dat6[,4:(dim(dat6)[2]-4)])/dim(dat6)[1]>.5)
#dat7<-data.frame(dat6[,c(1:3,113:116)],dat6[,ind])

#dat8<-dat7 %>%
#  gather(species,abund,ANSE4:TOPY)%>%
#  filter(class_3=="DM")

# ggplot(dat8,aes(x=sumallPC1,y=abund,col=species)) +
#   geom_point(stat="identity") +
#   geom_line(stat="smooth",method = "lm",size=.8,aes(group=species))

dat9<-dat6%>%
  filter(class_3=="DM")%>%
  select(ALGE:UNK)
sort(colSums(dat9))

dat7<-dat6%>%
  gather(species,abund,ALGE:UNK)%>%
  filter(species%in%c("KOMY","LICHN","GEROT","CARUD","SEDES","LLSE","TRIDA2","ORALA","MIOB2"),class_3=="DM")%>%
  group_by(species)
head(dat7)

ggplot(dat7,aes(x=sumallPC1,y=abund,col=species)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~species,scales="free")



#Lrr with 2012

dat7<-dat6%>%
  gather(species,abund,ALGE:UNK)%>%
  filter(species%in%c("KOMY","LICHN","GEROT","CARUD","SEDES","LLSE","TRIDA2","ORALA","MIOB2")&class_3=="DM"|species%in%c("GEROT","DECE","CASCS2")&class_3=="MM")#%>%
  #group_by(species)%>%
  #mutate(year12=ifelse(year==2012,"tms","con"))%>%
  #group_by(year12,species)%>%
  #summarise(mean=mean(abund),se=std.error(abund))
head(as.data.frame(dat7))

dat12<-dat7%>%
  filter(year==2012)%>%
  mutate(abund12=abund)%>%
  select(plot,class_3,species,abund12)
datother<-dat7%>%
  filter(year!=2012)%>%
  select(year,plot,class_3,species,abund)

dat13<-merge(datother,dat12,by=c("plot","species","class_3"))
head(dat13)
dat14<-dat13%>%
  filter(species%in%c("KOMY","GEROT","CARUD","MIOB2","DECE","LLSE","CASCS2","LICHN"))%>%
  mutate(lrr=log(abund12/abund))%>%
  group_by(species,year,class_3)%>%  
  filter(is.infinite(lrr)==F,is.na(lrr)==F)%>%
  summarise(lrr=mean(lrr))%>%
  group_by(species,class_3)%>%
  summarise(mean=mean(lrr),se=std.error(lrr))
dat14

ggplot(dat14,aes(x=species,y=mean,col=class_3)) +
  geom_point(stat="identity") +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25) +
  geom_hline(yintercept=0)






#Plotting means and se for individual species: GEROT,DECE,KOMY,CARUD,PRPA,SIPR,JUDR, SIACS2
dat5<-dat3%>%
  group_by(year,class_3,sumallPC1)%>%
  summarise(mean=mean(GEROT),se=std.error(GEROT))%>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")%>%
  filter(class_3=="DM")

ggplot(dat5,aes(x=year,y=mean,col=class_3)) +
  geom_point(stat="identity") +
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)

ggplot(dat5,aes(x=sumallPC1,y=mean,col=class_3)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(group=class_3))+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)

summary(lm(mean~sumallPC1,data=dat5))


dat3%>%
  group_by(class_3)%>%
  filter(year==2012)%>%
  summarise(mean=mean(GEROT),se=std.error(GEROT))%>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")%>%
  filter(class_3=="DM")




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
  
  



#Bill's 246 plots
bil<-read.csv("~/Dropbox/NWT_data/SppComp246.csv")
colnames(bil)[1]<-"year"
bil[which(is.na(bil),arr.ind = T)]<-0

bil2<-bil%>%
  subset(TRT==0)

bil3<-merge(bil2,tmspca)

rowSums(bil3[,4:60])

ggplot(bil3,aes(x=sumallPC1,y=SELDEN)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)



#ITEX experiment



