#Diversity analysis (Emily, Nov 2016)
#I'm just trying a million different things, so the code isn't pretty!

library(tidyr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(vegan)
library(nlme)

save.image("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/climatediversityanalyses.Rdata")

load("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/climatediversityanalyses.Rdata")

#Looking into climate, how extreme was 2012?
## Read in the climate data 
climate<-read.csv("~/Dropbox/NWT_data/NWT_ClimateData_2015-11-02.csv")
colnames(climate)[1]<-"year"

#included in PCA: "sum_meanT"   "sum_precip"  "sum_moisturedeficit" "sum_PET" "sum_GDD"   "fivedayrunning5C"  "fivedayrunning12C"   "GSLthreedayneg3C"    "iceoff_GL4"         

hist(climate$sum_GDD)
climate$sum_GDD[climate$year==2012]
mean(climate$sum_GDD)
mean(climate$sum_GDD)+sd(climate$sum_GDD)*2

climate$GSLthreedayneg3C[climate$year==2012]
mean(climate$GSLthreedayneg3C)
mean(climate$GSLthreedayneg3C)+sd(climate$GSLthreedayneg3C)*2

climate$sdl_meltout[climate$year==2012]
mean(climate$sdl_meltout,na.rm=T)
mean(climate$GSLthreeday0C)+sd(climate$GSLthreeday0C)*2

climate$iceoff_GL4[climate$year==2012]
mean(climate$iceoff_GL4)
min(climate$iceoff_GL4)
mean(climate$iceoff_GL4)-sd(climate$iceoff_GL4)

hist(climate$fivedayrunning12C)
climate$fivedayrunning12C[climate$year==2012]
mean(climate$fivedayrunning12C)
min(climate$fivedayrunning12C)
mean(climate$fivedayrunning12C)-sd(climate$fivedayrunning12C)

hist(climate$fivedayrunning5C)
climate$fivedayrunning5C[climate$year==2012]
mean(climate$fivedayrunning5C)
min(climate$fivedayrunning5C)
mean(climate$fivedayrunning5C)-sd(climate$fivedayrunning5C)

hist(climate$sum_precip)
climate$sum_precip[climate$year==2012]
mean(climate$sum_precip)
mean(climate$sum_precip)+sd(climate$sum_precip)*2

hist(climate$sum_meanT)
climate$sum_meanT[climate$year==2012]
mean(climate$sum_meanT)
mean(climate$sum_meanT)+sd(climate$sum_meanT)*2

hist(climate$sum_moisturedeficit)
climate$sum_moisturedeficit[climate$year==2012]
mean(climate$sum_moisturedeficit,na.rm=T)
mean(climate$sum_moisturedeficit,na.rm=T)-sd(climate$sum_moisturedeficit,na.rm=T)*2

cbind(dat3a$year,dat3a$sum_moisturedeficit)
#1989 was the year with the largest moisture deficit, this does not work for me beacuse it also had the 

climatesub<-subset(climate,climate$year%in%c(yrs))
yrs<-unique(dat$year)



#Trying PCA on 2 moisture variables and 2 growing season/temp variables. doesn't really change things
climateSummer2 <-climate %>%
  select(year, sum_meanT, sum_precip, sum_moisturedeficit,fivedayrunning5C, iceoff_GL4) %>%
  na.omit()
row.names(climateSummer2)<-climateSummer2$year
sumallPCA2 <-rda(na.exclude(climateSummer2[,2:ncol(climateSummer2)]), scale=T)
plot(sumallPCA2, scaling=3)
plot(sumallPCA, scaling=3)
summary(sumallPCA)



#Species comp. This file has top and bottom hits
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
dat2$tothits<-rowSums(dat2[,4:dim(dat2)[2]])

#Pull in too much summer PCA axis
tmspca<-read.csv("~/Dropbox/NWT_data/sumallyrsOutput.csv")
names(tmspca)[3]<-"year"
dat3<-merge(dat2,tmspca)
dat3a<-merge(dat3,climate,"year")

#Diversity and richness plots
dat4<-dat3%>%
  group_by(year,class_3,sumallPC1)%>%
  summarise(meandiv=mean(div),sediv=std.error(div),meanrich=mean(rich),serich=std.error(rich),meantot=mean(tothits),setot=std.error(tothits))%>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")

ggplot(dat4,aes(x=year,y=meanrich,col=class_3)) +
  geom_point(stat="identity") +
  geom_line()+
  geom_errorbar(aes(ymax=meanrich+serich,ymin=meanrich-serich),width=.25)

ggplot(dat4,aes(x=year,y=meandiv,col=class_3)) +
  geom_point(stat="identity") +
  geom_line()+
  geom_errorbar(aes(ymax=meandiv+sediv,ymin=meandiv-sediv),width=.25)

ggplot(dat4,aes(x=sumallPC1,y=meantot,col=class_3)) +
  geom_point(stat="identity") +
  geom_line()+
  geom_errorbar(aes(ymax=meantot+setot,ymin=meantot-setot),width=.25)

ggplot(dat4,aes(x=sumallPC1,y=meanrich,col=class_3)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(group=class_3))
  


#Plotting all data points for individual species
dat6<-dat3a %>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")

ggplot(dat6,aes(x=sumallPC1,y=DECE,col=class_3)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(group=class_3))

summary(lm(DECE~sumallPC1,data=subset(dat6,class_3=="MM")))




#Doing analyses on multiple species or selecting only abundant species

#ind<-which(colSums(dat6[,4:(dim(dat6)[2]-4)])/dim(dat6)[1]>.5)
#dat7<-data.frame(dat6[,c(1:3,113:116)],dat6[,ind])

#dat8<-dat7 %>%
#  gather(species,abund,ANSE4:TOPY)%>%
#  filter(class_3=="DM")

# ggplot(dat8,aes(x=sumallPC1,y=abund,col=species)) +
#   geom_point(stat="identity") +
#   geom_line(stat="smooth",method = "lm",size=.8,aes(group=species))

#List the most abundant species in each community type
dat9<-dat6%>%
  filter(class_3=="SB")%>%
  select(ALGE:UNK)
sort(colSums(dat9))


#FF plot of abund vs. PC1 for abundant species
datFF<-dat6%>%
  gather(species,abund,ALGE:UNK)%>%
  filter(species%in%c("CARUD","TRDA2","SIACS2","SEDES","MIOB2","GEROT"),class_3=="FF")%>%
  group_by(species)
head(datFF)

ggplot(datFF,aes(x=sum_moisturedeficit,y=abund,col=species)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~species,scales="free")

ggplot(datFF,aes(x=sumallPC1,y=abund,col=plot)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(col=plot,group=plot))+
  facet_wrap(~species,scales="free")


#DM plot of abund vs. PC1 for abundant species
datDM<-dat6%>%
  gather(species,abund,ALGE:UNK)%>%
  filter(species%in%c("KOMY","LICHN","GEROT","CARUD","SEDES","LLSE","TRDA2","ORALA","MIOB2"),class_3=="DM")%>%
  group_by(species)
head(datDM)

ggplot(datDM,aes(x=sum_moisturedeficit,y=abund,col=species)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~species,scales="free")

ggplot(datDM,aes(x=sumallPC1,y=abund,col=plot)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(col=plot,group=plot))+
  facet_wrap(~species,scales="free")

datDMGEROT<-datDM%>%
  filter(species=="KOMY")

ggplot(datDMGEROT,aes(x=sum_moisturedeficit,y=abund,col=plot)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)#aes(col=plot,group=plot)

m1<-lme(abund~sum_meanT+sdl_max_snwdpth+sum_moisturedeficit,data=datDMGEROT,random=~1|plot)
summary(m1)
datDMGEROT$res<-resid(m1)

m2<-lme(res~sdl_max_snwdpth,data=datDMGEROT,random=~1|plot)
summary(m2)


#MM plot of abund vs. PC1 for abundant species
datMM<-dat6%>%
  gather(species,abund,ALGE:UNK)%>%
  filter(species%in%c("GEROT","DECE","TRPAP","ARSC","CASCS2","POBI6","CALE4","MIOB2","LLSE","FEBR"),class_3=="MM")%>%
  group_by(species)
head(datMM)

ggplot(datMM,aes(x=sumallPC1,y=abund,col=species)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~species,scales="free")

ggplot(datMM,aes(x=sumallPC1,y=abund,col=plot)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(col=plot,group=plot))+
  facet_wrap(~species,scales="free")

datMMGEROT<-datMM%>%
  filter(species=="DECE")

ggplot(datMMGEROT,aes(x=sum_moisturedeficit,y=abund,col=plot)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)#aes(col=plot,group=plot)

m1<-lme(abund~sum_moisturedeficit,data=datMMGEROT,random=~1|plot)
summary(m1)


#WM plot of abund vs. PC1 for abundant species
datWM<-dat6%>%
  gather(species,abund,ALGE:UNK)%>%
  filter(species%in%c("CASCS2","CALE4","GEROT","SAPE18","DECE","ARSC"),class_3=="WM")%>%
  group_by(species)
head(datMM)

ggplot(datWM,aes(x=sumallPC1,y=abund,col=species)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~species,scales="free")

ggplot(datWM,aes(x=sumallPC1,y=abund,col=plot)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(col=plot,group=plot))+
  facet_wrap(~species,scales="free")

#SB plot of abund vs. PC1 for abundant species
datSB<-dat6%>%
  gather(species,abund,ALGE:UNK)%>%
  filter(species%in%c("SIPR","GEROT","TRPAP","CAPY3","DECE","MIOB2","ARSC","JUDR"),class_3=="SB")%>%
  group_by(species)
head(datSB)

ggplot(datSB,aes(x=sum_moisturedeficit,y=abund,col=species)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~species,scales="free")

ggplot(datSB,aes(x=sumallPC1,y=abund,col=plot)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8,aes(col=plot,group=plot))+
  facet_wrap(~species,scales="free")





##### Lrr with 2012 ####
#"LICHN","SEDES","LLSE","TRIDA2","ORALA","MIOB2"

dat7<-dat6%>%
  gather(species,abund,ALGE:UNK)%>%
  filter(species%in%c("KOMY","GEROT","CARUD","SEDES")&class_3=="DM"|species%in%c("GEROT","DECE","CASCS2","ARSC")&class_3=="MM"|species%in%c("CASCS2","CALE4","GEROT","DECE")&class_3=="WM"|species%in%c("SIPR","TRPAP","CAPY3","DECE")&class_3=="SB"|species%in%c("CARUD","TRDA2","SIACS2","SEDES")&class_3=="FF")#%>%
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
  filter(class_3!="FF")%>%
  mutate(lrr=log(abund12/abund))%>%
  group_by(species,year,class_3)%>%  
  filter(is.infinite(lrr)==F,is.na(lrr)==F)%>%
  summarise(lrr=mean(lrr))%>%
  group_by(species,class_3)%>%
  summarise(mean=mean(lrr),se=1.96*std.error(lrr))
dat14
dat14$class_3<-factor(dat14$class_3,levels = c("DM","MM","WM","SB"))
#names(dat14)[1]<-"Species"
#dat14$class_3<-factor(dat14$class_3,levels = c("FF","DM","MM","WM","SB"))

mygenera<-data.frame(species=c("ARSC","CALE4","CAPY3","CARUD","CASCS2","DECE","GEROT","KOMY","SEDES","SIPR","TRPAP"),genus=c("Artemisia","Caltha","Carex pyrenaica","Carex rupestris","Carex scopulorum","Deschampsia","Geum","Kobresia","Selaginella","Sibbaldia","Trifolium"))

dat15<-merge(dat14,mygenera)

names(dat15)[5]<-"Species"

ggplot(dat15,aes(x=class_3,y=mean,col=Species,group=Species)) +
  labs(x = "Community Type", y="Percent Cover") +
  geom_point(stat="identity",size=3,position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),size=1,width=.1,position=position_dodge(width=.5)) +
  geom_hline(yintercept=0)


#SB to DM ordering on X axis

dat14$class_3<-factor(dat14$class_3,levels = c("SB","WM","MM","DM"))
#names(dat14)[1]<-"Species"
#dat14$class_3<-factor(dat14$class_3,levels = c("FF","DM","MM","WM","SB"))

mygenera<-data.frame(species=c("ARSC","CALE4","CAPY3","CARUD","CASCS2","DECE","GEROT","KOMY","SEDES","SIPR","TRPAP"),genus=c("Artemisia","Caltha","Carex pyrenaica","Carex rupestris","Carex scopulorum","Deschampsia","Geum","Kobresia","Selaginella","Sibbaldia","Trifolium"))

dat15<-merge(dat14,mygenera)

names(dat15)[5]<-"Species"

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Proposals/NSFpreproposal2017/Figs/plantresponse2012acrosscommunitytype.pdf",height=4,width=5.6)
ggplot(dat15,aes(x=class_3,y=mean,col=Species,group=Species)) +
  labs(x = "Community Type", y="Percent Cover") +
  geom_point(stat="identity",size=3,position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),size=1,width=.1,position=position_dodge(width=.5)) +
  geom_hline(yintercept=0)
dev.off()



#Lrr with 2013

dat7<-dat6%>%
  gather(species,abund,ALGE:UNK)%>%
  filter(species%in%c("KOMY","GEROT","CARUD","SEDES")&class_3=="DM"|species%in%c("GEROT","DECE","CASCS2","ARSC")&class_3=="MM"|species%in%c("CASCS2","CALE4","GEROT","SAPE18","DECE")&class_3=="WM"|species%in%c("SIPR","GEROT","TRPAP","CAPY3","DECE")&class_3=="SB"|species%in%c("CARUD","TRDA2","SIACS2","SEDES")&class_3=="FF")#%>%
#group_by(species)%>%
#mutate(year12=ifelse(year==2012,"tms","con"))%>%
#group_by(year12,species)%>%
#summarise(mean=mean(abund),se=std.error(abund))
head(as.data.frame(dat7))

dat12<-dat7%>%
  filter(year==1989)%>%
  mutate(abund12=abund)%>%
  select(plot,class_3,species,abund12)
datother<-dat7%>%
  filter(year!=2013)%>%
  select(year,plot,class_3,species,abund)

dat13<-merge(datother,dat12,by=c("plot","species","class_3"))
head(dat13)
dat14<-dat13%>%
  filter(class_3!="FF")%>%
  mutate(lrr=log(abund12/abund))%>%
  group_by(species,year,class_3)%>%  
  filter(is.infinite(lrr)==F,is.na(lrr)==F)%>%
  summarise(lrr=mean(lrr))%>%
  group_by(species,class_3)%>%
  summarise(mean=mean(lrr),se=1.96*std.error(lrr))
dat14
dat14$class_3<-factor(dat14$class_3,levels = c("DM","MM","WM","SB"))
#dat14$class_3<-factor(dat14$class_3,levels = c("FF","DM","MM","WM","SB"))

ggplot(dat14,aes(x=class_3,y=mean,col=species,group=species)) +
  geom_point(stat="identity",position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.1,position=position_dodge(width=.5)) +
  geom_hline(yintercept=0)



#Looking closer at the geum DM data
dat13b<-dat13%>%
  filter(species=="GEROT"&class_3=="DM")%>%
  mutate(dif=abund12-abund)%>%
  mutate(lrr=log(abund12/abund))%>%
  group_by(year)%>%
  summarise(difm=mean(dif))
dat13b
mean(dat13b$difm)
std.error(dat13b$difm)



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




#Doing regressions of each species with sum_moisturedeficit and temp/fivedayrunning5C/sdl_max_snwdpth

yrs<-unique(dat$year)
climatesub<-subset(climate,climate$year%in%c(yrs))
climatesub$sum_moisturedeficitS<-(climatesub$sum_moisturedeficit-mean(climatesub$sum_moisturedeficit))/sd(climatesub$sum_moisturedeficit)
climatesub$sdl_max_snwdpthS<-(climatesub$sdl_max_snwdpth-mean(climatesub$sdl_max_snwdpth))/sd(climatesub$sdl_max_snwdpth)
climatesub$sum_meanTS<-(climatesub$sum_meanT-mean(climatesub$sum_meanT))/sd(climatesub$sum_meanT)
climatesub2<-climatesub%>%select(year,sum_moisturedeficitS,sdl_max_snwdpthS,sum_meanTS)
dat6a<-merge(dat6,climatesub2)

splist<-data.frame(rbind(cbind(species=c("CARUD","TRDA2","SIACS2","SEDES","MIOB2","GEROT"),class_3="FF"),cbind(species=c("KOMY","GEROT","CARUD","SEDES","LLSE","TRDA2","ORALA","MIOB2"),class_3="DM"),cbind(species=c("GEROT","DECE","TRPAP","ARSC","CASCS2","POBI6","CALE4","MIOB2","LLSE","FEBR"),class_3="MM"),cbind(species=c("CASCS2","CALE4","GEROT","SAPE18","DECE","ARSC"),class_3="WM"),cbind(species=c("SIPR","GEROT","TRPAP","CAPY3","DECE","MIOB2","ARSC","JUDR"),class_3="SB")))
splist<-data.frame(rbind(cbind(species=c("CARUD","TRDA2","SIACS2","SEDES","MIOB2"),class_3="FF"),cbind(species=c("KOMY","GEROT","CARUD","SEDES"),class_3="DM"),cbind(species=c("GEROT","DECE","TRPAP","ARSC","CASCS2"),class_3="MM"),cbind(species=c("CASCS2","CALE4","GEROT","SAPE18","DECE"),class_3="WM"),cbind(species=c("SIPR","GEROT","TRPAP","CAPY3","DECE"),class_3="SB")))
splist

outputmoisturedef<-matrix(nrow=dim(splist)[1],ncol=2)
outputsnow<-matrix(nrow=dim(splist)[1],ncol=2)

for(i in 1:dim(splist)[1]){
  classtmp<-splist$class_3[i]
  speciestmp<-splist$species[i]
  dattemp<-dat6a%>%
    gather(species,abund,ALGE:UNK)%>%
    mutate(class_3=as.character(class_3))%>%
    filter(species==speciestmp,class_3==classtmp)
  m1<-lme(abund~sum_moisturedeficitS,random=~1|plot,data=dattemp)#sum_moisturedeficitS+sdl_max_snwdpthS
  outputmoisturedef[i,1:2]<-summary(m1)$tTable[c(2),c(1:2)]
  #outputsnow[i,1:2]<-summary(m1)$tTable[c(3),c(1:2)]
}
outall<-cbind(splist,outputmoisturedef,outputsnow)
colnames(outall)[3:6]<-c("moisturecoef","moisturesd","snowcoef","snowsd")
#outall$snowcoef<-(-outall$snowcoef)
outall$class_3<-factor(outall$class_3,levels=c("FF","DM","MM","WM","SB"))

ggplot(outall,aes(x=class_3,y=moisturecoef,col=species,group=species)) +
  geom_point(stat="identity",position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymax=moisturecoef+moisturesd,ymin=moisturecoef-moisturesd),width=.1,position=position_dodge(width=.5)) +
  geom_hline(yintercept=0)
ggplot(outall,aes(x=class_3,y=snowcoef,col=species,group=species)) +
  geom_point(stat="identity",position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymax=snowcoef+snowsd,ymin=snowcoef-snowsd),width=.1,position=position_dodge(width=.5)) +
  geom_hline(yintercept=0)






#Graphing date of melt out for community types. I would need to look at the original data by plot.
climate2<-climate%>%
  select(year,ff_meltout:sb_meltout)%>%#ff_max_snwdpth:sb_max_snwdpth,
  gather(com,meltout,ff_meltout:sb_meltout)%>%
  separate(com,c("class_3","x"),sep="_")%>%
  filter(is.na(meltout)==F)%>%
  group_by(class_3)%>%
  summarise(meanmeltout=mean(meltout),semeltout=std.error(meltout))
climate2$class_3<-factor(climate2$class_3,levels=c("sb","wm","mm","dm","ff"))

ggplot(climate2,aes(x=class_3,y=meltout,col=class_3,group=class_3)) +
  geom_point(stat="identity",position=position_dodge(width=.5)) 

ggplot(climate2,aes(x=class_3,y=meanmeltout,col=class_3,group=class_3)) +
  geom_point(stat="identity",position=position_dodge(width=.5)) 

climate2<-climate%>%
  select(year,ff_max_snwdpth:sb_max_snwdpth)%>%#ff_max_snwdpth:sb_max_snwdpth,
  gather(com,snowdepth,ff_max_snwdpth:sb_max_snwdpth)%>%
  separate(com,c("class_3","x"),sep="_m")
climate2$class_3<-factor(climate2$class_3,levels=c("sb","wm","mm","dm","ff"))
ggplot(climate2,aes(x=class_3,y=snowdepth,col=class_3,group=class_3)) +
  geom_point(stat="identity",position=position_dodge(width=.5)) 




##### Snowdepth at the plot level data #####

snow<-read.csv("~/Dropbox/NWT_data/NWT_SaddleData_snowdepth.csv")
snow<-snow[,-1]
head(snow)

snow2<-merge(dat2,snow,by=c("plot","year"))

snow3<-snow2%>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")%>%
  filter(plot!=51,plot!=58)%>%
  gather(species,abun,ALGE:UNK)%>%
  filter(species%in%c("ARSC","CALE4","CAPY3","CARUD","CASCS2","DECE","GEROT","KOMY","SEDES","SIPR","TRPAP"))

mygenera<-data.frame(species=c("ARSC","CALE4","CAPY3","CARUD","CASCS2","DECE","GEROT","KOMY","SEDES","SIPR","TRPAP"),genus=c("Artemisia","Caltha","Carex pyrenaica","Carex rupestris","Carex scopulorum","Deschampsia","Geum","Kobresia","Selaginella","Sibbaldia","Trifolium"))

snow5<-merge(snow3,mygenera)

names(snow5)[10]<-"Species"

#Species plots

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Proposals/NSFpreproposal2017/Figs/snowdepthspeciesresponse.pdf",width=5.6,height=4)
ggplot(snow5,aes(x=max_snow,y=abun,color=Species)) +
  theme(text = element_text(size=15)) +#legend.position="none",
  labs(x ="Snow depth (cm)",y="Percent cover") +
  coord_cartesian(ylim = c(0, 40),xlim=c(0,500))+
  geom_smooth(span=.9,se=F)
  #geom_point(stat="identity",size=.1)
dev.off()

#ggplot(snow3,aes(x=max_snow,y=abun,color=species)) +
#  stat_smooth(method = 'lm', formula = y ~ poly(x,3),se=F)

#ggplot(snow3,aes(x=max_snow,y=abun,color=species)) +
  #geom_point(stat="identity",size=.1) +
#  geom_line(stat="smooth",method = "loess",size=.8)






#Snow depth by community type

#FF plots 51 and 58 have very high snowdepths, remove

snow4<-snow2%>%
  filter(class_3!="rock",class_3!="ST",class_3!="SF")%>%
  filter(plot!=51,plot!=58)%>%
  group_by(class_3,plot)%>%
  summarise(meandepth2=mean(max_snow),sedepth2=std.error(max_snow))%>%
  group_by(class_3)%>%
  summarise(meandepth=mean(meandepth2),sedepth=std.error(meandepth2))

snow4$class_3<-factor(snow4$class_3,levels=c("SB","WM","MM","DM","FF"))

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Proposals/NSFpreproposal2017/Figs/snowdepthcommunitytype.pdf",width=4,height=4)
ggplot(snow4,aes(x=class_3,y=meandepth,col=class_3)) +
  theme(legend.position="none",text = element_text(size=20)) +
  labs(x ="Community type",y="Snow depth (cm)") +
  geom_point(size=3) +
  geom_errorbar(aes(ymax=meandepth+sedepth,ymin=meandepth-sedepth),size=1,width=.5,stat="identity")
dev.off()






##### Meltout data at the plot level #####

#0 means the plot had no snow over the entire time period, NA means the plot never melted out (by the last survey date)
melt<-read.csv("/Users/farrer/Dropbox/NWT_data/NWT_SaddleData_meltout.csv")
head(melt)

melt2<-melt%>%
  filter(is.na(julian)==F)%>% #leave in 0s for now (plots that never had snow)
  filter(class_3%in%c("FF","DM","MM","WM","SB"))%>%
  group_by(class_3)%>%
  summarise(meanmelt=mean(julian),semelt=std.error(julian))
  
#taking out plot 51, it is FF but has very late meltout
melt2<-melt%>%
  filter(is.na(julian)==F)%>% #leave in 0s for now (plots that never had snow)
  filter(class_3%in%c("FF","DM","MM","WM","SB"))%>%
  filter(plot!=51,plot!=58)%>%
  group_by(class_3,plot)%>%
  summarise(meanmelt2=mean(julian),semelt2=std.error(julian))%>%
  group_by(class_3)%>%
  summarise(meanmelt=mean(meanmelt2),semelt=std.error(meanmelt2))

melt2$class_3<-factor(melt2$class_3,levels=c("SB","WM","MM","DM","FF"))

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Proposals/NSFpreproposal2017/Figs/meltoutcommunitytype.pdf",width=4,height=4)
ggplot(melt2,aes(x=class_3,y=meanmelt,col=class_3)) +
  theme(legend.position="none",text = element_text(size=20)) +
  labs(x ="Community type",y="Meltout (Julian day)") +
  geom_point(size=3) +
  geom_errorbar(aes(ymax=meanmelt+semelt,ymin=meanmelt-semelt),size=1,width=.5,stat="identity")
dev.off()







#Bill's 246 plots
bil<-read.csv("~/Dropbox/NWT_data/SppComp246.csv")
colnames(bil)[1]<-"year"
bil[which(is.na(bil),arr.ind = T)]<-0

bil2<-bil%>%
  subset(TRT==0)

bil3<-merge(bil2,tmspca)

rowSums(bil3[,4:60])

ggplot(bil3,aes(x=sumallPC1,y=GEUROS)) +
  geom_point(stat="identity") +
  geom_line(stat="smooth",method = "lm",size=.8)



#ITEX experiment



