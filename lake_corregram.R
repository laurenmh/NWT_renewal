
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(corrgram)
library(corrplot)

##LAKE CORREGRAMS WITH ERIC'S CLEANED DATA
##USES THE TIMEPOINT WITH MAX BIOVOLUME, AND AT A DEPTH OF 9 M
datpath = "~/Dropbox/NWT_data/" # this likely will be different for different folks

lakedat <- read.csv(file.path(datpath, 'DATA_GL4_phytos_2008-2014_9m_long.csv')) %>%
  tbl_df() %>%
  mutate(abund=mean_biovolume_um3_ml) 
  


lakedat2 <-lakedat %>%
  select(lowest.taxon, mean_biovolume_um3_ml, year) %>%
  spread(lowest.taxon, mean_biovolume_um3_ml, fill=0)
#Create a correlation matrix from the data
cormat<-cor(lakedat2[,2:ncol(lakedat2)])

##VISUALIZE THE CORREGRAM
corrplot(cormat,method="circle",type="upper", tl.col="black", tl.cex = .8, diag=F)




phylumlevel<-lakedat %>%
  group_by(year,  lowest.taxon) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  group_by(lowest.taxon) %>%
  summarize(phylumtempmean=mean(abund), phylumtempvar=var(abund))



alllevel<-lakedat %>%
  group_by(year) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  summarize(alltempmean=mean(abund), alltempvar=var(abund))



########## Portfolio effects
ObsMean<-log(alllevel$alltempmean); ObsMean
ObsVar<-log(alllevel$alltempvar); ObsVar


### Within each Phylum
phylumlevel$logMean<-log(phylumlevel$phylumtempmean)
phylumlevel$logVar<-log(phylumlevel$phylumtempvar)
phylumlevel$ObsMean<-ObsMean
phylumlevel$ObsVar <-ObsVar
m5<-lm(logVar~logMean,data=phylumlevel); summary(m5)
ggplot(phylumlevel, aes(x=logMean, y=logVar)) + geom_point(pch=1) + xlim(0, ObsMean+1)+ ylim(0, ObsVar+1) +
  geom_smooth(method="lm", se=F, fullrange = T) +
  geom_point(aes(x=ObsMean, y=ObsVar)) + theme_classic()

# plot(phylumlevel$logMean,phylumlevel$logVar,xlim=c(0,ObsMean+1),ylim=c(0,ObsVar+1)); abline(m5)
# points(ObsMean,ObsVar,pch=16,col="black")
PredVar<-predict(m5,data.frame(logMean=ObsMean))
BufPHY<-PredVar/ObsVar; BufPHY
expBufPHY<-exp(PredVar)/exp(ObsVar); expBufPHY
##Observed variance is 22% of expected variance
exp(ObsVar)/exp(predict(m5,data.frame(logMean=ObsMean)))

# pdf("lakecor_withcleandat.pdf")
# dev.off()


##Hi Eric - I'm taking a stab at the lake data to create corrgrams of synchrony and measure the portfolio effect
##The data are super messy - I've done a little tidying but I don't know enough about the taxa to be thorough
##See notes below - want to add in the cleaning? Then re-run the metrics.
##Thanks! Lauren

datpath = "~/Dropbox/NWT_data/" # this likely will be different for different folks
head(pcouts2)

#read in the data
lakedat <- read.csv(file.path(datpath, 'PhytoData_2008-2014_raw_data.csv')) %>%
  tbl_df() %>%
  #make a simpler abund column name
  mutate(abund=Corrected.Abundance..particles.mL.) %>%
  #some initial data cleaning of typos in Genus and Phylum
  mutate(Genus=as.character(Genus),
         Genus=ifelse(Genus=="unknown/ various", "unknown/various", Genus),
         Genus=ifelse(Genus=="Fragilaria sp. sp.", "Fragilaria sp.", Genus)) %>%
  mutate(Phylum=as.character(Phylum),
         Phylum=ifelse(Phylum=="unknwon", "unknown", Phylum),
         Phylum=ifelse(Phylum=="Bac", "Bacilariophyta", Phylum ),
         Phylum=ifelse(Phylum=="Filmantous Cyanobacteria", "Filamentous Cyanobacteria", Phylum),
         Phylum=ifelse(Phylum=="Chlorophta", "Chlorophyta", Phylum),
         Phylum=ifelse(Phylum=="unknown Bacilariophyte", "Bacilariophyta", Phylum)) %>%
  mutate(uniqueSpp=paste(Phylum, Genus, sep="_")) %>%
  mutate(year=Year) %>%
  mutate(present=ifelse(abund>0, 1, 0)) %>%
  group_by(uniqueSpp) %>%
  mutate(nopresent=sum(present))

#remove the infrequent phylum and the LGRT
ld2 <-lakedat %>%
  filter(nopresent>30) %>%
  filter(Phylum!="LGRT")

#spread the data for the corregram
#not sure what to do with the different depths - for  now removing outlet and inlet
#the positive correlations are most strong at 0 depth
ld3<-ld2 %>%
  mutate(ID=paste(year, Date, Lake, Depth, sep="_")) %>%
  #there are multiple rows that have the same ID - why? 
  #either an entry error or I miss grouped some Phylum
  #for now I'm adding them together
  group_by(ID, Phylum, Depth) %>%
  summarize(abund=sum(abund)) %>%
  select(ID, Phylum, abund, Depth) %>%
  filter(abund>0) %>%
  spread(Phylum, abund, fill=0) %>%
  #not very many reps of inlet or outlet, deleting
  filter(Depth!="OUTLET", Depth!="INLET") %>%
  select(-Depth)

#Do the same data spreadiong, but averaging within a year before running 
#I have no idea which is preferable - is there even sampling across the time period?
#Is it okay to do this?
ld4<-ld2 %>%
  #not very many reps of inlet or outlet, deleting
  filter(Depth!="OUTLET", Depth!="INLET") %>%
  #there are multiple rows that have the same ID - why? 
  #either an entry error or I miss grouped some Phylum
  #for now I'm adding them together
  group_by(year, Phylum) %>%
  summarize(abund=sum(abund)) %>%
  select(Phylum, abund) %>%
  filter(abund>0) %>%
  spread(Phylum, abund, fill=0)
#Create a correlation matrix from the data
cormat<-cor(ld4[,2:ncol(ld4)])

##VISUALIZE THE CORREGRAM
corrplot(cormat,method="circle",type="upper", tl.col="black", tl.cex = .8, diag=F)

##CALCULATE THE MEAN VARIANCE

phylumlevel<-ld2 %>%
  filter(Depth!="OUTLET", Depth!="INLET") %>%
  group_by(year,  Phylum) %>%
mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  group_by(Phylum) %>%
  summarize(phylumtempmean=mean(abund), phylumtempvar=var(abund))



alllevel<-ld2 %>%
  filter(Depth!="OUTLET", Depth!="INLET") %>%
  group_by(year) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  summarize(alltempmean=mean(abund), alltempvar=var(abund))



########## Portfolio effects
ObsMean<-log(alllevel$alltempmean); ObsMean
ObsVar<-log(alllevel$alltempvar); ObsVar


### Within each Phylum
phylumlevel$logMean<-log(phylumlevel$phylumtempmean)
phylumlevel$logVar<-log(phylumlevel$phylumtempvar)
phylumlevel$ObsMean<-ObsMean
phylumlevel$ObsVar <-ObsVar
m5<-lm(logVar~logMean,data=phylumlevel); summary(m5)
ggplot(phylumlevel, aes(x=logMean, y=logVar)) + geom_point(pch=1) + xlim(0, ObsMean+1)+ ylim(0, ObsVar+1) +
  geom_smooth(method="lm", se=F, fullrange = T) +
  geom_point(aes(x=ObsMean, y=ObsVar)) + theme_classic()

# plot(phylumlevel$logMean,phylumlevel$logVar,xlim=c(0,ObsMean+1),ylim=c(0,ObsVar+1)); abline(m5)
# points(ObsMean,ObsVar,pch=16,col="black")
PredVar<-predict(m5,data.frame(logMean=ObsMean))
BufPHY<-PredVar/ObsVar; BufPHY
expBufPHY<-exp(PredVar)/exp(ObsVar); expBufPHY
##Observed variance is 53% of expected variance
exp(ObsVar)/exp(predict(m5,data.frame(logMean=ObsMean)))
