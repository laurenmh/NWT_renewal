##Megan: Run the code fully through for the gridComposition_datacleaning.R file
#Then the following should give the means and variances at different levels of aggregation
library(tidyr)
library(dplyr)

#AT THE LEVEL OF THE FUNCTIONAL GROUP
#Returns a dataframe with the functional group, the habitat type (class_3), 
#the temporal mean of the summed functional group cover (functempmean),
#the temporal variance of the summed functional group cover (functempvar)
funclevel<-sppdat2 %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
  mutate(class_3=as.character(class_3), class_3=ifelse(class_3=="WM", "MM", class_3)) %>%
  group_by(year, func, class_3) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  group_by(func, class_3) %>%
  summarize(functempmean=mean(abund), functempvar=var(abund)) %>%
  tbl_df()

#AT THE LEVEL OF THE HABITAT GROUP
#Returns a dataframe with the the habitat type (class_3), 
#the temporal mean of the summed cover (habtempmean),
#the temporal variance of the summed (habtempvar)
habitatlevel<-sppdat2 %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
  mutate(class_3=as.character(class_3), class_3=ifelse(class_3=="WM", "MM", class_3)) %>%
  group_by(year, class_3) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  group_by(class_3) %>%
  summarize(habtempmean=mean(abund), habtempvar=var(abund))
  

#AT THE LEVEL OF THE RIDGE 
#R
#the temporal mean of the summed cover across the ridge (ridgetempmean),
#the temporal variance of the summed cover across the ridge (ridgetempvar)
ridgelevel <-sppdat %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
  group_by(year) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  summarize(ridgetempmean=mean(abund), ridgetempvar=var(abund))

## Double-check 
sum(funclevel$functempmean)==ridgelevel$ridgetempmean
sum(habitatlevel$habtempmean)==ridgelevel$ridgetempmean

########## Portfolio effects
ObsMean<-log(ridgelevel$ridgetempmean); ObsMean
ObsVar<-log(ridgelevel$ridgetempvar); ObsVar

### Within each habitat
funclevel$logMean<-log(funclevel$functempmean)
funclevel$logVar<-log(funclevel$functempvar)
# FF
FF<-funclevel[funclevel$class_3== "FF",]; FF
m1<-lm(logVar~logMean,data=FF); summary(m1)
plot(FF$logMean,FF$logVar,xlim=c(0,ObsMean+1),ylim=c(0,ObsVar+1)); abline(m1)
points(ObsMean,ObsVar,pch=16,col="black")
BufFF<-predict(m1,data.frame(logMean=ObsMean))/ObsVar; BufFF
expBufFF<-exp(predict(m1,data.frame(logMean=ObsMean)))/exp(ObsVar); expBufFF
# DM
DM<-funclevel[funclevel$class_3== "DM",]; DM
m2<-lm(logVar~logMean,data=DM); summary(m2)
plot(DM$logMean,DM$logVar,xlim=c(0,ObsMean+1),ylim=c(0,ObsVar+1)); abline(m2)
points(ObsMean,ObsVar,pch=16,col="black")
BufDM<-predict(m2,data.frame(logMean=ObsMean))/ObsVar; BufDM
expBufDM<-exp(predict(m2,data.frame(logMean=ObsMean)))/exp(ObsVar); expBufDM
# MM
MM<-funclevel[funclevel$class_3== "MM",]; MM
m3<-lm(logVar~logMean,data=MM); summary(m3)
plot(MM$logMean,MM$logVar,xlim=c(0,ObsMean+1),ylim=c(0,ObsVar+1)); abline(m3)
points(ObsMean,ObsVar,pch=16,col="black")
BufMM<-predict(m3,data.frame(logMean=ObsMean))/ObsVar; BufMM
expBufMM<-exp(predict(m3,data.frame(logMean=ObsMean)))/exp(ObsVar); expBufMM
# SB
SB<-funclevel[funclevel$class_3== "SB",]; SB
m4<-lm(logVar~logMean,data=SB); summary(m4)
plot(SB$logMean,SB$logVar,xlim=c(0,ObsMean+1),ylim=c(0,ObsVar+1)); abline(m4)
points(ObsMean,ObsVar,pch=16,col="black")
BufSB<-predict(m4,data.frame(logMean=ObsMean))/ObsVar; BufSB
expBufSB<-exp(predict(m4,data.frame(logMean=ObsMean)))/exp(ObsVar); expBufSB

### Among habitats
habitatlevel$logMean<-log(habitatlevel$habtempmean)
habitatlevel$logVar<-log(habitatlevel$habtempvar)
m5<-lm(logVar~logMean,data=habitatlevel); summary(m5)
plot(habitatlevel$logMean,habitatlevel$logVar,xlim=c(0,ObsMean+1),ylim=c(0,ObsVar+1)); abline(m5)
points(ObsMean,ObsVar,pch=16,col="black")
BufHAB<-predict(m5,data.frame(logMean=ObsMean))/ObsVar; BufHAB
expBufHAB<-exp(predict(m5,data.frame(logMean=ObsMean)))/exp(ObsVar); expBufHAB

### Mean-SD figure for renewal
SD<-sqrt(habitatlevel$habtempvar)
MeanVec<-seq(0,10000)
pdf("SD-Mean curve for plant cover across the landscape.pdf")
plot(habitatlevel$habtempmean,SD,xlim=c(0,10000),ylim=c(0,1600),
     ylab="SD plant cover",xlab="Mean plant cover")
points(ridgelevel$ridgetempmean,sqrt(ridgelevel$ridgetempvar),pch=16,col="black")
points(ridgelevel$ridgetempmean,sqrt(exp(predict(m5,data.frame(logMean=ObsMean)))))
lines(MeanVec,sqrt(exp(predict(m5,data.frame(logMean=log(MeanVec))))))
exp(ObsVar)/exp(predict(m5,data.frame(logMean=ObsMean)))
text(x=8000,y=800,"Observed variance = ")
text(x=8000,y=700,"42.87% of ")
text(x=8000,y=600,"predicted variance")
dev.off()

#### Lauren, it looks like there is some weak buffering among habitats
#### but that synchrony among functional groups actually increases temporal variance
#### Does this seem consistent with your corrgrams?
#### Are there any other contrasts you'd like to try? 