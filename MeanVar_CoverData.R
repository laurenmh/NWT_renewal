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
ObsMeanFF<-habitatlevel$logMean[habitatlevel$class_3=="FF"]
ObsVarFF<-habitatlevel$logVar[habitatlevel$class_3=="FF"]
plot(FF$logMean,FF$logVar,xlim=c(0,ObsMeanFF+1),ylim=c(0,ObsVarFF+1)); abline(m1)
points(ObsMeanFF,ObsVarFF,pch=16,col="black")
PredVarFF<-predict(m1,data.frame(logMean=ObsMeanFF))
BufFF<-PredVarFF/ObsVarFF; BufFF
expBufFF<-exp(PredVarFF)/exp(ObsVarFF); expBufFF
# DM
DM<-funclevel[funclevel$class_3== "DM",]; DM
m2<-lm(logVar~logMean,data=DM); summary(m2)
ObsMeanDM<-habitatlevel$logMean[habitatlevel$class_3=="DM"]
ObsVarDM<-habitatlevel$logVar[habitatlevel$class_3=="DM"]
plot(DM$logMean,DM$logVar,xlim=c(0,ObsMeanDM+1),ylim=c(0,ObsVarDM+1)); abline(m2)
points(ObsMeanDM,ObsVarDM,pch=16,col="black")
PredVarDM<-predict(m2,data.frame(logMean=ObsMeanDM))
BufDM<-PredVarDM/ObsVarDM; BufDM
expBufDM<-exp(PredVarDM)/exp(ObsVarDM); expBufDM
# MM
MM<-funclevel[funclevel$class_3== "MM",]; MM
m3<-lm(logVar~logMean,data=MM); summary(m3)
ObsMeanMM<-habitatlevel$logMean[habitatlevel$class_3=="MM"]
ObsVarMM<-habitatlevel$logVar[habitatlevel$class_3=="MM"]
plot(MM$logMean,MM$logVar,xlim=c(0,ObsMeanMM+1),ylim=c(0,ObsVarMM+1)); abline(m3)
points(ObsMeanMM,ObsVarMM,pch=16,col="black")
PredVarMM<-predict(m3,data.frame(logMean=ObsMeanMM))
BufMM<-PredVarMM/ObsVarMM; BufMM
expBufMM<-exp(PredVarMM)/exp(ObsVarMM); expBufMM
# SB
SB<-funclevel[funclevel$class_3== "SB",]; SB
m4<-lm(logVar~logMean,data=SB); summary(m4)
ObsMeanSB<-habitatlevel$logMean[habitatlevel$class_3=="SB"]
ObsVarSB<-habitatlevel$logVar[habitatlevel$class_3=="SB"]
plot(SB$logMean,SB$logVar,xlim=c(0,ObsMeanSB+1),ylim=c(0,ObsVarSB+1)); abline(m4)
points(ObsMeanSB,ObsVarSB,pch=16,col="black")
PredVarSB<-predict(m4,data.frame(logMean=ObsMeanSB))
BufSB<-PredVarSB/ObsVarSB; BufSB
expBufSB<-exp(PredVarSB)/exp(ObsVarSB); expBufSB

### Among habitats
habitatlevel$logMean<-log(habitatlevel$habtempmean)
habitatlevel$logVar<-log(habitatlevel$habtempvar)
m5<-lm(logVar~logMean,data=habitatlevel); summary(m5)
plot(habitatlevel$logMean,habitatlevel$logVar,xlim=c(0,ObsMean+1),ylim=c(0,ObsVar+1)); abline(m5)
points(ObsMean,ObsVar,pch=16,col="black")
PredVar<-predict(m5,data.frame(logMean=ObsMean))
BufHAB<-PredVar/ObsVar; BufHAB
expBufHAB<-exp(PredVar)/exp(ObsVar); expBufHAB

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