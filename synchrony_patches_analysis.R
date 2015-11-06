##Apply Kevin's patches framework to the NWT habitat types 
##Preliminary look at:
##1) How species synchrony and variance affect alpha variation within habitat type
##2) How patch synchrony and variance affect stability of each habitat type
##3) As of now, each habitat type is considered a "replicate" in the final model

##Uses sppdat from the point-count cleaning dataset

#remove unwanted habitat types
sppdat <- sppdat %>%
  filter(class_3 != "SF", class_3 != "ST", class_3!="rock")

##Species synchrony
sppsynch <- synchrony(sppdat, species.var="USDA_code", abundance.var = "abund", replicate.var = "plot")
names(sppsynch)[2]="sppsynch"
avgsppsynch<-merge(vegtype.key, sppsynch) %>%
  group_by(class_3) %>%
  summarize(avgsppsynch=mean(sppsynch))

##Species variance
##I *think* this should be switched to CV
sppvar <- sppdat %>%
  group_by(plot, USDA_code, class_3) %>%
  summarize(indvar=var(abund)) %>%
  tbl_df() %>%
  group_by(plot, class_3) %>%
  summarize(avgsppvar=mean(indvar))

avgsppvar<-sppvar %>%
  tbl_df() %>%
  group_by(class_3) %>%
  summarize(avsppvar=mean(avgsppvar))
    
##Plot synchrony
plotdat <-sppdat %>%
  filter(!is.na(USDA_code)) %>%
  group_by(plot, year, class_3) %>%
  summarize(totabund=sum(abund)) %>%
  arrange(class_3)

plotsynch<-synchrony(plotdat, species.var = "plot", abundance.var = "totabund", replicate.var="class_3")
names(plotsynch)[2]="spatialsynch"

##Plot variance
##I *think* this should be switched to CV
plotvar <- plotdat %>%
  group_by(plot, class_3) %>%
  summarize(plotvar=var(totabund)) 

avgplotvar<-plotvar %>%
  group_by(class_3) %>%
  summarize(avgplotvar=mean(plotvar))

##Habitat synchrony
habitatdat <-plotdat %>%
  group_by(year, class_3) %>%
  summarize(meanabund=mean(totabund))

habitatsynch<-synchrony(habitatdat, species.var = "class_3", abundance.var = "meanabund")


##Habitat variance
##I *think* this should be switched to CV
habitatvar <- habitatdat %>%
  group_by(class_3, year) %>%
  summarize(habitatabund=mean(meanabund)) %>%
  tbl_df() %>%
  group_by(class_3) %>%
  summarize(habitatvar=var(habitatabund))

##Put the all together
tog0<-merge(avgplotvar, plotsynch)
tog1<-merge(tog0, avgsppvar)
tog2<-merge(tog1, avgsppsynch)
tog3<-merge(tog2, habitatvar)

##Log transform
tog4 <-tog3 %>%
  mutate(logavg.sp.sync=log(avgsppsynch)) %>%
  mutate(logavg.sp.var=log(avsppvar)) %>%
  mutate(logalpha.var=log(avgplotvar)) %>%
  mutate(logpatch.sync=log(spatialsynch)) %>%
  mutate(loggamma.var=log(habitatvar))
  
##Put it together in an SEM framework
##Did this in a haste - check the model
library(lavaan)

model<-'
loggamma.var ~ logpatch.sync + logalpha.var
logalpha.var ~ logavg.sp.var + logavg.sp.sync 
'
fit <- sem(model,std.ov=T,missing="ml", data=tog4)#this uses robust chi square, which I've read is good and conservative 
summary(fit, fit.measures=TRUE,rsquare=T) 




