
library(ggplot2)
library(dplyr)

#### Fit model to correct for differences in cushion plant harvest methods ####

cushion<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/SaddleNPP/saddle_grid_biomass_2015.working.csv")
head(cushion)

comtype<-read.csv("~/Dropbox/NWT_data/NWT_Saddle_ComType.csv")
comtype$class_3

cushion2<-merge(cushion,comtype,by=c("plot"))
head(cushion2)

#I tried a lot of different model formulations, but the modified logistic fit the best especially at low totalgm2 values (which we have a lot of in the dataset we have to convert). most models went above the 1:1 line at low totalmg2 values which would mean that the woody+green biomass is less than the green only biomass. Note that the modified logistic model gives negative biomass values for totalgm2 < 36. This is not a problem because our lowest value to convert is 39.7

ggplot(cushion2)+
  aes(x=totalgm2,y=alllivegm2)+ theme_classic()+
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a/(1+exp(-b*x))-c', start = list(a=107,b=.005,c=100),se=FALSE)

mymod<-nls(alllivegm2~a/(1+exp(-b*totalgm2))-c,start=list(a=107,b=.005,c=100),data=cushion2,trace=T,nls.control(maxiter=10000,tol=0.00008,minFactor=0.00001))
nullmod<-nls(alllivegm2~a/(1+exp(-0*totalgm2))-0,start=list(a=1),data=cushion2)

#check that ggplot is giving the same curve as nls(), yes.
plot(cushion2$totalgm2,cushion2$alllivegm2,pch=16,xlim=c(0,700),ylim=c(0,350),cex=.5)
curve(530.48918/(1+exp(-.01106*(x)))-316.70505,add=T,col=3) #modified logistic model
curve(327.5/(1+exp(-0*(x)))-0,add=T,col=3) #null model (mean)
abline(a=0,b=1) #the curve should be lower than the 1:1 line

#other models I tried
#curve((81.78685*log(x)-279.41985),add=T,col=2) #secound choice
#curve(14.8346*x^0.4396,add=T,col=1)
#curve(14.8346*(x-30)^0.4396,add=T,col=1) #model won't fit in nls
#curve(235.48*(1-exp(-0.00547*x)),add=T,col=5)
#curve((318*x)/(210+x),add=T,col=5)

#calculate R2 and p value: sometimes R2 is not valid in nonlinear models, but it is valid in this case because the null model is nested within the full model
RSS <- sum(residuals(mymod)^2)
TSS <- sum((cushion2$alllivegm2 - mean(cushion2$alllivegm2))^2)
1 - (RSS/TSS) #R2 = 0.522
anova(nullmod,mymod,test="F")#the F test uses mean squares to compare fit, P=0.00001






#### Apply the correction model to snowprod for dm and ff plots for years prior to 2008 ####

snowprod<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/wholesite/NWT_SnowXProd.csv",na.strings = c("."))
snowprod$plot<-as.factor(snowprod$plot)
#I'm leaving plot 51 in for now, it is an outlier in terms of snowdepth not ff community or prod  #snowprod$anpp[which(snowprod$plot=="51")]<-NA

#apply the correction to the DM/FF yr<2008 plots, then rbind everything back together again
snowprodsub1<-snowprod %>%
  filter(class_3=="DM"|class_3=="FF",year<2008) %>%
  filter(!is.na(anpp)) %>%
  rename(anppold=anpp) %>%
  mutate(anpp=530.48918/(1+exp(-.01106*(anppold)))-316.70505)
snowprodsub1$anppold<-NULL

snowprodsub2<-snowprod %>%
  filter(class_3=="DM"|class_3=="FF",year>=2008) %>%
  filter(!is.na(anpp))

snowprodsub3<-snowprod %>%
  filter(class_3=="MM"|class_3=="SB"|class_3=="WM") %>%
  filter(!is.na(anpp))

snowprodcor<-arrange(rbind(snowprodsub1,snowprodsub2,snowprodsub3),year,sort_num)


