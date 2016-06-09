#This code corrects the productivity data for differences in cushion plant harvesting methods - output clean file is "NWT_SnowXProdCorrected.csv"

library(ggplot2)
library(dplyr)



#### Fit model to correct for differences in cushion plant harvest methods ####

cushion<-read.csv("~/Dropbox/NWT_data/saddle_grid_biomass_2015.working.csv")
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
pdf("/Users/farrer/Desktop/plot1.pdf")
plot(cushion2$totalgm2,cushion2$alllivegm2,pch=16,xlim=c(0,700),ylim=c(0,350),cex=.5,xlab="total gm2",ylab="all live gm2")
curve(530.48918/(1+exp(-.01106*(x)))-316.70505,add=T,col=2) #modified logistic model
text(600,300,"R2 = 0.522")
dev.off()
#curve(327.5/(1+exp(-0*(x)))-0,add=T,col=3) #null model (mean)
#abline(a=0,b=1) #the curve should be lower than the 1:1 line

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

snowprod<-read.csv("~/Dropbox/NWT_data/NWT_SnowXProd.csv",na.strings = c("."))
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
write.csv(snowprodcor,"~/Dropbox/NWT_data/NWT_SnowXProdCorrected.csv",row.names=F)






###### Trying a few more relationships and subset for Hope ######

#Taking out the "outlier"

cushion3<-cushion2 %>%
  filter(alllivegm2<300)
  
ggplot(cushion3)+
  aes(x=totalgm2,y=alllivegm2)+ theme_classic()+
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a/(1+exp(-b*x))-c', start = list(a=107,b=.005,c=100),se=FALSE)

mymod<-nls(alllivegm2~a/(1+exp(-b*totalgm2))-c,start=list(a=107,b=.005,c=100),data=cushion3,trace=T,nls.control(maxiter=10000,tol=0.00008,minFactor=0.00001))
nullmod<-nls(alllivegm2~a/(1+exp(-0*totalgm2))-0,start=list(a=1),data=cushion3)

#check that ggplot is giving the same curve as nls(), yes.
pdf("/Users/farrer/Desktop/plot2.pdf")
plot(cushion3$totalgm2,cushion3$alllivegm2,pch=16,xlim=c(0,700),ylim=c(0,350),cex=.5,xlab="total gm2",ylab="all live gm2")
curve(576.57372/(1+exp(-0.01384*(x)))-380.53050,add=T,col=3) #modified logistic model
curve(530.48918/(1+exp(-.01106*(x)))-316.70505,add=T,col=2) #with outlier 
legend("topleft",c("no outlier","all data"),col=c(3,2),lty=1,bty="n")
text(600,300,"R2 = 0.584")
dev.off()

#calculate R2 and p value: sometimes R2 is not valid in nonlinear models, but it is valid in this case because the null model is nested within the full model
RSS <- sum(residuals(mymod)^2)
TSS <- sum((cushion3$alllivegm2 - mean(cushion3$alllivegm2))^2)
1 - (RSS/TSS) #R2 = 0.584
anova(nullmod,mymod,test="F")#the F test uses mean squares to compare fit

#trying another model form, not very good
mymod<-nls(alllivegm2~(a*log(totalgm2)-b),start=list(a=81,b=279),data=cushion3,trace=T,nls.control(maxiter=10000,tol=0.00008,minFactor=0.00001))
curve((72.09*log(x)-231.20),add=T,col=2)




#fellfield only
cushion2FF<-subset(cushion2,X2008_class=="FF")
head(cushion2FF)

ggplot(cushion2FF)+
  aes(x=totalgm2,y=alllivegm2)+ theme_classic()+
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a/(1+exp(-b*x))-c', start = list(a=107,b=.005,c=100),se=FALSE)

mymod<-nls(alllivegm2~a/(1+exp(-b*totalgm2))-c,start=list(a=107,b=.005,c=100),data=cushion2FF,trace=T,nls.control(maxiter=10000,tol=0.00008,minFactor=0.00001))
nullmod<-nls(alllivegm2~a/(1+exp(-0*totalgm2))-0,start=list(a=1),data=cushion2FF)

#check that ggplot is giving the same curve as nls(), yes.
pdf("/Users/farrer/Desktop/plot3.pdf")
plot(cushion2FF$totalgm2,cushion2FF$alllivegm2,pch=16,xlim=c(0,700),ylim=c(0,350),cex=.5,xlab="total gm2",ylab="all live gm2")
curve(862.47689/(1+exp(-0.01642*(x)))-673.05439,add=T,col=3) #modified logistic model
curve(530.48918/(1+exp(-.01106*(x)))-316.70505,add=T,col=2) #model from all data
legend("topleft",c("FF only","all data"),col=c(3,2),lty=1,bty="n")
text(600,300,"R2=0.621")
dev.off()

RSS <- sum(residuals(mymod)^2)
TSS <- sum((cushion2FF$alllivegm2 - mean(cushion2FF$alllivegm2))^2)
1 - (RSS/TSS) #R2 = 0.621
anova(nullmod,mymod,test="F")#the F test uses mean squares to compare fit


#dm only
cushion2DM<-subset(cushion2,X2008_class=="DM")
cushion3DM<-subset(cushion3,X2008_class=="DM")#without the "outlier"
head(cushion2DM)

ggplot(cushion3DM)+
  aes(x=totalgm2,y=alllivegm2)+ theme_classic()+
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a/(1+exp(-b*x))-c', start = list(a=107,b=.005,c=100),se=FALSE)

mymod<-nls(alllivegm2~a/(1+exp(-b*totalgm2))-c,start=list(a=107,b=.005,c=100),data=cushion3DM,trace=T,nls.control(maxiter=10000,tol=0.00008,minFactor=0.00001))
nullmod<-nls(alllivegm2~a/(1+exp(-0*totalgm2))-0,start=list(a=1),data=cushion3DM)

#check that ggplot is giving the same curve as nls(), yes.
pdf("/Users/farrer/Desktop/plot4.pdf")
plot(cushion3DM$totalgm2,cushion3DM$alllivegm2,pch=16,xlim=c(0,700),ylim=c(0,350),cex=.5,xlab="total gm2",ylab="all live gm2")
curve(505.2036/(1+exp(-0.006087422*(x)))-228.0156,add=T,col=3) #modified logistic model
curve(530.48918/(1+exp(-.01106*(x)))-316.70505,add=T,col=2) #model from all data
legend("topleft",c("DM only","all data"),col=c(3,2),lty=1,bty="n")
text(600,300,"R2=0.640")
dev.off()

RSS <- sum(residuals(mymod)^2)
TSS <- sum((cushion3DM$alllivegm2 - mean(cushion3DM$alllivegm2))^2)
1 - (RSS/TSS) #R2 = 0.64
anova(nullmod,mymod,test="F")#the F test uses mean squares to compare fit




#Taking out the plot with high total gm2 (>600)

cushion4<-cushion3 %>%
  filter(totalgm2<600)

ggplot(cushion4)+
  aes(x=totalgm2,y=alllivegm2)+ theme_classic()+
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a/(1+exp(-b*x))-c', start = list(a=107,b=.005,c=100),se=FALSE)

mymod<-nls(alllivegm2~a/(1+exp(-b*totalgm2))-c,start=list(a=500,b=.006,c=228),data=cushion4,trace=T,nls.control(maxiter=100000,tol=0.00008,minFactor=0.00000001))
nullmod<-nls(alllivegm2~a/(1+exp(-0*totalgm2))-0,start=list(a=1),data=cushion4)

#check that ggplot is giving the same curve as nls(), yes.
pdf("/Users/farrer/Desktop/plot2.pdf")
plot(cushion4$totalgm2,cushion4$alllivegm2,pch=16,xlim=c(0,700),ylim=c(0,350),cex=.5,xlab="total gm2",ylab="all live gm2")
curve(573.16922/(1+exp(-0.01373*(x)))-376.66783,add=T,col=4) #modified logistic model
curve(576.57372/(1+exp(-0.01384*(x)))-380.53050,add=T,col=3) #with high plot
legend("topleft",c("no high plot","all data (no outlier)"),col=c(4,3),lty=1,bty="n")
text(600,300,"R2 = 0.578")
dev.off()

#calculate R2 and p value: sometimes R2 is not valid in nonlinear models, but it is valid in this case because the null model is nested within the full model
RSS <- sum(residuals(mymod)^2)
TSS <- sum((cushion4$alllivegm2 - mean(cushion4$alllivegm2))^2)
1 - (RSS/TSS) #R2 = 0.578
anova(nullmod,mymod,test="F")#the F test uses mean squares to compare fit

