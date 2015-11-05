
library(ggplot2)

cushion<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/SaddleNPP/saddle_grid_biomass_2015.working.csv")
head(cushion)

comtype<-read.csv("~/Dropbox/NWT_data/NWT_Saddle_ComType.csv")
comtype$class_3

cushion2<-merge(cushion,comtype,by=c("plot"))
head(cushion2)

ggplot(cushion2)+
  aes(x=totalgm2,y=alllivegm2)+ theme_classic()+
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a*x^b', start = list(a=1,b=1),se=FALSE)
#+ geom_smooth() #+ facet_wrap(~class_3)

#fit a model that forces through 0,0 and is monotonic increasing (but doesn't asymtote). rationale is that cushion volume (woody) will increase to the third power and cusion area (live, green) will increase to the squared power.
mymod<-nls(alllivegm2~a*totalgm2^b,start=list(a=7,b=1),data=cushion2)
nullmod<-nls(alllivegm2~a*totalgm2^0,start=list(a=1),data=cushion2)

#check that ggplot is giving the same curve as nls(), yes.
plot(cushion2$totalgm2,cushion2$alllivegm2,col=cushion2$class_3,pch=16,xlim=c(50,700),ylim=c(0,350),cex=.5)
curve(14.8346*x^0.4396,add=T,col=1)

#calculate R2: sometimes R2 is not valid in nonlinear models, but it is valid in this case because the null model is nested within the full model
RSS <- sum(residuals(mymod)^2)
TSS <- sum((cushion2$alllivegm2 - mean(cushion2$alllivegm2))^2)
1 - (RSS/TSS) #R2 = 0.426

anova(nullmod,mymod,test="F")#the F test uses mean squares to compare fit, P=0.00003



#








abline(lm(alllivegm2~totalgm2,data=cushion2dm),col=1)
abline(lm(alllivegm2~totalgm2,data=cushion2ff),col=2)
legend(600,350,c("DM","FF","MM"),pch=16,col=c(1,2,3),box.col ="white")


cushion2dm<-subset(cushion2,class_3=="DM")
cushion2ff<-subset(cushion2,class_3=="FF")

plot(cushion2dm$totalgm2,cushion2dm$alllivegm2,col=1)
plot(cushion2ff$totalgm2,cushion2ff$alllivegm2,col=2)

summary(lm(alllivegm2~totalgm2,data=cushion2))
summary(lm(alllivegm2~totalgm2,data=cushion2dm))
summary(lm(alllivegm2~totalgm2,data=cushion2ff))
summary(nls(alllivegm2~a*totalgm2^b,data=cushion2ff))
curve(15.0732*x^.4175,add=T)

dmcushionintercept<-lm(alllivegm2~totalgm2,data=cushion2dm)$coefficients[1]
dmcushionslope<-lm(alllivegm2~totalgm2,data=cushion2dm)$coefficients[2]
ffcushionintercept<-lm(alllivegm2~totalgm2,data=cushion2ff)$coefficients[1]
ffcushionslope<-lm(alllivegm2~totalgm2,data=cushion2ff)$coefficients[2]

dmcushionintercept+dmcushionslope*260.01#looks ok


#start with snowprod and do a correction for dm and ff plots for years prior to 2008
snowprodcor<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/wholesite/NWT_SnowXProd.csv",na.strings = c("."))
snowprodcor$plot<-as.factor(snowprodcor$plot)
#take out plot 51 outlier, I'm leavng this in for now, it is an outlier in terms of snowdepth not ff community or prod
#snowprodcor$anpp[which(snowprodcor$plot=="51")]<-NA
snowprodcor$anppcor<-snowprodcor$anpp
ind<-which(snowprodcor$class_3=="DM"&snowprodcor$year<2008)
snowprodcor$anppcor[ind]<-dmcushionintercept+dmcushionslope*snowprodcor$anpp[ind]
ind<-which(snowprodcor$class_3=="FF"&snowprodcor$year<2008)
snowprodcor$anppcor[ind]<-ffcushionintercept+ffcushionslope*snowprodcor$anpp[ind]
snowprodcor[ind,]

prodcormeans<-aggregate.data.frame(snowprodcor$anppcor,by=list(year=snowprodcor$year,class_3=snowprodcor$class_3),function(x){mean(x,na.rm=T)})
prodcormeansshort<-cast(prodcormeans,year~class_3,value="x",fun=mean)
prodcormeansshort$rock<-NULL
prodcormeansshort$ST<-NULL
prodcormeansshort$SF<-NULL
colnames(prodcormeansshort)<-c("water_year","DMprod","FFprod","MMprod","SBprod","WMprod")
prodcormeanall<-aggregate.data.frame(snowprodcor$anppcor,by=list(year=snowprodcor$year),function(x){mean(x,na.rm=T)})

prodcormeansshort2<-cbind(prodcormeansshort,prodtot=prodcormeanall$x)
