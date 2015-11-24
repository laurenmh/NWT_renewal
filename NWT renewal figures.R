rm(list=ls(all=TRUE))
.pardefault <- par()

df<-read.csv("Master-01to15-subset.csv")[,-1]
head(df); sapply(df,class)
df$year<-df$time
df$time<-as.factor(df$time)
df$Plt<-as.factor(df$Plt)
df$TransNest<-as.factor(substr(df$Trans,2,2))

require(corrplot)
require(Hmisc)
require(raster)

# create growth vector on absolute scale
df$Gro<-df$AreaT1-df$AreaT0

########### Create correlation plots for annual growth rate of Silene

### Among size classes of Silene within population SN1
# Assign individuals to size classes
MeanSize<-aggregate(df$AreaT1,list(df$PltID),mean,na.rm=TRUE)
hist(MeanSize$x,breaks=50)
MeanGrowth<-aggregate(df$AreaGro,list(df$PltID),mean,na.rm=TRUE)
plot(MeanSize$x,MeanGrowth$x)
quantile(MeanSize$x)
# assign sign classes 0-15, 15-30, 30-50, 50-100, >100
SizeClass<-cut(MeanSize$x,breaks=c(0,15,30,50,100,200,1000),labels=FALSE)
boxplot(MeanSize$x~SizeClass)
df$SizeClass<-0
for (i in 1:length(levels(df$PltID))){  
  df$SizeClass[df$PltID==levels(df$PltID)[i]]<-SizeClass[i]
}
df$SizeClass<-as.factor(df$SizeClass)

# Create correlation matrix for population SN1
df2<-subset(df,df$Pop=="SN1")
df2<-droplevels(df2)
df2$SzYear<-interaction(df2$SizeClass,df2$time)
SzYearMeans<-aggregate(df2$Gro,list(df2$SzYear),sum,na.rm=TRUE)$x
SzCorMat<-cor(matrix(SzYearMeans,nrow=length(levels(df2$time)),ncol=length(levels(df2$SizeClass)),byrow=TRUE))
colnames(SzCorMat)<-c("0-15","15-30","30-50","50-100",">100")
rownames(SzCorMat)<-c("0-15","15-30","30-50","50-100",">100")
corrplot(SzCorMat,method="circle",type="upper",tl.srt=45,tl.col="black",
         diag=FALSE,addCoef.col="gray20",mar=c(0,0,4,0),
         main=expression("Size classes within a"~italic(Silene)~"population"))

### Among populations of Silene 
df$PopYear<-interaction(df$Pop,df$time)
PYmeans<-aggregate(df$Gro,by=list(df$PopYear),sum,na.rm=TRUE)$x
Mpop<-cor(matrix(PYmeans,nrow=11,ncol=4,byrow=TRUE))
colnames(Mpop)<-c("P1","P2","P3","P4")
rownames(Mpop)<-c("P1","P2","P3","P4")
corrplot(Mpop,method="circle",type="upper",tl.srt=45,tl.col="black",
         diag=FALSE,addCoef.col="gray20",mar=c(0,0,4,0),
         main=expression("Among"~italic(Silene)~"populations"))

# reset graphical parameters
par(.pardefault)

################### Create barplot of portfolio effects
######## Quantify portfolio effects
### I think we should use the mean-variance relationship for now (i.e. renewal figure),
### so I'm commenting out the unneccesary code

# # function to estimate expected variance given perfect correlations
# # takes a vector of the variances of each variable 
# # and a vector of the sensitivities of each variable
# Vperf<-function(varX,sensX){
#   sdX<-sqrt(varX)
#   sum(varX*I(sensX^2)) + sum(combn(sensX,m=2,FUN=prod)*combn(sdX,m=2,FUN=prod))
# }
# 
# # function to estimate expected variance given zero correlations
# # takes a vector of the variances of each variable 
# # and a vector of the sensitivities of each variable
# Vind<-function(varX,sensX){
#   sum(varX*I(sensX^2))
# }

############# To use mean-variance method:
# 1) response for each sub-unit at each time step (e.g. total biomass for each individual or each transect)
# 2) mean response for each sub-unit across time (e.g. mean biomass through time for each individual)
# 3) variance in response for each sub-unit across time (e.g. variance in biomass through time for each individual)
# 4) aggregate response at each time step (e.g. total biomass for the whole population/community,etc)
# Note: #4 should be a sum of the values of #1
# 5) mean aggregate response through time (e.g. mean total biomass for whole populaiton/community through time)
# 6) variance in aggregate response through time

#### Among individuals within a population (for total biomass)
SzMean<-log(aggregate(df2$AreaT1,list(df2$Plt),mean,na.rm=TRUE)$x) # 2
SzVar<-aggregate(df2$AreaT1,list(df2$Plt),var,na.rm=TRUE)$x # 3
# method using variance-mean relationship to predict expected variance
# fit linear regression of #2 and #3 on log-scale
m1<-lm(log(SzVar)~SzMean); summary(m1)
plot(SzMean,log(SzVar)); abline(m1)
ObsMean<-log(mean(aggregate(df2$AreaT1,list(df2$time),sum,na.rm=TRUE)$x)) # 5
ObsVar<-var(aggregate(df2$AreaT1,list(df2$time),sum,na.rm=TRUE)$x) # 6
# predict what aggregate variance should be based on regression
Vpred<-predict(m1,data.frame(SzMean=ObsMean))
# compare predicted and observed aggregate variances
Buf1<-Vpred/log(ObsVar)


# # method using ratio of CVs
# SzCV<-aggregate(df2$AreaT1,list(df2$Plt),cv,na.rm=TRUE)$x
# ObsCV<-cv(aggregate(df2$AreaT1,list(df2$time),sum,na.rm=TRUE)$x)
# Buf2<-mean(SzCV)/ObsCV
# # method scaling observed variance by expected variance given perfect correlations or independence
# Vp<-Vperf(varX=SzVar,sensX=rep(1,length(SzVar))) # total biomass is a simple sum of biomass of each individual, so sensitivity=1
# Vi<-Vind(varX=SzVar,sensX=rep(1,length(SzVar)))
# Buf3<-((ObsVar-Vi)/(Vp-Vi))

#### Among transects (for total biomass)
df$TransYear<-interaction(df$Trans,df$time)
table(df$Trans,df$time)
df3<-subset(df,!df$Trans%in%c("2A","2C"))
# remove transects 2A and 2C b/c missing data in 2005 and 2006
Biomass2<-matrix(aggregate(df3$AreaT1,list(df3$TransYear),sum,na.rm=TRUE)$x,nrow=11,ncol=12,byrow=TRUE) #1
SzMean2<-log(apply(Biomass2,2,mean,na.rm=TRUE)) #2
SzVar2<-apply(Biomass2,2,var,na.rm=TRUE) #3
# method using variance-mean relationship to predict expected variance
m2<-lm(log(SzVar2)~SzMean2); summary(m2)
plot(SzMean2,log(SzVar2)); abline(m2)
ObsMean2<-log(mean(aggregate(df3$AreaT1,list(df3$time),sum,na.rm=TRUE)$x)) #5
ObsVar2<-var(aggregate(df3$AreaT1,list(df3$time),sum,na.rm=TRUE)$x) #6
Vpred2<-predict(m2,data.frame(SzMean2=ObsMean2))
Buf1b<-Vpred2/log(ObsVar2)

# # method using ratio of CVs
# SzCV2<-apply(Biomass2,2,cv,na.rm=TRUE)
# ObsCV2<-cv(aggregate(df3$AreaT1,list(df3$time),sum,na.rm=TRUE)$x)
# Buf2b<-mean(SzCV2)/ObsCV2
# # method scaling observed variance by expected variance given perfect correlations or independence
# Vp2<-Vperf(varX=SzVar2,sensX=rep(1,length(SzVar2))) # total biomass is a simple sum of biomass of each transect, so sensitivity=1
# Vi2<-Vind(varX=SzVar2,sensX=rep(1,length(SzVar2)))
# Buf3b<-((ObsVar2-Vi2)/(Vp2-Vi2))

#### Figure to illustrate magnitude of buffering at different scale
par(.pardefault)
y<-c(Buf1,Buf1b) # Lauren, you can add in your buffering estimates here
# values greater than one indicate buffering
# values less than one indicate more variance than expected (strong positive correlations)
barplot(y,
        names=c("Individuals","Transects"),
        ylim=c(0,2))
abline(h=1)
