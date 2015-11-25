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

df<-subset(df,!df$Trans%in%c("2A","2C"))
# remove transects 2A and 2C b/c missing data in 2005 and 2006

########### Create correlation plots for annual growth of Silene

### Among size classes of Silene within population SN1
# Assign individuals to size classes
MeanSize<-aggregate(df$AreaT1,list(df$PltID),mean,na.rm=TRUE)
hist(MeanSize$x,breaks=50)
MeanGrowth<-aggregate(df$AreaGro,list(df$PltID),mean,na.rm=TRUE)
plot(MeanSize$x,MeanGrowth$x)
quantile(MeanSize$x)
# assign sign classes 0-20, 20-50, 50-100, >100
SizeClass<-cut(MeanSize$x,breaks=c(0,20,50,100,1000),labels=FALSE)
boxplot(MeanSize$x~SizeClass)
df$SizeClass<-0
for (i in 1:length(levels(df$PltID))){  
  df$SizeClass[df$PltID==levels(df$PltID)[i]]<-SizeClass[i]
}
df$SizeClass<-as.factor(df$SizeClass)

# Create correlation matrix for population SN4
df2<-subset(df,df$Pop=="SN4")
df2<-droplevels(df2)
df2$SzYear<-interaction(df2$SizeClass,df2$time)
table(df2$SizeClass,df2$time)
SzYearMeans<-aggregate(df2$AreaT1,list(df2$SzYear),sum,na.rm=TRUE)$x
SzCorMat<-cor(matrix(SzYearMeans,nrow=length(levels(df2$time)),ncol=length(levels(df2$SizeClass)),byrow=TRUE))
colnames(SzCorMat)<-c("0-20","20-50","50-100",">100")
rownames(SzCorMat)<-c("0-20","20-50","50-100",">100")
pdf("Size classes within Silene population.pdf")
corrplot(SzCorMat,method="circle",type="upper",tl.srt=45,tl.col="black",
         diag=FALSE,addCoef.col="gray20",mar=c(0,0,4,0),
         main=expression("Size classes within a"~italic(Silene)~"population"))
dev.off()

### Among populations of Silene 
df$PopYear<-interaction(df$Pop,df$time)
PYmeans<-aggregate(df$Gro,by=list(df$PopYear),sum,na.rm=TRUE)$x
Mpop<-cor(matrix(PYmeans,nrow=11,ncol=4,byrow=TRUE))
colnames(Mpop)<-c("P1","P2","P3","P4")
rownames(Mpop)<-c("P1","P2","P3","P4")
pdf("Among populations of Silene.pdf")
corrplot(Mpop,method="circle",type="upper",tl.srt=45,tl.col="black",
         diag=FALSE,addCoef.col="gray20",mar=c(0,0,4,0),
         main=expression("Among"~italic(Silene)~"populations"))
dev.off()

### Among transects of Silene 
df$TransYear<-interaction(df$Trans,df$time)
table(df$Trans,df$time)
df3<-subset(df,!df$Trans%in%c("2A","2C"))
# remove transects 2A and 2C b/c missing data in 2005 and 2006
Tmeans<-aggregate(df3$Gro,by=list(df3$TransYear),sum,na.rm=TRUE)$x
Mtran<-cor(matrix(Tmeans,nrow=11,ncol=12,byrow=TRUE))
colnames(Mtran)<-paste(rep(c("P1","P2","P3","P4"),c(3,1,3,5)),c("a","b","c","c","a","b","c","a","b","c","d","e"),sep="")
rownames(Mtran)<-paste(rep(c("P1","P2","P3","P4"),c(3,1,3,5)),c("a","b","c","c","a","b","c","a","b","c","d","e"),sep="")
pdf("Among transects of Silene.pdf")
corrplot(Mtran,method="circle",type="upper",tl.srt=45,tl.col="black",
         diag=FALSE,addCoef.col="gray20",mar=c(0,0,4,0),
         main=expression("Among"~italic(Silene)~"transects"),xpd=TRUE)
dev.off()


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
# SN1
SzMean<-log(aggregate(df2$AreaT1,list(df2$Plt),mean,na.rm=TRUE)$x) # 2
SzVar<-aggregate(df2$AreaT1,list(df2$Plt),var,na.rm=TRUE)$x # 3
# method using variance-mean relationship to predict expected variance
# fit linear regression of #2 and #3 on log-scale
m1<-lm(log(SzVar)~SzMean); summary(m1)
ObsMeana<-log(mean(aggregate(df2$AreaT1,list(df2$time),sum,na.rm=TRUE)$x)) # 5
ObsVara<-var(aggregate(df2$AreaT1,list(df2$time),sum,na.rm=TRUE)$x) # 6
plot(SzMean,log(SzVar),ylim=c(0,log(ObsVara)+1),xlim=c(0,ObsMeana+1)); abline(m1)
points(ObsMeana,log(ObsVara),pch=16,col="black")
# predict what aggregate variance should be based on regression
Vpreda<-predict(m1,data.frame(SzMean=ObsMeana))
# compare predicted and observed aggregate variances
Buf1<-Vpreda/log(ObsVara)

# SN2
df2b<-subset(df,df$Pop=="SN2")
SzMeanb<-log(aggregate(df2b$AreaT1,list(df2b$Plt),mean,na.rm=TRUE)$x) 
SzVarb<-aggregate(df2b$AreaT1,list(df2b$Plt),var,na.rm=TRUE)$x 
m1b<-lm(log(SzVarb)~SzMeanb); summary(m1b)
ObsMeanb<-log(mean(aggregate(df2b$AreaT1,list(df2b$time),sum,na.rm=TRUE)$x)) 
ObsVarb<-var(aggregate(df2b$AreaT1,list(df2b$time),sum,na.rm=TRUE)$x) 
plot(SzMeanb,log(SzVarb),ylim=c(0,log(ObsVarb)+1),xlim=c(0,ObsMeanb+1)); abline(m1b)
points(ObsMeanb,log(ObsVarb),pch=16,col="black")
Vpredb<-predict(m1b,data.frame(SzMeanb=ObsMeanb))
Buf2<-Vpredb/log(ObsVarb)

# SN3
df2c<-subset(df,df$Pop=="SN3")
SzMeanc<-log(aggregate(df2c$AreaT1,list(df2c$Plt),mean,na.rm=TRUE)$x) 
SzVarc<-aggregate(df2c$AreaT1,list(df2c$Plt),var,na.rm=TRUE)$x 
m1c<-lm(log(SzVarc)~SzMeanc); summary(m1c)
ObsMeanc<-log(mean(aggregate(df2c$AreaT1,list(df2c$time),sum,na.rm=TRUE)$x)) 
ObsVarc<-var(aggregate(df2c$AreaT1,list(df2c$time),sum,na.rm=TRUE)$x) 
plot(SzMeanc,log(SzVarc),ylim=c(0,log(ObsVarc)+1),xlim=c(0,ObsMeanc+1)); abline(m1c)
points(ObsMeanc,log(ObsVarc),pch=16,col="black")
Vpredc<-predict(m1c,data.frame(SzMeanc=ObsMeanc))
Buf3<-Vpredc/log(ObsVarc)

# SN4
df2d<-subset(df,df$Pop=="SN4")
SzMeand<-log(aggregate(df2d$AreaT1,list(df2d$Plt),mean,na.rm=TRUE)$x) 
SzVard<-aggregate(df2d$AreaT1,list(df2d$Plt),var,na.rm=TRUE)$x 
m1d<-lm(log(SzVard)~SzMeand); summary(m1d)
ObsMeand<-log(mean(aggregate(df2d$AreaT1,list(df2d$time),sum,na.rm=TRUE)$x)) 
ObsVard<-var(aggregate(df2d$AreaT1,list(df2d$time),sum,na.rm=TRUE)$x) 
plot(SzMeand,log(SzVard),ylim=c(0,log(ObsVard)+1),xlim=c(0,ObsMeand+1)); abline(m1d)
points(ObsMeand,log(ObsVard),pch=16,col="black")
Vpredd<-predict(m1d,data.frame(SzMeand=ObsMeand))
Buf4<-Vpredd/log(ObsVard)


# # method using ratio of CVs
# SzCV<-aggregate(df2$AreaT1,list(df2$Plt),cv,na.rm=TRUE)$x
# ObsCV<-cv(aggregate(df2$AreaT1,list(df2$time),sum,na.rm=TRUE)$x)
# Buf2<-mean(SzCV)/ObsCV
# # method scaling observed variance by expected variance given perfect correlations or independence
# Vp<-Vperf(varX=SzVar,sensX=rep(1,length(SzVar))) # total biomass is a simple sum of biomass of each individual, so sensitivity=1
# Vi<-Vind(varX=SzVar,sensX=rep(1,length(SzVar)))
# Buf3<-((ObsVar-Vi)/(Vp-Vi))

#### Among transects (for total biomass)
Biomass2<-matrix(aggregate(df3$AreaT1,list(df3$TransYear),sum,na.rm=TRUE)$x,nrow=11,ncol=12,byrow=TRUE) #1
SzMean2<-log(apply(Biomass2,2,mean,na.rm=TRUE)) #2
SzVar2<-apply(Biomass2,2,var,na.rm=TRUE) #3
# method using variance-mean relationship to predict expected variance
m2<-lm(log(SzVar2)~SzMean2); summary(m2)
ObsMean2<-log(mean(aggregate(df3$AreaT1,list(df3$time),sum,na.rm=TRUE)$x)) #5
ObsVar2<-var(aggregate(df3$AreaT1,list(df3$time),sum,na.rm=TRUE)$x) #6
plot(SzMean2,log(SzVar2),ylim=c(0,log(ObsVar2)+1),xlim=c(0,ObsMean2+1)); abline(m2)
points(ObsMean2,log(ObsVar2),pch=16,col="black")
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

#### Figure to illustrate magnitude of buffering at different scales
## Note: must run MeanVar_CoverData.R to get buffering at community and landscape scales
par(.pardefault)
y<-c(Buf1,Buf2,Buf3,Buf4,Buf1b,BufFF,BufDM,BufMM,BufSB,BufHAB) 
x<-c(exp(Vpreda)/ObsVara,exp(Vpredb)/ObsVarb, exp(Vpredc)/ObsVarc,exp(Vpredd)/ObsVard,exp(Vpred2)/ObsVar2, 
     expBufFF,expBufDM,expBufMM,expBufSB,expBufHAB)
labs<-c("P1","P2","P3","P4",expression(italic(Silene)), "Fellfield","Dry meadow","Moist meadow","Snowbed","Landscape")
# values greater than one indicate buffering
# values less than one indicate more variance than expected (strong positive correlations)

# buffering on a log scale
pdf("Portfolio effects on log scale.pdf")
par(oma=c(4,1,1,1))
b1<-barplot(y,names="",ylab="log predicted variance / log observed variance",
        col=c(rep("gray80",4),"gray20",rep("white",4),"gray60")
        ,ylim=c(0,2))
b2<-barplot(y,names="",
            col=c(rep("black",4),"gray20",rep("black",4),"gray60"),
            density=c(rep(10,4),-3,rep(10,4),-3),add=TRUE)
abline(h=1)
text(b1-.5,par("usr")[1]+.07,labels=labs,srt=60,pos=1,xpd=TRUE)
legend("topright",legend=c("Among individuals within a population", 
                           expression("Among transects of"~italic(Silene)),
                           "Among functional groups within a habitat",
                           "Among habitats across the landscape"),
       fill=c("gray80","gray20","white","gray60"),border="black",
       density=-3,bty="n",pt.cex=2)
legend("topright",legend=c("Among individuals within a population", 
                           expression("Among transects of"~italic(Silene)),
                           "Among functional groups within a habitat",
                           "Among habitats across the landscape"),
       fill=c("black","gray20","black","gray60"),border="black",
       density=c(20,-3,20,-3),bty="n",pt.cex=2)
dev.off()
### buffering on the raw scale
pdf("Portfolio effects on raw scale.pdf")
par(oma=c(4,1,1,1))
b3<-barplot(x,names="", ylab="Predicted variance / Observed variance",
            col=c(rep("gray80",4),"gray20",rep("white",4),"gray60")
            ,ylim=c(0,10))
b4<-barplot(x,names="",
            col=c(rep("black",4),"gray20",rep("black",4),"gray60"),
            density=c(rep(10,4),-3,rep(10,4),-3),add=TRUE)
abline(h=1)
text(b1-.5,par("usr")[1]-0.7,labels=labs,srt=60,pos=1,xpd=TRUE)
legend("topright",legend=c("Among individuals within a population", 
                           expression("Among transects of"~italic(Silene)),
                           "Among functional groups within a habitat",
                           "Among habitats across the landscape"),
       fill=c("gray80","gray20","white","gray60"),border="black",
       density=-3,bty="n",pt.cex=2)
legend("topright",legend=c("Among individuals within a population", 
                           expression("Among transects of"~italic(Silene)),
                           "Among functional groups within a habitat",
                           "Among habitats across the landscape"),
       fill=c("black","gray20","black","gray60"),border="black",
       density=c(20,-3,20,-3),bty="n",pt.cex=2)
dev.off()