library(tidyr)
library(dplyr)
library(ggplot2)
library(codyn)
library(grid)
library(gridExtra)
library(corrgram)
library(corrplot)

##NEED TO RUN climate_pca.R and gridComposition_datacleaning.R first

corstab <- sppdat2 %>%
  filter(class_3 != "rock", class_3 != "SF", class_3 !="ST") %>%
  mutate(class_3=as.character(class_3),
         class_3=ifelse(class_3=="WM", "MM", class_3)) %>%
  tbl_df() %>%
  group_by(plot, class_3, USDA_code, func) %>%
  mutate(meanabund=mean(abund), cvabund=sd(abund)/meanabund, sdabund=sd(abund)) %>%
  tbl_df() %>%
  group_by(class_3) %>%
  mutate(meanclassabund = mean(meanabund)) %>%
  tbl_df() %>%
  group_by(class_3, USDA_code) %>%
  mutate(spatmean=mean(meanabund), spatcv=mean(cvabund), spatsd=mean(sdabund)) %>%
  tbl_df() %>%
  group_by(USDA_code) %>%
  mutate(allspatmean=mean(meanabund), allspatcv=mean(cvabund), allspatsd=mean(sdabund))

datpath = "~/Dropbox/NWT_data/" # this likely will be different for different folks

#read in the data
snowdat <- read.csv(file.path(datpath, 'NWT_SnowXprod.csv')) %>%
  tbl_df() %>%
  group_by(plot) %>%
  summarize(maxsnow=mean(max_snow))

alldat0 <- merge(corstab, pcouts2) %>%
  filter(class_3 != "SF", class_3 != "ST") %>%
  arrange() %>%
  filter(allspatmean > 3) %>%
  tbl_df()

classdat <- alldat0 %>%
  filter(abund>0) %>%
  select(USDA_code, class_3) %>%
  unique() %>%
  group_by(USDA_code) %>%
  summarize(classcount=n())


alldat01 <-merge(alldat0, classdat) %>%
  filter(classcount>3) %>%
  filter(USDA_code !="TRDA2")

alldat02 <-merge(alldat01, pclags)

alldat <-merge(alldat02, snowdat) %>%
  tbl_df() %>%
  mutate(sumallbin=1, sumallbin=ifelse(sumallPC1<0, 0, sumallbin))
#,
 #        sumallbin=ifelse(sumallPC1>1.2, 2, sumallbin),
  #       sumallbin=iflese(sumallPC1<-.5, -1, sumallbin))

focaldat <-alldat %>%
  filter(USDA_code=="KOMY" | USDA_code=="2LICHN" |
           USDA_code=="2MOSS" | USDA_code=="ARSC" |
           USDA_code=="CARUD" | USDA_code =="DECE" | USDA_code =="GEROT"|
           USDA_code=="TRPAP" | USDA_code=="MIOB2" | USDA_code=="SIACS2" ) %>%
  group_by(USDA_code) %>%
  mutate(low25=quantile(maxsnow)[2],
         high25=quantile(maxsnow)[4]) %>%
  mutate(snowbin=2, snowbin=ifelse(maxsnow< low25, 1, snowbin), snowbin=ifelse(maxsnow >high25, 3, snowbin))
         
 # mutate(todrop=0, todrop=ifelse((USDA_code=="KOMY" | USDA_code=="CARUD") & maxsnow>100, 1, todrop)) %>%
  #filter(todrop==0) %>%

focaldat2 <- focaldat %>%
  group_by(USDA_code, year) %>%
  summarize(abund=mean(abund)) %>%
  tbl_df() %>%
  #mutate(plot=paste(USDA_code,class_3, sep=", ")) %>%
  tbl_df() %>%
  select(year, abund, USDA_code) %>%
  spread(USDA_code, abund, fill=0)

cormat<-cor(focaldat2[,2:ncol(focaldat2)])

pdf("corplot_focalspp.pdf")
corrplot(cormat,method="circle",type="upper", tl.col="black", tl.cex = .8, diag=F)
dev.off()


ggplot(focaldat, aes(x=maxsnow, y=abund, group=sumallbin)) + 
  facet_wrap(~USDA_code, scale="free")  + geom_point(aes(color=sumallPC1)) +
  geom_smooth(se=F, aes(color=sumallbin), lwd=2) + labs(x="Average max snow", y="Abundance", color="PC1")

pdf("Abund_PC1_binsnow.pdf")
ggplot(subset(focaldat, abund>0), aes(x=sumallPC1, y=abund, group=snowbin)) + 
  facet_wrap(~USDA_code, scale="free")  + geom_point(aes(color=snowbin)) +
  geom_smooth( aes(color=snowbin), lwd=2, method="lm") + labs(x="PC1", y="Abundance", color="Max snow") + theme_bw()


ggplot(subset(focaldat, abund>0), aes(x=sumallPC1, y=(abund), group=snowbin)) + 
  facet_wrap(~USDA_code, scale="free")  + geom_point(aes(color=snowbin)) + scale_y_log10() +
  geom_smooth( aes(color=snowbin), lwd=2, method="lm") + labs(x="PC1", y="Abundance", color="Max snow") + theme_bw()
dev.off()


l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="2LICHN" & abund>0))
summary(l)

l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="2MOSS" & abund>0))
summary(l)

l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="ARSC" & abund>0))
summary(l)

l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="CARUD" & abund>0))
summary(l)

l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="DECE" & abund>0))
summary(l)

l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="GEROT" & abund>0))
summary(l)

l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="MIOB2" & abund>0))
summary(l)

l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="TRPAP" & abund>0))
summary(l)

l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="SIACS2" & abund>0))
summary(l)

l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="KOMY" & abund>0))
summary(l)


###GRAPHS OF SENSITIVITY WITHOUT SNOW BINS
###SHOWS THAT SPECIES AT LOWER SNOW PACK ARE MORE SENSITIVE TO LONGER SUMMER
fdat <- focaldat %>%
  filter(USDA_code=="KOMY" | USDA_code=="2LICHN" |
           USDA_code=="2MOSS" |
           USDA_code=="CARUD" | USDA_code =="DECE" | USDA_code =="GEROT") %>%
  mutate(toremove=ifelse((USDA_code=="KOMY" & maxsnow>100), 1, 0),
        toremove=ifelse((USDA_code=="CARUD" & maxsnow>150), 1, toremove),
         toremove=ifelse((USDA_code=="2LICHN" &maxsnow>350), 1, toremove )) %>%
  filter(toremove!=1) %>%
  mutate(spppos=ifelse(( USDA_code=="2MOSS" | USDA_code =="DECE" | USDA_code =="GEROT"),
         "Upper snow range", "Lower snow range")) %>%
  mutate(notsig=ifelse(( USDA_code=="2MOSS" | USDA_code =="DECE" | USDA_code =="GEROT" | USDA_code=="CARUD"), "yes", "no"))


fdat2 <-fdat %>%
group_by(USDA_code, year) %>%
  summarize(abund=mean(abund)) %>%
  tbl_df() %>%
  #mutate(plot=paste(USDA_code,class_3, sep=", ")) %>%
  tbl_df() %>%
  select(year, abund, USDA_code) %>%
  spread(USDA_code, abund, fill=0)

cormat<-cor(fdat2[,2:ncol(fdat2)])

pdf("corplot_focalspp_2.pdf")
corrplot(cormat,method="circle",type="upper", tl.col="black", tl.cex = .8, diag=F)
dev.off()



a <- ggplot(fdat, aes(x=maxsnow, y=abund, color=USDA_code)) + facet_wrap(~spppos) + geom_point() + ylim(0,100) +
  geom_smooth(se=F, lwd=2, method="lm", formula=y ~ poly(x, 2)) +
  labs(x="Average max snow depth (cm)", y="Percent abundance", color="Species") + theme_bw() +
  theme(strip.background = element_rect(colour = NA, fill = NA, size = 3), 
        strip.text.x = element_text(colour = "black",  size = 20),
        text = element_text(size=20),
        panel.grid.minor = element_blank(),
        panel.grid.major=element_blank())

  


b<- ggplot(subset(fdat), aes(x=sumallPC1, y=abund, color=USDA_code)) + 
  facet_wrap(~spppos)  + geom_point() +
  geom_smooth(se=F, lwd=2, method="lm", aes(linetype=notsig)) + 
  labs(x="Length of summer (PC1)", y="Percent abundance", color="Species") + theme_bw() +
  theme(strip.background = element_rect(colour = NA, fill = NA, size = 3), 
        strip.text.x = element_text(colour = "black",  size = 20),
        text = element_text(size=20),
        panel.grid.minor = element_blank(),
        panel.grid.major=element_blank())



g <- ggplotGrob(a + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

lheight <- sum(legend$height)

tiff("Summer_sensitivity_focal.tiff", width=600, height=900)
grid.arrange(arrangeGrob(a + theme(legend.position="none"), 
                         b + theme(legend.position="none",  strip.text.x = element_blank())), legend, ncol=1,
             heights = unit.c(unit(1, "npc") - lheight, lheight))
dev.off()


##MEAN VARIANCE ON FOCALDAT (THE LARGER SUBSET) AND FDAT (THE SMALLER SUBSET)



focallevel<-fdat %>%
  group_by(year,  USDA_code) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  group_by(USDA_code) %>%
  summarize(phylumtempmean=mean(abund), phylumtempvar=var(abund))


alllevel<-fdat %>%
  group_by(year) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  summarize(alltempmean=mean(abund), alltempvar=var(abund))



########## Portfolio effects
ObsMean<-log(alllevel$alltempmean); ObsMean
ObsVar<-log(alllevel$alltempvar); ObsVar


### Within each Phylum
focallevel$logMean<-log(focallevel$phylumtempmean)
focallevel$logVar<-log(focallevel$phylumtempvar)
focallevel$ObsMean<-ObsMean
focallevel$ObsVar <-ObsVar
m5<-lm(logVar~logMean,data=focallevel); summary(m5)
ggplot(focallevel, aes(x=logMean, y=logVar)) + geom_point(pch=1) + xlim(0, ObsMean+1)+ ylim(0, ObsVar+1) +
  geom_smooth(method="lm", se=F, fullrange = T) +
  geom_point(aes(x=ObsMean, y=ObsVar)) + theme_classic()

# plot(phylumlevel$logMean,phylumlevel$logVar,xlim=c(0,ObsMean+1),ylim=c(0,ObsVar+1)); abline(m5)
# points(ObsMean,ObsVar,pch=16,col="black")
PredVar<-predict(m5,data.frame(logMean=ObsMean))
BufPHY<-PredVar/ObsVar; BufPHY
expBufPHY<-exp(PredVar)/exp(ObsVar); expBufPHY
##Observed variance is 22% of expected variance
exp(ObsVar)/exp(predict(m5,data.frame(logMean=ObsMean)))


# 
# pdf("allfocalspp_cor_meanvar.pdf")
# corrplot(cormat,method="circle",type="upper", tl.col="black", tl.cex = .8, diag=F)
# ggplot(focallevel, aes(x=logMean, y=logVar)) + geom_point(pch=1) + xlim(0, ObsMean+1)+ ylim(0, ObsVar+1) +
#   geom_smooth(method="lm", se=F, fullrange = T) +
#   geom_point(aes(x=ObsMean, y=ObsVar)) + theme_classic()
# dev.off()

ggplot(subset(fdat, abund>0), aes(x=sumallPC1, y=abund, color=USDA_code)) + 
  facet_wrap(~spppos)  + geom_point() +
  geom_smooth( lwd=2, method="lm") + labs(x="PC1", y="Abundance", color="Max snow") + theme_bw() +





ggplot(fdat, aes(x=maxsnow, y=abund, color=USDA_code)) + geom_point() + facet_wrap(~spppos) + 
  geom_smooth(se=F, lwd=2, method="lm", formula=y ~ poly(x, 2)) + labs(x="Average max snow", y="Abundance", color="PC1")



ggplot(fdat, aes(x=maxsnow, y=abund)) + 
  facet_wrap(~USDA_code)  + geom_point() +
  geom_smooth(se=F, lwd=2, method="lm", formula=y ~ poly(x, 2)) + labs(x="Average max snow", y="Abundance", color="PC1")


ggplot(subset(fdat, abund>0), aes(x=sumallPC1, y=abund)) + 
  facet_wrap(~USDA_code, scale="free")  + geom_point() +
  geom_smooth( lwd=2, method="lm") + labs(x="PC1", y="Abundance", color="Max snow") + theme_bw()




##YES
l<-lm(abund~sumallPC1, data=subset(focaldat, USDA_code=="2LICHN" & abund>0))
summary(l)

##NO
l<-lm(abund~sumallPC1, data=subset(focaldat, USDA_code=="2MOSS" & abund>0))
summary(l)

##YES
l<-lm(abund~sumallPC1, data=subset(focaldat, USDA_code=="ARSC" & abund>0))
summary(l)

##NO
l<-lm(abund~sumallPC1, data=subset(focaldat, USDA_code=="CARUD" & abund>0))
summary(l)

#SLIGHT
l<-lm(abund~sumallPC1, data=subset(focaldat, USDA_code=="DECE" & abund>0))
summary(l)

##NO
l<-lm(abund~sumallPC1, data=subset(focaldat, USDA_code=="GEROT" & abund>0))
summary(l)


##YES
l<-lm(abund~sumallPC1, data=subset(focaldat, USDA_code=="MIOB2" & abund>0))
summary(l)

#YES
l<-lm(abund~sumallPC1, data=subset(focaldat, USDA_code=="TRPAP" & abund>0))
summary(l)

#YES
l<-lm(abund~sumallPC1*snowbin, data=subset(focaldat, USDA_code=="SIACS2" & abund>0))
summary(l)

#YES
l<-lm(abund~sumallPC1, data=subset(focaldat, USDA_code=="KOMY" & abund>0))
summary(l)


#| USDA_code=="SIACS2"
ggplot(focaldat, aes(x=year, y=abund, group=maxsnow)) + 
  facet_wrap(~USDA_code, scale="free")  + geom_point(aes(color=maxsnow)) +
  geom_line(aes(color=maxsnow))
  geom_smooth(se=F, aes(color=sumallPC1), lwd=1)

#| USDA_code=="SIACS2"
ggplot(subset(focaldat, abund>0), aes(x=sumallPC1, y=maxsnow)) + 
  facet_wrap(~USDA_code, scale="free")  + geom_point(aes(color=log(abund))) +
  geom_smooth(se=F, aes(color=sumallPC1), lwd=1)

ggplot(alldat, aes(x=maxsnow, y=abund))  + geom_point(aes(color=USDA_code)) +
  geom_smooth()

l<-lm(abund~maxsnow + sumallPC1, data=subset(alldat, USDA_code=="KOMY"))
summary(l)

Kob <- focaldat %>%
  filter(USDA_code=="KOMY") %>%
  filter(maxsnow<80 & maxsnow>30) %>%
  mutate(snow2=maxsnow*maxsnow) 

l<-lm(abund~maxsnow*sumallPC1, data=Kob)
summary(l)


require(rms)
Kob2 <- Kob %>%
  select(abund, maxsnow, sumallPC1)
dd <- datadist(Kob2); options(datadist='dd')  # facilitates plotting
f <- ols(abund ~ rcs(maxsnow, 3) * sumallPC1, data=Kob)
anova(f)    # tests for interaction (shape differences across T, 3 d.f.)
# anova includes a test for nonlinear interaction
# also provides a global test for T, 4 d.f.
plot(Predict(f, maxsnow, sumallPC1))   # shows 2 estimated curves for 2 values of T
ggplot(Predict(f, maxsnow, sumallPC1))  # will be in next release; uses ggplot2



l<-lm(abund~maxsnow + sumallPC1, data=subset(alldat, USDA_code=="GEROT"))
summary(l)

ggplot(subset(alldat, abund>0), aes(x=sumallPC1, y=abund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code)) + scale_y_log10()

alldatmean <- alldat %>%
  group_by(class_3, sumallPC1, year, sumallPC2, USDA_code, USDA_name, sumallPC1_lag, sumallPC2_lag, func) %>%
  summarize(meanabund=mean(abund), seabund=sd(abund)/sqrt(length(abund))) %>%
  tbl_df()

alldatmean %>%
  select(USDA_code, USDA_name) %>%
  unique()


PC1graph<- ggplot(subset(alldatmean, meanabund>0), aes(x=sumallPC1, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_grid(func~class_3, scale="free") + geom_smooth(se=F, method="lm", aes(color=USDA_code)) +  
  theme_bw() + theme(legend.position="none") #+  scale_y_log10()

PC1laggraph<- ggplot(subset(alldatmean, meanabund>0), aes(x=sumallPC1_lag, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_grid(func~class_3, scale="free") + geom_smooth(se=F, method="lm", aes(color=USDA_code)) +  
  theme_bw() + theme(legend.position="none") #+  scale_y_log10()


PC2graph<- ggplot(subset(alldatmean, meanabund>0), aes(x=sumallPC2, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_grid(func~class_3, scale="free") + geom_smooth(se=F, method="lm", aes(color=USDA_code)) +  
  theme_bw() + theme(legend.position="none") #+  scale_y_log10()


PC2laggraph<-ggplot(subset(alldatmean, meanabund>0), aes(x=sumallPC2_lag, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_grid(func~class_3, scale="free") + geom_smooth(se=F, method="lm", aes(color=USDA_code)) +  
  theme_bw() + theme(legend.position="none") #+  scale_y_log10()

PC1graph + theme(legend.position="right")
grid.arrange(PC1graph, PC1laggraph, PC2graph, PC2laggraph)

ggplot(subset(alldatmean, meanabund>0 & func=="Sedge"), aes(x=sumallPC1, y=meanabund)) +
  geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3, scale="free_y") + geom_smooth(se=F, method="lm", aes(color=USDA_code)) +  
  theme_bw() + theme(legend.position="none")# +  scale_y_log10()


ggplot(subset(alldatmean, meanabund>0 & func=="Sedge"), aes(x=sumallPC1, y=meanabund)) +
  geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3, scale="free_y") + geom_smooth(se=F, method="lm", aes(color=USDA_code)) +  
  theme_bw() #+ theme(legend.position="none")# +  scale_y_log10()

alldatmean %>%
  filter(func=="Sedge") %>%
  select(USDA_name) %>%
  unique()
#what is it about community types (that is arbitrary)
#we have average snow cover at each plot over every year
#talk to emily (average in snow depth, etc...)
#for this, take average peak snow depth, have a number for each plot 
#that gives enviro variability and spatial
#could use snow depth as a covariate, or if there is an interaction with time and space
#kobresia, des, geom, lichen, moss, silene (continuity), carex rupestrus - des analog in the DM
#space as covariate and remove, or check for interactive effect
#need to make it integrative
##looking for something that can change (esp grow) fast
##max cover change as the (positive change) as the metric
#not sure whether the converse is helpful (negative change)
#first graph like megans but arrayed by type
#second is the pc1 analysis looking at tradeoffs
#some of these things might be cool but substantial steps
#pursue these things but have a time that is my cutoff


ggplot(subset(alldat, abund>0), aes(x=sumallPC1, y=abund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_grid(func~class_3, scale="free_y") + geom_smooth(se=F, method="lm", aes(color=USDA_code)) +  
  scale_y_log10()

library(vegan)

l<-lm(abund~sumallPC1*class_3, data=subset(alldat, abund>0 & USDA_code=="KOMY"))
summary(l)

l<-lm(abund~sumallPC2*class_3*USDA_code, data=subset(alldat, abund>0 & func=="Forb"))
summary(l)

l<-lm(abund~sumallPC1*class_3, data=subset(alldat,  func=="Grass"))
summary(l)

l<-lm(abund~sumallPC1*class_3*USDA_code, data=subset(alldat, abund>0 & func=="Sedge"))
summary(l)

l<-lm(abund~sumallPC1*class_3*USDA_code, data=subset(alldat, abund>0 & func=="Cushion"))
summary(l)

l<-lm(abund~sumallPC1*class_3, data=subset(alldat, abund>0 & func=="Legume"))
summary(l)

l<-lm(abund~sumallPC1*class_3, data=subset(alldat, abund>0 & func=="Lichen"))
summary(l)

l<-lm(abund~sumallPC1*class_3, data=subset(alldat, abund>0 & func=="Moss"))
summary(l)

l<-lm(abund~sumallPC2_lag*class_3 + sumallPC1_lag*class_3, data=subset(alldat, USDA_code=="GEROT"))
summary(l)


ggplot(subset(alldatmean, meanabund>0), aes(x=sumallPC1, y=meanabund)) +geom_point(aes(color=class_3)) + #geom_line() + 
  facet_wrap(~USDA_code) + geom_smooth(se=F, method="lm", aes(color=class_3)) +  
  scale_y_log10()
#   scale_y_log10(limits=c(.1, 100), breaks=c(.1,1,10,100))  +
#   geom_errorbar(aes(ymin=meanabund-seabund, ymax=meanabund+seabund, color=class_3)) 

# ggplot(subset(alldat, abund>0), aes(x=sumallPC1, y=abund)) +geom_point(aes(color=class_3)) + #geom_line() + 
#   facet_wrap(~USDA_code) + geom_smooth(se=F, method="lm", aes(color=class_3)) +  
#   scale_y_log10()  


ggplot(alldatmean, aes(x=sumallPC1_lag, y=meanabund)) +geom_point(aes(color=class_3)) + #geom_line() + 
  facet_wrap(~USDA_code) + geom_smooth(se=F, method="lm", aes(color=class_3)) + scale_y_log10()

ggplot(alldatmean, aes(x=sumallPC1_lag, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code)) #+ scale_y_log10()

ggplot(alldatmean, aes(x=sumallPC2, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code)) + scale_y_log10()

ggplot(alldatmean, aes(x=sumallPC2, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code)) + scale_y_log10()



ggplot(alldatmean, aes(x=sumallPC2_lag, y=(meanabund))) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code)) + scale_y_log10()

seddatmean %>%
  arrange(sumallPC1)

##sedges##

corstab <- sppdat2 %>%
  filter(class_3 != "rock", class_3 != "SF", class_3 !="ST") %>%
  group_by(plot, class_3, USDA_code, func) %>%
  mutate(meanabund=mean(abund), cvabund=sd(abund)/meanabund, sdabund=sd(abund)) %>%
  tbl_df() %>%
  group_by(class_3) %>%
  mutate(meanclassabund = mean(meanabund)) %>%
  tbl_df() %>%
  group_by(class_3, USDA_code) %>%
  mutate(spatmean=mean(meanabund), spatcv=mean(cvabund), spatsd=mean(sdabund)) %>%
  tbl_df() %>%
  group_by(USDA_code) %>%
  mutate(allspatmean=mean(meanabund), allspatcv=mean(cvabund), allspatsd=mean(sdabund))


seddat <- merge(corstab, pcouts2) %>%
  filter(func == "Sedge") %>%
  filter(class_3 != "SF", class_3 != "ST") %>%
  arrange() %>%
  filter(allspatmean > 4) %>%
  tbl_df()

seddat %>%
  select(USDA_code, USDA_name) %>%
  unique()

ggplot(seddat, aes(x=sumallPC1, y=abund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code))

seddat %>%
  select(USDA_code, USDA_name) %>%
  unique()

seddatmean <- merge(seddat, pclags) %>%
  group_by(class_3, sumallPC1, year, sumallPC2, USDA_code, USDA_name, sumallPC1_lag, sumallPC2_lag) %>%
  summarize(meanabund=mean(abund), seabund=sd(abund)/sqrt(length(abund))) %>%
  tbl_df() %>%
  filter(meanabund>0)


ggplot(seddatmean, aes(x=sumallPC1, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code)) + scale_y_log10()

ggplot(seddatmean, aes(x=sumallPC1_lag, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code)) + scale_y_log10()

ggplot(seddatmean, aes(x=sumallPC2, y=meanabund)) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code)) + scale_y_log10()

ggplot(seddatmean, aes(x=sumallPC2_lag, y=(meanabund))) +geom_point(aes(color=USDA_code)) + #geom_line() + 
  facet_wrap(~class_3) + geom_smooth(se=F, method="lm", aes(color=USDA_code)) + scale_y_log10()

seddatmean %>%
  arrange(sumallPC1)



