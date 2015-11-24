library(lme4)
library(lubridate)

pikasurvival<-read.csv("~/Dropbox/NWT_data/NWT_pika_data/Pika_survival.csv") %>%
  tbl_df() %>%
  mutate(year=Year)

pcouts3<-merge(pcouts2, pclags, all=T)
pika2 <-merge(pikasurvival, pcouts3, all.x=T) 

pika3 <- pika2 %>%
  group_by(sumallPC1, sumallPC2, Site, sumallPC1_lag, sumallPC2_lag) %>%
  summarize(totpik=n(), livpik=sum(Survival), propsurvive=livpik/totpik)


ggplot(pika2, aes(x=sumallPC1, y=Survival)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Site)
ggplot(pika2, aes(x=sumallPC1_lag, y=Survival)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Site)

ggplot(pika2, aes(x=sumallPC1, y=GCM)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Site)
ggplot(pika2, aes(x=sumallPC1, y=Weight)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Site)

ggplot(pika2, aes(x=sumallPC1, y=Survival)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Site)

ggplot(pika2, aes(x=sumallPC1, y=Survival)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Elevation)
ggplot(pika2, aes(x=sumallPC1, y=GCM)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Elevation)
ggplot(pika2, aes(x=sumallPC1, y=Weight, color=Sex)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Elevation)


ggplot(pika3, aes(x=sumallPC1, y=propsurvive)) + geom_point() + geom_smooth(method="lm", se=F) + facet_wrap(~Site)
ggplot(pika3, aes(x=sumallPC2, y=propsurvive)) + geom_point() + geom_smooth(method="lm", se=F) + facet_wrap(~Site)

l<-glm(Survival~sumallPC1, data=subset(pika2, Site=="LL"), family="binomial")
summary(l)

pika4 <- pika2 %>%
  group_by(sumallPC1, sumallPC2, sumallPC1_lag, sumallPC2_lag) %>%
  summarize(totpik=n(), livpik=sum(Survival), propsurvive=livpik/totpik)
ggplot(pika4, aes(x=sumallPC1_lag, y=propsurvive)) + geom_point() + geom_smooth(method="lm", se=F)


pikaweight<-read.csv("~/Dropbox/NWT_data/NWT_pika_data/PikaDataNWT2008-2014_ChrisRay_4HopeHumphries.csv") %>%
  tbl_df() %>%
  filter(Weight!="NS", Weight!="<NA>") %>%
  mutate(Weight=as.numeric(as.character(Weight)))

pikaweight2 <-merge(pikaweight, pcouts3)
ggplot(pikaweight2, aes(x=sumallPC2, y=Weight)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_grid(Location~Stage)

pikaweightmean <-pikaweight2 %>%
  group_by(Location, year, sumallPC1, sumallPC2, sumallPC1_lag, sumallPC2_lag) %>%
  summarize(meanWeight=mean(Weight), seWeight=sd(Weight)/sqrt(length(Weight)))

ggplot(pikaweightmean, aes(x=sumallPC1, y=meanWeight)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Location) + 
  geom_errorbar(aes(x=sumallPC1, ymin=meanWeight-seWeight, ymax=meanWeight+seWeight))

ggplot(pikaweightmean, aes(x=sumallPC1_lag, y=meanWeight)) + geom_point() + geom_smooth(method="lm", se=F)  + facet_wrap(~Location) + 
  geom_errorbar(aes(x=sumallPC1, ymin=meanWeight-seWeight, ymax=meanWeight+seWeight))

pikaweightmean2 <-pikaweight2 %>%
  group_by(year, sumallPC1, sumallPC2, sumallPC1_lag, sumallPC2_lag) %>%
  summarize(meanWeight=mean(Weight), seWeight=sd(Weight)/sqrt(length(Weight)))

ggplot(pikaweightmean2, aes(x=sumallPC1, y=meanWeight)) + geom_point(size=4) + 
  geom_errorbar(aes(x=sumallPC1, ymin=meanWeight-seWeight, ymax=meanWeight+seWeight)) + 
  theme_classic() + theme(text=element_text(size=16)) + 
  labs(x="PC1 (Length of summer)", y="Mean pika weight (g/")

ggplot(pikaweightmean2, aes(x=sumallPC1_lag, y=meanWeight)) + geom_point() + geom_smooth(method="lm", se=F)  +
  geom_errorbar(aes(x=sumallPC1, ymin=meanWeight-seWeight, ymax=meanWeight+seWeight))



l<-lm(meanWeight~sumallPC1 + Location, data=pikaweightmean)
summary(l)

l<-lm(meanWeight~sumallPC1, data=pikaweightmean2)
summary(l)


##BRING IN THE LONG TIME SERIES
pikaweight2<-merge(read.csv("~/Dropbox/NWT_data/NWT_pika_data/PikaWeights_1980-Ray.csv"), pcouts3, all.x=T) %>%
  tbl_df() %>%
  filter(!is.na(Weight.g))
str(pikaweight2)

ggplot(pikaweight2, aes(x=sumallPC1, y=Weight.g)) + geom_point() + facet_wrap(~Stage)
ggplot(pikaweight2, aes(x=sumallPC1_lag, y=Weight.g)) + geom_point() + facet_wrap(Site~Stage)

pikaweight2mean_bystage <- pikaweight2 %>%
 # filter(Site=="WK") %>%
  filter(!is.na(Stage)) %>%
  group_by(sumallPC1, sumallPC1_lag, sumallPC2, sumallPC2_lag, year, Stage) %>%
  summarize(meanWeight=mean(Weight.g), seWeight=sd(Weight.g)/sqrt(length(Weight.g)))


meanweight_bystage<- ggplot(pikaweight2mean_bystage, aes(x=sumallPC1, y=meanWeight)) + geom_point() + geom_smooth(method="lm", se=T)  +
  geom_errorbar(aes(x=sumallPC1, ymin=meanWeight-seWeight, ymax=meanWeight+seWeight)) + facet_wrap(~Stage, ncol=1, scale="free") +
  theme_classic() + theme(strip.background = element_blank(), text=element_text(size=16)) +
  labs(x="PC1 (Length of summer)", y = "Mean pika weight (g)")


tiff("pikaweight_byPC1Stage.tiff", width=400, height=800)
meanweight_bystage
dev.off()


meanweight_bystage_onepanel<- ggplot(pikaweight2mean_bystage, aes(x=sumallPC1, y=meanWeight, color=Stage)) + geom_point(size=4)+ geom_smooth(method="lm", se=F, lwd=2)  +
  geom_errorbar(aes(x=sumallPC1, ymin=meanWeight-seWeight, ymax=meanWeight+seWeight)) +
  theme_classic() + theme(strip.background = element_blank(), text=element_text(size=16)) +
  labs(x="PC1 (Length of summer)", y = "Mean pika weight (g)")


tiff("pikaweight_byPC1Stage_onepanel.tiff", width=400, height=400)
meanweight_bystage_onepanel
dev.off()


ggplot(subset(pikaweight2, !is.na(Stage)), aes(x=sumallPC1, y=Weight.g)) + geom_point() + facet_wrap(~Stage, ncol=1, scale="free") +
  theme_classic() + theme(strip.background = element_blank(), text=element_text(size=16)) +
  labs(x="PC1 (Length of summer)", y = "Mean pika weight (g)") + geom_smooth(method="lm")
