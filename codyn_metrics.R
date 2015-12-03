######### Applying the codyn metrics to the grid composition dataset#########
# library(devtools)
# install_github("laurenmh/codyn")

library(codyn)
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)

#set theme
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank())

##requires sppdat2 from gridComposition_datacleaning.R
##and pcouts2 from climate_PCA.R
head(sppdat2)

#identify plots that only ever have one species
sppdat3<-sppdat2 %>%
  select(plot, USDA_code) %>%
  unique() %>%
  group_by(plot) %>%
  mutate(totrich=n())

#remove plots that only ever have one species
sppdat4<-merge(sppdat2, sppdat3) %>%
  filter(totrich>1)

#read in the snow data; remove unwanted habitat types
snowdat <- read.csv(file.path(datpath, 'NWT_SnowXprod.csv')) %>%
  tbl_df() %>%
  group_by(plot, class_3) %>%
  summarize(maxsnow=mean(max_snow)) %>%
  tbl_df() %>%
  filter(class_3 != "SF", class_3!="ST", class_3!="rock") %>%
  mutate(class_3=as.character(class_3), 
         class_3=ifelse(class_3=="WM", "MM", class_3)) 


#calculate turnover metrics (note: this isn't ideal because the time intervals are not consistent)
turn.dat<-turnover(sppdat2, abundance.var="abund", species.var="USDA_code", replicate.var="plot")

app.dat<-turnover(sppdat2, abundance.var="abund", species.var="USDA_code", replicate.var="plot", metric="appearance")
head(app.dat)

disapp.dat<-turnover(sppdat2, abundance.var="abund", species.var="USDA_code", replicate.var="plot", metric="disappearance")
head(disapp.dat)

#visualize the turnover metrics
turn.all <-merge(turn.dat, merge(app.dat, merge(disapp.dat, snowdat))) %>%
  tbl_df()

ggplot(turn.all, aes(x=maxsnow, y=total, color=year)) + geom_point() + facet_wrap(~class_3, scale="free") + geom_smooth()
ggplot(turn.all, aes(x=maxsnow, y=appearance, color=year)) + geom_point() + facet_wrap(~class_3, scale="free") + geom_smooth()
ggplot(turn.all, aes(x=maxsnow, y=disappearance, color=year)) + geom_point() + facet_wrap(~class_3, scale="free") + geom_smooth()
ggplot(turn.all, aes(x=maxsnow, y=disappearance, color=year)) + geom_point() + facet_wrap(~class_3, scale="free") + geom_smooth()

#add in the pca data
turn.all.PCA <- merge(turn.all, pcouts2)
#only visualize the years that are continuous
ggplot(subset(turn.all.PCA, year>2010), aes(x=sumallPC1, y=total)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~class_3)
ggplot(subset(turn.all.PCA, year>2010), aes(x=sumallPC1, y=disappearance)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~class_3)
ggplot(subset(turn.all.PCA, year>2010), aes(x=sumallPC1, y=appearance)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~class_3)
ggplot(subset(turn.all.PCA, year>2010), aes(x=maxsnow, y=appearance)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~class_3, scale="free")

#calculate the synchrony metrics
synch<-synchrony(sppdat4, abundance.var="abund", species.var="USDA_code", replicate.var="plot", 
                 metric="Loreau")

synch2<-synchrony(sppdat4, abundance.var="abund", species.var="USDA_code", replicate.var="plot", 
                 metric="Gross")

var.dat<-variance_ratio(sppdat4, abundance.var="abund", species.var="USDA_code", replicate.var="plot", 
                        bootnumber = 10, average.replicates = F)

#calculate the mean rank shift 
mrs.dat<-mean_rank_shift(sppdat4, abundance.var="abund", species.var="USDA_code", replicate.var="plot")

#calculate rate change
rate.dat<-rate_change(sppdat4, abundance.var="abund", species.var="USDA_code", replicate.var="plot")
rate.dat.sum<-merge(rate.dat, snowdat) 

#summarize
rate.dat.sum2 <-rate.dat.sum %>%
  group_by(class_3) %>%
  summarize(meanratechange=mean(rate_change), varratechange=var(rate_change), seratechange=sd(rate_change)/sqrt(length(rate_change)))

#graph rate change summary
nwt_rate_summary<-ggplot(rate.dat.sum2, aes(x=class_3, y=meanratechange)) + geom_bar(stat="identity", fill="grey") + 
  geom_errorbar(aes(ymin=meanratechange-seratechange, ymax=meanratechange+seratechange), width=.25) + 
  labs(x="Habitat type", y="Mean rate of change")

#calculate rate change interval
rate.dat2<-rate_change_interval(sppdat4, abundance.var="abund", species.var="USDA_code", replicate.var="plot")
rate.dat3<-merge(rate.dat2, snowdat) 

#visualize rate change slopes for each plot
nwt_rate_pic <- ggplot(rate.dat3, aes(x=interval, y=distance, color=maxsnow, group=maxsnow)) + geom_point() + 
  geom_smooth(method="lm", se=F) + facet_wrap(~class_3)  + 
  labs(x="Time interval", y="Euclidean distance") + theme(legend.position="none")

#look at rate of change in relation to snow depth
rate_snow<-ggplot(rate.dat.sum, aes(x=maxsnow, y=rate_change)) + 
  geom_point(size=2) + facet_wrap(~class_3, scale="free" ) + geom_smooth() + 
  labs(x="Max snow depth", y="Rate of change")# + ylim(0, 4)

#output the rate change story
tiff("NWT_ratechange2.tiff", width=800, height=800)
grid.arrange(nwt_rate_pic,nwt_rate_summary,  rate_snow, ncol=2)
dev.off()


# ggplot(rate.dat.sum, aes(x=maxsnow, y=rate_change)) + 
#   geom_point(size=2)  + geom_smooth()


