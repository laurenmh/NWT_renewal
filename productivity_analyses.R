#Analyses for productivity data

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrgram)
library(plotrix)
library(codyn)
library(Hmisc)

prod<-read.csv("~/Dropbox/NWT_data/NWT_SnowXProdCorrected.csv")
head(prod)
prod %>% group_by(class_3,year) %>% summarise(count = n()) %>% print.data.frame()
#in 1997 only a subset of plots were done, this included only 1FF, 2DM, 2WM. Thus the distribution of plots among community types are not the same as in other years. So when doing averages, it biases toward the high biomass values of MM. I think I will delete 1997 in some analyses


#### Corrgram of prod across plots ####
prod_forcor <- prod %>% 
  #filter(year!=1997) %>%
  mutate(plot=paste(class_3, plot, sep="_")) %>%
  select(plot, anpp, year) %>%
  spread(plot, anpp, fill=NA) %>%
  tbl_df()

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/prod_corrgram_byplot.pdf")
corrgram(prod_forcor[,2:ncol(prod_forcor)], order=NULL, upper.panel=panel.shade,lower.panel=NULL)
dev.off()



#### Corrgram of prod by community type ####
prodmean<-summarise(group_by(prod,year,class_3),anpp = mean(anpp, na.rm = TRUE))
prodmeantot<-summarise(group_by(prod,year),anpp = mean(anpp, na.rm = TRUE))

prodmean_forcor <- prodmean %>%
  filter(year!=1997) %>%
  select(class_3, anpp, year) %>%
  spread(class_3, anpp, fill=0) %>%
  tbl_df()

corrgram(prodmean_forcor[,2:ncol(prodmean_forcor)], order=NULL, upper.panel=panel.shade,lower.panel=panel.pts)



#### Mean and SE of prod over time ####
#Prod by class, I deleted 1997, if you wanted, you could include SB and MM for 1997 (but not other classes)
prodmeanse <- prod %>% filter(year!=1997) %>% group_by(year,class_3) %>% select(year,class_3,anpp) %>% summarise_each(funs(mean,sd,std.error))  

limits <- aes(ymax = mean + std.error, ymin = mean - std.error)

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/prodovertimebyclass.pdf")
ggplot(prodmeanse) + aes(x=year,y=mean,color=class_3) +# theme_classic() +
  geom_line() +
  #facet_wrap(~class_3) +
  geom_errorbar(limits, width=0.2)
dev.off()

#Total productivity
prodtotmeanse <- prod %>% filter(year!=1997) %>%group_by(year) %>% select(year,anpp) %>% summarise_each(funs(mean,sd,std.error))  

limits <- aes(ymax = mean + std.error, ymin = mean - std.error)

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/prodovertime.pdf")
ggplot(prodtotmeanse) + aes(x=year,y=mean) + #theme_classic() +
  geom_line() +
  geom_errorbar(limits, width=0.2)
dev.off()



#Other figs
ggplot(prodmean, aes(x=year, y=anpp, color=class_3)) + geom_line()

ggplot(prod, aes(x=year, y=anpp, color=class_3, group= plot)) + geom_line() + facet_wrap(~class_3)

prodsynch<-synchrony(prodmean, time.var = "year", species.var = "class_3", abundance.var = "anpp")




#### Relating prod to climate variables ####
climate<-read.csv("~/Dropbox/NWT_data/NWT_ClimateData_2015-11-02.csv")
colnames(climate)[1]<-"year"

#creating one year lagged variables for all variables
climatelags<-cbind(climate,as.data.frame(matrix(NA,nrow=33,ncol=68)))
for (i in 1:68){
  vars<-colnames(climate)[2:69]
  climatelags[,i+69]<-c(NA,climate[,vars[i]][1:32])
}
colnames(climatelags)[70:137]<-paste(vars,"_lag",sep = "")

ggplot(climatelags) + aes(x=year,y=GSLthreedayneg3C_lag) + geom_point() + stat_smooth(method="lm", se=FALSE)

colnames(climatelags)

prodmean2<-filter(merge(prodmean,climatelags,"year"),year!=1997)

#Figs of prod vs. climate
pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/spr_precip_lag.pdf")
ggplot(prodmean2) + aes(x=spr_precip_lag,y=anpp) + geom_point() + facet_wrap(~class_3) + stat_smooth(method="lm", se=FALSE)
dev.off()

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/sum_meanT.pdf")
ggplot(prodmean2) + aes(x=sum_meanT,y=anpp) + geom_point() + facet_wrap(~class_3) + stat_smooth(method="lm", se=FALSE)
dev.off()

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/may_frosts0C.pdf")
ggplot(prodmean2) + aes(x=may_frosts0C,y=anpp ) + geom_point() + facet_wrap(~class_3) + stat_smooth(method="lm", se=FALSE)
dev.off()

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/GSLthreedayneg3C_lag.pdf")
ggplot(prodmean2) + aes(x=GSLthreedayneg3C_lag,y=anpp ) + geom_point() + facet_wrap(~class_3) + stat_smooth(method="lm", se=FALSE)
dev.off()

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/spr_precip.pdf")
ggplot(prodmean2) + aes(x=spr_precip,y=anpp) + geom_point() + facet_wrap(~class_3) + stat_smooth(method="lm", se=FALSE)
dev.off()

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/fal_PET.pdf")
ggplot(prodmean2) + aes(x=fal_PET,y=anpp) + geom_point() + facet_wrap(~class_3) + stat_smooth(method="lm", se=FALSE)
dev.off()

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/MAT_lag.pdf")
ggplot(prodmean2) + aes(x=MAT_lag,y=anpp) + geom_point() + facet_wrap(~class_3) + stat_smooth(method="lm", se=FALSE)
dev.off()

#for class specific climate variables, sb_max_snwdpth, sdl_max_snwdpth_lag, ff_meltout_lag          

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/sum_meanT.pdf")
ggplot(prodmean2) + aes(x=spr_precip_lag,y=anpp) + geom_point() + facet_wrap(~class_3) + stat_smooth(method="lm", se=FALSE)
dev.off()


#lm models
results<-prodmean2 %>% group_by(class_3) %>% do(model = lm(anpp ~ sum_precip, data = .)) 
summary(results$model[[1]]) #DM
summary(results$model[[2]]) #FF
summary(results$model[[3]]) #MM
summary(results$model[[4]]) #SB
summary(results$model[[5]]) #WM


#correlations with all climate current and lagged variables
prodmean2DM<-prodmean2 %>% filter(class_3=="DM")
prodmean2FF<-prodmean2 %>% filter(class_3=="FF")
prodmean2MM<-prodmean2 %>% filter(class_3=="MM")
prodmean2SB<-prodmean2 %>% filter(class_3=="SB")
prodmean2WM<-prodmean2 %>% filter(class_3=="WM")

DMcorr<-rcorr(as.matrix(prodmean2DM[,3:139]))
DMcorr2<-as.data.frame(cbind(r=DMcorr$r[,1],p=DMcorr$P[,1]))
DMcorr2[order(DMcorr2$p),]

FFcorr<-rcorr(as.matrix(prodmean2FF[,3:139]))
FFcorr2<-as.data.frame(cbind(r=FFcorr$r[,1],p=FFcorr$P[,1]))
FFcorr2[order(FFcorr2$p),]

MMcorr<-rcorr(as.matrix(prodmean2MM[,3:139]))
MMcorr2<-as.data.frame(cbind(r=MMcorr$r[,1],p=MMcorr$P[,1]))
MMcorr2[order(MMcorr2$p),]

SBcorr<-rcorr(as.matrix(prodmean2SB[,3:139]))
SBcorr2<-as.data.frame(cbind(r=SBcorr$r[,1],p=SBcorr$P[,1]))
SBcorr2[order(SBcorr2$p),]

WMcorr<-rcorr(as.matrix(prodmean2WM[,3:139]))
WMcorr2<-as.data.frame(cbind(r=WMcorr$r[,1],p=WMcorr$P[,1]))
WMcorr2[order(WMcorr2$p),]
#
















output<-list(NA)
for(i in 1:5){
  classes<-c("DM","FF","MM","SB","WM")
  temp<-subset(prodmean2,class_3==classes[i])
  output[[i]]<-lm(anpp~spr_precip + spr_precip_lag,data=temp)
}

summary(output[[1]])
summary(output[[2]])
summary(output[[3]])
summary(output[[4]])
summary(output[[5]])




