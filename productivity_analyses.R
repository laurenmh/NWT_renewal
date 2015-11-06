#Analyses for productivity data

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrgram)
library(plotrix)

prod<-read.csv("~/Dropbox/NWT_data/NWT_SnowXProdCorrected.csv")
head(prod)
prod %>% group_by(class_3,year) %>% summarise(count = n()) %>% print.data.frame()
#in 1997 only a subset of plots were done, this included only 1FF, 2DM, 2WM. Thus the distribution of plots among community types are not the same as in other years. So when doing averages, it biases toward the high biomass values of MM. I think I will delete 1997 in some analyses


#### Corrgram of prod across plots ####
prod_forcor <- prod %>%
mutate(plot=paste(class_3, plot, sep="_")) %>%
  select(plot, anpp, year) %>%
spread(plot, anpp, fill=0) %>%
  tbl_df()

pdf("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Figures/prod_corrgram_byplot.pdf")
corrgram(prod_forcor[,2:ncol(prod_forcor)], order=NULL, upper.panel=panel.shade,lower.panel=NULL)
dev.off()



#### Corrgram of prod by community type ####
prodmean<-summarise(group_by(prod,year,class_3),anpp = mean(anpp, na.rm = TRUE))
prodmeantot<-summarise(group_by(prod,year),anpp = mean(anpp, na.rm = TRUE))

prodmean_forcor <- prodmean %>%
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







ggplot(prod) + aes(x=year,y=anpp) + theme_classic() +
  #geom_point() +
  facet_wrap(~class_3) +
  stat_summary(fun.data=mean_se, colour = "red") 




ggplot(prodmean, aes(x=year, y=anpp, color=class_3)) + geom_line()

ggplot(snowprod, aes(x=year, y=anpp, color=class_3, group= plot)) + geom_line() + facet_wrap(~class_3)

prodsynch<-synchrony(prodmean, time.var = "year", species.var = "class_3", abundance.var = "anpp")




#### Relating prod to climate variables ####
climate<-read.csv("~/Dropbox/NWT_data/NWT_ClimateData_2015-11-02.csv")
colnames(climate)[1]<-"year"


prodmean2<-merge(prodmean,climate,"year")

colnames(climate)

ggplot(prodmean2) + aes(x=sum_moisturedeficit,y=anpp) + theme_classic() +
  geom_point()+
  facet_wrap(~class_3)+
  stat_smooth(method="lm", se=FALSE)




