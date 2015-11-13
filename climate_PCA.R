###Climate visualizations###
library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(corrgram)

#### Relating prod to climate variables ####
climate<-read.csv("~/Dropbox/NWT_data/NWT_ClimateData_2015-11-02.csv")
colnames(climate)[1]<-"year"
climate <- tbl_df(climate)

#creating one year lagged variables for all variables
climatelags<-cbind(climate,as.data.frame(matrix(NA,nrow=33,ncol=68)))
for (i in 1:68){
  vars<-colnames(climate)[2:69]
  climatelags[,i+69]<-c(NA,climate[,vars[i]][1:32])
}
colnames(climatelags)[70:137]<-paste(vars,"_lag",sep = "")

#ggplot(climatelags) + aes(x=year,y=GSLthreedayneg3C_lag) + geom_point() + stat_smooth(method="lm", se=FALSE)


##climate without Green Lakes
row.names(climate) <-climate$year
climateA<-climate %>%
  select(-c(GSLthreeday0C:daysicefree_Mean))
  str(climateA)

  
##climate without snow melt
climateB <-climateA %>%
  filter(year>1992) %>%
  arrange(year) 
row.names(climateB)=climateB$year
  
# #PCA with all the non-lake variables
# climPCA <-rda(climateB[,2:ncol(climateB)], scale=T)
# varout<-as.data.frame(scores(climPCA, choices=c(1,2), display=c("species")))
# varout$type<-"variables"
# varout$name<-rownames(varout)
# yearout<-as.data.frame(scores(climPCA, choices=c(1,2), display=c("sites")))
# yearout$type<-"year"
# yearout$name<-row.names(climateB)  
# climateBoutput<-rbind(varout, yearout)
# summary(climPCA)
# 
# plot(climPCA, type="n", scaling=3)
# points(climPCA, pch=21, col="red", bg="yellow", cex=1.2, scaling=3)

##LOOK AT RELATIONSHIP BETWEEN SNOW MELT OUT AND SNOW DEPTH ACROSS HABITAT TYPES
##climate without snow melt
##This is cool - shows that the snow melt across the terrestrial habitats types are overall synchronous
##But that on the second PCA axis there is spread across habitat types (DM and FF vs SB, MM, WM)
climateMeltout <-climateA %>%
  filter(year>1992) %>%
  arrange(year) %>%
  select(year, sdl_max_snwdpth:sb_meltout) %>%
  select(-sdl_max_snowdpth, -sdl_meltout)
rownames(climateMeltout)=climateMeltout$year

meltPCA<-rda(climateMeltout[,2:ncol(climateMeltout)], scale=T)
plot(meltPCA, scaling=3)
summary(meltPCA)
# varout<-as.data.frame(scores(meltPCA, choices=c(1,2), display=c("species")))
# varout$type<-"variables"
# varout$name<-rownames(varout)
meltOutput<-as.data.frame(scores(meltPCA, choices=c(1,2), display=c("sites")))
meltOutput$year<-as.numeric(row.names(climateMeltout))
# meltoutput<-rbind(varout, yearout)
ggplot(meltOutput, aes(x=year, y=PC1)) + geom_point() + geom_line() + geom_smooth(method="lm")

##should this be coded with habitat type as a factor?
# climateMeltout2 <- climateMeltout %>%
#   gather(variable, value, sdl_max_snwdpth:sb_meltout) %>%
#   separate(variable, c("site", "filler", "variable")) %>%
#   mutate(variable=ifelse(filler=="meltout", "meltout", variable)) %>%
#   select(-filler) %>%
#   spread(variable, value)
# 
# melt2PCA<-rda(climateMeltout2[,3:ncol(climateMeltout2)], scale=T)
# plot(melt2PCA, scaling=3)



#an overall correlogram visualization of climate
corrgram(climate[,2:ncol(climate)], order=F, upper.panel=panel.shade,
         lower.panel=NULL)

##Look at just the annual mean variables across terrestrial and aquatic
climateMeans <-climate %>%
  select(year, MAT, tot_precip, moisturedeficit, PET, GDD, fivedayrunning5C, fivedayrunning12C, 
         sdl_max_snwdpth, sdl_meltout, iceon_Mean, daysicefree_Mean)

corrgram(climateMeans[,2:ncol(climateMeans)], order=F, upper.panel=panel.shade,
         lower.panel=NULL)


##Summer-only variables with all the possible variables (so, fewer years)
climateSummeralldat <-climate %>%
  select(year, sum_meanT, sum_precip, sum_moisturedeficit, sum_PET, sum_GDD, 
         fivedayrunning5C, fivedayrunning12C, 
         GSLthreedayneg3C,
         sdl_max_snwdpth, sdl_meltout, 
         iceoff_Mean, iceon_Mean, daysicefree_Mean) %>%
         na.omit()
row.names(climateSummeralldat)<-climateSummeralldat$year

corrgram(climateSummeralldat[,2:ncol(climateSummeralldat)], order=T, upper.panel=panel.shade,
         lower.panel=NULL)

sumPCA <-rda(na.exclude(climateSummeralldat[,2:ncol(climateSummeralldat)]), scale=T)
plot(sumPCA, scaling=3)
summary(sumPCA)

sumOutput<-as.data.frame(scores(sumPCA, choices=c(1,2), display=c("sites")))
sumOutput$site<-row.names(climateSummeralldat)
names(sumOutput)[1:2]=c("sumPC1", "sumPC2")


##Summer-only variables with all the possible years (so, fewer variables)
climateSummer <-climate %>%
  select(year, sum_meanT, sum_precip, sum_moisturedeficit, sum_PET, sum_GDD, 
         fivedayrunning5C, fivedayrunning12C, 
         GSLthreedayneg3C) %>%
        na.omit()
row.names(climateSummer)<-climateSummer$year

#  swemax, swemay13,
corrgram(climateSummer[,2:ncol(climateSummer)], order=T, upper.panel=panel.shade,
         lower.panel=NULL)

sumallPCA <-rda(na.exclude(climateSummer[,2:ncol(climateSummer)]), scale=T)
plot(sumallPCA, scaling=3)
summary(sumallPCA)

sumallyrsOutput<-as.data.frame(scores(sumallPCA, choices=c(1,2), display=c("sites"))) %>%
  mutate(site=row.names(climateSummer))
names(sumallyrsOutput)[1:2]=c("sumallPC1", "sumallPC2")



sumallyrsVarout<-as.data.frame(scores(sumallPCA, choices=c(1,2), display=c("species")))
sumallyrsVarout$variable<-rownames(sumallyrsVarout)

##All seasons - only variables across all years
climateAll <-climate %>%
  select(year, wnt_meanT, spr_meanT, fal_meanT, sum_meanT, wnt_precip, spr_precip, fal_precip, sum_precip, 
  sum_moisturedeficit, fal_moisturedeficit, spr_PET, fal_PET, sum_PET, sum_GDD, 
         fivedayrunning5C, fivedayrunning12C, 
          GSLthreedayneg3C) %>%
  na.omit()
row.names(climateAll) <-climateAll$year

#  swemax, swemay13,
corrgram(climateAll[,2:ncol(climateAll)], order=T, upper.panel=panel.shade,
         lower.panel=NULL) 


allPCA <-rda(na.exclude(climateAll[,2:ncol(climateAll)]), scale=T)
plot(allPCA, scaling=3)
summary(allPCA)

allOutput<-as.data.frame(scores(allPCA, choices=c(1,2), display=c("sites")))
allOutput$site=row.names(climateAll)
names(allOutput)[1:2]=c("allPC1", "allPC2")



##Put the three PCAs together
pcouts<-merge(allOutput, sumallyrsOutput, all=T)
pcouts2<-merge(pcouts, sumOutput, all=T) %>%
  mutate(year=as.numeric(site))

#creating one year lagged variables for all variables
pclags<-pcouts2 %>%
  select(year, sumallPC1, sumallPC2) %>%
  mutate(year=year+1)
names(pclags)[2:3]=c("sumallPC1_lag", "sumallPC2_lag")


#write.csv(pcouts2, "climatePCA.csv")

#Test that they are related - they are!
plot(sumPC1 ~ sumallPC1, data=pcouts2)
plot(allPC1 ~ sumPC1, data=pcouts2)
plot(allPC1 ~ sumallPC1, data=pcouts2)


plot(sumPC2 ~ sumallPC2, data=pcouts2)
plot(allPC2 ~ sumPC2, data=pcouts2)
plot(allPC2 ~ sumallPC2, data=pcouts2)

##Visualize change in the axes over time
##More summer over time (PC1 increasing over years)
names(sumallyrsVarout)[1:2]=c("sumallPC1", "sumallPC2")
variable2<-c("Temp", "Precip", "MoistureDeficit", "PET", "GDD", "DaysTo5C", "DaysTo12C", "GSL")


sumallPCA <-rda(na.exclude(climateSummer[,2:ncol(climateSummer)]), scale=T)

##A pretty version of the all years, summer axis
summer_PCA <- ggplot(sumallyrsOutput, aes(x=sumallPC1, y=sumallPC2))+ geom_text(aes(label=site), size=5) +
  geom_text(data=sumallyrsVarout, aes(x=sumallPC1, y=sumallPC2, label=variable2), size=5, color="blue") +
  geom_segment(data = sumallyrsVarout,
               aes(x = 0, xend = .9 * sumallPC1,
                   y = 0, yend = .9 * sumallPC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") + 
  #scale x and y by the minimum and maximum observed values 
  scale_y_continuous(limits=c(min(sumallyrsOutput$sumallPC2)-.2,max(sumallyrsOutput$sumallPC2)+.2)) + 
  scale_x_continuous(limits=c(min(sumallyrsOutput$sumallPC1)-.2,max(sumallyrsOutput$sumallPC1)+.2)) +
  #name axes with the proporiton of variance explained
  xlab(paste("PC1 (",sprintf("%.1f",sumallPCA$CA$eig["PC1"]/sumallPCA$tot.chi*100,3),"%)",sep="")) +
  ylab(paste("PC2 (",sprintf("%.1f",sumallPCA$CA$eig["PC2"]/sumallPCA$tot.chi*100,3),"%)",sep="")) +
  #add x and y zero lines
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=18))
  


#graph of PC1 over time
summer_overtime <- ggplot(pcouts2, aes(y=sumallPC1, x=year)) + geom_point() + geom_line() +theme_classic() + geom_smooth(method="lm") + 
  labs(x="Year", y="PC1 (Length of summer)") + theme(text=element_text(size=18)) 

l<-lm(sumallPC1~year, data=pcouts2)
summary(l)

tiff("Summer_PCA_overtime.tiff", width=1000, height=500)
grid.arrange(summer_PCA, summer_overtime, ncol=2)
dev.off()


##Other, related visuales
ggplot(pcouts2, aes(y=sumallPC2, x=year)) + geom_point() + geom_line() +theme_bw() + geom_smooth(method="lm") + 
  labs(x="Year", y="Wetness of summer (PC2)")
l<-lm(sumallPC2~year, data=pcouts2)
summary(l)
plot(sumallPCA)


ggplot(pcouts2, aes(y=allPC1, x=year)) + geom_point() + geom_line() +theme_bw() + geom_smooth(method="lm")
l<-lm(allPC1~year, data=pcouts2)
summary(l)
plot(allPCA)


ggplot(pcouts2, aes(y=allPC2, x=year)) + geom_point() + geom_line() +theme_bw() + geom_smooth(method="lm")
l<-lm(allPC2~year, data=pcouts2)
summary(l)
plot(allPCA)
