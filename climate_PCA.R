###Climate visualizations###
library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(corrgram)
library(grid)
library(gridExtra)

## Read in the climate data 
climate<-read.csv("~/Dropbox/NWT_data/NWT_ClimateData_2015-11-02.csv")
colnames(climate)[1]<-"year"
climate <- tbl_df(climate)

##Quick visualization
##Look at just the annual mean variables across terrestrial and aquatic
climateMeans <-climate %>%
  select(year, MAT, tot_precip, moisturedeficit, PET, GDD, fivedayrunning5C, fivedayrunning12C, 
         sdl_max_snwdpth, sdl_meltout, iceon_Mean, daysicefree_Mean)

corrgram(climateMeans[,2:ncol(climateMeans)], order=F, upper.panel=panel.shade,
         lower.panel=NULL)

###################
###LAGGED PRECIP####
###################
## Creating one year lagged variables for all variables
climatelags<-cbind(climate,as.data.frame(matrix(NA,nrow=33,ncol=68)))
for (i in 1:68){
  vars<-colnames(climate)[2:69]
  climatelags[,i+69]<-c(NA,climate[,vars[i]][1:32])
}
colnames(climatelags)[70:137]<-paste(vars,"_lag",sep = "")


###################
###PCA ANALYSES####
###################

##THE PCA FOR SUBSEQUENT ANALYSIS: SUMMER-RELATED VARIABLES WITH LONG-TIME SERIES
##Summer-only variables with all the possible years (so, fewer variables)
climateSummer <-climate %>%
  select(year, sum_meanT, sum_precip, sum_moisturedeficit, sum_PET, sum_GDD, 
         fivedayrunning5C, fivedayrunning12C, 
         GSLthreedayneg3C, iceoff_GL4) %>%
        na.omit()
row.names(climateSummer)<-climateSummer$year

# Visualize with a correlogram
corrgram(climateSummer[,2:ncol(climateSummer)], order=T, upper.panel=panel.shade,
         lower.panel=NULL)

# Make the summer all-years PCA output dataframe 
sumallPCA <-rda(na.exclude(climateSummer[,2:ncol(climateSummer)]), scale=T)
plot(sumallPCA, scaling=3)
summary(sumallPCA)

#Make the summer all years output (year scores)
sumallyrsOutput<-as.data.frame(scores(sumallPCA, choices=c(1,2), display=c("sites"))) %>%
  mutate(site=row.names(climateSummer))
names(sumallyrsOutput)[1:2]=c("sumallPC1", "sumallPC2")

# Make the summer all years output (variable scores)
sumallyrsVarout<-as.data.frame(scores(sumallPCA, choices=c(1,2), display=c("species")))
sumallyrsVarout$variable<-rownames(sumallyrsVarout)


##THE FIRST PCA TO CROSS-CHECK TO SUMMER MODEL
##Summer-only variables with all the possible variables (so, fewer years)
climateSummeralldat <-climate %>%
  select(year, sum_meanT, sum_precip, sum_moisturedeficit, sum_PET, sum_GDD, 
         fivedayrunning5C, fivedayrunning12C, 
         GSLthreedayneg3C,
         sdl_max_snwdpth, sdl_meltout, 
         iceoff_Mean, iceon_Mean, daysicefree_Mean) %>%
  na.omit()
row.names(climateSummeralldat)<-climateSummeralldat$year

# Visualize with a correlogram
corrgram(climateSummeralldat[,2:ncol(climateSummeralldat)], order=T, upper.panel=panel.shade,
         lower.panel=NULL)

# Make the summer PCA (all the variables, fewer years)
sumPCA <-rda(na.exclude(climateSummeralldat[,2:ncol(climateSummeralldat)]), scale=T)
plot(sumPCA, scaling=3)
summary(sumPCA)

# Make the summer PCA output dataframe 
#will use this to check that the PCA with all the years is representative 
sumOutput<-as.data.frame(scores(sumPCA, choices=c(1,2), display=c("sites")))
sumOutput$site<-row.names(climateSummeralldat)
names(sumOutput)[1:2]=c("sumPC1", "sumPC2")

##THE SECOND PCA TO CROSS-CHECK TO SUMMER MODEL
##All seasons - only variables across all years
climateAll <-climate %>%
  select(year, wnt_meanT, spr_meanT, fal_meanT, sum_meanT, wnt_precip, spr_precip, fal_precip, sum_precip, 
  sum_moisturedeficit, fal_moisturedeficit, spr_PET, fal_PET, sum_PET, sum_GDD, 
         fivedayrunning5C, fivedayrunning12C, 
          GSLthreedayneg3C) %>%
  na.omit()
row.names(climateAll) <-climateAll$year

# Visualize with correlogram
corrgram(climateAll[,2:ncol(climateAll)], order=T, upper.panel=panel.shade,
         lower.panel=NULL) 

# Make the all-variables, all-years PCA output dataframe 
allPCA <-rda(na.exclude(climateAll[,2:ncol(climateAll)]), scale=T)
plot(allPCA, scaling=3)
summary(allPCA)

allOutput<-as.data.frame(scores(allPCA, choices=c(1,2), display=c("sites")))
allOutput$site=row.names(climateAll)
names(allOutput)[1:2]=c("allPC1", "allPC2")


##PUT THE THREE PCAS TOGETHER
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

####EXPORTED VISUALS#######
###USE THE ALL-YRS SUMMER PCA TO CHARACTERIZE CHANGE IN SUMMER OVER TIME
##Visualize change in the axes over time
##More summer over time (PC1 increasing over years)
#Clean up dataframe for visualizes
names(sumallyrsVarout)[1:2]=c("sumallPC1", "sumallPC2")
variable2<-c("Temp", "Precip", "MoistureDeficit", "PET", "GDD", "DaysTo5C", "DaysTo12C", "GSL", "IceOff")
sumallPCA <-rda(na.exclude(climateSummer[,2:ncol(climateSummer)]), scale=T)


## -- CW edits to PC1 over time for draft 2
## [1] add in Eric Sokol's theme
# -- plotting parameters and themes
text.size<-16
margins.plot<-unit(c(1,0.5,0.5,2.5), 'lines') #top, right, bottom, left
margins.axes<-unit(.25,'lines')
margins.panel<-unit(3,'lines')

plottheme<-theme(plot.margin = margins.plot,
                 axis.ticks.margin = margins.axes,
                 axis.title = element_text(face='plain'))


##Make a pretty PCA visual of the all years, summer axis
##CW edit [2]: change axis labels, remove blue labels (will place manually), add theme
summer_PCA <- ggplot(sumallyrsOutput, aes(x=sumallPC1, y=sumallPC2))+ geom_text(aes(label=site), size=5) +
  #geom_text(data=sumallyrsVarout, aes(x=sumallPC1, y=sumallPC2, label=variable2), size=5, color="blue") +
  geom_segment(data = sumallyrsVarout,
               aes(x = 0, xend = .9 * sumallPC1,
                   y = 0, yend = .9 * sumallPC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") + 
  #scale x and y by the minimum and maximum observed values 
  scale_y_continuous(limits=c(min(sumallyrsOutput$sumallPC2)-.2,max(sumallyrsOutput$sumallPC2)+.2)) + 
  scale_x_continuous(limits=c(min(sumallyrsOutput$sumallPC1)-.2,max(sumallyrsOutput$sumallPC1)+.2)) +
  #name axes with the proporiton of variance explained
  #xlab(paste("PC1 (",sprintf("%.1f",sumallPCA$CA$eig["PC1"]/sumallPCA$tot.chi*100,3),"%)",sep="")) +
  #ylab(paste("PC2 (",sprintf("%.1f",sumallPCA$CA$eig["PC2"]/sumallPCA$tot.chi*100,3),"%)",sep="")) +
  labs(x="PC1 (Extended Summer)", y="PC2 (precipitation quantity") +
  #add x and y zero lines
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  #theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=18))
  theme_classic() +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size), 
              axis.text.y = element_text(size=text.size)) +
        plottheme
        
#Make a graph of PC1 over time
summer_overtime <- ggplot(pcouts2, aes(y=sumallPC1, x=year)) + geom_point() + geom_line() +theme_classic() + geom_smooth(method="lm") + 
  labs(x="Year", y="PC1 (Length of summer)") + theme(text=element_text(size=18)) 

l<-lm(sumallPC1~year, data=pcouts2)
summary(l)

#Export the summer PCA and change over time
tiff("Summer_PCA_overtime.tiff", width=1000, height=500)
grid.arrange(summer_PCA, summer_overtime, ncol=2)
dev.off()


## CW edit [3] -- remake Lauren's PC1 over time figure with color and thematic adjustments
summer_overtime_v2 <- ggplot(pcouts2, aes(y=sumallPC1, x=year)) + 
        geom_point(size=4, color="black") + 
        geom_point(size=3, color="cyan3") +
        geom_line(size=1) +
        theme_classic() + 
        geom_smooth(method="lm", color="black") + 
        labs(x="Year", y="PC1 (Extended Summer)") +
        scale_x_continuous(breaks=seq(from=1980, to=2015, by=5)) +
        theme_classic() +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size), 
              axis.text.y = element_text(size=text.size)) +
        plottheme




##Other, related visuales
ggplot(pcouts2, aes(y=sumallPC2, x=year)) + geom_point() + geom_line() +theme_bw() + geom_smooth(method="lm") + 
  labs(x="Year", y="Wetness of summer (PC2)")
l<-lm(sumallPC2~year, data=pcouts2)
summary(l)
plot(sumallPCA)



##LOOK AT RELATIONSHIP BETWEEN SNOW MELT OUT AND SNOW DEPTH ACROSS HABITAT TYPES

## Climate without Green Lakes
row.names(climate) <-climate$year
climateA<-climate %>%
  select(-c(GSLthreeday0C:daysicefree_Mean))
str(climateA)

## Climate without snow melt
climateB <-climateA %>%
  filter(year>1992) %>%
  arrange(year) 
row.names(climateB)=climateB$year

##Snow melt out over time and habitats
##This is cool - shows that the snow melt across the terrestrial habitats types are overall synchronous
##But that on the second PCA axis there is spread across habitat types (DM and FF vs SB, MM, WM)

climateMeltout <-climateA %>%
  filter(year>1992) %>%
  arrange(year) %>%
  select(year, sdl_max_snwdpth:sb_meltout) %>%
  select(-sdl_max_snwdpth, -sdl_meltout)
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


#CW edit [4] -- add in ice out date by year and by PC1 with elevation shown (Fig 5 in renewal)
iceout<-read.csv("~/Dropbox/NWT_data/NWT_GreenLakes_data/ice, climate and flow_D1_Dan.csv")
elevation <- read.csv("~/Dropbox/NWT_data/iceout_lakes_elev.csv")

climate_iceout <- iceout[,c(1,3:9)]
climate_iceout <- climate_iceout %>%
        gather(Key, DaysfromApril, -year)
climate_iceout <- merge(climate_iceout, elevation, by ="Key")

#merge PCA scores with iceout climate data
sumallyrsOutput <- rename(sumallyrsOutput, year=site)
climate_iceout <- merge(climate_iceout, sumallyrsOutput, by="year")

#plotting ice out by year
IceoutPanel <- ggplot(climate_iceout, aes(year, DaysfromApril, color=Elevation_m)) + 
        geom_point(size=4, color="black") +
        geom_point(size=3) + 
        scale_color_gradientn(colours=terrain.colors(7), name="Elevation (m)") +
        geom_smooth(method="lm", color="black") +
        scale_x_continuous(breaks=seq(from=1980, to =2015, by=5)) +
        labs(x="Year", y="Ice-off Date (days from April 1)") +
        theme_classic() +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size), 
              axis.text.y = element_text(size=text.size)) +
        plottheme

#plotting ice out by PC1 -- 1/6/16: Katie wants to use this over ice out vs. year
Iceout_byPC1 <- ggplot(climate_iceout, aes(sumallPC1, DaysfromApril, color=Elevation_m)) + 
        geom_point(size=4, color="black") +
        geom_point(size=3) + 
        scale_color_gradientn(colours=terrain.colors(7), name="Elevation (m)") +
        geom_smooth(method="lm", color="black") +
        #scale_x_continuous(breaks=seq(from=1980, to =2015, by=5)) +
        labs(x="PC1 (Extended Summer)", y="Ice-off Date (days from April 1)") +
        theme_classic() +
        theme(text = element_text(size = text.size), 
              axis.text.x = element_text(size=text.size), 
              axis.text.y = element_text(size=text.size),
              legend.position="none") +
        plottheme

#print 3-panel plot        
library(cowplot)
library(gridExtra)

#3 panel plot from draft 1 (PC1 x PC2, PC1 over time, Ice out by year)
multiplot <- plot_grid(summer_PCA, summer_overtime_v2, IceoutPanel,
                       nrow=1, 
                       ncol=3,
                       align="h",
                       rel_widths = c(1,1,1.2),
                       labels=c("A", "B", "C"))

#save_plot('Fig5_revised.pdf', multiplot,
#          base_height = 8,
#          base_aspect_ratio = 2.5)


#3 panel plot with Ice out by PC1 substituted in
multiplot_wPC1 <- plot_grid(summer_PCA, summer_overtime_v2, Iceout_byPC1,
                       nrow=2, 
                       ncIcol=3,
                       align="h",
                       rel_widths = c(1,1,1.2))

#save_plot('Fig5_revised.pdf', multiplot,
#          base_height = 8,
#          base_aspect_ratio = 2.5)


#3 panel plot with 2 rows, PC1 by year on bottom -- Katie uses this for draft 2
Fig5 <- ggdraw() +
        draw_plot(summer_PCA, 0, .5, .5, .5) +
        draw_plot(Iceout_byPC1, .5, .5, .5, .5) +
        draw_plot(summer_overtime_v2, 0, 0, 1, .5)

save_plot('Fig5_revised_2.pdf', Fig5,
          base_height = 11,
          base_aspect_ratio = 1.3)