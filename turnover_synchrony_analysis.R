library(dplyr)
library(tidyr)
library(ggplot2)
library(codyn)

#Turnover and synchrony calculations for Niwot Ridge saddle data
##Use saddat from gridComposition_datacleaning

#calculate "turnover" - will only give 2 values - 1 if the species changed, 0 if it did not
#for right now, included bare as a "species" - should do a more refined analysis of transitions from bare to veg, and veg to bare
sadturn<-turnover(saddat, time.var="year", species.var = "USDA_code", abundance.var = "abund", replicate.var = "plot_point_hit")

#aggregate turnover within a plot, only use those years for which turnover was calculated for the immediate year prior
sadturn2<-merge(sadturn, saddat, all.x=T) %>%
  group_by(year, plot) %>%
  summarize(percent_change=sum(total)) %>%
  filter(year==1996 | year == 1997 | year==1990 | year == 2011 | year ==2012 | year ==2013)

sadturn2<-merge(sadturn2, vegtype.key, by="plot")

#aggregate turnover within a plot and by rough veg type, only use those years for which turnover was calculated for the immediate year prior
#percent_change now a misnomer - total, percent varies based on how much was bare versus veg initially
sadturn3<-merge(sadturn, saddat, all.x=T) %>%
  group_by(year, plot, is_veg) %>%
  summarize(percent_change=sum(total)) %>%
  filter(year==1996 | year == 1997 | year==1990 | year == 2011 | year ==2012 | year ==2013) %>%
  tbl_df()
sadturn3<-merge(sadturn3, vegtype.key, by="plot")


ggplot(sadturn2, aes(x=year, y=percent_change, group=plot)) + geom_point() + facet_wrap(~class_3)
ggplot(sadturn3, aes(x=year, y=percent_change, group=plot)) + geom_point() + facet_wrap(class_3~is_veg)

##some different synchrony metrics

#synchrony at the species level
datspp<- merge(saddat, vegtype.key, by="plot") %>%
  filter(is_veg > 0, category != "nonveg") %>%
  group_by(year, plot, USDA_code, orig_cluster, class_3, category, Weber_family) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() 

sppsynch <- synchrony(datspp, time.var="year", species.var = "USDA_code", abundance.var="abund", replicate.var="plot")
sppsynch2 <- merge(sppsynch, vegtype.key, by="plot") %>%
  mutate(type="species")
ggplot(sppsynch2, aes(x=synchrony)) + geom_bar() + facet_wrap(~orig_cluster)
ggplot(sppsynch2, aes(x=synchrony)) + geom_bar() + facet_wrap(~class_3)

#synchrony at the category level
datcat<- merge(saddat, vegtype.key, by="plot") %>%
  filter(is_veg > 0, category != "nonveg") %>%
  filter(class_3 != "SF", class_3 != "ST") %>%
  group_by(year, plot,  orig_cluster, class_3, category) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() 

ggplot(datcat, aes(x=year, y=abund, color=category,  group=interaction(plot, category))) + geom_line() + facet_wrap(~class_3)

catsynch <- synchrony(datcat, time.var="year", species.var = "category", abundance.var="abund", replicate.var="plot")
catsynch2 <- merge(catsynch, vegtype.key, by="plot") %>%
  mutate(type="category")

#ggplot(catsynch2, aes(x=synchrony)) + geom_bar() + facet_wrap(~orig_cluster)
ggplot(catsynch2, aes(x=synchrony)) + geom_bar() + facet_wrap(~class_3)


#synchrony at the Weber family level
#lots of Weber_family =="N/A", check this
datweb<- merge(saddat, vegtype.key, by="plot") %>%
  filter(is_veg > 0, category != "nonveg", !is.na(Weber_family)) %>%
  filter(class_3 != "SF", class_3 != "ST") %>%
  group_by(year, plot,  orig_cluster, class_3, Weber_family) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() 


ggplot(datweb, aes(x=year, y=abund, color=Weber_family,  group=interaction(plot, Weber_family))) + geom_line() + facet_wrap(~class_3) + theme_bw()

websynch <- synchrony(datweb, time.var="year", species.var = "Weber_family", abundance.var="abund", replicate.var="plot")
websynch2 <- merge(websynch, vegtype.key, by="plot") %>%
  mutate(type="family")

#ggplot(websynch2, aes(x=synchrony)) + geom_bar() + facet_wrap(~orig_cluster)
ggplot(websynch2, aes(x=synchrony)) + geom_bar() + facet_wrap(~class_3)

#Put all the synchrony outputs together, make a panel graph
aggsynch <- rbind(sppsynch2, catsynch2, websynch2) %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock")
ggplot(aggsynch, aes(x=synchrony)) + geom_bar() + facet_grid(type~class_3) + theme_bw()


#Put all the raw data together, make a panel graph
datspp2 <- datspp %>%
  mutate(type="species") %>%
  select(-Weber_family, - category)
names(datspp2)[3]="mylevel"

datweb2 <- datweb %>%
  mutate(type="family") 
names(datweb2)[5]="mylevel"

datcat2 <- datcat %>%
  mutate(type="category") 
names(datcat2)[5]="mylevel"

datgraph <- rbind(datspp2, datweb2, datcat2)  %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock")


ggplot(datgraph, aes(x=year, y=abund, color=mylevel, group=interaction(mylevel, plot))) + 
  geom_line() + facet_grid(type~class_3) + theme_bw() + theme(legend.position="none")
ggplot(aggsynch, aes(x=synchrony)) + geom_bar() + facet_grid(type~class_3) + theme_bw()


