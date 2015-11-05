### SADDLE GRID ANALYSIS AT THE POINT (NOT PLOT) LEVEL###

library(tidyr)
library(dplyr)
library(ggplot2)
library(codyn)

# set universal data pathway
datpath = "~/Dropbox/NWT_data/" # this likely will be different for different folks

#read in the data
rawdat <- read.csv(file.path(datpath, 'saddle_grid_point_quadrat_89-13.csv')) %>%
  tbl_df() %>%
  select(year, plot, point, hit_type, USDA_code)

#create a unique species key
sppkey<-read.csv(file.path(datpath, "pspecies.mw.data.csv")) %>%
  tbl_df() %>%
  select(is_veg, USDA_code, USDA_name, Weber_family, category) %>%
  unique()

#create a vegetation type key
vegtype.key <- read.csv(file.path(datpath, 'NWT_Saddle_ComType.csv')) %>%
  tbl_df() %>%
  select(plot, orig_cluster, class_3)

#gather saddle data
#we should always remove middle points because they were inconsistently collected
#for now I am removing all but the bottom, because there aren't always top hits 
#will need to fill those in as 0s in future, or find another way to think about them
saddat<- merge(rawdat, sppkey, all.x=T) %>%
  filter(hit_type!="middle1", hit_type!="middle2") %>%
 filter(hit_type=="bottom") %>%
  select(year, plot, point, USDA_code, USDA_name, hit_type, is_veg) %>%
  mutate(plot_point_hit = paste(plot, point, hit_type, sep="_")) %>%
  mutate(abund=1)

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
  filter(year==1996 | year == 1997 | year==1990 | year == 2011 | year ==2012 | year ==2013)
sadturn3<-merge(sadturn3, vegtype.key, by="plot")


ggplot(sadturn2, aes(x=year, y=percent_change, group=plot)) + geom_point() + facet_wrap(~class_3)
ggplot(sadturn3, aes(x=year, y=percent_change, group=plot)) + geom_point() + facet_wrap(class_3~is_veg)

