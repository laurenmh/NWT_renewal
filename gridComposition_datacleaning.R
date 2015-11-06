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
  select(year, plot, point, hit_type, USDA_code) %>%
  #Hope's species conversion key doesn't include "CAREX1-CAREX3" so renaming them all CAREX
  mutate(USDA_code=as.character(USDA_code),
         USDA_code = ifelse(USDA_code=="CAREX1", "CAREX", USDA_code),
         USDA_code = ifelse(USDA_code=="CAREX2", "CAREX", USDA_code),
         USDA_code = ifelse(USDA_code=="CAREX3", "CAREX", USDA_code))

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
  select(year, plot, point, USDA_code, USDA_name, hit_type, is_veg, category, Weber_family) %>%
  mutate(plot_point_hit = paste(plot, point, hit_type, sep="_")) %>%
  mutate(abund=1) %>%
  tbl_df() %>%
  mutate(func="Forb", func=ifelse(Weber_family=="Cyperaceae", "Sedge", func), 
         func=ifelse(Weber_family=="Poaceae", "Grass", func),
         func=ifelse(Weber_family=="Fabaceae", "Legume", func),
         func=ifelse(is_veg==0, "Nonveg", func),
         func=ifelse(USDA_code=="2GRAM", "Grass", func),
         func=ifelse(USDA_code=="2LICHN", "Lichen", func),
         func=ifelse(USDA_code=="2MOSS", "Moss", func),
         func=ifelse(USDA_code=="2UNKCU", "Forb", func),
         func=ifelse(USDA_code=="2UNKCU", "Forb", func),
         func=ifelse(USDA_code=="2UNKGM", "Nonveg", func),
         func=ifelse(USDA_code=="2UNKSC", "Nonveg", func),
         func=ifelse(USDA_code=="2UNKGS", "Nonveg", func),
         func=ifelse(USDA_code=="2UNKSC", "Nonveg", func),
         func=ifelse(category=="lichen", "Lichen", func),
         func=ifelse(category=="moss", "Moss", func))

#Assign the unknowns to functional groups
saddat %>%
  filter(Weber_family=="N/A" & func !="Nonveg") %>%
  select(USDA_code) %>%
  unique

saddat %>%
  select(category)%>%
  unique()

# Not sure what to do about the 14 points of unknown matt
#saddat %>%
 # filter(USDA_code=="2UNKMA")
