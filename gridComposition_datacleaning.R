### SADDLE GRID ANALYSIS AT THE POINT (NOT PLOT) LEVEL###

library(tidyr)
library(dplyr)
library(ggplot2)
library(codyn)

# set universal data pathway
datpath = "~/Dropbox/NWT_data/" # this likely will be different for different folks

#read in the data
rawdat <- read.csv(file.path(datpath, 'saddle_grid_point_quadrat_89-14_single_col_format_PRELIM-1.csv')) %>%
  tbl_df() %>%
  select(year, plot, point, hit_type, USDA_code) %>%
  #Hope's species conversion key doesn't include "CAREX1-CAREX3" so renaming them all CAREX
  mutate(USDA_code=as.character(USDA_code),
         USDA_code = ifelse(USDA_code=="CAREX1", "CAREX", USDA_code),
         USDA_code = ifelse(USDA_code=="CAREX2", "CAREX", USDA_code),
         USDA_code = ifelse(USDA_code=="CAREX3", "CAREX", USDA_code),
         USDA_code = ifelse(USDA_code=="CAREX4", "CAREX", USDA_code))

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
  #filter(hit_type!="middle1", hit_type!="middle2") %>%
  #filter(hit_type=="bottom") %>%
  select(year, plot, point, USDA_code, USDA_name, hit_type, is_veg, category, Weber_family) %>%
  mutate(plot_point_hit = paste(plot, point, hit_type, sep="_")) %>%
  mutate(abund=1) %>%
  tbl_df() %>%
  mutate(category=as.character(category),
         #Decided that of the 21 unknown points, most likely are vascular and forb
         category=ifelse(USDA_code=="2UNK", "vascular", category)) %>%
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
         func=ifelse(category=="moss", "Moss", func),
         func=ifelse(USDA_code=="SIACS2", "Cushion", func),
         func=ifelse(USDA_code=="MIOB2", "Cushion", func),
         func=ifelse(USDA_code=="MIRU3", "Cushion", func),
         func=ifelse(USDA_code=="PHPU5", "Cushion", func),
         func=ifelse(USDA_code=="DRBRC", "Cushion", func),
         #decided that cushion more important than legume for Trifolium nanum
         func=ifelse(USDA_code=="TRNA2", "Cushion", func),
         func=ifelse(USDA_code=="ERNA", "Cushion", func), 
         func=ifelse(Weber_family=="Salicaceae", "Woody", func))

#Assign the unknowns to functional groups
saddat %>%
  filter(Weber_family=="N/A" & func !="Nonveg") %>%
  select(USDA_code) %>%
  unique

chksaddat <- saddat %>%
  select(USDA_code, USDA_name, category, func, Weber_family)%>%
  unique()

##middle2 only hpanned 109 times, and always there was an associated middle1
saddat5 <- saddat %>%
  filter(hit_type=="middle1" | hit_type=="middle2") %>%
  select(hit_type, plot, point, year) %>%
  unique() %>%
 mutate(abund=1) %>%
  spread(hit_type, abund) %>%
  mutate(midtog= middle1 + middle2) %>%
  filter(midtog==2)

saddat6 <- saddat %>%
#  filter(hit_type=="middle1" | hit_type=="middle2") %>%
  select(hit_type, plot, point, year, func) %>%
  spread(hit_type, func) %>%
  mutate(count=1) %>%
  group_by(bottom, middle1, middle2, top) %>%
  summarize(count=sum(count))


# Not sure what to do about the 14 points of unknown matt
#saddat %>%
 # filter(USDA_code=="2UNKMA")

# #Summarize abundance at the species and functional group level
# aggdat<- merge(saddat, vegtype.key, by="plot") %>%
#   filter(is_veg > 0, category != "nonveg") %>%
#   group_by(year, plot, USDA_code, orig_cluster, class_3, category, func) %>%
#   summarize(abund=sum(abund)) %>%
#   tbl_df() 

#Summarize abundace at the species and functional level; exclude middle
aggdat <- merge(saddat, vegtype.key, by="plot") %>%
  filter(is_veg > 0, category != "nonveg") %>%
  filter(hit_type !="middle1", hit_type != "middle2") %>%
  group_by(year, plot, USDA_code, orig_cluster, class_3, category, func) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() 


#Alternatively, summarize by level
aggdat_byhittype <- merge(saddat, vegtype.key, by="plot") %>%
  filter(is_veg > 0, category != "nonveg") %>%
  group_by(year, plot, USDA_code, orig_cluster, class_3, category, func, hit_type) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() 


#create a key to merge
sampled_plots <- saddat %>%
  select(plot, year) %>%
  unique()

aggdat2<-merge(aggdat, sampled_plots, id=c("plot", "year"), all.y=T)

# a function to fill in 0s for species present in the plot but not that year
fill_zeros <- function (df, year="year", USDA_code="USDA_code", abund="abund") {
  nosp <- length(unique(df[,USDA_code]))
  df2 <- df[c(year, USDA_code, abund)] %>%
    spread(USDA_code, abund, fill=0) %>%
    gather(USDA_code, abund, 2:(nosp+1))
  return(df2)
}

# apply the fill_zeros function across aggdat2
X <- split(aggdat2, aggdat2$plot)
out <- lapply(X, FUN=fill_zeros)
ID <- unique(names(out))
out <- mapply(function(x, y) "[<-"(x, "plot", value = y) ,
              out, ID, SIMPLIFY = FALSE)
aggdat3 <- do.call("rbind", out) %>%
  tbl_df()

#how many plots had absolutely nothing ever
aggdat3 %>%
  filter(is.na(USDA_code)) %>%
  select(plot) %>%
  unique() %>%
  nrow()

sppdat0<- merge(aggdat3, sppkey) %>%
  filter(!is.na(USDA_code)) %>%
  tbl_df() 

sppdat <-merge(sppdat0, vegtype.key) %>%
  tbl_df()
