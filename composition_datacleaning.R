library(tidyr)
library(dplyr)
library(ggplot2)

datpath = "~/Dropbox/NWT_data/" # this likely will be different for different folks

#read in the data
rawdat <- read.csv(file.path(datpath, 'saddle_point_quadrat_89-13_matrix_no_middle_hits_PRELIM.csv')) 

#preliminary cleaning to remove unwanted columns, gather species
dat <- rawdat %>%
  tbl_df() %>%
  #remove the aggregate columns
  select(-c(X2BARE:X2X)) %>%
  
  #gather species
  gather(species, abundance, ALGE:VILA10) %>%
  
  #remove NA
  filter(!is.na(abundance))

#check if all plots had some species
spp_present <- dat %>%
  select(plotseq, plot, year) %>%
  unique() %>%
  nrow()

all_plots <-rawdat %>%
  select(plotseq, plot, year) %>%
  unique() %>%
  nrow()
  
spp_present==all_plots
spp_present
all_plots

#they did not, so create a key to merge
sampled_plots <- rawdat %>%
  select(plotseq, plot, year) %>%
  unique()

dat2<-merge(dat, sampled_plots, id=c("plotseq", "plot", "year"), all.y=T)

# a function to fill in 0s for species present in the plot but not that year
fill_zeros <- function (df, year="year", species="species", abundance="abundance") {
 nosp <- length(unique(df[,species]))
   df2 <- df[c(year, species, abundance)] %>%
    spread(species, abundance, fill=0) %>%
    gather(species, abundance, 2:(nosp+1))
    return(df2)
}

# apply the fill_zeros function across dat2
X <- split(dat2, dat2$plot)
out <- lapply(X, FUN=fill_zeros)
ID <- unique(names(out))
out <- mapply(function(x, y) "[<-"(x, "plot", value = y) ,
              out, ID, SIMPLIFY = FALSE)
dat3 <- do.call("rbind", out) %>%
  tbl_df()

#how many plots had absolutely nothing ever
dat3 %>%
  filter(is.na(species)) %>%
  select(plot) %>%
  unique() %>%
  nrow()

dat4<- dat3 %>%
filter(!is.na(species))

#read in species key
sppkey <- read.csv(file.path(datpath, 'pspecies.mw.data.csv')) %>%
  tbl_df() %>%
  mutate(species=USDA_code) %>%
  select(species, USDA_family, category)

myspp <- unique(dat4$species)
sppkey2<-sppkey[which(myspp%in%sppkey$species),] %>%
  tbl_df()

#merge with composition data
dat5 <- merge(dat4, sppkey2, id="species", all.x=T)


#read in plot-level data
plotkey <- read.csv(file.path(datpath, 'NWT_Saddle_ComType.csv')) %>%
  tbl_df() 

plotkey2 <-plotkey %>%
  select(plot, class_3)

#merge with composition data
dat6 <-merge(dat5, plotkey2)

#aggregate by family
famdat <- dat6 %>%
  group_by(USDA_family, plot, year, class_3) %>%
  summarize(abundance=sum(abundance))

ggplot(famdat, aes(x=year, y=abundance, color=USDA_family)) + geom_line(facet_wrap(~class_3)
