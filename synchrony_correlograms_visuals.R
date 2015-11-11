library(corrgram)

#Correlograms by functional group to visualize patterns of synchrony across NWT
#Highlights whether different habitat types have synchronous or asynchronous responses within functional groups

##Use saddat from gridComposition_datacleaning

#Summarize abundance at the species and functional group level
aggdat<- merge(saddat, vegtype.key, by="plot") %>%
  filter(is_veg > 0, category != "nonveg") %>%
  group_by(year, plot, USDA_code, orig_cluster, class_3, category, func) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() 

#Summarize abundance at the functional group level
corfuncdat <- aggdat %>%
  group_by(year, plot, func, class_3) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
  mutate(plot=paste(class_3, plot, sep="_")) 

# A big comparison of grass versus forb - too big to be practical
# corgrassforb <- aggdat %>%
#   group_by(year, plot, func, class_3) %>%
#   summarize(abund=mean(abund)) %>%
#   tbl_df() %>%
#   filter(func=="Forb"| func=="Grass") %>%
#   mutate(plot=paste(func, class_3, plot, sep="_")) %>%
#   filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
#     select(plot, abund, year) %>%
#     spread(plot, abund, fill=0) %>%
#     tbl_df()


# corrgram(corgrassforb[,2:ncol(corgrassforb)], order=NULL, upper.panel=panel.shade,
#          lower.panel=NULL))


#Create a pdf of correlograms by functional group
pdf("Correlograms_byFunctionalGroup.pdf")
funcgroups<-unique(corfuncdat$func)[-7]

for (i in 1:length(funcgroups)) {
  subfunc = funcgroups[i]
  subdat<-corfuncdat[which(corfuncdat$func==subfunc),] %>%
    select(-class_3) %>%
    spread(plot, abund, fill=0) %>%
    tbl_df()

  corrgram(subdat[,3:ncol(subdat)], order=NULL, upper.panel=panel.shade,
         lower.panel=NULL,
         main=subfunc)
}

dev.off()
 

##Look at synch over time across functional group and habitat (averaged within habitat)
meanfuncdat <- aggdat %>%
  filter(func !="Nonveg") %>%
  group_by(year, plot, func, class_3) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() %>%
  group_by(year, func, class_3) %>%
  summarize(abund=mean(abund)) %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") 

pdf("Correlograms_meanFuncHabitat.pdf")
funcdatmean <- meanfuncdat %>%
  mutate(plot=paste( func,class_3, sep="_")) %>%
  tbl_df() %>%
  select(year, abund, plot) %>%
  spread(plot, abund, fill=0)

 corrgram(funcdatmean[,2:ncol(funcdatmean)], order=NULL, upper.panel=panel.shade,
          lower.panel=NULL, main="Correlations over time by functional group")

 ## and now do the same but group primarily by habitat type
 funcdatmean2 <- meanfuncdat %>%
   mutate(plot=paste( class_3, func,sep="_")) %>%
   tbl_df() %>%
   select(year, abund, plot) %>%
   spread(plot, abund, fill=0)
 
 corrgram(funcdatmean2[,2:ncol(funcdatmean2)], order=NULL, upper.panel=panel.shade,
          lower.panel=NULL, main="Correlations over time by habitat type")

 dev.off()
 
 ##Look at synch over time across functional group and habitat (averaged within habitat)
 meanfuncdat <- aggdat %>%
   filter(func !="Nonveg") %>%
   group_by(year, plot, func, class_3) %>%
   summarize(abund=sum(abund)) %>%
   tbl_df() %>%
   group_by(year, func, class_3) %>%
   summarize(abund=mean(abund)) %>%
   filter(class_3 != "SF", class_3 !="ST", class_3!="rock") 
 
 funcdatmean <- meanfuncdat %>%
   mutate(plot=paste( func,class_3, sep="_")) %>%
   tbl_df() %>%
   select(year, abund, plot) %>%
   spread(plot, abund, fill=0)
 
 corrgram(funcdatmean[,2:ncol(funcdatmean)], order=NULL, upper.panel=panel.shade,
          lower.panel=NULL)
 
 ## and now do the same but group primarily by habitat type
 funcdatmean2 <- meanfuncdat %>%
   mutate(plot=paste( class_3, func,sep="_")) %>%
   tbl_df() %>%
   select(year, abund, plot) %>%
   spread(plot, abund, fill=0)
 
 corrgram(funcdatmean2[,2:ncol(funcdatmean2)], order=NULL, upper.panel=panel.shade,
          lower.panel=NULL)
 
 
 
 #Create a pdf of correlograms by functional group averaged within habitat type
 pdf("Correlograms_byMeanFunctionalGroup.pdf")
 funcgroups<-unique(corfuncdat$func)[-7]
 
 for (i in 1:length(funcgroups)) {
   subfunc = funcgroups[i]
   subdat<-corfuncdat[which(corfuncdat$func==subfunc),] %>%
     group_by(class_3, year) %>%
     summarize(abund=mean(abund)) %>%
     spread(class_3, abund, fill=0) %>%
     tbl_df()
   
   corrgram(subdat[,2:ncol(subdat)], order=NULL, upper.panel=panel.shade,
            lower.panel=NULL,
            main=subfunc)
 }
 
 dev.off()
 

 
##and now look at aggregate cover across habitat types
 cortotdat <- aggdat %>%
   filter(func !="Nonveg") %>%
   group_by(year, plot,  class_3) %>%
   summarize(abund=sum(abund)) %>%
   tbl_df() %>%
   filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
   mutate(plot=paste(class_3, plot, sep="_"))  %>%
   select(-class_3) %>%
   spread(plot, abund, fill=0) %>%
   tbl_df()
 
 pdf("corr_totcover.pdf")
 corrgram(cortotdat[,2:ncol(cortotdat)], order=NULL, upper.panel=panel.shade,
          lower.panel=NULL)
 dev.off()
 
 cortotdat <- aggdat %>%
   filter(func !="Nonveg") %>%
   group_by(year, plot,  class_3) %>%
   summarize(abund=sum(abund)) %>%
   tbl_df() %>%
   group_by(year, class_3) %>%
   summarize(abund=mean(abund)) %>%
   filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
   #mutate(plot=paste(class_3, plot, sep="_"))  %>%
  # select(-class_3) %>%
   spread(class_3, abund, fill=0) %>%
   tbl_df()
 
 pdf("corr_totcover_mean.pdf")
 corrgram(cortotdat[,2:ncol(cortotdat)], order=NULL, upper.panel=panel.shade,
          lower.panel=NULL)
 dev.off()
 
 #synchrony at the species level
corsppdat <- aggdat %>%
  group_by(year, plot, USDA_code, class_3) %>%
  summarize(abund=mean(abund)) %>%
  tbl_df() %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
  mutate(plot=paste(class_3, plot, sep="_")) 


#look at some example species, GEROT and DECE, etc...
subsppdat<-corsppdat %>%
  filter(USDA_code=="DECE") %>%
  select(-class_3) %>%
  spread(plot, abund, fill=0) %>%
  tbl_df()


corrgram(subsppdat[,3:ncol(subsppdat)], order=NULL, upper.panel=panel.shade,
         lower.panel=NULL)
