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
  summarize(abund=mean(abund)) %>%
  tbl_df() %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
  mutate(plot=paste(class_3, plot, sep="_")) 

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
