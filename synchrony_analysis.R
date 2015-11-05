

#synchrony at the species level
datspp<- merge(saddat, vegtype.key, by="plot") %>%
  filter(is_veg > 0, category != "nonveg") %>%
  group_by(year, plot, USDA_code, orig_cluster, class_3, category, Weber_family) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() 

#synchrony at the category level
datcat<- merge(saddat, vegtype.key, by="plot") %>%
  filter(is_veg > 0, category != "nonveg") %>%
  filter(class_3 != "SF", class_3 != "ST") %>%
  group_by(year, plot,  orig_cluster, class_3, category) %>%
  summarize(abund=sum(abund)) %>%
  tbl_df() 

library(corrgram)
corrgram(mtcars, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Car Milage Data (unsorted)")
data(mtcars)
head(mtcars)

corsppdat <- datspp %>%
  group_by(plot, USDA_code, class_3) %>%
  summarize(abund=mean(abund)) %>%
  tbl_df() %>%
  spread(USDA_code, abund, fill=0) %>%
  tbl_df()
corrgram(corsppdat[,3:113])


corgeumdat <- datspp %>%
  group_by(year, plot, USDA_code, class_3) %>%
  summarize(abund=mean(abund)) %>%
  tbl_df() %>%
  filter(USDA_code=="DECE") %>%
  spread(plot, abund, fill=0) %>%
  tbl_df()
corrgram(corgeumdat[,4:47])
