##Megan: Run the code fully through for the gridComposition_datacleaning.R file
#Then the following should give the means and variances at different levels of aggregation
library(tidyr)
library(dplyr)

#AT THE LEVEL OF THE FUNCTIONAL GROUP
#Returns a dataframe with the functional group, the habitat type (class_3), 
#the temporal mean of the summed functional group cover (functempmean),
#the temporal variance of the summed functional group cover (functempvar)
funclevel<-sppdat2 %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
  mutate(class_3=as.character(class_3), class_3=ifelse(class_3=="WM", "MM", class_3)) %>%
  group_by(year, func, class_3) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  group_by(func, class_3) %>%
  summarize(functempmean=mean(abund), functempvar=var(abund)) %>%
  tbl_df()

#AT THE LEVEL OF THE HABITAT GROUP
#Returns a dataframe with the the habitat type (class_3), 
#the temporal mean of the summed cover (habtempmean),
#the temporal variance of the summed (habtempvar)
habitatlevel<-sppdat2 %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
  mutate(class_3=as.character(class_3), class_3=ifelse(class_3=="WM", "MM", class_3)) %>%
  group_by(year, class_3) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  group_by(class_3) %>%
  summarize(habtempmean=mean(abund), habtempvar=var(abund))
  

#AT THE LEVEL OF THE RIDGE 
#R
#the temporal mean of the summed cover across the ridge (ridgetempmean),
#the temporal variance of the summed cover across the ridge (ridgetempvar)
ridgelevel <-sppdat %>%
  filter(class_3 != "SF", class_3 !="ST", class_3!="rock") %>%
  group_by(year) %>%
  mutate(abund=sum(abund)) %>%
  tbl_df() %>%
  summarize(ridgetempmean=mean(abund), ridgetempvar=var(abund))


