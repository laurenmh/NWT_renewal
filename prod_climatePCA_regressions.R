##Uses pcouts2 from climate_PCA file
##alternatively, read in as
#pcouts2 <- read.csv("climatePCA.csv")

prod<-read.csv("~/Dropbox/NWT_data/NWT_SnowXProdCorrected.csv")
head(prod)
prod %>% group_by(class_3,year) %>% summarise(count = n()) %>% print.data.frame()
#in 1997 only a subset of plots were done, this included only 1FF, 2DM, 2WM. Thus the distribution of plots among community types are not the same as in other years. 
#So when doing averages, it biases toward the high biomass values of MM. I think I will delete 1997 in some analyses

prodclim <-merge(prod, pcouts2, by="year") %>%
  tbl_df() 
ggplot(prodclim, aes(x=sumallPC1, y=anpp)) + geom_point() + facet_wrap(~class_3, scale="free")  + geom_smooth(method="lm")
ggplot(prodclim, aes(x=sumPC1, y=anpp)) + geom_point() + facet_wrap(~class_3, scale="free")  + geom_smooth(method="lm")
ggplot(prodclim, aes(x=allPC1, y=anpp)) + geom_point() + facet_wrap(~class_3, scale="free")  + geom_smooth(method="lm")

l<-lm(anpp~sumallPC1, subset(prodclim, class_3=="SB"))
summary(l)

#take means, remove years with very low sample sizes
prodmean <- prodclim %>%
  group_by(class_3, year) %>%
  mutate(count=n()) %>%
  mutate(toremove=0,
         toremove=ifelse(count< 10 & class_3=="DM", 1, toremove),
         toremove=ifelse(year==1997 | year==2011 & class_3=="SB", 1, toremove),
         toremove=ifelse(count<7 & class_3=="FF", 1, toremove)) %>%
  filter(toremove==0) %>%
  tbl_df() %>%
  mutate(class_3=as.character(class_3)) %>%
  mutate(class_3=ifelse(class_3=="WM", "MM", class_3)) %>%
  group_by(class_3, sumallPC1, sumPC1, sumallPC2, sumPC2, allPC1, allPC2, year) %>%
  summarize(meananpp=mean(anpp), seanpp=sd(anpp)/sqrt(length(anpp)))
#
# ggplot(prodmean, aes(x=sumallPC1, y=meananpp)) + geom_point() + facet_wrap(~class_3, scale="free")  + geom_smooth(method="lm") +
#   geom_errorbar(aes(x=sumallPC1, ymin=meananpp-seanpp, ymax=meananpp + seanpp))
# 
# ggplot(prodmean, aes(x=sumallPC1, y=meananpp, color=class_3)) + geom_point()   + geom_smooth(method="lm", se=F) +
#   geom_errorbar(aes(x=sumallPC1, ymin=meananpp-seanpp, ymax=meananpp + seanpp)) #+ facet_wrap(~class_3, scale="free")

## I think this is the best graph for differences in productivity versus summer length
ggplot(prodmean, aes(x=sumallPC1, y=meananpp)) + geom_point() + facet_wrap(~class_3, scale="free", ncol=1)  + geom_smooth(method="lm") +
  geom_errorbar(aes(x=sumallPC1, ymin=meananpp-seanpp, ymax=meananpp + seanpp)) +
  theme_classic() + theme(strip.background = element_blank(), text=element_text(size=16)) +
  labs(x="PC1 (Length of summer)", y = expression(paste("Mean ANPP (g/m"^2, ")")))

l<-lm(meananpp~sumallPC1, subset(prodmean, class_3=="DM"))
summary(l)
l<-lm(meananpp~sumallPC1, subset(prodmean, class_3=="SB"))
summary(l)

##Graph by PC2 as well
#remember here that the larger PC2 values are drier years
ggplot(prodmean, aes(x=sumallPC2, y=meananpp)) + geom_point() + facet_wrap(~class_3, scale="free")  + geom_smooth(method="lm") +
  geom_errorbar(aes(x=sumallPC2, ymin=meananpp-seanpp, ymax=meananpp + seanpp))

ggplot(subset(prodmean, year!=1993), aes(x=sumallPC2, y=meananpp)) + geom_point() + facet_wrap(~class_3, scale="free")  + geom_smooth(method="lm") +
  geom_errorbar(aes(x=sumallPC2, ymin=meananpp-seanpp, ymax=meananpp + seanpp))

l<-lm(meananpp~sumallPC2, subset(prodmean,  year!=1993 & class_3=="MM"))
summary(l)
l<-lm(meananpp~sumallPC2, subset(prodmean,  year!=1993 & class_3=="SB"))
summary(l)

ggplot(prodclim, aes(x=sumallPC2, y=anpp)) + geom_point() + facet_wrap(~class_3, scale="free") + geom_smooth(method="lm")

