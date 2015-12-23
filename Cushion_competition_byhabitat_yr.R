##Competition coefficients for focal spp in relation to cuhsion plants
## uses sppdat2 from gridComposition_datacleaning


##SNOW DATA
snowdat <- read.csv(file.path(datpath, 'NWT_SnowXprod.csv')) %>%
  tbl_df() %>%
  group_by(plot) %>%
  summarize(maxsnow=mean(max_snow)) %>%
  tbl_df() 
snowdat$ssnow<-as.numeric(scale(snowdat$maxsnow))


##CUSHION SET UP 
cushiondat <- sppdat2 %>%
  filter(func=="Cushion") %>%
  group_by(func, plot, year, class_3) %>%
  summarize(abund=sum(abund))

cushiondatprev <- cushiondat %>%
  tbl_df() %>%
  mutate(year=year+1) %>%
  mutate(prevabund=abund) %>%
  select(-abund)

cushiontog<-merge(cushiondat, cushiondatprev) %>%
  tbl_df() %>%
  mutate(cushionabund=abund, cushionprevabund=prevabund) %>%
  select(plot, year, class_3, cushionabund, cushionprevabund) %>%
  mutate(has_cushion=ifelse(cushionabund>0, 1, 0))

cushiontog2 <-merge(snowdat, cushiontog)

##OTHER SPECIES SETUP
dat <- sppdat2

datprev <- dat %>%
  tbl_df() %>%
  mutate(year=year+1) %>%
  mutate(prevabund=abund) %>%
  select(-abund)

tog <- merge(dat, datprev) %>%
  mutate(GR=(abund-prevabund)/prevabund) %>%
  select(USDA_code, year, plot, abund, prevabund, class_3, Weber_family, USDA_name) %>%
  mutate(is_present=ifelse(abund>0, 1, 0))

cushall <-merge(tog, cushiontog2) %>%
  tbl_df()  %>%
  group_by(USDA_code) %>%
  mutate(low25=quantile(maxsnow)[2],
         high25=quantile(maxsnow)[4]) %>%
  mutate(snowbin=2, snowbin=ifelse(maxsnow< low25, 1, snowbin), snowbin=ifelse(maxsnow >high25, 3, snowbin)) %>%
  mutate(spp_snow=paste(USDA_code, snowbin, sep="_")) %>%
mutate(class_3=as.character(class_3), 
       class_3=ifelse(class_3=="WM", "MM", class_3)) %>%
  filter(class_3!="SF") %>%
  mutate(habitat="DMFF", habitat=ifelse(class_3=="SB", "MMSB", habitat), 
         habitat=ifelse(class_3=="MM", "MMSB", habitat)) %>%
  tbl_df() %>%
  group_by(USDA_code, habitat) %>%
#   mutate(low25hab=quantile(maxsnow)[2],
#          high25hab=quantile(maxsnow)[4]) %>%
#   mutate(snowbinhab=2, snowbinhab=ifelse(maxsnow< low25hab, 1, snowbinhab), snowbinhab=ifelse(maxsnow >high25hab, 3, snowbinhab)) %>%
#   tbl_df() %>%
#   mutate(hab_snow=paste(habitat, snowbinhab, sep="_")) %>%
#   mutate(spp_hab_snow=paste(USDA_code, hab_snow, sep="_")) %>%
  mutate(hab50=quantile(maxsnow)[3]) %>%
  mutate(snowbinhab=2, snowbinhab=ifelse(maxsnow< hab50, 1, snowbinhab)) %>%
  tbl_df() %>%
  mutate(hab_snow=paste(habitat, snowbinhab, sep="_")) %>%
  mutate(spp_hab_snow=paste(USDA_code, hab_snow, sep="_")) %>%
  mutate(spp_hab=paste(USDA_code, habitat, sep="_"))


# cushsummary <- cushall %>%
#   group_by(USDA_code, has_cushion, USDA_name, Weber_family) %>%
#   summarize(totoccur=sum(is_present), totabund=sum(abund)) %>%
#   group_by(USDA_code, USDA_name, Weber_family) %>%
#   mutate(alloccur=sum(totoccur), allabund=sum(totabund),
#          peroccur=totoccur/alloccur, perabund=totabund/allabund) %>%
#   filter(has_cushion==1)


##SUBSET AND MODEL SPP BY ROUGH HABITAT TYPE
myspp <- c( "GEROT", "DECE", "CARUD", "CAUN2", "LLSE", "LUSP4", "TRPAP")
cushfocus <- cushall[which(cushall$USDA_code%in%myspp),]
myreps <- sort(unique(cushfocus$spp_hab))

myout <-as.data.frame(cbind(model=as.character(), spp_hab=as.character(), modelAIC=as.numeric(), lambda=as.numeric(), aii=as.numeric(), cush=as.numeric(), lambda.se=as.numeric(), aii.se=as.numeric(), cush.se=as.numeric(), lambda.p=as.numeric(), aii.p=as.numeric(), cush.p=as.numeric()))


for (i in 1:length(myreps)){
 subber = subset(cushall, spp_hab==myreps[i])
 m1o <- nlsLM(m1, start=list(lambda=1, aii=.01, cush=.01),
                control=nls.lm.control(maxiter=500), trace=T, data=subber)
  m1s <- as.data.frame(summary(m1o)$coef)
 m1report <-as.data.frame(cbind("m1", myreps[i], AIC(m1o), m1s[1,1], m1s[2,1], m1s[3, 1], m1s[1,2], m1s[2,2], m1s[3,2], m1s[1,4], m1s[2,4], m1s[3,4]))
 names(m1report)=c("model", "spp_hab", "modelAIC", "lambda", "aii", "cush", "lambda.se", "aii.se", "cush.se", "lambda.p", "aii.p", "cush.p")
 
 m4o <- nlsLM(m4, start=list(lambda=1, aii=.01, cush=.01),
              control=nls.lm.control(maxiter=500), trace=T, data=subber)
 m4s <- as.data.frame(summary(m4o)$coef)
 m4report <-as.data.frame(cbind("m4", myreps[i], AIC(m4o), m4s[1,1], m4s[2,1], m4s[3, 1], m4s[1,2], m4s[2,2], m4s[3,2], m4s[1,4], m4s[2,4], m4s[3,4]))
 names(m4report)=c("model", "spp_hab", "modelAIC", "lambda", "aii", "cush", "lambda.se", "aii.se", "cush.se", "lambda.p", "aii.p", "cush.p")
 myout<-rbind(myout, m1report, m4report)
}
 

# myoutbest<-myout 

myout2 <- myout %>%
  gather(variable, value, modelAIC:cush.p) %>%
  mutate(value=as.numeric(as.character(value))) %>%
  spread(variable, value) %>%
  mutate(spp_hab=as.character(spp_hab)) %>%
  separate(spp_hab, c("species", "habitat")) %>%
  tbl_df()


myout2 %>%
  group_by(model) %>%
  summarize(AICsum=sum(modelAIC))

myoutsig <- myout2 %>%
  filter(lambda.p<.05) %>%
  filter(model=="m4")



tiff("CushCompetition_byHabitat.tiff")
ggplot(subset(myoutsig2, model=="m4"), aes(x=lambda, y=cush)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~hab) +
  theme_bw() + theme(text = element_text(size=20)) + geom_hline(y=0) + 
  geom_text(data=subset(myoutsig2), aes(x=lambda, y=cush, label=species)) + labs(x="Lambda", y="Competition from cushion") +
  xlim(.4, 2.8) + 
  geom_errorbar(aes(x=lambda, ymin=cush-cush.se, ymax=cush+cush.se))
dev.off()
write.csv(myoutbest2, "CushCompetition_byHabitat.csv")






##JUST WITHIN MMSB
myspp <- c( "GEROT", "DECE", "CARUD", "CAUN2", "LLSE", "LUSP4", "TRPAP")
cushfocus <- cushall[which(cushall$USDA_code%in%myspp),] %>%
  filter(habitat=="MMSB")
myreps <- sort(unique(cushfocus$spp_hab_snow))

myout <-as.data.frame(cbind(model=as.character(), spp_hab_snow=as.character(), modelAIC=as.numeric(), lambda=as.numeric(), aii=as.numeric(), cush=as.numeric(), lambda.se=as.numeric(), aii.se=as.numeric(), cush.se=as.numeric(), lambda.p=as.numeric(), aii.p=as.numeric(), cush.p=as.numeric()))


for (i in 1:length(myreps)){
  subber = subset(cushall, spp_hab_snow==myreps[i])
  m1o <- nlsLM(m1, start=list(lambda=1, aii=.01, cush=.01),
               control=nls.lm.control(maxiter=500), trace=T, data=subber)
  m1s <- as.data.frame(summary(m1o)$coef)
  m1report <-as.data.frame(cbind("m1", myreps[i], AIC(m1o), m1s[1,1], m1s[2,1], m1s[3, 1], m1s[1,2], m1s[2,2], m1s[3,2], m1s[1,4], m1s[2,4], m1s[3,4]))
  names(m1report)=c("model", "spp_hab_snow", "modelAIC", "lambda", "aii", "cush", "lambda.se", "aii.se", "cush.se", "lambda.p", "aii.p", "cush.p")
  
  m4o <- nlsLM(m4, start=list(lambda=1, aii=.01, cush=.01),
               control=nls.lm.control(maxiter=500), trace=T, data=subber)
  m4s <- as.data.frame(summary(m4o)$coef)
  m4report <-as.data.frame(cbind("m4", myreps[i], AIC(m4o), m4s[1,1], m4s[2,1], m4s[3, 1], m4s[1,2], m4s[2,2], m4s[3,2], m4s[1,4], m4s[2,4], m4s[3,4]))
  names(m4report)=c("model", "spp_hab_snow", "modelAIC", "lambda", "aii", "cush", "lambda.se", "aii.se", "cush.se", "lambda.p", "aii.p", "cush.p")
  myout<-rbind(myout, m1report, m4report)
}


myoutbest2 <- myout %>%
  gather(variable, value, modelAIC:cush.p) %>%
  mutate(value=as.numeric(as.character(value))) %>%
  spread(variable, value) %>%
  mutate(spp_hab_snow=as.character(spp_hab_snow)) %>%
  separate(spp_hab_snow, c("species", "habitat", "snow")) %>%
  tbl_df()


myoutsig2 <- myoutbest2 %>%
  filter(lambda.p<.05) %>%
  filter(model=="m4")


myoutsig2 %>%
  group_by(model) %>%
  summarize(AICsum=sum(modelAIC))



ggplot(subset(myoutbest2, model=="m4"), aes(x=lambda, y=cush, color=snow)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~species) +
  theme_bw() + geom_hline(y=0) + geom_text(data=myoutsig2, aes(x=lambda, y=cush, label=species)) + labs(x="Lambda", y="Competition from cushion")




##JUST WITHIN SNOWBIN
myspp <- c( "GEROT", "DECE", "CARUD", "CAUN2", "LLSE", "LUSP4", "TRPAP")
cushfocus <- cushall[which(cushall$USDA_code%in%myspp),] %>%
  filter(spp_snow!="CAUN2_3")
myreps <- sort(unique(cushfocus$spp_snow))


myout <-as.data.frame(cbind(model=as.character(), spp_snow=as.character(), modelAIC=as.numeric(), lambda=as.numeric(), aii=as.numeric(), cush=as.numeric(), lambda.se=as.numeric(), aii.se=as.numeric(), cush.se=as.numeric(), lambda.p=as.numeric(), aii.p=as.numeric(), cush.p=as.numeric()))


for (i in 1:length(myreps)){
  subber = subset(cushall, spp_snow==myreps[i])
  m1o <- nlsLM(m1, start=list(lambda=1, aii=.01, cush=.01),
               control=nls.lm.control(maxiter=500), trace=T, data=subber)
  m1s <- as.data.frame(summary(m1o)$coef)
  m1report <-as.data.frame(cbind("m1", myreps[i], AIC(m1o), m1s[1,1], m1s[2,1], m1s[3, 1], m1s[1,2], m1s[2,2], m1s[3,2], m1s[1,4], m1s[2,4], m1s[3,4]))
  names(m1report)=c("model", "spp_snow", "modelAIC", "lambda", "aii", "cush", "lambda.se", "aii.se", "cush.se", "lambda.p", "aii.p", "cush.p")
  
  m4o <- nlsLM(m4, start=list(lambda=1, aii=.01, cush=.01),
               control=nls.lm.control(maxiter=500), trace=T, data=subber)
  m4s <- as.data.frame(summary(m4o)$coef)
  m4report <-as.data.frame(cbind("m4", myreps[i], AIC(m4o), m4s[1,1], m4s[2,1], m4s[3, 1], m4s[1,2], m4s[2,2], m4s[3,2], m4s[1,4], m4s[2,4], m4s[3,4]))
  names(m4report)=c("model", "spp_snow", "modelAIC", "lambda", "aii", "cush", "lambda.se", "aii.se", "cush.se", "lambda.p", "aii.p", "cush.p")
  myout<-rbind(myout, m1report, m4report)
}

myoutbest2 <- myout %>%
  gather(variable, value, modelAIC:cush.p) %>%
  mutate(value=as.numeric(as.character(value))) %>%
  spread(variable, value) %>%
  mutate(spp_snow=as.character(spp_snow)) %>%
  separate(spp_snow, c("species",  "snow")) %>%
  tbl_df() %>%
  mutate(equilnocush=exp(log(lambda)/aii)-1) %>%
  mutate(equilcush=exp(log(lambda)/(aii+cush))-1)


myoutbest2 %>%
  group_by(model) %>%
  summarize(AICsum=sum(modelAIC))


myoutsig2 <- myoutbest2 %>%
  filter(lambda.p<.055) %>%
  filter(model=="m4")


ggplot(subset(myoutbest2, model=="m4"), aes(x=lambda, y=cush, color=snow)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~species) +
  theme_bw() + geom_hline(y=0) + geom_text(data=subset(myoutbest2, model=="m4"), aes(x=lambda, y=cush, label=species)) + labs(x="Lambda", y="Competition from cushion")

ggplot(subset(myoutbest2, model=="m4" ), aes(x=snow, y=cush)) + geom_bar(stat='identity') + geom_smooth(method="lm") + facet_wrap(~species) +
  theme_bw() 

ggplot(subset(myoutbest2, model=="m4" & lambda<0.055), aes(x=snow, y=equilnocush)) + geom_bar(stat='identity') + geom_smooth(method="lm") + facet_wrap(~species) +
  theme_bw() 
hist(myoutbest2$equilnocush)






####ALL BY 2012 and Not 2012
cushall2 <- cushall %>%
  mutate(yr2012="no", yr2012=ifelse(year==2012, "yes", yr2012)) %>%
  mutate(spp_yr2012=paste(USDA_code, yr2012, sep="_"))


myspp <- c( "GEROT", "DECE", "CARUD", "CAUN2", "LLSE", "LUSP4", "TRPAP")
cushfocus <- cushall2[which(cushall2$USDA_code%in%myspp),]
myreps <- sort(unique(cushfocus$spp_yr2012))



myout <-as.data.frame(cbind(model=as.character(), spp_yr2012=as.character(), modelAIC=as.numeric(), lambda=as.numeric(), aii=as.numeric(), cush=as.numeric(), lambda.se=as.numeric(), aii.se=as.numeric(), cush.se=as.numeric(), lambda.p=as.numeric(), aii.p=as.numeric(), cush.p=as.numeric()))

for (i in 1:length(myreps)){
  subber = subset(cushall2, spp_yr2012==myreps[i])
#   m1o <- nlsLM(m1, start=list(lambda=1, aii=.01, cush=.01),
#                control=nls.lm.control(maxiter=500), trace=T, data=subber)
#   m1s <- as.data.frame(summary(m1o)$coef)
#   m1report <-as.data.frame(cbind("m1", myreps[i], AIC(m1o), m1s[1,1], m1s[2,1], m1s[3, 1], m1s[1,2], m1s[2,2], m1s[3,2], m1s[1,4], m1s[2,4], m1s[3,4]))
#   names(m1report)=c("model", "spp_yr2012", "modelAIC", "lambda", "aii", "cush", "lambda.se", "aii.se", "cush.se", "lambda.p", "aii.p", "cush.p")
#   
  m4o <- nlsLM(m4, start=list(lambda=1, aii=.01, cush=.01),
               control=nls.lm.control(maxiter=500), trace=T, data=subber)
  m4s <- as.data.frame(summary(m4o)$coef)
  m4report <-as.data.frame(cbind("m4", myreps[i], AIC(m4o), m4s[1,1], m4s[2,1], m4s[3, 1], m4s[1,2], m4s[2,2], m4s[3,2], m4s[1,4], m4s[2,4], m4s[3,4]))
  names(m4report)=c("model", "spp_yr2012", "modelAIC", "lambda", "aii", "cush", "lambda.se", "aii.se", "cush.se", "lambda.p", "aii.p", "cush.p")
  myout<-rbind(myout,  m4report)
}


# myoutbest<-myout 

myoutbest2 <- myout %>%
  filter(!is.na(spp_yr2012)) %>%
  gather(variable, value, modelAIC:cush.p) %>%
  mutate(value=as.numeric(as.character(value))) %>%
  spread(variable, value) %>%
  mutate(spp_yr2012=as.character(spp_yr2012)) %>%
  separate(spp_yr2012, c("species", "yr2012")) %>%
  tbl_df()


myoutsig <- myoutbest2 %>%
  filter(lambda.p<.05) %>%
  filter(model=="m4")

ggplot(myoutbest2, aes(x=lambda, y=cush)) + geom_point() + facet_wrap(~yr2012)

ggplot(myoutbest2, aes(x=log(lambda), y=cush, color=yr2012)) + geom_point() + geom_smooth(method="lm", se=F) + theme_bw()
# l<-lm(cush~log(lambda)*yr2012, data=myoutbest2)
# summary(l)


pdf("CushComp_YR2012.pdf")
ggplot(myoutbest2, aes(x=species, y=cush, fill=yr2012)) + geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(x=species, ymax=cush+cush.se, ymin=cush-cush.se, width=.2), position=position_dodge(.9)) + 
  labs(x="Species", y="Competition from cushion") + theme_bw() + theme(text=element_text(size=20))

ggplot(subset(myoutbest2, lambda.p<0.055), aes(x=species, y=lambda, fill=yr2012)) + geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(x=species, ymax=lambda+lambda.se, ymin=lambda-lambda.se, width=.2), position=position_dodge(.9))+ 
  labs(x="Species", y="Lambda") + theme_bw() + theme(text=element_text(size=20))
dev.off()

ggplot(myoutbest2, aes(x=species, y=lambda, fill=yr2012)) + geom_bar(stat="identity", position="dodge") 



ggplot(subset(myoutbest2, cush.p<0.055), aes(x=species, y=cush, fill=yr2012)) + geom_bar(stat="identity", position="dodge") 




