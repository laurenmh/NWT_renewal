###  COMPUTE WILLMOTT / THORTHWAITE AET USING *DAILY* DATA
###  MODIFIED FROM FORTRAN PROGRAM 'WATBUG' BY C. WILLMOTT
###  ftp://climate.geog.udel.edu/pub/software/watbug
###  THIS VERSION WRITTEN BY DANIEL GAVIN, University of Oregon, March 9, 2009.

###  The program will interpolate to fill in any missing values.
###  COPY AND PASTE THE FUNCTION BELOW INTO R.  SPECIFY MONTHLY LAPSE RATES IF NEEDED.
###  THEN, run as:  aet_results <- aet()

###  REQUIRED INPUTS:
###  1.  A dialog box will appear, where you can choose an HCN-formatted (and comma-delimited)
###  input file (see example).
###  2.  The program adjusts for elevation between the study site of interest and the elevation
###  of the climate station.
###  In the call of the program, you need to specify the values for:
###  station.elev and site.elev (elevations of the climate data station and study site in meters)
###  lapse = array of 12 monthly temperature lapse rates in deg C/km.
###  fc = field capacity of the soil (in mm)
#100mm, Global Environment Change: Remote Sensing and GIS Perspectives edited by R.B. Singh (thin rocky soils of the Front Range)
#150mm, http://climhy.lternet.edu/documents/climdes/nwt/nwtclim.htm, says it is from D1. Cliff has "tundra" (not saddle) data, with an average soil depth of 39cm (but the max they measured to was 100, so that might be underestimating), so 150/100*50=75 is probably a good estimate
#possibly I need to adjust for soil depth (I think the assumption is 1 m) in Mountain Geography: Physical and Human Dimensions, edited by Martin F. Price, Alton C. Byers, Donald A. Friend, Thomas Kohler, Larry W. Price, they have a table from niwot that goes down to 60cm. Also the Temperature Regime for Selected SOils in the United States USDA pub has Niwot soil temp probes going down 50cm. Costello and Schmidt Environmental Microbiology (2006) in wet meadow did sampling down to ~35cm. 
###  latitude (the latitude of the study site (in degrees)

### NOTE: the water in the soil is carried over from one year to the next; 
### This may cause problems if there is no cool season during which soils are recharged to
### maximum field capacity.

#AET = actual evapotranspiration
#DEF = the deficit
#SUR = surplus

### example lapse rates used with the example data set:
#lr <- c(-2.33,-3.49,-5.06,-6.63,-5.65,-6.45,-5.82,-5.4,-5.75,-5.24,-5.87,-3.82)
lr<-c(0,0,0,0,0,0,0,0,0,0,0,0)
aet <- function(station.elev=3528, site.elev=3528, lapse=lr, fc=75, latitude=40.05){#values for D1 are: elev=3739, latitutde=40.06
  
  ###   LOAD DATA FILE AND CREATE MATRICES:
  ###  tmin[ye,mo,da], tmax[ye,mo,da], pcp[ye,mo,da] 
  ###   == minimum daily temperature, maximum daily temperature and precip for the year, month, and day.
  ###     ye ranges from 1 to n.years.
  
  hcn<-read.table(file="/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/Saddle_precip_temp_formoisturedeficit1982.csv", header=TRUE, sep=",", na.strings=".")#file.choose() 
  #hcn<-read.table(file="/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/D1temp&ppt/D1_precip_temp_formoisturedeficit.csv", header=TRUE, sep=",", na.strings=".") #the file needs to start with Jan 1
  summary(hcn)
  ###  vars to use: Day Month Year TMIN TMAX PCP
  
  year.1 <- hcn$Year[1]  # first year in the data file
  n.years <- hcn$Year[length(hcn$Year)]-year.1+1
  
  tmin <- array(data=NA, dim=c(n.years, 12, 31))
  tmax <- array(data=NA, dim=c(n.years, 12, 31))
  pcp <- array(data=NA, dim=c(n.years, 12, 31))
  #tmin <- array(data=NA, dim=c(n.years, 12, 31), dimnames=c("year","month","day"))
  #tmax <- array(data=NA, dim=c(n.years, 12, 31), dimnames=c("year","month","day"))
  #pcp <- array(data=NA, dim=c(n.years, 12, 31), dimnames=c("year","month","day"))
  
  ### CONVERT TO DEG C AND TO MM PRECIP
  for(i in 1:length(hcn$TMAX)){
    ye <- hcn$Year[i]-year.1+1
    mo <- hcn$Month[i]
    da <- hcn$Day[i]
    tmin[ye,mo,da] <- hcn$TMIN[i]#(hcn$TMIN[i]-32)*5/9
    tmax[ye,mo,da] <- hcn$TMAX[i]#(hcn$TMAX[i]-32)*5/9
    pcp[ye,mo,da] <- hcn$PCP[i]#(hcn$PCP[i])*25.4
  }
  
  ## len.month[year,month] = number of days in month
  len.month <- matrix(NA,n.years,12)
  for(ye in 1:n.years){
    for(mo in 1:12){
      len.month[ye,mo]<-length(subset(hcn$TMIN,as.integer(hcn$Year-year.1+1)==as.integer(ye) & as.integer(hcn$Month)==as.integer(mo)))
    }
  }
  
  ###  USING MEAN MONTHLY LAPSE RATES TO ADJUST TEMPERATURE DATA TO THE ACTUAL SITE ELEVATION
  ## degC/1000m
  lapse.adjust<-lapse*(site.elev-station.elev)/1000
  
  ## SET UP OUTPUT MATRIX
  AET_out<-matrix(NA,n.years,1+4*12)
  colnames(AET_out)<-c("year","AET.Jan","AET.Feb","AET.Mar","AET.Apr","AET.May","AET.Jun","AET.Jul","AET.Aug","AET.Sep","AET.Oct","AET.Nov","AET.Dec",
                       "DEF.Jan","DEF.Feb","DEF.Mar","DEF.Apr","DEF.May","DEF.Jun","DEF.Jul","DEF.Aug","DEF.Sep","DEF.Oct","DEF.Nov","DEF.Dec",
                       "SUR.Jan","SUR.Feb","SUR.Mar","SUR.Apr","SUR.May","SUR.Jun","SUR.Jul","SUR.Aug","SUR.Sep","SUR.Oct","SUR.Nov","SUR.Dec",
                       "PET.Jan","PET.Feb","PET.Mar","PET.Apr","PET.May","PET.Jun","PET.Jul","PET.Aug","PET.Sep","PET.Oct","PET.Nov","PET.Dec")
  
  ###  DEFINE SOME CONSTANTS
  PI <- 3.1415926535897932
  ###   phi=constant based on julian date.  These are for the 15th of the month
  phi<-array(NA,12)
  phi[1] <- -0.374329268   
  phi[2] <- -0.231959485
  phi[3] <- -0.042826351
  phi[4] <- 0.165449779
  phi[5] <- 0.326011171
  phi[6] <- 0.406337883
  phi[7] <- 0.37810974
  phi[8] <- 0.249923394
  phi[9] <- 0.058515387
  phi[10] <- -0.143269334
  phi[11] <- -0.318990215
  phi[12] <- -0.405457073
  
  ###  DAYLENGTHS for each month
  daylength<-array(NA,12)
  for(i in 1:12){
    tx <- ( sin(latitude*PI/180)*sin(phi[i]) )/( cos(latitude*PI/180)*cos(phi[i]) )
    daylength[i] <- 24-(24/PI)*acos(tx)
  }
  
  ws.previous<-0  # start out with no snow
  w.previous <- fc  # start out with soil water at field capacity
  
  ###  THE MAIN LOOP
  
  for(ye in 1:n.years){   ### First year
    ix <- 0
    AET_out[ye,1]<-ye+year.1-1
    
    ###  MONTHLY MEANS OF DAILY DATA, FILL IN MISSING VALUES
    tmean.mo <- array(NA,12)
    for(mo in 1:12){
      tsum <- 0
      for(da in 1:len.month[ye,mo]){
        tmin.site <- tmin[ye,mo,da]+lapse.adjust[mo]
        tmax.site <- tmax[ye,mo,da]+lapse.adjust[mo]
        
        # fill in missing values
        if(is.na(tmin.site)){
          tmin.site <- mean(tmin[max(1,ye-15):min(ye+15,n.years),mo,da],na.rm=TRUE)+lapse.adjust[mo]
        }
        if(is.na(tmax.site)){
          tmax.site <- mean(tmax[max(1,ye-15):min(ye+15,n.years),mo,da],na.rm=TRUE)+lapse.adjust[mo]
        }
        tsum <- tsum + (tmin.site+tmax.site)/2
      }  ### next day
      tmean.mo[mo]<-tsum/len.month[ye,mo]
    }  ### next month
    
    ### PET COMPUTED FROM DAILY TEMPERATURES
    for(mo in 1:12){
      if(tmean.mo[mo] > 0){
        ix <- ix+((tmean.mo[mo]/5)^1.514)
      }
      ax=.000000675*(ix^3)-.0000771*(ix^2)+.0179*ix+.49   ### ax=a monthly constant
    }
    
    julian <- 1
    pet <- array(NA,366)
    ws <- array(NA,366)
    w <- array(NA,366)
    pet.mo <- array(NA,12)
    surplus <- array(NA,12)
    deficit <- array(NA,12)
    E <- array(NA,12)
    
    for(mo in 1:12){
      pet_sum <- 0
      for(da in 1:len.month[ye,mo]){
        tmin.site <- tmin[ye,mo,da]+lapse.adjust[mo]
        tmax.site <- tmax[ye,mo,da]+lapse.adjust[mo]
        
        # fill in missing values
        if(is.na(tmin.site)){
          tmin.site <- mean(tmin[max(1,ye-15):min(ye+15,n.years),mo,da],na.rm=TRUE)+lapse.adjust[mo]
        }
        if(is.na(tmax.site)){
          tmax.site <- mean(tmax[max(1,ye-15):min(ye+15,n.years),mo,da],na.rm=TRUE)+lapse.adjust[mo]
        }
        tmean.site <- (tmin.site+tmax.site)/2
        if(tmean.site<0){
          pet[julian] <- 0
        } else {
          if(tmean.site<26.5){
            pet[julian] <- 16*(10*tmean.site/ix)^ax
          } else {
            pet[julian] <- 415.85+32.24*tmean.site-.43*tmean.site*tmean.site
          }
        }
        pet_sum <- pet_sum+pet[julian]
        julian <- julian+1
      }  ### next day
      pet.mo[mo] <- pet_sum/len.month[ye,mo]
    }  ### next month
    
    ###  COMPUTE AET
    test <- 0
    run <- 0
    Ps <- 0
    Pr <- 0
    
    while(test<1){
      wd<-0  ###  day 1 is Jan 1
      Md_ysum <- 0
      Ps_ysum<- 0
      for(mo in 1:12){
        Ps_sum <- 0
        Md_sum <- 0
        S_sum <- 0
        Pr_sum <- 0
        Def_sum <- 0
        for(da in 1:len.month[ye,mo]){
          tmin.site <- tmin[ye,mo,da]+lapse.adjust[mo]
          tmax.site <- tmax[ye,mo,da]+lapse.adjust[mo]
          # fill in missing values
          if(is.na(tmin.site)){
            tmin.site <- mean(tmin[max(1,ye-15):min(ye+15,n.years),mo,da],na.rm=TRUE)+lapse.adjust[mo]
          }
          if(is.na(tmax.site)){
            tmax.site <- mean(tmax[max(1,ye-15):min(ye+15,n.years),mo,da],na.rm=TRUE)+lapse.adjust[mo]
          }
          tmean.site <- (tmin.site+tmax.site)/2
          if(is.na(pcp[ye,mo,da])){   ### replace NAs in precip with zeros
            pcp[ye,mo,da]<-0
          }
          
          if(tmean.site < -1){  ### //rain or snow
            Ps <- pcp[ye,mo,da]
            Pr <- 0
          } else {
            Ps <- 0
            Pr <- pcp[ye,mo,da]
          }
          
          if(tmean.site>=0){
            Md <- 2.63+2.55*tmean.site+0.0912*tmean.site*Pr    ### //snowmelt function
          } else {
            Md <- 0
          }
          
          wd <- wd+1
          if(wd>1){
            ws.previous <- ws[wd-1]
            w.previous<-w[wd-1]
          }
          if(Md>(ws.previous+Ps)){
            Md <- (ws.previous+Ps)  ###  //can't melt more snow than exists
          }
          ws[wd] <- ws.previous+Ps-Md  ###   //swe at end of day wd
          Dd <- Md+Pr-(pet[wd]/len.month[ye,mo])	###  //evap demand on soil
          if(Dd<0){
            beta <- (w.previous/fc)/.7
            if(beta>1){
              beta <- 1
            }
            ## beta <- 1-exp(-6.68*w.previous/fc)   ###  //beta function (declining availability function)
          } else {
            beta <- 1
          }
          w[wd] <- w.previous+beta*Dd  ### //soil moisture at end of day wd
          if(w[wd]>fc){
            Surp <- w[wd]-fc ### //surplus
            w[wd] <- fc
          } else {
            Surp <- 0
          }
          
          if(Dd<0){
            Def_sum <- Def_sum-Dd*(1-beta)
          }
          Md_sum <- Md_sum+Md
          S_sum <- S_sum+Surp
          Ps_sum <- Ps_sum+Ps
          Pr_sum <- Pr_sum+Pr
        }  ### next day
        
        ###   Monthly evapotranspiration (E) 
        E[mo] <- Pr_sum+Md_sum-(w[wd]-w[wd-len.month[ye,mo]+1])-S_sum   ### rain + snowmelt - diff in soil moisture - surplus
        Md_ysum <- Md_ysum+Md_sum
        Ps_ysum <- Ps_ysum+Ps_sum
        if(E[mo]<(1E-10)){
          E[mo] <- 0
        }
        ####   //Monthly surplus
        surplus[mo] <- S_sum
        deficit[mo] <- Def_sum
      }    ##### next month
      
      if(ye==1){    ###  TEST IF THE MODEL BALANCES
        if(run>0){
          if(abs(w[wd]-w[1])<1 & abs(ws[wd]-ws[1])<1){
            test <- 1
          }
        }
        ###  //Snow accumulation, does not balance
        if((Ps_ysum-Md_ysum)>2 & run>1){  
          test <- 1
        }
        ###   //If for some reason the model is not balancing...
        if(run > 1000){
          test <- 1
        }
        ###  //if none of the above, use ending moisture as start of next year
        w.previous <- w[wd]
        ws.previous <- ws[wd]
        run <- run+1
      } else {   ###IF NOT THE FIRST YEAR, WRAP SOIL MOISTURE FROM LAST DAY TO FIRST DAY OF NEXT YEAR
        w.previous <- w[wd]
        ws.previous <- ws[wd]
        test <- 1
      }
    }   ###  Loop
    
    ###  Output results
    for(mo in 1:12){
      AET_out[ye,mo+1] <- E[mo]    ### AET
      AET_out[ye,mo+13] <- deficit[mo]    ### DEF
      AET_out[ye,mo+25] <- surplus[mo]    ### surplus
      AET_out[ye,mo+37] <- pet.mo[mo]    ### potential
      
    }  ### next mo
  }    ###  next year
  
  return(AET_out)
}
## write.table(AET_out,"aet.txt")
aet_results75 <- aet()
#aet_results75D1 <- aet()#D1 needs to be rerun with actual elevation
plot(1:12,aet_results75[31,14:25])



#Change it to water year
def<-as.data.frame(cbind(year=aet_results75[,1],aet_results75[,14:25]))
def2<-melt.data.frame(def,id.vars=c('year'))
def3<-separate(data=def2,col=variable,into=c("def","month"),sep="\\.")
def3$def<-NULL
colnames(def3)[3]<-"def"
head(def3)

#moisture deficit - define water year, same as year for all months except 9, 10, 11, 12, for which it is the following year
def4<-def3[order(def3$year),]
def4$month<-rep(1:12)
def4$water_yr<-def4$year
def4$water_yr[def4$month==9]<-def4$water_yr[def4$month==9]+1
def4$water_yr[def4$month==10]<-def4$water_yr[def4$month==10]+1
def4$water_yr[def4$month==11]<-def4$water_yr[def4$month==11]+1
def4$water_yr[def4$month==12]<-def4$water_yr[def4$month==12]+1

def5<-aggregate.data.frame(def4$def,by=list(year=def4$water_yr),sum)
colnames(def5)[2]<-"def"
def5<-def5[-1,];def5<-def5[-33,]#remove first and last row b/c not full data
Saddlemoisturewateryear<-def5

#use def from above to get sum and fal, deficit is 0 in dec, jan, feb, mar, apr, may
defsum<-rowSums(def[7:9])
deffal<-rowSums(def[10:12]) #fall needs to be ajusted to be on wateryear
deffal2<-c(NA,deffal[1:32])
defseason<-cbind(year=def$year,defsum,deffal2)



#PET - Change it to water year
pet<-as.data.frame(cbind(year=aet_results75[,1],aet_results75[,38:49]))
pet2<-melt.data.frame(pet,id.vars=c('year'))
pet3<-separate(data=pet2,col=variable,into=c("pet","month"),sep="\\.")
pet3$pet<-NULL
colnames(pet3)[3]<-"pet"
head(pet3)

#define water year, same as year for all months except 9,10, 11, 12, for which it is the following year
pet4<-pet3[order(pet3$year),]
pet4$month<-rep(1:12)
pet4$water_yr<-pet4$year
pet4$water_yr[pet4$month==9]<-pet4$water_yr[pet4$month==9]+1
pet4$water_yr[pet4$month==10]<-pet4$water_yr[pet4$month==10]+1
pet4$water_yr[pet4$month==11]<-pet4$water_yr[pet4$month==11]+1
pet4$water_yr[pet4$month==12]<-pet4$water_yr[pet4$month==12]+1

pet5<-aggregate.data.frame(pet4$pet,by=list(year=pet4$water_yr),sum)
colnames(pet5)[2]<-"pet"
pet5<-pet5[-1,];pet5<-pet5[-33,]#remove first and last row b/c not full data
SaddlePETwateryear<-pet5

#write.csv(SaddlePETwateryear,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/SaddlePETwateryear.csv",row.names=F,quote=F)

#use pet from above to get spr, sum and fal. I could do winter but it is really low and I don't think it is biolgically useful. 
petspr<-rowSums(pet[4:6])
petsum<-rowSums(pet[7:9])
petfal<-rowSums(pet[10:12]) #fall needs to be ajusted to be on wateryear
petfal2<-c(NA,petfal[1:32])
petseason<-cbind(year=pet$year,petspr,petsum,petfal2)

defpetseason<-cbind(defseason,petseason[,2:4])
#write.csv(defpetseason,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/defpetseason.csv",row.names=F,quote=F)

#potential evapotranspiration of 360 is the treeline cutoff. 320 is my greatest one from D1 (335 is the greatest one from Saddle) so that looks reasonable. See Global Environment Change: Remote Sensing and GIS Perspectives, edited by R.B. Singh

