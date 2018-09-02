##### This file extracts samples from NHTS 2017 data.



##### Explanation for NHTS data. #####
# One NHTS sample is a traveler who uses personal vehicle as main mode.
#
# Useful data fields:
#
# DAYV2PUB.csv
# DWELTIME: calculated time (min) at destination, -9=not ascertained, 0-1439
# TRPTRANS: transportation mode used on trip, 1~7=privately operated vehicle (POV)
# DRVR_FLG: subject was driver on this trip, 1=yes
# VMT_MILE: calculated trip distance (mi) for driver trips, 0~5600
# TRPMILES: calculated trip distance converted into miles, 0-9000
# STRTTIME: Trip START time in military, 0000-2359
# ENDTIME: Trip END time in military, 0000-2359
# TDAYDATE: Date of Travel Day (YYYYMM)
# TDCASEID: trip number
# TDTRPNUM: travel day trip number, trips for each respondent are numbered consecutively by start time, 1~32
# HOMEOWN: Housing unit owned or rented?
# HOMETYPE: type of housing unit (But this is not in NHTS2017)
# TRVL_MIN: derived trip time - minutes
# TDWKND: trip was on weekend?
#
# TRIPPURP: General Trip Purpose (Home-Based Purpose types)
# AWAYHOME: Travel day reason S was away from home
# FRSTHM17: Did Person Start Travel Day at Home?
# WHYTO: reason of trip
# WHYFROM: reason of previous trip
# WHYTRP90: travel day trip purpose, derived from WHYFROM&WHYTO
# WHYTRP1S: travel day trip purpose, derived from WHYTO&AWAYHOME
# 
# VEHV2PUB.csv
# BESTMILE: best estimate of annual miles, 0-200000
# VEHCOMM: commercial license plate, 02=no
# FUELTYPE: type of fuel
# VEHTYPE: vehicle type, similar to TRPTRANS, also in Travel Day file
# HYBRID: vehicle is hybrid or uses alternate fuel
# VEHAGE: age of vehicle in years.
# VEHCOMM: commercial license plate.
# WHOMAIN: person number of primary driver.
# 
# PERV2PUB.csv
# R_AGE: respondent age, 5-88, 92=89+
# R_SEX: respondent gender, 01~02
# DISTTOWK: one-way distance to workplace (mi), 0~990
# GCDWORK: great circle distance (mi) between home and work, 0~3899
# DRIVER: driver status of S, 1=yes a driver
# WKSTFIPS: state FIPS code for work address, 01~56
# WRKTIME: Usual arrival time at work, 01:00AM-12:57PM
# WRKTRANS: transportation mode to work last week
# WORKER: subject worker status
# WKFMHMXX: frequency of working from home in past month
# 
# HHV2PUB.csv
# HHFAMINC: derived total HH income, -7~-9, 01~18
# HHC_MSA: CMSA FIPS code for HH address
# HHSTFIPS: State FIPS for HH address
# HH_CBSA: CBSA FIPS code for HH address
# HHSTATE: State HH location
# HOMETYPE: type of housing unit



##### Tips for extracting samples. #####
# In the Travel Day file, DRVR_FLG was created to allow the data user
# to select the vehicle trip recorde.
# Bus/taxi trips are counted as person trips, instead of vehicle trips.






##### Extract samples #####
rm(list = ls())
library(plyr)
setwd("H:/NHTS2017/Survey Data/")

#read NHTS Travel Day data
trip <- read.csv("trippub.csv")

#get all POV trips 
pov <- subset(trip,
              TRPTRANS %in% c(3,4,5,6) &
              DRVR_FLG==1) #car,SUV,Van,Pickup truck

#create unique vehicle ID
pov$HOUSE_VEH_ID <- as.character(100*pov$HOUSEID+pov$VEHID)
length(unique(pov$HOUSE_VEH_ID))

#create unique person ID
pov$HOUSE_PERSON_ID <- as.character(100*pov$HOUSEID+pov$PERSONID)
length(unique(pov$HOUSE_PERSON_ID))

#trip distance
summary(pov$VMT_MILE)
quantile(pov$VMT_MILE, 0.999) #279.0921 mi
VMT_ub <- quantile(pov$VMT_MILE, 0.999)
nrow(subset(pov, VMT_MILE<0))
unique(pov$VMT_MILE[pov$VMT_MILE<0])

#trip start time
summary(pov$STRTTIME)
nrow(subset(pov, STRTTIME<0))
unique(pov$STRTTIME[pov$STRTTIME<0])

#trip end time
summary(pov$ENDTIME)
nrow(subset(pov, ENDTIME==-1))
unique(pov$ENDTIME[pov$ENDTIME<0])

#dwell time
summary(pov$DWELTIME) 
#How to deal with DWELTIME==-9?
#Reason for -9: dwell time for last trip of the day is assigned -9
nrow(subset(pov, DWELTIME<0))
unique(pov$DWELTIME[pov$DWELTIME<0])

#origin
summary(pov$WHYFROM)
nrow(subset(pov, WHYFROM<0))
unique(pov$WHYFROM[pov$WHYFROM<0])
count(pov$WHYFROM)
#create origin based on WHYFROM, main used when trip is the 1st trip of the day
pov$ORIGIN <- 'unknown'
pov$ORIGIN[pov$WHYFROM %in% c(1,2)] <- 'home'
pov$ORIGIN[pov$WHYFROM %in% c(3,4)] <- 'work'
pov$ORIGIN[pov$WHYFROM>=5] <- 'public'
count(pov$ORIGIN)

#destination
summary(pov$WHYTRP1S)
nrow(subset(pov, WHYTRP1S<0))
unique(pov$WHYTRP1S[pov$WHYTRP1S<0])
count(pov$WHYTRP1S)
summary(pov$WHYTO)
nrow(subset(pov, WHYTO<0))
unique(pov$WHYTO[pov$WHYTO<0])
count(pov$WHYTO)
#create destination based on WHYTRP1S
pov$DESTINATION <- 'unknown'
pov$DESTINATION[pov$WHYTRP1S==1] <- 'home'
pov$DESTINATION[pov$WHYTRP1S==10] <- 'work'
pov$DESTINATION[pov$WHYTRP1S>=20] <- 'public'
count(pov$DESTINATION)

#trips on weekend? 1=Yes
summary(pov$TDWKND)
count(pov$TDWKND)

#remove missing values and errors, delete all affected HOUSE_VEH_ID
pov$VMT_MILE_flag <- 1
pov$VMT_MILE_flag[pov$VMT_MILE<0 | pov$VMT_MILE>VMT_ub] <- 0
pov$STRTTIME_flag <- 1
pov$STRTTIME_flag[pov$STRTTIME<0] <- 0
pov$ENDTIME_flag <- 1
pov$ENDTIME_flag[pov$ENDTIME<0] <- 0
pov$WHYTRP1S_flag <- 1
pov$WHYTRP1S_flag[pov$WHYTRP1S<0] <- 0
pov$WHYFROM_flag <- 1
pov$WHYFROM_flag[pov$WHYFROM<0] <- 0
pov$flag <- pov$VMT_MILE_flag * pov$STRTTIME_flag * pov$ENDTIME_flag * pov$WHYTRP1S_flag * pov$WHYFROM_flag
temp <- pov[, c("HOUSE_VEH_ID", "flag")]
temp_agg <- aggregate(temp$flag, by=list(Category=temp$HOUSE_VEH_ID), FUN=prod)
Remove_HOUSE_VEH_ID <- subset(temp_agg, x==0)
pov$VMT_MILE_flag <- NULL
pov$STRTTIME_flag <- NULL
pov$ENDTIME_flag <- NULL
pov$WHYTRP1S_flag <- NULL
pov$WHYFROM_flag <- NULL
pov$flag <- NULL
pov <- subset(pov, !(HOUSE_VEH_ID %in% Remove_HOUSE_VEH_ID$Category))

#calculate new dwell time for vehicle = starttime - endtime
pov$STRTTIME_new <- ifelse(pov$STRTTIME>=400, pov$STRTTIME-400, pov$STRTTIME+2000) #convert time because 4:00 is beginning of day
pov$ENDTIME_new <- ifelse(pov$ENDTIME>=400, pov$ENDTIME-400, pov$ENDTIME+2000)
pov <- pov[with(pov, order(HOUSE_VEH_ID, STRTTIME_new)),] #order vehicle first, then order trips
temp <- pov$STRTTIME_new
temp <- temp[2:length(temp)]
temp[length(temp)+1] <- 2359
pov$STRTTIME_new_next <- temp
pov$DWELTIME_new <- (pov$STRTTIME_new_next%/%100*60+pov$STRTTIME_new_next%%100)-(pov$ENDTIME_new%/%100*60+pov$ENDTIME_new%%100)

head <- 1
for (i in 2:nrow(pov)) {
  if (pov$HOUSE_VEH_ID[i]!=pov$HOUSE_VEH_ID[i-1]) {
    pov$DWELTIME_new[i-1] <- (1440 - (pov$ENDTIME_new[i-1]%/%100*60+pov$ENDTIME_new[i-1]%%100) + 
                              (pov$STRTTIME_new[head]%/%100*60+pov$STRTTIME_new[head]%%100))
    pov$STRTTIME_new_next[i-1] <- pov$STRTTIME_new[head]
    head <- i
  }
  if(i%%50000==0) print(i)
}
pov$DWELTIME_new[i] <- (1440 - (pov$ENDTIME_new[i]%/%100*60+pov$ENDTIME_new[i]%%100) + 
                            (pov$STRTTIME_new[head]%/%100*60+pov$STRTTIME_new[head]%%100))

summary(pov$DWELTIME_new)
nrow(subset(pov, DWELTIME_new<0))
#remove error dwelltime
pov$DWELTIME_new_flag <- 1
pov$DWELTIME_new_flag[pov$DWELTIME_new<0] <- 0
temp <- pov[, c("HOUSE_VEH_ID", "DWELTIME_new_flag")]
temp_agg <- aggregate(temp$DWELTIME_new_flag, by=list(Category=temp$HOUSE_VEH_ID), FUN=prod)
Remove_HOUSE_VEH_ID <- subset(temp_agg, x==0)
pov$DWELTIME_new_flag <- NULL
pov <- subset(pov, !(HOUSE_VEH_ID %in% Remove_HOUSE_VEH_ID$Category))

summary(pov$DWELTIME)
summary(pov$DWELTIME_new)

#drop not useful columns
pov1 <- pov[, -c(9,10,12,16,22,34:46,48:59,84,85,87:93,99)]

#write pov and pov1
write.csv(pov, file="H:\\NHTS2017\\Survey Data\\POV.csv", row.names=F)
write.csv(pov1, file="H:\\NHTS2017\\Survey Data\\POV1.csv", row.names=F)



#split pov1, write each vehicle into a file
s <- split(x=pov1, f=pov1$HOUSE_VEH_ID)
sapply(s, function(x) {
  write.csv(x, file=paste0("H:\\NHTS2017\\POV1\\", x$HOUSE_VEH_ID[1], '.csv'), row.names=F)
})






