##### This file applies CPT charging decision model to every charging opportunity

rm(list = ls())

#source parameters
source("C:\\Users\\lianghu\\Desktop\\Smart mobility charging behavior project\\code\\0_Set parameters.R")

#create directory if not exist
path <- paste0("H:\\NHTS2017\\Result Data\\POV1_", scenario)
dir.create(path, showWarnings=FALSE)

#delete old files
del_filename <- dir(path, full.names=T)
unlink(del_filename, recursive = FALSE)

#for each vehicle trip file
library(parallel)
no_cores <- 7
cl <- makeCluster(no_cores)
filename <- dir("H:\\NHTS2017\\POV1")
#random choose vehicle
#set.seed(2017)
filename <- sample(filename, bev_num, replace=FALSE) #bev_num
#select BEV
#filename <- filename[filename %in% paste0(bev$HOUSE_VEH_ID, '.csv')]

f <- function(filename) {
  setwd("H:\\NHTS2017\\POV1")
  df <- read.csv(file=filename, stringsAsFactors = F)
  
  #source scenario parameters
  source("C:\\Users\\lianghu\\Desktop\\Smart mobility charging behavior project\\code\\0_Set parameters.R")
  
  #create scenarios
  set.seed(as.numeric(substr(filename, 1, 10))-3000000000) #this seed covers whole code for this vehicle
  home_charge <- rbinom(1, size=1, prob=coverage_home) #maintain home charge constant
  work_charge <- rbinom(1, size=1, prob=coverage_work) #maintain work charge constant
  #run Bernoulli distribution for each row
  for (j in 1:nrow(df)) {
    if (df$DESTINATION[j]=='home') {
      df$Charger[j] <- home_charge
    } else if (df$DESTINATION[j]=='work') {
      df$Charger[j] <- work_charge
    } else {
      df$Charger[j] <- rbinom(1, size=1, prob=coverage_public)
    }
  }
  df$Power <- ifelse(df$DESTINATION=='home', power_h, 
                     ifelse(df$DESTINATION=='work', power_w, power_p))
  df$Power[df$Charger==0] <- 0 #if no charger, the power is 0
  
  #source DistanceToNextCharger.R to calculate distance between charges
  source("C:\\Users\\lianghu\\Desktop\\Smart mobility charging behavior project\\code\\DistanceToNextCharger.R")
  df$Between_d <- DistanceToNextCharger(df[, c('Charger', 'VMT_MILE')])
  #df$LookAhead_d <- LookAheadDistanceToNextCharger(df[, c('Charger', 'Between_d')])
  df$ToLast_d <- DistanceToLastDest(df[, c('Charger', 'VMT_MILE', 'Between_d')])
  df$ToLast_charger <- ChargingOpportunityToLastDest(df[, c('Charger', 'VMT_MILE')])
  
  #source charging decision function CPTChargingDecision_6.R
  source("C:\\Users\\lianghu\\Desktop\\Smart mobility charging behavior project\\code\\CPTChargingDecision_6.R")
  
  #add BEV range
  df$RANGE <- sample(EV_range_list, size=1, prob=EV_range_prob_list) #randomly draw a range
  #df$RANGE <- bev$RANGE[bev$HOUSE_VEH_ID==df$HOUSE_VEH_ID[1]]
  
  #assume start range is 80~100% range
  #because home charge coverage is assumed as 1, and many start travel day from home
  #start_range <- df$RANGE[1]
  set.seed(as.numeric(substr(filename, 1, 10))-3000000000)
  start_range <- runif(1, min = 0.78*df$RANGE[1], max = df$RANGE[1]) #use 0.78 instead of 0.8, as requested by reviewer 3
  for (i in 1:nrow(df)) {
    ifelse(i>1, df$SOC_D[i] <- df$SOC_D[i-1] + df$SOC_add[i-1] - df$VMT_MILE[i],
           df$SOC_D[i] <- start_range - df$VMT_MILE[i])
    
    #option A. no look ahead decision
    #df$Decision[i] <- ifelse(df$Charger[i]==1 & df$DWELTIME_new[i]>time_hassel, 
    #                         CPTChargingDecision(df$SOC_D[i], df$Between_d[i], df$DWELTIME_new[i], df$Power[i], df$DESTINATION[i], df$RANGE[1], df$WHYTO[i]), 
    #                         0)
    #option B. look ahead decision
    #if (df$Charger[i]==1 & df$DWELTIME_new[i]>time_hassel) {
    #  decision_temp <- CPTChargingDecision(df$SOC_D[i], df$Between_d[i], df$DWELTIME_new[i], df$Power[i], df$DESTINATION[i], df$RANGE[1], df$WHYTO[i])
    #  if (decision_temp==1) {
    #    df$Decision[i] <- 1
    #  } else {
    #    df$Decision[i] <- CPTChargingDecisionLookAhead(df$SOC_D[i], df$ToLast_d[i], df$DWELTIME_new[i], df$Power[i], df$DESTINATION[i], df$RANGE[1], df$WHYTO[i], df$ToLast_charger[i])
    #  }
    #} else {
    #  df$Decision[i] <- 0
    #}
    #option C. merge option A and B
    df$Decision[i] <- ifelse(df$Charger[i]==1 & df$DWELTIME_new[i]>time_hassel, 
                             CPTChargingDecisionLookAhead(df$SOC_D[i], df$Between_d[i], df$ToLast_d[i], df$DWELTIME_new[i], df$Power[i], df$DESTINATION[i], df$RANGE[1], df$WHYTO[i], df$ToLast_charger[i]), 
                             0)
    
    #SOC increase
    df$SOC_add[i] <- min(df$DWELTIME_new[i]/60*df$Power[i]/consumption, df$RANGE[1]-df$SOC_D[i]) * df$Decision[i]
    #add a new charging start time, in actual time
    df$STRTTIME_charging[i] <- df$ENDTIME[i]
  }
  
  #if (!(TRUE %in% c(df$SOC_D<0)) & df$ORIGIN[1]=='home' & (df$TRAVDAY[1] %in% c(2:6))) { #if range is enough & start travel day from home & Mon~Fri
  if (!(TRUE %in% c(df$SOC_D<0))) { 
    setwd(paste0("H:\\NHTS2017\\Result Data\\POV1_", scenario))
    write.csv(df, file=filename, row.names=F)
  }
  
}

parLapply(cl, filename, f)
stopCluster(cl)


