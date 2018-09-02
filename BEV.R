###This file explores current BEV users

#BEV in the dataset
veh <- read.csv("H:\\NHTS2017\\Survey Data\\vehpub.csv")
veh$HOUSE_VEH_ID <- as.character(100*veh$HOUSEID+veh$VEHID)
bev <- subset(veh, HFUEL==3)
write.csv(bev, file="H:\\NHTS2017\\Survey Data\\BEV.csv", row.names = F)

#BEV make and model
make <- sort(unique(as.character(bev$MAKE)))
make

model <- sort(unique(as.character(bev$MODEL)))
model

#select actual BEVs
bev_model <- c(12037, 20029, 29005, 30042, 34043, 35055, 36038, 37039, 49402, 63037, 65031)
bev_model_name <- c('Focus', 'Spark', 'Tesla', 'e-Golf', 'BMW 1 Series ActiveE', 'Leaf', 
                    'Fiat 500e', 'Honda Fit EV', 'Toyota RAV4 EV', 'Kia Soul', 'Smart Fortwo')
bev_model_range <- c(76, 82, 249, 83, 94, 73, 87, 82, 103, 93, 68) #mile
#Nissan Leaf
#2011/2012: 73; 2013/2014/2015: 84; 2016:107

bev <- subset(bev, bev$MODEL %in% as.character(bev_model))

#add range
bev$RANGE <- 0
for (i in 1:length(bev_model)) {
  bev$RANGE[bev$MODEL==as.character(bev_model[i])] <- bev_model_range[i]
}
bev$RANGE[bev$MODEL=='35055' & bev$VEHYEAR %in% c(2013,2014,2015)] <- 84
bev$RANGE[bev$MODEL=='35055' & bev$VEHYEAR %in% c(2016,2017)] <- 107

write.csv(bev, file="H:\\NHTS2017\\Survey Data\\BEV1.csv", row.names = F)



#which BEVs in the POV1
filename <- dir("H:\\NHTS2017\\POV1")
bev_filename <- filename[filename %in% paste0(bev$HOUSE_VEH_ID, '.csv')]
bev_id <- substr(bev_filename, start=1, stop=10)
bev_sub <- subset(bev, bev$HOUSE_VEH_ID %in% bev_id)
sort(unique(as.character(bev_sub$MODEL)))
count(bev_sub$MODEL)



#daily VMT
tesla <- subset(bev, bev$MODEL == 29005)
filename <- dir("H:\\NHTS2017\\POV1")
DailyVMT <- function(filename) {
  setwd("H:\\NHTS2017\\POV1")
  df <- read.csv(file=filename)[, c('HOUSE_VEH_ID', 'VMT_MILE')]
  return(data.frame(HOUSE_VEH_ID=df$HOUSE_VEH_ID[1], DVMT=sum(df$VMT_MILE)))
}
library(data.table)
DVMT <- rbindlist(lapply(filename, DailyVMT), fill=TRUE)
DVMT$type <- 'GV'
DVMT$type[DVMT$HOUSE_VEH_ID %in% bev$HOUSE_VEH_ID] <- 'BEV'
DVMT$tesla <- DVMT$type
DVMT$tesla[DVMT$HOUSE_VEH_ID %in% tesla$HOUSE_VEH_ID] <- 'Tesla'
table(DVMT$type)
table(DVMT$tesla)

write.csv(DVMT, file="H:\\NHTS2017\\Survey Data\\POV1_DVMT.csv", row.names = F)

#compare mean
DVMT <- read.csv("H:\\NHTS2017\\Survey Data\\POV1_DVMT.csv")
library(ggplot2)
ggplot(DVMT, aes(type, DVMT)) +
  geom_boxplot()
hist(DVMT$DVMT)
table(DVMT$type)
table(DVMT$tesla)
t.test(DVMT$DVMT[DVMT$type=='BEV'], DVMT$DVMT[DVMT$type=='GV'], alternative = 'less')
t.test(DVMT$DVMT[DVMT$tesla=='BEV'], DVMT$DVMT[DVMT$tesla=='GV'], alternative = 'less')
t.test(DVMT$DVMT[DVMT$tesla=='Tesla'], DVMT$DVMT[DVMT$tesla=='GV'], alternative = 'greater')

