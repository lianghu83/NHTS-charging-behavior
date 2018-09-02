##### This file checks the relationship between charger availability and charging decision
##### Plot SOC when decide to charge
##### Examine collective effects

library(ggplot2)

image_path <- "C:\\Users\\lianghu\\Desktop\\Smart mobility charging behavior project\\images\\"

#merge the csv files in POV1 scenario folder
path <- paste0("H:\\NHTS2017\\Result Data\\POV1_", scenario)
filename_feasible <- dir(path) #feasible vehicles
setwd(path)
#set.seed(1991)
#filename <- sample(filename, 5000, replace=FALSE)
ReadCSV <- function(x) {
  read.csv(x, header = T)
  #read.csv(x, header = T)[, c('Charger', 'Decision', 'SOC_D', 'DESTINATION')]
}
df <- do.call("rbind", lapply(filename_feasible, ReadCSV))

#check relationship
df$diff <- df$Charger-df$Decision
summary(df$diff)

#how many % that SOC_D<20 miles?
nrow(subset(df, df$SOC_D<20))/nrow(df)*100 

#convert SOC_D to %
df$SOC_D <- df$SOC_D/df$RANGE*100

#how many charges during the day
times_charge <- aggregate(df$Decision, by=list(Category=df$HOUSE_VEH_ID), FUN=sum)
summary(times_charge$x)

#extract charge
charge <- subset(df, Charger==1 & Decision==1)
charge$DESTINATION <- factor(charge$DESTINATION, levels=c('public','work','home'))
write.csv(charge, 
          paste0("H:\\NHTS2017\\Result Data\\charge_", scenario, ".csv"), 
          row.names=F)



#Result 1: SOC distribution when decide to charge
#charge <- read.csv(paste0("H:\\NHTS2017\\Result Data\\charge_", scenario, ".csv"))
summary(charge$SOC_D)
tapply(charge$SOC_D, charge$DESTINATION, summary)
table(charge$DESTINATION)
#how many charges are below 50% SOC?
nrow(subset(charge, SOC_D<50))/nrow(charge)
#how many charges are within anxiety range?
nrow(subset(charge, SOC_D/100*RANGE<20))/nrow(charge)
#how many charges are below 20% SOC?
nrow(subset(charge, SOC_D<20))/nrow(charge)
#how many charges are above 80% SOC?
nrow(subset(charge, SOC_D>80))/nrow(charge)
#SOC distribution
ArrivalSOC <- function(df) {
  p <- ggplot(df, aes(SOC_D, ..density..)) +
    geom_histogram(breaks=seq(0,100,10), fill='#FFCC99', col='black') + #'#99CCFF'
    labs(x='SOC (%)', y='Density') +
    scale_x_continuous(limits=c(0, 100), breaks=seq(0,100,10), minor_breaks=seq(0,100,20)) + 
    theme_bw() +
    theme(axis.title=element_text(size=10), 
          axis.text = element_text(size=9),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank()) 
  return(p)
}
ArrivalSOCByLocation <- function(df) {
  p <- ggplot(df, aes(SOC_D, ..density.., fill=DESTINATION)) +
    geom_histogram(breaks=seq(0,100,10), col='black') +
    labs(x='SOC (%)', y='Density') +
    scale_x_continuous(limits=c(0, 100), breaks=seq(0,100,10), minor_breaks=seq(0,100,20)) + 
    theme_bw() +
    theme(axis.title=element_text(size=10), 
          axis.text = element_text(size=9),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.position=c(0.1,0.9),
          legend.margin=margin(0,0,0,0),
          #legend.direction="horizontal",
          legend.text=element_text(size=8),
          legend.title=element_blank()) 
  return(p)
}
p <- ArrivalSOC(charge)
p
#p <- ArrivalSOCByLocation(charge)
#p
#ggsave(paste0(image_path, "SOC_", scenario, ".jpeg"), p, width=5, height=2.5, units='in')



#Aggregate charging power and events
interval <- 60 #minute
GridPower <- function(aa, place) {
  
  if (place=='total') {
    aa <- aa
  } else {
    aa <- subset(aa, aa$DESTINATION==place)
  }
  
  aa$charging_start <- aa$ENDTIME%/%100*60+aa$ENDTIME%%100
  aa$charging_duration <- round(aa$SOC_add*consumption/aa$Power*60, 0)
  aa$charging_end <- aa$charging_start + aa$charging_duration
  
  temp <- data.frame(start=seq(0,1440*2-interval,interval), end=seq(interval,1440*2,interval))
  for (i in 1:nrow(temp)) {
    temp$power1[i] <- sum(aa$Power[aa$charging_start<=temp$end[i] & aa$charging_end>=temp$start[i]])
    temp$event1[i] <- nrow(subset(aa, aa$charging_start<=temp$end[i] & aa$charging_end>=temp$start[i]))
  }
  
  grid <- temp[1:(1440/interval), ]
  grid$power2 <- temp$power1[(1440/interval+1):nrow(temp)]
  grid$event2 <- temp$event1[(1440/interval+1):nrow(temp)]
  grid$power3 <- (grid$power1 + grid$power2)*NHTS_weight #kW, multiply NHTS weight to represent national sum
  grid$event3 <- (grid$event1 + grid$event2)*NHTS_weight #total charges nationwide
  
  grid$hour <- (grid$start+grid$end)/2/60
  
  grid$place <- place
  
  return(grid)
}

grid_total <- GridPower(charge, "total")
grid_total$power3_ratio <- grid_total$power3/grid_total$power3
grid_total$event3_ratio <- grid_total$event3/grid_total$event3

grid_home <- GridPower(charge, "home")
grid_home$power3_ratio <- grid_home$power3/grid_total$power3
grid_home$event3_ratio <- grid_home$event3/grid_total$event3

grid_work <- GridPower(charge, "work")
grid_work$power3_ratio <- grid_work$power3/grid_total$power3
grid_work$event3_ratio <- grid_work$event3/grid_total$event3

grid_public <- GridPower(charge, "public")
grid_public$power3_ratio <- grid_public$power3/grid_total$power3
grid_public$event3_ratio <- grid_public$event3/grid_total$event3

grid <- rbind(grid_total, grid_home, grid_work, grid_public)
grid$place <- factor(grid$place, levels=c('total', 'home', 'work', 'public'))



#Result 2: charging events/number of vehicles in charging
ChargingEventPlotTwoAxes <- function(grid) {
  
  grid_sub <- subset(grid, place!='total')
  grid_sub$place <- factor(grid_sub$place, levels=c('public','work','home'))
  
  p <- ggplot(grid_sub, aes(x=hour)) + 
    geom_bar(aes(y=event3/1000/1000, lty='vehicles'), stat='identity', fill='grey') + 
    scale_x_continuous(limits=c(0,24), breaks=seq(0,24,1)) +
    scale_y_continuous(limits=c(0,2.5), 
                       sec.axis=sec_axis(~.*40, name="Charging locations (%)", breaks=seq(0,100,10))) +
    geom_line(aes(y=event3_ratio*100/40, color=place)) +
    geom_point(aes(y=event3_ratio*100/40, color=place)) +
    labs(x='Time of day (h)', y='Vehicles in charging (million)') + 
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text=element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.title=element_blank(),
          legend.key.size=unit(0.15,'in'),
          legend.position=c(0.12,0.65),
          legend.margin=margin(0,0,0,0),
          #legend.direction="horizontal",
          legend.text=element_text(size=8)) 
  
  return(p)
} 
p <- ChargingEventPlotTwoAxes(grid)
p
#ggsave(paste0(image_path, "event_", scenario, ".jpeg"), p, width=5, height=2.5, units='in')



#Result 3: Electricity demand profile
ChargingProfileTwoAxes <- function(grid) {
  
  grid_sub <- subset(grid, place!='total')
  grid_sub$place <- factor(grid_sub$place, levels=c('public','work','home'))
  
  p <- ggplot(grid_sub, aes(x=hour)) + 
    geom_bar(aes(y=power3/1000/1000, lty='demand'), stat='identity', fill='grey') + #fill='#FFCC99'
    scale_x_continuous(limits=c(0,24), breaks=seq(0,24,1)) +
    scale_y_continuous(limits=c(0,25), 
                       sec.axis=sec_axis(~.*4, name="Charging locations (%)", breaks=seq(0,100,10))) +
    geom_line(aes(y=power3_ratio*100/4, color=place)) +
    geom_point(aes(y=power3_ratio*100/4, color=place)) +
    labs(x='Time of day (h)', y='Power demand (GW)') + 
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text=element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.title=element_blank(),
          legend.key.size=unit(0.15,'in'),
          legend.position=c(0.12,0.6),
          legend.margin=margin(0,0,0,0),
          #legend.direction="horizontal",
          legend.text=element_text(size=8)) 
  
  return(p)
} 
p <- ChargingProfileTwoAxes(grid)
p
#ggsave(paste0(image_path, "grid_", scenario, ".jpeg"), p, width=5, height=2.5, units='in')

ChargingProfileStackBar <- function(grid) {
  
  grid_sub <- subset(grid, place!='total')
  grid_sub$place <- factor(grid_sub$place, levels=c('public','work','home'))
  
  p <- ggplot(grid_sub, aes(x=hour, y=power3/1000/1000, fill=place)) +
    geom_bar(stat='identity') +
    labs(x='Time of day (h)', y='Power demand (GW)') + 
    scale_x_continuous(limits=c(0,24), breaks=seq(0,24,1)) + 
    #scale_y_continuous(limits=c(0,25)) +
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.position=c(0.1,0.85),
          legend.margin=margin(0,0,0,0),
          #legend.direction="horizontal",
          legend.key.size=unit(0.15,'in'),
          legend.text=element_text(size=8),
          legend.title=element_blank()) 
  return(p)
}
p <- ChargingProfileStackBar(grid)
p
#ggsave(paste0(image_path, "grid_bar_", scenario, ".jpeg"), p, width=5, height=2.5, units='in')



#Result 4: BEV feasibility
length(filename_feasible)/length(filename)



#Split number of charges/powers at home/work/public
#get a table summarizing the charging
SummaryTable <- function(charge) {
  charge_table <- data.frame(place=c('total','home','work','public'), 
                             events=0, percent=0, 
                             SOC_25=0, SOC_mean=0, SOC_median=0, SOC_75=0, SOC_sd=0,
                             stringsAsFactors=F)
  charge_table$events[1] <- nrow(charge)
  charge_table$percent[1] <- 1
  charge_table$SOC_25[1] <- quantile(charge$SOC_D, probs=0.25) 
  charge_table$SOC_mean[1] <- mean(charge$SOC_D)
  charge_table$SOC_median[1] <- median(charge$SOC_D)
  charge_table$SOC_75[1] <- quantile(charge$SOC_D, probs=0.75) 
  charge_table$SOC_sd[1] <- sd(charge$SOC_D)
  for (i in 2:4) {
    charge_table$events[i] <- nrow(subset(charge, DESTINATION==charge_table$place[i]))
    charge_table$percent[i] <- charge_table$events[i]/nrow(charge)
    charge_table$SOC_25[i] <- quantile(charge$SOC_D[charge$DESTINATION==charge_table$place[i]], probs=0.25) 
    charge_table$SOC_mean[i] <- mean(charge$SOC_D[charge$DESTINATION==charge_table$place[i]])
    charge_table$SOC_median[i] <- median(charge$SOC_D[charge$DESTINATION==charge_table$place[i]])
    charge_table$SOC_75[i] <- quantile(charge$SOC_D[charge$DESTINATION==charge_table$place[i]], probs=0.75) 
    charge_table$SOC_sd[i] <- sd(charge$SOC_D[charge$DESTINATION==charge_table$place[i]])
  }
  return(charge_table)
}
charge_table <- SummaryTable(charge)
#add power to charge_table
charge_table$power <- aggregate(grid$power3, by=list(place=grid$place), FUN=sum)$x
charge_table$power_percent <- charge_table$power/charge_table$power[1]
#add feasibility
charge_table$feasibility <- length(filename_feasible)/length(filename)
#add public charger utilization
charge_table$public_utilization <- nrow(subset(charge, DESTINATION=='public'))/nrow(subset(df, Charger==1 & DESTINATION=='public'))
  
#output
write.csv(charge_table, 
          paste0("H:\\NHTS2017\\Result Data\\charge_table_", scenario, ".csv"), 
          row.names=F)





#Result 5: Sensitivity analysis
#a=b
ct_name <- paste0('charge_table_S2_ab=', as.character(seq(5,95,10)/100), '.csv')
setwd("H:\\NHTS2017\\Result Data")
ct <- do.call("rbind", lapply(ct_name, read.csv))
ct$alpha_beta <- rep(seq(5,95,10)/100, each=4)
ct$place <- factor(ct$place, levels=c('public','work','home','total'))

ImpactOnSOCAlphaBeta <- function(ct) {
  ct_sub <- subset(ct, place=='total')
  p <- ggplot(ct_sub, aes(x=alpha_beta)) +
    #geom_line(aes(y=SOC_25, color='25%ile')) + 
    #geom_point(aes(y=SOC_25, color='25%ile'), shape=17) +
    #geom_line(aes(y=SOC_75, color='75%ile')) + 
    #geom_point(aes(y=SOC_75, color='75%ile'), shape=17) +
    #geom_line(aes(y=SOC_mean, color='average')) + 
    #geom_point(aes(y=SOC_mean, color='average'), shape=17) +
    geom_line(aes(y=SOC_mean), color='blue') + 
    geom_point(aes(y=SOC_mean), shape=17, color='blue') +
    labs(x=expression(paste(alpha,' and ',beta)), y='SOC (%)') +
    scale_x_continuous(limits=c(0,1), breaks=seq(0.05,0.95,0.1)) + 
    scale_y_continuous(limits=c(34,42), breaks=seq(34,42,2)) + #limits=c(20,55)
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.title=element_blank(),
          #legend.key.size=unit(0.15,'in'),
          legend.position=c(0.375,0.925),
          legend.margin=margin(0,0,0,0),
          legend.direction="horizontal",
          legend.text=element_text(size=7)) 
  return(p)
}
p <- ImpactOnSOCAlphaBeta(ct)
p
#ggsave(paste0(image_path, "alpha=beta.jpeg"), p, width=3.2, height=2, units='in')



#lambda
ct_name <- paste0('charge_table_S2_l=', as.character(seq(1.25,5.75,0.5)), '.csv')
setwd("H:\\NHTS2017\\Result Data")
ct <- do.call("rbind", lapply(ct_name, read.csv))
ct$lambda <- rep(seq(1.25,5.75,0.5), each=4)
ct$place <- factor(ct$place, levels=c('public','work','home','total'))

ImpactOnSOCLambda <- function(ct) {
  ct_sub <- subset(ct, place=='total')
  p <- ggplot(ct_sub, aes(x=lambda)) +
    #geom_line(aes(y=SOC_25, color='25%ile')) + 
    #geom_point(aes(y=SOC_25, color='25%ile'), shape=17) +
    #geom_line(aes(y=SOC_75, color='75%ile')) + 
    #geom_point(aes(y=SOC_75, color='75%ile'), shape=17) +
    #geom_line(aes(y=SOC_mean, color='average')) + 
    #geom_point(aes(y=SOC_mean, color='average'), shape=17) +
    geom_line(aes(y=SOC_mean), color='blue') + 
    geom_point(aes(y=SOC_mean), shape=17, color='blue') +
    labs(x=expression(lambda), y='SOC (%)') +
    scale_x_continuous(limits=c(1,6), breaks=seq(1.25,5.75,0.5)) + 
    #scale_y_continuous(limits=c(40,42), breaks=seq(40,42,0.5)) +
    scale_y_continuous(limits=c(34,42), breaks=seq(34,42,2)) +
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.title=element_blank(),
          #legend.key.size=unit(0.15,'in'),
          legend.position=c(0.375,0.925),
          legend.margin=margin(0,0,0,0),
          legend.direction="horizontal",
          legend.text=element_text(size=7)) 
  return(p)
}
p <- ImpactOnSOCLambda(ct)
p
#ggsave(paste0(image_path, "lambda.jpeg"), p, width=3.2, height=2, units='in')



#public charging coverage
ct_name <- paste0('charge_table_S2_p=', as.character(c(1:10)/10), '.csv')
setwd("H:\\NHTS2017\\Result Data")
ct <- do.call("rbind", lapply(ct_name, read.csv))
ct$coverage <- rep(c(1:10)/10, each=4)
ct$place <- factor(ct$place, levels=c('public','work','home','total'))

ImpactOnSOCPublicCoverage <- function(ct) {
  ct_sub <- subset(ct, place=='total')
  p <- ggplot(ct_sub, aes(x=coverage)) +
    #geom_line(aes(y=SOC_25, color='25%ile')) + 
    #geom_point(aes(y=SOC_25, color='25%ile'), shape=17) +
    #geom_line(aes(y=SOC_75, color='75%ile')) + 
    #geom_point(aes(y=SOC_75, color='75%ile'), shape=17) +
    #geom_line(aes(y=SOC_mean, color='average')) + 
    #geom_point(aes(y=SOC_mean, color='average'), shape=17) +
    geom_line(aes(y=SOC_mean), color='blue') + 
    geom_point(aes(y=SOC_mean), shape=17, color='blue') +
    labs(x='Public charger network coverage', y='SOC (%)') +
    scale_x_continuous(limits=c(0.1,1), breaks=seq(0.1,1,0.1)) + 
    #scale_y_continuous(limits=c(39.5,42.5), breaks=seq(39.5,42.5,0.5)) +
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.title=element_blank(),
          #legend.key.size=unit(0.15,'in'),
          legend.position=c(0.375,0.925),
          legend.margin=margin(0,0,0,0),
          legend.direction="horizontal",
          legend.text=element_text(size=7)) 
  return(p)
}
p <- ImpactOnSOCPublicCoverage(ct)
p
#ggsave(paste0(image_path, "public_SOC.jpeg"), p, width=3.2, height=2, units='in')

ImpactOnChargeLocationEvent <- function(ct) {
  ct <- subset(ct, place!='total')
  p <- ggplot(ct, aes(x=coverage, y=percent*100, fill=place)) +
    geom_bar(stat='identity') +
    labs(x='Public charger network coverage', y='Vehicles in charging by location (%)') +
    scale_x_continuous(limits=c(0.05,1.05), breaks=seq(0.1,1,0.1)) + 
    scale_y_continuous(breaks=seq(0,100,20)) + 
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.position='top',
          legend.margin=margin(0,0,0,0),
          legend.key.size=unit(0.1,'in'),
          legend.text=element_text(size=7),
          legend.title=element_blank()) 
  return(p)
}
p <- ImpactOnChargeLocationEvent(ct)
p
#ggsave(paste0(image_path, "public_event.jpeg"), p, width=3.2, height=2.6, units='in')

ImpactOnChargeLocationEventAbs <- function(ct) {
  ct <- subset(ct, place!='total')
  p <- ggplot(ct, aes(x=coverage, y=events*NHTS_weight/1000/1000, fill=place)) +
    geom_bar(stat='identity') +
    labs(x='Public charger network coverage', y='Vehicles in charging (million)') +
    scale_x_continuous(limits=c(0.05,1.05), breaks=seq(0.1,1,0.1)) + 
    #scale_y_continuous(limits=c(0,8), breaks=seq(0,8,2)) + 
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.position='top',
          legend.margin=margin(0,0,0,0),
          legend.key.size=unit(0.1,'in'),
          legend.text=element_text(size=7),
          legend.title=element_blank()) 
  return(p)
}
p <- ImpactOnChargeLocationEventAbs(ct)
p
#ggsave(paste0(image_path, "public_event_abs.jpeg"), p, width=3.2, height=2.6, units='in')

ImpactOnChargeLocationGrid <- function(ct) {
  ct <- subset(ct, place!='total')
  p <- ggplot(ct, aes(x=coverage, y=power_percent*100, fill=place)) +
    geom_bar(stat='identity') +
    labs(x='Public charger network coverage', y='Power demand by location (%)') +
    scale_x_continuous(limits=c(0.05,1.05), breaks=seq(0.1,1,0.1)) + 
    scale_y_continuous(breaks=seq(0,100,20)) + 
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.position='top',
          legend.margin=margin(0,0,0,0),
          legend.key.size=unit(0.1,'in'),
          legend.text=element_text(size=7),
          legend.title=element_blank()) 
  return(p)
}
p <- ImpactOnChargeLocationGrid(ct)
p
#ggsave(paste0(image_path, "public_grid.jpeg"), p, width=3.2, height=2.6, units='in')

ImpactOnChargeLocationGridAbs <- function(ct) {
  ct <- subset(ct, place!='total')
  p <- ggplot(ct, aes(x=coverage, y=power/1000/1000, fill=place)) +
    geom_bar(stat='identity') +
    labs(x='Public charger network coverage', y='Power demand (GW)') +
    scale_x_continuous(limits=c(0.05,1.05), breaks=seq(0.1,1,0.1)) + 
    #scale_y_continuous(limits=c(0,300), breaks=seq(0,300,50)) + 
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.position='top',
          legend.margin=margin(0,0,0,0),
          legend.key.size=unit(0.1,'in'),
          legend.text=element_text(size=7),
          legend.title=element_blank()) 
  return(p)
}
p <- ImpactOnChargeLocationGridAbs(ct)
p
#ggsave(paste0(image_path, "public_grid_abs.jpeg"), p, width=3.2, height=2.6, units='in')

#impact on charger utilization
ImpactOnPublicUtilization <- function(ct) {
  ct <- subset(ct, place=='total')
  p <- ggplot(ct, aes(x=coverage, y=public_utilization)) +
    geom_line(color='purple') + 
    geom_point(color='purple', shape=15) +
    labs(x='Public charger network coverage', y='Utilization') +
    scale_x_continuous(limits=c(0.05,1.05), breaks=seq(0.1,1,0.1)) + 
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank()) 
  return(p)
}
p <- ImpactOnPublicUtilization(ct)
p
#ggsave(paste0(image_path, "public_utilization.jpeg"), p, width=3.2, height=2, units='in')





#Result 6: How many charges when SOC<20 miles?
library(plyr)

#alpha_beta
alpha_beta <- seq(5,95,10)/100
ct <- ldply(alpha_beta, 
       function (x) {
         setwd("H:\\NHTS2017\\Result Data")
         charge <- read.csv(paste0('charge_S2_ab=', as.character(x), '.csv'))
         return(nrow(subset(charge, SOC_D/100*RANGE<20))/nrow(charge)*100)
       })
colnames(ct) <- 'less_than_20'
ct$alpha_beta <- alpha_beta

ImpactOn20ChargeAlphaBeta <- function(ct) {
  p <- ggplot(ct, aes(x=alpha_beta)) +
    geom_line(aes(y=less_than_20), color='red') + 
    geom_point(aes(y=less_than_20), shape=16, color='red') +
    labs(x=expression(paste(alpha,' and ',beta)), y='Charges under 20 mi (%)') +
    scale_x_continuous(limits=c(0,1), breaks=seq(0.05,0.95,0.1)) + 
    scale_y_continuous(limits=c(5,21), breaks=seq(5,20,5)) + 
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank()) 
  return(p)
}
p <- ImpactOn20ChargeAlphaBeta(ct)
p
#ggsave(paste0(image_path, "alpha=beta_anxiety.jpeg"), p, width=3.2, height=2, units='in')

#lambda
lambda <- seq(1.25,5.75,0.5)
ct <- ldply(lambda, 
            function (x) {
              setwd("H:\\NHTS2017\\Result Data")
              charge <- read.csv(paste0('charge_S2_l=', as.character(x), '.csv'))
              return(nrow(subset(charge, SOC_D/100*RANGE<20))/nrow(charge)*100)
            })
colnames(ct) <- 'less_than_20'
ct$lambda <- lambda

ImpactOn20ChargeLambda <- function(ct) {
  p <- ggplot(ct, aes(x=lambda)) +
    geom_line(aes(y=less_than_20), color='red') + 
    geom_point(aes(y=less_than_20), shape=16, color='red') +
    labs(x=expression(lambda), y='Charges under 20 mi (%)') +
    scale_x_continuous(limits=c(1,6), breaks=seq(1.25,5.75,0.5)) + 
    #scale_y_continuous(limits=c(min(ct$less_than_20),7.5), breaks=seq(7.2,7.5,0.1)) +
    scale_y_continuous(limits=c(5,21), breaks=seq(5,20,5)) + 
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank()) 
  return(p)
}
p <- ImpactOn20ChargeLambda(ct)
p
#ggsave(paste0(image_path, "lambda_anxiety.jpeg"), p, width=3.2, height=2, units='in')

#coverage
coverage <- c(1:10)/10
ct <- ldply(coverage, 
            function (x) {
              setwd("H:\\NHTS2017\\Result Data")
              charge <- read.csv(paste0('charge_S2_p=', as.character(x), '.csv'))
              return(nrow(subset(charge, SOC_D/100*RANGE<20))/nrow(charge)*100)
            })
colnames(ct) <- 'less_than_20'
ct$coverage <- coverage

ImpactOn20ChargeCoverage <- function(ct) {
  p <- ggplot(ct, aes(x=coverage)) +
    geom_line(aes(y=less_than_20), color='red') + 
    geom_point(aes(y=less_than_20), shape=16, color='red') +
    labs(x='Public charger network coverage', y='Charges under 20 mi (%)') +
    scale_x_continuous(limits=c(0.1,1), breaks=seq(0.1,1,0.1)) + 
    #scale_y_continuous(limits=c(min(ct$less_than_20),7.5), breaks=seq(7.2,7.5,0.1)) +
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank()) 
  return(p)
}
p <- ImpactOn20ChargeCoverage(ct)
p
ggsave(paste0(image_path, "public_anxiety.jpeg"), p, width=3.2, height=2, units='in')





#Result 7: TOU price
#make comparison with Scenario S2
charge_S2 <- read.csv(paste0("H:\\NHTS2017\\Result Data\\charge_", "S2", ".csv"))
grid_S2 <- GridPower(charge_S2, "total")
grid_S2$power3_ratio <- grid_S2$power3/grid_S2$power3
grid_S2$event3_ratio <- grid_S2$event3/grid_S2$event3

GridPowerDelayCharging <- function(aa, place) {
  
  if (place=='total') {
    aa <- aa
  } else {
    aa <- subset(aa, aa$DESTINATION==place)
  }
  
  aa$charging_start <- aa$ENDTIME%/%100*60+aa$ENDTIME%%100
  aa$charging_duration <- round(aa$SOC_add*consumption/aa$Power*60, 0)
  aa$charging_end <- aa$charging_start + aa$charging_duration
  
  #delay charging if the charge satisfies conditions
  aa$SOC_D <- aa$SOC_D/100*aa$RANGE #convert SOC in % to SOC in mile
  condition_1 <- aa$DESTINATION=='home'
  condition_2 <- round(aa$SOC_D+aa$SOC_add,0)==aa$RANGE
  condition_3 <- aa$charging_start>=14*60 & aa$charging_start<23*60 #arrive at 14:00~23:00
  aa$starttime_next <- aa$charging_start+aa$DWELTIME_new
  condition_4 <- aa$starttime_next>23*60 #next depart after 23:00
  condition_5 <- (23*60+aa$charging_duration)<=aa$starttime_next
  delay_condition <- condition_1 & condition_2 & condition_3 & condition_4 & condition_5
  aa$charging_start[delay_condition] <- 23*60+1 #start charging from 23:01
  aa$charging_end <- aa$charging_start + aa$charging_duration
  
  temp <- data.frame(start=seq(0,1440*2-interval,interval), end=seq(interval,1440*2,interval))
  for (i in 1:nrow(temp)) {
    temp$power1[i] <- sum(aa$Power[aa$charging_start<=temp$end[i] & aa$charging_end>=temp$start[i]])
    temp$event1[i] <- nrow(subset(aa, aa$charging_start<=temp$end[i] & aa$charging_end>=temp$start[i]))
  }
  
  grid <- temp[1:(1440/interval), ]
  grid$power2 <- temp$power1[(1440/interval+1):nrow(temp)]
  grid$event2 <- temp$event1[(1440/interval+1):nrow(temp)]
  grid$power3 <- (grid$power1 + grid$power2)*NHTS_weight #kW, multiply NHTS weight to represent national sum
  grid$event3 <- (grid$event1 + grid$event2)*NHTS_weight #total charges nationwide
  
  grid$hour <- (grid$start+grid$end)/2/60
  
  grid$place <- place
  
  return(grid)
}

grid_total <- GridPowerDelayCharging(charge, "total")
grid_total$power3_ratio <- grid_total$power3/grid_total$power3
grid_total$event3_ratio <- grid_total$event3/grid_total$event3
grid <- grid_total

sum(grid$power3[grid$place=='total']/1000/1000)
sum(grid_S2$power3/1000/1000)

ChargingProfileComparison <- function(grid, grid_S2) {
  
  grid_sub <- subset(grid, place=='total')
  grid_sub$scenario <- 'time-of-use rate'
  grid_S2$scenario <- 'constant rate'
  grid_all <- rbind(grid_S2, grid_sub)
  
  p <- ggplot(grid_all, aes(x=hour, y=power3/1000/1000, color=scenario)) +
    geom_line(stat='identity', position='identity') +
    geom_point(shape=17) +
    scale_color_manual(values=c("#E69F00", "#009E73")) +
    labs(x='Time of day (h)', y='Power demand (GW)') + 
    scale_x_continuous(limits=c(0,24), breaks=seq(0,24,1)) + 
    scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5)) +
    theme_bw() +
    theme(axis.title=element_text(size=9), 
          axis.text = element_text(size=8),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #legend
          legend.position=c(0.25,0.85),
          legend.margin=margin(0,0,0,0),
          #legend.direction="horizontal",
          legend.key.size=unit(0.15,'in'),
          legend.text=element_text(size=8),
          legend.title=element_blank()) 
  return(p)
}
p <- ChargingProfileComparison(grid, grid_S2)
p
#ggsave(paste0(image_path, "load_compare_", scenario, ".jpeg"), p, width=5, height=2.5, units='in')

#on-peak load changes by what %?
sum(grid_S2$power3[grid_S2$hour>=15 & grid_S2$hour<19]/1000/1000)
sum(grid$power3[grid$place=='total' & grid$hour>=15 & grid$hour<19]/1000/1000)
#max load changes by what %
max(grid_S2$power3/1000/1000)
max(grid$power3[grid$place=='total']/1000/1000)
(max(grid_S2$power3/1000/1000)-max(grid$power3[grid$place=='total']/1000/1000))/max(grid_S2$power3/1000/1000)
#change in percent by hour
(grid$power3[grid$place=='total']/1000/1000-grid_S2$power3/1000/1000)/(grid_S2$power3/1000/1000)*100




