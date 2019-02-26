##### This function models charging decision based on CPT
##### Consider range variation
##### use individual empirical outcomes, rather than the outcomes for population
##### outcomes in range are converted to monetary cost
##### calculate CPT for not charging, as well as charging
##### set a new reference point (negative)

#source scenario parameters
source("C:\\Users\\lianghu\\Desktop\\Smart mobility charging behavior project\\code\\0_Set parameters.R")

#random CPT parameters
#alpha <- runif(1) #0.88
#beta <- runif(1) #0.88
#lamda <- runif(1, min=1, max=3) #2.25
#gamma <- runif(1) #0.6
#delta <- runif(1) #0.69

#define weight functions
WeightGain <- function(p) {
  p <- min(p, 1.0)
  p^gamma/(p^gamma+(1-p)^gamma)^(1/gamma)
}
WeightLoss <- function(p) {
  p <- min(p, 1.0)
  p^delta/(p^delta+(1-p)^delta)^(1/delta)
}

#define cummulative weights function
CumWeight <- function(outcome, prob, asset_0) { #outcome is asset
  k <- length(outcome) #k should be >=3
  weight <- c()
  weight[1] <- ifelse(outcome[1]<asset_0, 
                      WeightLoss(prob[1]), 
                      WeightGain(1)-WeightGain(sum(prob[(1+1):k])))
  for (j in 2:(k-1)) {
  weight[j] <- ifelse(outcome[j]<asset_0, 
                        WeightLoss(sum(prob[1:j]))-WeightLoss(sum(prob[1:(j-1)])), 
                        WeightGain(sum(prob[j:k]))-WeightGain(sum(prob[(j+1):k])))
  }
  weight[k] <- ifelse(outcome[k]<asset_0, 
                      WeightLoss(1)-WeightLoss(sum(prob[1:(k-1)])),
                      WeightGain(prob[k]))
  return(weight)
}

#define asset function
AssetNotCharge <- function(r) {
  A <- 0
  ifelse(r>=SOC_0, A,
         ifelse(r>=0, A-(SOC_0-r)/SOC_0*cost_tow,
                A-cost_tow-cost_taxi*abs(r)-taxi_initial))
}
AssetCharge <- function(r, SOC_add, dwell_place, hour) {
  #cost_charging <- SOC_add*consumption*cost_elec
  cost_charging <- SOC_add*consumption*ElectricityPrice1(hour)
  #service cost only at public
  A <- CostHassel(dwell_place, WHYTO) + cost_charging + cost_service*ifelse(dwell_place %in% c('public'), 1, 0) 
  ifelse(r>=SOC_0, -A,
         ifelse(r>=0, -A-(SOC_0-r)/SOC_0*cost_tow,
                -A-cost_tow-cost_taxi*abs(r)-taxi_initial))
}
AssetChargeDynamicPricing <- function(r, SOC_add, dwell_place, cost_charging, WHYTO) {
  #cost_charging <- SOC_add*consumption*cost_elec
  #cost_charging <- SOC_add*consumption*DynamicElectricityPrice(hour)
  #service cost only at public
  A <- CostHassel(dwell_place, WHYTO) + cost_charging + cost_service*ifelse(dwell_place %in% c('public'), 1, 0) 
  ifelse(r>=SOC_0, -A,
         ifelse(r>=0, -A-(SOC_0-r)/SOC_0*cost_tow,
                -A-cost_tow-cost_taxi*abs(r)-taxi_initial))
}
ChargingCost <- function(STRTTIME, duration, power) {
  start_hour <- STRTTIME%/%100
  end_hour <- (STRTTIME%/%100*60+STRTTIME%%100 + duration)%/%60
  all_hour <- seq(start_hour, end_hour)
  all_hour <- ifelse(all_hour<24, all_hour, all_hour-24)
  all_hour <- all_hour*100
  all_price <- ElectricityPrice1(all_hour)
  all_percent <- rep(1, length(all_hour)) #indicate the percent of minutes in a hour
  all_percent[1] <- 1-STRTTIME%%100/60
  all_percent[length(all_percent)] <- (STRTTIME%/%100*60+STRTTIME%%100 + duration)%%60/60
  cost_charging <- sum(all_percent*power*all_price)
  if (start_hour==end_hour) {
    cost_charging <- duration/60*power*all_price[1]
  }
  return(cost_charging)
}

#define value function for not charging
ValueNotCharge <- function(outcome) {
  #ifelse(outcome>=SOC_0, (outcome-SOC_0)^alpha, -lamda*(SOC_0-outcome)^beta)
  #ifelse(outcome>=SOC_0, 0, -lamda*(SOC_0-outcome)^beta)
  #ifelse(outcome>=SOC_0, 0, ifelse(outcome>=0, -cost_anxiety, -cost_tow))
  ifelse(outcome>=0, outcome^alpha, -lamda*(-outcome)^beta)
}
#define value function for charging
ValueCharge <- function(outcome) {
  #ifelse(outcome>=SOC_0, (outcome-SOC_0)^alpha, -lamda*(SOC_0-outcome)^beta)
  #ifelse(outcome>=SOC_0, 0, -lamda*(SOC_0-outcome)^beta)
  #ifelse(outcome>=SOC_0, -cost_hassel, ifelse(outcome>=0, -cost_anxiety-cost_hassel, -cost_tow-cost_hassel))
  ifelse(outcome>=0, outcome^alpha, -lamda*(-outcome)^beta)
}
#define value function
Value <- function(asset, asset_0) {
  ifelse(asset>=asset_0, (asset-asset_0)^alpha, -lamda*(asset_0-asset)^beta)
}

#plot value functions
if (0) {
  x <- seq(-50,50,1)
  asset <- NetAsset(x)
  y_n <- ValueNotCharge(asset)
  y_c <- ValueCharge(asset)
  par(mfrow=c(2,1))
  plot(x, y_n, type='p', col='red')
  plot(x, y_c, type='p', col='blue')
}

#define range variation function
library(truncnorm)
RangeDistribution <- function(SOC) {
  
  cv <- 0.234 #coefficient of variance, 0.234
  mu <- SOC
  sigma <- abs(SOC*cv)
  
  #truncate normal distribution
  confidence <- 0.99
  k <- 10 #how many outcomes
  a <- mu - sigma*qnorm(0.5+0.5*confidence)
  b <- mu + sigma*qnorm(0.5+0.5*confidence)
  interval <- seq(a, b, length.out=(k+1))
  prob <- diff(ptruncnorm(interval, a=a, b=b, mean=mu, sd=sigma))
  
  outcome <- c()
  for (i in 2:length(interval)) {
    outcome[i-1] <- qtruncnorm((ptruncnorm(interval[i], a=a, b=b, mean=mu, sd=sigma)-
                                    ptruncnorm(interval[i-1], a=a, b=b, mean=mu, sd=sigma))/2+
                                   ptruncnorm(interval[i-1], a=a, b=b, mean=mu, sd=sigma), 
                               a=a, b=b, mean=mu, sd=sigma)
  }
  #outcome <- round(outcome, 0)
  
  #return a dataframe
  if (SOC!=0) {
    #aggregate 
    temp <- data.frame(Range=outcome, Prob=prob)
    temp <- aggregate(Prob ~ Range, temp, FUN=sum)
    temp <- temp[order(temp$Range),]
    return(temp)
  } else {
    return(data.frame(Range=rep(SOC, k), Prob=rep(1/k, k))) #if SOC=0, return prob=1
  }
    
}

#define when to start the delay charging function
WhenStartDelayedCharging <- function(TIME, empty_duration, charging_duration) {
  #TIME is the actual time
  #empty_duration is the future plug-in but no charging duration, in minutes
  start_hour <- TIME%/%100
  end_hour <- (TIME%/%100*60+TIME%%100 + empty_duration)%/%60
  all_hour <- seq(start_hour, end_hour)
  all_hour <- ifelse(all_hour<24, all_hour, all_hour-24)
  all_hour <- all_hour*100
  
  #1: randomly return a hour
  #return(sample(all_hour, 1))
  
  #2: return the cheapest hour
  #min_index <- which.min(DynamicElectricityPrice(all_hour))
  #if (all_hour[min_index]==start_hour) {
  #  return(max(TIME, all_hour[min_index]))
  #} else {
  #  return(all_hour[min_index])
  #}
  
  #3: return the lowest cost start time
  #cost_charging <- c()
  #for (i in 1:length(all_hour)) {
  #  cost_charging[i] <- ChargingCost(STRTTIME=all_hour[i], duration=charging_duration, power=power_h)
  #}
  #min_index <- which.min(cost_charging)
  #if (all_hour[min_index]==start_hour) {
  #    return(max(TIME, all_hour[min_index]))
  #  } else {
  #    return(all_hour[min_index])
  #  }
  
  #4: return several lowest cost
  #cost_charging <- c()
  #for (i in 1:length(all_hour)) {
  #  cost_charging[i] <- ChargingCost(STRTTIME=all_hour[i], duration=charging_duration, power=power_h)
  #}
  #selected_index <- sort(cost_charging, index.return=T)$ix[1:ceiling(length(cost_charging)*0.75)]
  #return(sample(all_hour[selected_index], 1))
  
  #5: return a random hour, but with lower charging cost than the present
  cost_charging <- c()
  for (i in 1:length(all_hour)) {
    cost_charging[i] <- ChargingCost(STRTTIME=all_hour[i], duration=charging_duration, power=power_h)
  }
  return(sample(all_hour[cost_charging<=cost_charging[1]], 1))
}

#define charging decision function
CPTChargingDecision <- function(SOC, distance_next, dwell_time, power, dwell_place, total_range, start_charging_time, WHYTO) {
  
  ##0.determine RP: asset_0
  asset_0 <- -VOR*distance_next
  
  ##1.not charge
  SOC_next_n <- SOC - distance_next
  temp_n <- RangeDistribution(SOC_next_n)
  r_n <- temp_n$Range
  prob_n <- temp_n$Prob
  asset_n <- AssetNotCharge(r_n) #asset is outcome
  value_n <- Value(asset_n, asset_0)
  weight_n <- CumWeight(asset_n, prob_n, asset_0)
  CPT_n <- sum(value_n*weight_n)
  
  ##2.charge
  SOC_add <- min(dwell_time/60*power/consumption, total_range-SOC) 
  SOC_next_c <- SOC + SOC_add - distance_next
  temp_c <- RangeDistribution(SOC_next_c)
  r_c <- temp_c$Range
  prob_c <- temp_c$Prob
  
  charging_duration <- round(SOC_add*consumption/power*60, 0) #minutes
  cost_charging <- ChargingCost(STRTTIME=start_charging_time, duration=charging_duration, power=power)
  asset_c <- AssetChargeDynamicPricing(r_c, SOC_add, dwell_place, cost_charging, WHYTO)

  value_c <- Value(asset_c, asset_0)
  weight_c <- CumWeight(asset_c, prob_c, asset_0)
  CPT_c <- sum(value_c*weight_c)
  
  ##3.charging yes=1 or no=0
  if (CPT_n<CPT_c) {
    return(1)
  } else {
    return(0)
  }
}

#define charging decision function, "LOOKING AHEAD"
CPTChargingDecisionLookAhead <- function(SOC, distance_next, distance_last, dwell_time, power, dwell_place, total_range, start_charging_time, WHYTO, charger_last) {
  
  ##0.determine RP: asset_0
  asset_0 <- -VOR*distance_next
  
  ##1.not charge
  SOC_next_n <- SOC - distance_next
  temp_n <- RangeDistribution(SOC_next_n)
  r_n <- temp_n$Range
  prob_n <- temp_n$Prob
  asset_n <- AssetNotCharge(r_n) #asset is outcome
  value_n <- Value(asset_n, asset_0)
  weight_n <- CumWeight(asset_n, prob_n, asset_0)
  CPT_n <- sum(value_n*weight_n)
  
  ##2.charge
  SOC_add <- min(dwell_time/60*power/consumption, total_range-SOC) 
  SOC_next_c <- SOC + SOC_add - distance_next
  temp_c <- RangeDistribution(SOC_next_c)
  r_c <- temp_c$Range
  prob_c <- temp_c$Prob
  
  charging_duration <- round(SOC_add*consumption/power*60, 0) #minutes
  cost_charging <- ChargingCost(STRTTIME=start_charging_time, duration=charging_duration, power=power)
  asset_c <- AssetChargeDynamicPricing(r_c, SOC_add, dwell_place, cost_charging, WHYTO)
  
  value_c <- Value(asset_c, asset_0)
  weight_c <- CumWeight(asset_c, prob_c, asset_0)
  CPT_c <- sum(value_c*weight_c)
  
  ##3.charging yes=1 or no=0
  scale_param <- 10
  prob_threshold <- 1/(1+exp(-scale_param*charger_last/distance_last))
  prob_charging <- 1/(1+exp(CPT_n-CPT_c))
  if (prob_charging>prob_threshold) {
    return(1)
  } else {
    return(0)
  }
}













