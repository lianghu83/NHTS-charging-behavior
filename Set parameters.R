##### This file set basic parameters and scenario parameters

#set basic parameters
consumption <- 0.3 #kWh/mi
power_1 <- 3.3 #kW
power_2 <- 6.6 #kW
power_3 <- 19.2 #kW
power_4 <- 50 #kW

#scenario name
scenario <- 'S2_R2_TOU4'
#'S2_delay_charging' for delay charging scenario

#set scenario parameters
EV_range_list <- c(100,200,300) #mi
EV_range_prob_list <- c(0.587, 0.411, 0.003) #c(0.5867, 0.4106, 0.0027)
#EV_range_list <- c(rep(249,121),rep(84,117),rep(73,45),rep(87,27),rep(107,20),rep(82,17),rep(83,14),rep(68,11),rep(76,6),rep(103,5),rep(93,5),rep(82,3),rep(94,1))

power_h <- power_2
power_w <- power_3
power_p <- power_4

coverage_home <- 1.0 #% chargers at home
coverage_work <- 0.5 #% chargers at workplace
coverage_public <- 0.5 #% chargers at public

#cost
cost_tow <- 109*(1+0.021) #road service fee, towing fee
time_hassel <- 1+1
#cost_hassel <- 0.27*time_hassel #charging hassel worth $1
cost_elec <- 0.12 #$/kWh
cost_taxi <- 2.51/(1+0.021) #$/mi, 2.4
taxi_initial <- 0 #$, 2.9
cost_service <- 5*(1+0.007)*(1+0.021) #$ service fee
#VOR <- 3.5/27.5 #value of range, 3.5/27.5, gasoline cost
VOR <- consumption*cost_elec #electricity cost

#CPT parameters, with defaults in comments
alpha <- 0.88 #0.88
beta <- 0.88 #0.88
lamda <- 2.25 #2.25
gamma <- 0.61 #0.61
delta <- 0.69 #0.69
SOC_0 <- 20 #range anxiety (mi), 20~25% of the range

#BEV in the dataset
#bev <- read.csv("H:\\NHTS2017\\Survey Data\\BEV1.csv")
#bev <- subset(bev, bev$MODEL != 29005) #remove/keep Tesla

#car ownership
veh_num <- 256115 #in the VEHICLE file, weighted sum is 222578947
NHTS_weight <- 222578947/veh_num
passenger_veh_num <- 242160
pov_veh_num <- 153776
market_penetration <- 0.17 #the Developed scenario
bev_num <- round(market_penetration * veh_num, 0)

#hassel cost function
CostHassel <- function(location, WHYTO) {
  if(location %in% c('home', 'public')) {
    return(13.6/60*time_hassel*(1+0.021))
  }
  if(location=='work' & WHYTO!=4) {
    return(13.6/60*time_hassel*(1+0.021))
  }
  if(location=='work' & WHYTO==4) {
    return(25.4/60*time_hassel*(1+0.021))
  }
}

#TOU electricity pricing used for delay charging
ElectricityPrice <- function(hour) {
  hour <- hour%/%100
  ifelse(hour<11, 0.06,
         ifelse(hour<14, 0.12,
                ifelse(hour<19, 0.18,
                       ifelse(hour<23, 0.12, 0.06))))
}

ElectricityPrice1 <- function(hour) {
  hour <- hour%/%100
  #TOU1
  if(0) {
    return(ifelse(hour<11, 0.06,
           ifelse(hour<14, 0.12,
                  ifelse(hour<19, 0.18,
                         ifelse(hour<23, 0.12, 0.06)))))
  }
  #TOU2
  if(0) {
    return(ifelse(hour<11, 0.03,
           ifelse(hour<14, 0.12,
                  ifelse(hour<19, 0.24,
                         ifelse(hour<23, 0.12, 0.03)))))
  }
  #TOU3
  if(0) {
    return(ifelse(hour<11, 0.06,
           ifelse(hour<16, 0.12,
                  ifelse(hour<21, 0.18,
                         ifelse(hour<24, 0.12, 0.06)))))
  }
  #TOU4
  if(1) {
    return(ifelse(hour<11, 0.03,
           ifelse(hour<16, 0.12,
                  ifelse(hour<21, 0.24,
                         ifelse(hour<24, 0.12, 0.03)))))
  }
}

#dynamic electricity pricing
DynamicElectricityPrice <- function(hour) {
  demand_change_ratio <- 1.0*c(-0.47489774, -0.66005544, -0.79245769, -0.87478866, -0.90303867, -0.82541327,
                               -0.67314488, -0.37439066, -0.27582215, -0.33349856, -0.21125306, -0.02500181,
                               0.27885564,  0.41107563,  0.38935378,  0.45062565,  0.74959527,  1.15361185,
                               1.20600278,  0.92070252,  0.63932910,  0.35598397,  0.06783386, -0.19920745)
  hour <- hour%/%100
  i <- hour+1
  return(cost_elec*(1+demand_change_ratio[i]))
}



