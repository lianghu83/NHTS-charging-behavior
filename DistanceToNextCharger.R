##### This function returns a vector of distance to next charger for a given vehicle trip file
DistanceToNextCharger <- function(df) { #df=df[, c('Charger', 'VMT_MILE')]
  n <- nrow(df)
  df <- rbind(df, df)

  Charger_Num <- which(df$Charger %in% 1)
  if (length(Charger_Num)>0) {
    for (i in 1:nrow(df)) {
      if (i %in% Charger_Num[1:(length(Charger_Num)-1)]) {
        j <- Charger_Num[match(i, Charger_Num)+1] #i is current charger number, j is next charger number
        df$Between_d[i] <- sum(df$VMT_MILE[(i+1):j])
      } else {
        df$Between_d[i] <- 0
      }
    }
  } else {
    df$Between_d <- 0 #0 is flag that the between distance is not available
  }
  
  df <- df[1:n, ]
  
  return(df$Between_d)
}



##### This function returns a vector of AVERAGE distance to next charger for a given vehicle trip file,
##### CONSIDERING "LOOKING AHEAD"
LookAheadDistanceToNextCharger <- function(df) {

  for (i in 1:nrow(df)) {
    if (df$Charger[i]!=0) {
      temp <- df[i:nrow(df), ]
      temp <- subset(temp, temp$Charger==1)
      if (nrow(temp)>1) {
        temp <- temp[-nrow(temp), ]
        n_temp <- nrow(temp)
        df$LookAhead_d[i] <- sum(temp$Between_d * c(n_temp:1)/n_temp)
      } else {
        df$LookAhead_d[i] <- df$Between_d[i]
      }
    } else {
      df$LookAhead_d[i] <- 0
    }
  }
  
  return(df$LookAhead_d)

}



##### This function returns a vector of distance to LAST detination of the day
##### CONSIDERING "LOOKING AHEAD"
DistanceToLastDest <- function(df) {
  n <- nrow(df)
  
  if (n>1) {
    for (i in 1:(n-1)) {
      df$ToLast_d[i] <- sum(df$VMT_MILE[(i+1):n])
    }
    df$ToLast_d[n] <- df$Between_d[n]
  } else {
    df$ToLast_d[n] <- df$Between_d[n]
  }
  
  #return(df$ToLast_d)
  return(sapply(df$ToLast_d, function(x) max(x, 0.01))) #ToLast_d could be 0
}



##### This function returns a vector of remaining charging opportunities to the last destination
##### CONSIDERING "LOOKING AHEAD"
ChargingOpportunityToLastDest <- function(df) {
  n <- nrow(df)
  
  if (n>1) {
    for (i in 1:(n-1)) {
      df$ToLast_charger[i] <- max(sum(df$Charger[(i+1):n])-1, 0)
    }
    df$ToLast_charger[n] <- 0
  } else {
    df$ToLast_charger[n] <- 0
  }
  
  return(df$ToLast_charger)
}






