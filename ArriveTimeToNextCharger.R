##### This function returns a vector of distance to next charger for a given vehicle trip file

ArriveTimeToNextCharger <- function(df) { #df=df[, c('Charger', 'ENDTIME_new')]
  n <- nrow(df)
  df <- rbind(df, df)

  Charger_Num <- which(df$Charger %in% 1)
  if (length(Charger_Num)>0) {
    for (i in 1:nrow(df)) {
      if (i %in% Charger_Num[1:(length(Charger_Num)-1)]) {
        j <- Charger_Num[match(i, Charger_Num)+1] #i is current charger number, j is next charger number
        df$ARRIVETIME_next[i] <- df$ENDTIME_new[j]
      } else {
        df$ARRIVETIME_next[i] <- -1
      }
    }
  } else {
    df$ARRIVETIME_next <- -1 #-1 is flag that the between distance is not available
  }
  
  df <- df[1:n, ]
  
  return(df$ARRIVETIME_next)
}





