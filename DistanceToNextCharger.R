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





