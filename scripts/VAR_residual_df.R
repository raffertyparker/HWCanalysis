# This script creates and saves a dataframe comprised of the residuals
# of the VAR models created to forecast hot water electricity demand.
# VAR model no longer in use so this can probably be disregarded.

library(TSA)
library(vars)

houseNums <-  c("06", "08", "13", "22", "24", "25", "27", "29", "30", "31", "32",
                "33", "34", "35", "36", "37", "38", "40", "42", "44", "45")
mins <- "30"  # Enter minute sample (30 recommended)

i = "06" 

for (i in houseNums){
  # load and wrangle data
  df <- read.csv(paste("~/GridspyData/rf_", i, "/25May2015-25May2016at", mins, "_rf_", i,
                       "_clean.csv", sep = ""))
  allElec <- rowSums(df[sapply(df, is.numeric)], na.rm = TRUE)
  HWelec <- df[ , grepl("HotWater" , names(df))]
  HWelec[is.na(HWelec)] <- 0
  nonHWelec <- allElec - HWelec
  df1 <- data.frame(df["Time"], HWelec, nonHWelec)
  colnames(df1) <- c("times", "HWelec", "nonHWelec")
  df2 <- df1[ , 2:3]
  # use frequency analysis to determine periodicity
  Q = periodogram(df2$HWelec, plot = FALSE) 
  dd = data.frame(freq=Q$freq, spec=Q$spec)
  order = dd[order(-dd$spec),]
  top2 = head(order, 2)
  time3 = 1/top2$f
  # create VAR model 
  varLoop <- VAR(df2, p = 50, season = time3) 
  # obtain residuals
  HWres <- varLoop$varresult$HWelec$residuals
  #  res <- residuals(varLoop)  superceeded by preceeding line
  #  HWres <- res["HWelec"]
  # create dataframe, note that VAR drops first p values
  times <- tail(df1["times"], nrow(df1)-50)
  house <- rep(i, length(times))
  houseDF <- cbind(times, house, HWres)
  # Inititalise for first house
  if (i == "06"){
    resDF <- houseDF
  } else {
    resDF <- rbind(resDF, houseDF)
  }
}

resDF$times <- as.POSIXlt(resDF$times)
resDF$HWresAbs <- abs(resDF$HWres)

w <- aggregate(list(HWres = resDF$HWres, HWresAbs = resDF$HWresAbs), 
               list(halfHour = cut(resDF$times, "30 min")), mean)

save(resDF, file = paste0(dFile, "resDF.Rda"))
save(w, file = paste0(dFile, "amalgamatedRes.Rda"))