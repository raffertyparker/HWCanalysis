# frequency analysis through fourier transform
if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("DT")){
  load(paste0(dFile, "DT.Rda"))
}

for (house in unique(DT$linkID)){
  p <- TSA::periodogram(DT$HWelec[DT$linkID == house], plot = FALSE) # Create periodogram
  dd <- data.frame(freq=p$freq, spec=p$spec) # Select freqencies and spec (refer to documentation)
  order <- dd[order(-dd$spec),] # Order by the most dominant
  top3 <- head(order, 3)# Select top 3 most dominant
  q <- data.frame(1/top3$freq)
  names(q)[names(q) == "X1.top3.freq"] <- house
  assign(house, q) # Assign these 3 to dataframe names according to house
}
  
freq_DF <- dplyr::bind_cols(rf_01,rf_02,rf_06,rf_08,rf_11,rf_12,rf_13,rf_14,
                            rf_15b,rf_16,rf_17a,rf_18,rf_20,rf_22,rf_23,rf_24,
                            rf_25,rf_27,rf_29,rf_30,rf_31, rf_32,rf_33,rf_34,
                            rf_35,rf_36,rf_37,rf_38,rf_39,rf_40,rf_42,rf_44,rf_45)

freq_DF$Dominance <- as.character(c(1,2,3))

freq_DF <- reshape2::melt(freq_DF)
names(freq_DF) <- c("Dominance", "Household", "Frequency")
freq_DF <- freq_DF[, c(2,1,3)]
freq_DF$Frequency <- freq_DF$Frequency/60  # Gives values in hours

save(freq_DF, file = paste0(dFile, "freq_DF.Rda"))