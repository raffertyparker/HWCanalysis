##############################################
# frequency analysis through fourier transform
##############################################
if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
if (!exists("DT")){
  load(paste0(dFolder, "DT.Rda"))
}

# Refer to periodogram documentation for clarification
for (house in unique(DT$linkID)){
  p <- TSA::periodogram(DT$HWelec[DT$linkID == house], 
                        plot = FALSE) # Create periodogram
  dd <- data.frame(freq=p$freq, 
                   spec=p$spec) # Select freqencies and spec
  order <- dd[order(-dd$spec),] # Order by the most dominant
  top3 <- head(order, 3)# Select top 3 most dominant
  q <- data.frame(1/top3$freq)
  names(q)[names(q) == "X1.top3.freq"] <- house
  ifelse(house == unique(DT$linkID)[1], 
         freq_DF <- as.data.frame(q), 
         freq_DF <- cbind(freq_DF, as.data.frame(q)))
  }


freq_DF$Dominance <- as.character(c(1,2,3))

freq_DF <- reshape2::melt(freq_DF)
names(freq_DF) <- c("Dominance", "Household", "Frequency")
freq_DF <- freq_DF[, c(2,1,3)]
freq_DF$Frequency <- round(freq_DF$Frequency/60, 2)  # Values in hours

save(freq_DF, file = paste0(dFolder, "freq_DF.Rda"))