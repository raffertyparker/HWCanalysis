#This created the acv plots
if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}
# Load the processed dataframe
if (!exists("DT")){
  load(paste0(dFile, "DT.Rda"))
}

for (house in unique(DT$linkID)){
  q <- forecast::Acf(DT$HWelec[DT$linkID == house], lag.max = 60*24*7, # one week
                     type = "correlation", 
                     plot = FALSE, na.action = na.pass)
  names(q$acf) <- house
  assign(house, data.frame(q$acf))
}

acvDT <- bind_cols(rf_01,rf_06,rf_08,rf_11,rf_13,rf_14,
                   rf_15b,rf_17a,rf_22,rf_24,rf_25,rf_27,
                   rf_29,rf_30,rf_31, rf_32,rf_33,rf_34,
                   rf_35,rf_36,rf_37,rf_38,rf_39,rf_40,
                   rf_42,rf_44,rf_45)

names(acvDT) <- unique(DT$linkID)
  

acvDT$lag <- seq(from = 1, to = 60*24*7 + 1)
acvDT$lag <- as.character(acvDT$lag) # Necessary for correct melt

################################################################
# There *should* be a much more elegant way to do the above
# Something along the lines of:

#s <- names(.GlobalEnv)
#names <- s[grepl("rf_", s) == TRUE]

#lapply(get(names), cbind)
################################################################

acv_DT <- reshape2::melt(acvDT)
acv_DT$lag <- as.numeric(acv_DT$lag)

save(acv_DT, file = paste0(dFile, "acv_DT.Rda"))
#load(paste0(dFile, "acv_DT.Rda"))

ggplot(acv_DT, aes(lag,value,colour=variable)) +
       geom_line() + 
  labs(x = "Lag (minutes)", y = "Autocovariance")
ggsave(paste0(pFile, "acfAllHouses.pdf"))
######################################