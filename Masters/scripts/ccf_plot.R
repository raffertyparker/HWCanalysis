# CCV PLOTS #
###############################################
# If plot location pFile and data location dFile
# already exist in global variables
# make sure they point to correct paths
# If they are not, make sure the following lines
# define them correctly
###############################################
if (!exists("pFile")){
  pFile <- "/home/parra358/HWCanalysis/Masters/plots/"
}
if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("DT")){
  load(paste0(dFile, "DT.Rda"))
}
for (house in unique(DT$linkID)){
  q <- forecast::Ccf(DT$HWelec[DT$linkID == house], DT$nonHWelec[DT$linkID == house], lag.max = 60*24,
                     type = "correlation",
                     plot = FALSE, na.action = na.pass)
  assign(house, data.frame(q$acf))
}

ccvDT <- bind_cols(rf_01,rf_06,rf_08,rf_11,rf_13,rf_14,
                   rf_15b,rf_17a,rf_22,rf_24,
                   rf_25,rf_27,rf_29,rf_30,rf_31, rf_32,rf_33,rf_34,
                   rf_35,rf_36,rf_37,rf_38,rf_39,rf_40,rf_42,rf_44,rf_45)

names(ccvDT) <- c("rf_01","rf_06","rf_08","rf_11","rf_13","rf_14",
                  "rf_15b","rf_16","rf_17a","rf_18","rf_20","rf_22","rf_23","rf_24",
                  "rf_25","rf_27","rf_29","rf_30","rf_31","rf_32","rf_33",
                  "rf_34","rf_35","rf_36","rf_37","rf_38","rf_39","rf_40","rf_42",
                  "rf_44","rf_45") 


ccvDT$lag <- seq(from = 1, to = 2881)
ccvDT$lag <- as.character(ccvDT$lag) # Necessary for correct melt

ccv_DT <- reshape2::melt(ccvDT)
ccv_DT$lag <- as.numeric(ccv_DT$lag)
ccv_DT$lag <- ccv_DT$lag - 1441

ccv_DT <- data.table(ccv_DT)

save(ccv_DT, file = paste0(dFile, "ccv_DT.Rda"))

# The following finds the value of ccv_DT that has the maximum correlation
# for each household and for lags greater than 30 mins

maxCor <- ccv_DT %>%
  group_by(variable) %>%
  filter(lag >= 30) %>%
  slice(which.max(value))

save(maxCor, file = paset0(dFile, "ccv_max.Rda"))

ggplot(maxCor, aes(x = lag,fill=variable)) +
  geom_histogram() + 
  labs(x = "Lag (minutes)", y = "Count", fill = "Household")
ggsave(paste0(pFile, "maxXcorAllHouses.pdf"))

# This perhaps needs editing to feed variables into a function
# Rather than just copy/pasted

ggplot(ccv_DT[ccv_DT$lag > 0, ], aes(lag,value,colour=variable)) +
  geom_line() + 
  labs(x = "Lag (minutes)", y = "Cross-correlogram", colour = "House")
ggsave(paste0(pFile, "ccfAllHouses.pdf"))

ggplot(ccv_DT[ccv_DT$lag > 0 & ccv_DT$lag < 60, ], aes(lag,value,colour=variable)) +
  geom_line() + 
  labs(x = "Lag (minutes)", y = "Cross-correlogram", colour = "House")
ggsave(paste0(pFile, "ccfAllHouses1Hour.pdf"))

ggplot(ccv_DT[ccv_DT$lag > 0 & ccv_DT$lag < 120, ], aes(lag,value,colour=variable)) +
  geom_line() + 
  labs(x = "Lag (minutes)", y = "Cross-correlogram", colour = "House")
ggsave(paste0(pFile, "ccfAllHouses2Hour.pdf"))
