#This created the acv plots

# Load the processed dataframe
if (!exists("DT")){
  load("~/HWCanalysis/Masters/data/DT.Rda")
}

for (house in unique(DT$linkID)){
  q <- forecast::Acf(DT$HWelec[DT$linkID == house], lag.max = 60*24*7, # one week
                     type = "correlation", 
                     plot = FALSE, na.action = na.pass)
  assign(house, data.frame(q$acf))
  if (house == "rf_01"){
    acvDT <- data.frame(q$acf)
  } else {
  bind_cols(acvDT, eval(as.name(house)))
  }
}

acvDT <- bind_cols(rf_01,rf_02,rf_06,rf_08,rf_11,rf_12,rf_13,rf_14,rf_15b,rf_16,rf_17a,
          rf_18,rf_20,rf_22,rf_23,rf_24,rf_25,rf_27,rf_29,
          rf_30,rf_31, rf_32,rf_33,rf_34,rf_35,
          rf_36,rf_37,rf_38,rf_39,rf_40,rf_42,rf_44,rf_45)

names(acvDT) <- c("rf_01","rf_02","rf_06","rf_08","rf_11","rf_12","rf_13","rf_14",
                  "rf_15b","rf_16","rf_17a","rf_18","rf_20","rf_22","rf_23","rf_24",
                  "rf_25","rf_27","rf_29","rf_30","rf_31","rf_32","rf_33",
                  "rf_34","rf_35","rf_36","rf_37","rf_38","rf_39","rf_40","rf_42",
                  "rf_44","rf_45") 

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

save(acv_DT, file = "~/HWCanalysis/Masters/data/acv_DT.Rda")
#load("~/HWCanalysis/Masters/data/acv_DT.Rda")

ggplot(acv_DT, aes(lag,value,colour=variable)) +
       geom_line() + 
  labs(x = "Lag (minutes)", y = "Autocovariance")
ggsave("~/HWCanalysis/Masters/plots/acfAllHouses.pdf")
######################################