# This created the acv plots

# ACV PLOTS #
###############################################

if (!exists("pFile")){
  pFile <- "~/HWCanalysis/plots/"
}
if (!exists("dFile")){
  dFile <- "~/HWCanalysis/data/" 
}
if (!exists("DT")){
  load(paste0(dFile, "DT.Rda"))
}

library(ggplot2)
theme_set(theme_minimal())

for (house in unique(DT$linkID)){
  q <- forecast::Acf(DT$HWelec[DT$linkID == house], lag.max = 60*24*7, # one week
                     type = "correlation", 
                     plot = FALSE, na.action = na.pass)
  s <- as.data.frame(q$acf)
  names(s) <- "value"
  s$household <- house
  s$lag <- seq(from = 0, to = 10080)
  # s$lag <- s$lag - 1441
  ifelse(house == unique(DT$linkID)[1],
         acvDT <- s,
         acvDT <- rbind(acvDT, s))
}

acvDT <- data.table::data.table(acvDT)
acvDT$lag <- as.numeric(acvDT$lag)

save(acvDT, file = paste0(dFile, "acvDT.Rda"))

# lag > 60 selected to remove acv = 1 effect at lag = 0 and corresponding effect on plot
ggplot(acvDT[lag > 60], aes(lag,value,colour=household)) +
       geom_line() + 
  labs(x = "Lag (minutes)", y = "Autocovariance", 
       colour = "Household")
ggsave(paste0(pFile, "acfAllHouses.pdf"))