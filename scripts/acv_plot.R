# This creates the acv plots
# Could probably be updated to use half-hour averaged data 

###############################################

if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/"
}
if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
if (!exists("DT")){
  load(paste0(dFolder, "DT.Rda"))
}

library(ggplot2)
theme_set(theme_minimal())
# house <- "rf_06"
for (house in unique(DT$linkID)){
  q <- forecast::Acf(DT$HWelec[DT$linkID == house], lag.max = 60*24*10, # 10 days
                     type = "correlation", 
                     plot = FALSE, na.action = na.pass)
  s <- as.data.frame(q$acf)
  names(s) <- "value"
  s$household <- house
  s$lag <- seq(from = 0, to = nrow(s)-1)
  # s$lag <- s$lag - 1441
  ifelse(house == unique(DT$linkID)[1],
         acvDT <- s,
         acvDT <- rbind(acvDT, s))
}

acvDT <- data.table::data.table(acvDT)
acvDT$lag <- as.numeric(acvDT$lag)

save(acvDT, file = paste0(dFolder, "acvDT.Rda"))

# lag > 60 selected to remove acv = 1 effect at lag = 0 and corresponding effect on plot
ggplot(acvDT[lag > 60], aes(lag,value,colour=household)) +
       geom_line() + 
  labs(x = "Lag (minutes)", y = "Autocovariance", 
       colour = "") +
  theme(legend.position="top") +
  guides(colour=guide_legend(nrow=3,byrow=TRUE))
ggsave(paste0(pFolder, "acfAllHouses.pdf"))