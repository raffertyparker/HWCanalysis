#This created the acv plots

# ACV PLOTS #
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

acvDT <- data.table(acvDT)

save(acvDT, file = paste0(dFile, "acvDT.Rda"))

ggplot(acvDT[lag > 1], aes(lag,value,colour=household)) +
       geom_line() + 
  labs(x = "Lag (minutes)", y = "Autocovariance", 
       colour = "Household")
ggsave(paste0(pFile, "acfAllHouses.pdf"))
######################################