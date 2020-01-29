# CCV PLOTS #
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

types <- c("acv", "ccv")


for (house in unique(DT$linkID)){
  q <- forecast::Ccf(DT$HWelec[DT$linkID == house], DT$nonHWelec[DT$linkID == house], lag.max = 60*24,
                     type = "correlation",
                     plot = FALSE, na.action = na.pass)
  s <- as.data.frame(q$acf)
  names(s) <- "value"
  s$household <- house
  s$lag <- seq(from = -1440, to = 1440)
  ifelse(house == unique(DT$linkID)[1],
    ccvDT <- s,
    ccvDT <- rbind(ccvDT, s))
}

ccvDT <- data.table(ccvDT)

save(ccvDT, file = paste0(dFile, "ccvDT.Rda"))

# The following finds the value of ccvDT that has the maximum correlation
# for each household and for lags greater than 30 mins

maxCor <- ccvDT %>%
  group_by(household) %>%
  filter(lag >= 30) %>%
  slice(which.max(value))

save(maxCor, file = paset0(dFile, "ccv_max.Rda"))

ggplot(maxCor, aes(x = lag,fill=household)) +
  geom_histogram() + 
  labs(x = "Lag (minutes)", y = "Count", fill = "Household")
ggsave(paste0(pFile, "maxXcorAllHouses.pdf"))

# This could do with being edited to feed variables into a function
# Rather than just copy/pasted

ggplot(ccvDT[ccvDT$lag > 0, ], aes(lag,value,colour=household)) +
  geom_line() + 
  labs(x = "Lag (minutes)", y = "Cross-correlogram", colour = "House")
ggsave(paste0(pFile, "ccfAllHouses.pdf"))

ggplot(ccvDT[ccvDT$lag > 0 & ccvDT$lag < 60, ], aes(lag,value,colour=household)) +
  geom_line() + 
  labs(x = "Lag (minutes)", y = "Cross-correlogram", colour = "House")
ggsave(paste0(pFile, "ccfAllHouses1Hour.pdf"))

ggplot(ccvDT[ccvDT$lag > 0 & ccvDT$lag < 120, ], aes(lag,value,colour=household)) +
  geom_line() + 
  labs(x = "Lag (minutes)", y = "Cross-correlogram", colour = "House")
ggsave(paste0(pFile, "ccfAllHouses2Hour.pdf"))
