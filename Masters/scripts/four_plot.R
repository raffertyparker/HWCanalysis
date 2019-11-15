# Plots 4 houses
# Needs model in the environment to be named according to household (rf_XX)
# Could be improved i.e. to start and end at midnight

if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}

library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)

modelName <- "ARIMAX"
fourHouses <- c("rf_06", "rf_13", "rf_22", "rf_40") 

for (house in fourHouses){ # Load appropriate models
  ifelse(file.exists(paste0(dFile, "models/", modelName, "/", house, "_model.rds")),
         assign(paste0(house, "_model"), 
                readRDS(paste0(dFile, "models/", modelName, "/", house, "_model.rds"))),
         assign(paste0(house, "_model"), 
                readRDS(paste0(dFile, "models/", modelName, "/", house, "_validated_model.rds")))
         )
}

for (house in fourHouses){
  assign("pMdl", as.data.table(get(paste0(house, "_model"))$x))
  names(pMdl) <- c("dateTime_nz", "Actual")
  ifelse("fitted.values" %in% names(get(paste0(house, "_model"))), 
         pMdl$Fitted <- as.numeric(get(paste0(house, "_model"))$fitted.values),
         pMdl$Fitted <- as.numeric(get(paste0(house, "_model"))$fitted))
  pMdl$linkID <- house
  startTime <- lubridate::as_datetime(paste(as.character( # startTime is midnight on first day of next month
    rollback(pMdl$dateTime_nz[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)), "00:00:00"), 
    tz = 'Pacific/Auckland')
  pMdl <- data.table(pMdl)
  pMdl <- pMdl[!is.na(pMdl$Fitted)] 
  pMdl <- pMdl[pMdl$dateTime_nz %within% interval(startTime, startTime + days(1)), ]
  
  ifelse(house == fourHouses[1], 
         plotDT <- pMdl,
         plotDT <- rbind(plotDT, pMdl))
}

plotDT <- melt(plotDT, id.vars = c("dateTime_nz", "linkID"))
#plotDT <- arrange(plotDT, dateTime_nz)

#library('ggplot2')
theme_set(theme_minimal(base_size = 13))

p <- ggplot(data = plotDT, aes(x = dateTime_nz)) +
  geom_line(aes(y = value, colour = variable), size = 1.5) +
  facet_wrap(~linkID, ncol = 1, scales = "free")
#facet_wrap(. ~ linkID, scales = "free")
p + labs(y = "Power (W)", x = "", colour = "")
ggsave(filename = paste0(pFile, modelName, "/fourHouses.pdf"))
ggsave(filename = paste0(pFile, modelName, "/fourHouses.png"))
