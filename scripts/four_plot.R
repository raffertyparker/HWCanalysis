# Plots 4 houses
# Needs model in the environment to be named according to household (rf_XX)
# Quite a 'manual' process to change parameters but it will do for now

if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/" 
}

library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 20),
  strip.text = element_blank())

theme_set(theme_minimal())

modelName <- "SVM" # For manual creation 
#models <- c("naive", "seasonalNaive", "ARIMA", "ARIMAX", "SARIMA", "simpleLinear")
fourHouses <- c("rf_06", "rf_13", "rf_22", "rf_40") 

for (modelName in models){
  for (house in fourHouses){ # Load appropriate models
    ifelse(file.exists(paste0(dFolder, "models/", modelName, "/", house, "_model.rds")),
           assign(paste0(house, "_model"), 
                  readRDS(paste0(dFolder, "models/", modelName, "/", house, "_model.rds"))),
           assign(paste0(house, "_model"), 
                  readRDS(paste0(dFolder, "models/", modelName, "/", house, "_validated_model.rds")))
           )
  }
  
  for (house in fourHouses){
    assign("pMdl", as.data.table(get(paste0(house, "_model"))$x))
    names(pMdl) <- c("dateTime_nz", "Actual")
    ifelse("fitted.values" %in% names(get(paste0(house, "_model"))), 
           pMdl$Predicted <- as.numeric(get(paste0(house, "_model"))$fitted.values),
           pMdl$Predicted <- as.numeric(get(paste0(house, "_model"))$fitted))
      pMdl$linkID <- house
    startTime <- lubridate::as_datetime(paste(as.character( # startTime is midnight on first day of next month
      rollback(pMdl$dateTime_nz[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)), "00:00:00"), 
      tz = 'Pacific/Auckland')
    pMdl <- data.table(pMdl)
    pMdl <- pMdl[!is.na(pMdl$Predicted)] 
    pMdl <- pMdl[pMdl$dateTime_nz %within% interval(startTime, startTime + days(1)), ]
    
    ifelse(house == fourHouses[1], 
           plotDT <- pMdl,
           plotDT <- rbind(plotDT, pMdl))
  }

  plotDT <- melt(plotDT, id.vars = c("dateTime_nz", "linkID"))
  #plotDT <- arrange(plotDT, dateTime_nz)
  
  p <- ggplot(data = plotDT, aes(x = dateTime_nz)) +
    geom_line(aes(y = value, colour = variable)) +
    facet_wrap(~linkID, ncol = 1, scales = "free") +
    labs(y = "Power (W)", x = "Time", colour = "", title = '')
  # facet_wrap(. ~ linkID, scales = "free")
  p
  ggsave(filename = paste0(pFolder, modelName, "/fourHouses.pdf"))
  p + My_Theme
  ggsave(filename = paste0(pFolder, modelName, "/fourHouses.png"))
}