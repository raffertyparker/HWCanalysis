# Plots 4 houses for SVM model
# Necessary as the SVM model is in a drastically different format to the others


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
library(e1071)

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 20),
  strip.text = element_blank())

theme_set(theme_minimal(base_size = 14))

#modelName <- "naive" # For manual creation 
Model <- "SVM" 
fourHouses <- c("rf_06", "rf_13", "rf_22", "rf_40") 
#house <- fourHouses[1]
#for (modelName in models){
  for (house in fourHouses){ # Load appropriate models
           assign(paste0(house, "_model"), 
                  readRDS(paste0(dFolder, "models/", Model,"/", house, "_fitted_model.rds")))
  
    dt_val <- as.data.table(readr::read_csv(paste0(dFolder,"households/validating/",house,
                                                   "_at_30_min_for_validating.csv")))    
    dt_val$hHour <- lubridate::as_datetime(dt_val$hHour, tz = 'Pacific/Auckland')
    dt_val$dHour <- hour(dt_val$hHour) + minute(dt_val$hHour)/60
    dt_val$nonHWshift1 <- shift(dt_val$nonHWelec)
    dt_val$nonHWshift2 <- shift(dt_val$nonHWelec, 2)
    dt_val$HWshift1 <- shift(dt_val$HWelec)
    dt_val$HWshift2 <- shift(dt_val$HWelec, 2)
    dt_val$dow <- weekdays(dt_val$hHour)
    
    predicted_values <- predict(get(paste0(house, "_model")), dt_val) # Make prediction on validating data from model
    val_mdl <- dt_val[3:nrow(dt_val)] #  Remove first two rows in order to build validated model format  necessary to build plots etc
    val_mdl <- val_mdl %>%
      select("HWelec", "hHour")
    val_mdl$Predicted <- predicted_values
    setnames(val_mdl, "HWelec", "Actual")
    val_mdl$residual <- val_mdl$HWelec - predicted_values
    #names(val_mdl)[names(val_mdl) == 'HWelec'] <- 'x'
    saveRDS(val_mdl, file = paste0(dFolder, "models/", Model,"/", house, "_validated_model.rds"))
  #  assign(paste0(house, "_validated_model"), dt_val)
  # saveRDS(get, file = paste0(dFolder, "models/", Model,"/", house, "_fitted_model.rds"))
    val_mdl$linkID <- house
    startTime <- lubridate::as_datetime(paste(as.character( # startTime is midnight on first day of next month
      rollback(val_mdl$hHour[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)), "00:00:00"), 
      tz = 'Pacific/Auckland')
   # val_mdl <- data.table(val_mdl)
   # val_mdl <- val_mdl[!is.na(val_mdl$fitted)] 
    val_mdl <- val_mdl[val_mdl$hHour %within% interval(startTime, startTime + days(1)), ]
    val_mdl[,residual:=NULL]
    ifelse(house == fourHouses[1], 
           plotDT <- val_mdl,
           plotDT <- rbind(plotDT, val_mdl))
  }
    
  plotDT <- melt(plotDT, id.vars = c("hHour", "linkID"))
  #plotDT <- arrange(plotDT, dateTime_nz)
  
  p <- ggplot(data = plotDT, aes(x = hHour)) +
    geom_line(aes(y = value, colour = variable)) +
    facet_wrap(~linkID, ncol = 1, scales = "free") +
    labs(y = "Power (W)", x = "Time", colour = "", title = '')
  # facet_wrap(. ~ linkID, scales = "free")
  p
  ggsave(filename = paste0(pFolder, Model, "/fourHouses.pdf"))
  p + My_Theme
  ggsave(filename = paste0(pFolder, Model, "/fourHouses.png"))