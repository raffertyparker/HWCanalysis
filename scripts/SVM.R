# This scripts creates the models used for the analysis, saves them,
# calculates time taken to fit, RMSE and size of file.
# Has been assimilated into modelling.R


if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}
if (!exists("sFile")){
  sFile <- "~/HWCanalysis/Masters/scripts/" 
}

source(paste0(sFile, "plot_model.R"))
library(forecast)
library(xts)
library(data.table)
library(ggplot2)
library(dplyr)

load(paste0(dFile, "houses.Rda"))

# Create empty dataframe to full with summary of models
DFsummary <- data.frame(model=character(),
                        household=character(),
                        RMSE=numeric(),
                        fittingTime=numeric(),
                        memSize=numeric(),
                        stringsAsFactors=FALSE)
#names(DFsummary) <- c("model", "household", "RMSE", "fittingTime", "memSize")
#for (house in houses){
house <- houses[1]  
  
  dt_fit <- as.data.table(readr::read_csv(paste0(dFile,"households/fitting/",house,
                                                 "_at_30_min_for_fitting.csv")))
  dt_val <- as.data.table(readr::read_csv(paste0(dFile,"households/validating/",house,
                                                 "_at_30_min_for_validating.csv")))
  dt_fit$hHour <- lubridate::as_datetime(dt_fit$hHour, tz = 'Pacific/Auckland')
  dt_fit$dHour <- hour(dt_fit$hHour) + minute(dt_fit$hHour)/60
  dt_val$hHour <- lubridate::as_datetime(dt_val$hHour, tz = 'Pacific/Auckland')
  dt_val$dHour <- hour(dt_val$hHour) + minute(dt_val$hHour)/60
  dt_val$nonHWshift1 <- shift(dt_val$nonHWelec)
  dt_val$nonHWshift2 <- shift(dt_val$nonHWelec, 2)
  dt_val$HWshift1 <- shift(dt_val$HWelec)
  dt_val$HWshift2 <- shift(dt_val$HWelec, 2)
  dt_fit$nonHWshift1 <- shift(dt_fit$nonHWelec)
  dt_fit$nonHWshift2 <- shift(dt_fit$nonHWelec, 2)
  dt_fit$HWshift1 <- shift(dt_fit$HWelec)
  dt_fit$HWshift2 <- shift(dt_fit$HWelec, 2)
  dt_fit$dow <- weekdays(dt_fit$hHour)
  dt_val$dow <- weekdays(dt_val$hHour)
  ts_fit <- as.xts(dt_fit)
  ts_val <- as.xts(dt_val)
  
  Model <- "SVM"  
  fitTime <- system.time(
    fitSVM <- svm(HWelec ~ dHour + nonHWshift1 + nonHWshift2
                  + HWshift1 + HWshift2 + dow, dt_fit))[3]
  dir.create(paste0(dFile, "models/", Model,"/"), showWarnings = FALSE)
  saveRDS(fitSVM, file = paste0(dFile, "models/", Model,"/", house, "_fitted_model.rds"))
  predicted_values <- predict(fitSVM, dt_val)
  SVMresidual <- dt_val$HWelec[2:nrow(dt_val)] - predicted_values
  plotModelSVM(dt_val, predicted_values)
  sVec <- data.frame(model=Model,
                     household=house,
                     RMSE=sqrt(mean(SVMresidual^2)),
                     fittingTime=as.numeric(fitTime),
                     memSize=as.numeric(object.size(fitSVM)),
                     stringsAsFactors=FALSE)
  DFsummary <- rbind(DFsummary, sVec)
  
  