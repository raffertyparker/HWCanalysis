# This scripts creates the models used for the analysis, saves them,
# calculates time taken to fit, RMSE and size of file

  if (!exists("dFile")){
    dFile <- "~/HWCanalysis/data/" 
  }
  if (!exists("pFile")){
    pFile <- "~/HWCanalysis/plots/" 
  }
  if (!exists("sFile")){
    sFile <- "~/HWCanalysis/scripts/" 
  }
  
  source(paste0(sFile, "plot_model.R"))
  source(paste0(sFile, "plot_model_SVM.R"))
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
  for (house in houses){
#  house <- houses[1]  
  
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
    ts_fit <- as.xts(dt_fit)
    ts_val <- as.xts(dt_val)
    dt_fit$dow <- weekdays(dt_fit$hHour)
    dt_val$dow <- weekdays(dt_val$hHour)

    
    Model <- "SVM"  
    fitTime <- system.time(
      fitSVM <- svm(HWelec ~ dHour + nonHWshift1 + nonHWshift2
                    + HWshift1 + HWshift2 + dow, dt_fit))[3]
    dir.create(paste0(dFile, "models/", Model,"/"), showWarnings = FALSE)
    saveRDS(fitSVM, file = paste0(dFile, "models/", Model,"/", house, "_fitted_model.rds"))
    predicted_values <- predict(fitSVM, dt_val)
    SVMresidual <- dt_val$HWelec[3:nrow(dt_val)] - predicted_values
    plotModelSVM(dt_val, predicted_values)
    sVec <- data.frame(model=Model,
                       household=house,
                       RMSE=sqrt(mean(SVMresidual^2)),
                       fittingTime=as.numeric(fitTime),
                       memSize=as.numeric(object.size(fitSVM)),
                       stringsAsFactors=FALSE)
    DFsummary <- rbind(DFsummary, sVec)
  
  Model <- "naive"
  mdl <- naive(ts_val$HWelec, h = 1) # Fits model
  mdl$x <- ts_val$HWelec
  dir.create(paste0(dFile, "models/", Model,"/"), showWarnings = FALSE)
  saveRDS(mdl, file = paste0(dFile, "models/", Model,"/", house, "_model.rds"))
  plotModel(mdl, Model)
  sVec <- data.frame(model=Model,
                     household=house,
                     RMSE=sqrt(mean(mdl$residuals^2, na.rm = TRUE)),
                     fittingTime=0,
                     memSize=as.numeric(object.size(ts_val$HWelec[1])),
                     stringsAsFactors=FALSE)
  DFsummary <- rbind(DFsummary, sVec)
  
  Model <- "seasonalNaive"
  ts_val_HW_seasonal <- ts(dt_val$HWelec, frequency = 48*7)
  mdl <- snaive(ts_val_HW_seasonal, h = 1) # Fits model
  mdl$x <- ts_val$HWelec
  dir.create(paste0(dFile, "models/", Model,"/"), showWarnings = FALSE)
  saveRDS(mdl, file = paste0(dFile, "models/", Model,"/", house, "_model.rds"))
  plotModel(mdl, Model)
  sVec <- data.frame(model=Model,
                     household=house,
                     RMSE=sqrt(mean(mdl$residuals^2, na.rm = TRUE)),
                     fittingTime=0,
                     memSize=as.numeric(object.size(ts_val$HWelec[1:48*7])),
                     stringsAsFactors=FALSE)
  DFsummary <- rbind(DFsummary, sVec)
  
  Model <- "simpleLinear"
  fitTime <- system.time(
    fitMdl <- lm(ts_val$HWelec~ts_val$nonHWshift1))[3] # Fits model and returns time taken to do so
  dir.create(paste0(dFile, "models/", Model,"/"), showWarnings = FALSE)
  saveRDS(fitMdl, file = paste0(dFile, "models/", Model,"/", house, "_fitted_model.rds"))
  valMdl <- lm(fitMdl, newdata = ts_fit, h = 1)
  valMdl$x <- ts_val$HWelec[2:nrow(ts_val)]
  saveRDS(valMdl, file = paste0(dFile, "models/", Model,"/", house, "_validated_model.rds"))
  plotModel(valMdl, Model)
  sVec <- data.frame(model=Model,
                     household=house,
                     RMSE=sqrt(mean(valMdl$residuals^2)),
                     fittingTime=as.numeric(fitTime),
                     memSize=as.numeric(object.size(fitMdl)),
                     stringsAsFactors=FALSE)
  DFsummary <- rbind(DFsummary, sVec)

  Model <- "ARIMA"
  #proc.time() <- fitTime
  fitTime <- system.time(
    fitArima <- auto.arima(ts_fit$HWelec))[3] # Fits model and returns time taken to do so
 # fitTime <- proc.time()
  dir.create(paste0(dFile, "models/", Model,"/"), showWarnings = FALSE)
  saveRDS(fitArima, file = paste0(dFile, "models/", Model,"/", house, "_fitted_model.rds"))
  valArima <- Arima(ts_val$HWelec, model = fitArima)
  saveRDS(valArima, file = paste0(dFile, "models/", Model,"/", house, "_validated_model.rds"))
  plotModel(valArima, Model)
  sVec <- data.frame(model=Model,
                          household=house,
                          RMSE=sqrt(mean(valArima$residuals^2)),
                          fittingTime=as.numeric(fitTime),
                          memSize=as.numeric(object.size(fitArima)),
                          stringsAsFactors=FALSE)
  DFsummary <- rbind(DFsummary, sVec)
  
  Model <- "ARIMAX"
  fitTime <- system.time(fitArima <- auto.arima(ts_fit$HWelec, 
                                                xreg = ts_fit$nonHWelec))[3] # Fits model and returns time taken to do so
  dir.create(paste0(dFile, "models/", Model,"/"), showWarnings = FALSE)
  saveRDS(fitArima, file = paste0(dFile, "models/", Model,"/", house, "_fitted_model.rds"))
  valArima <- Arima(ts_val$HWelec, xreg = ts_val$nonHWelec, model = fitArima)
  saveRDS(valArima, file = paste0(dFile, "models/", Model,"/", house, "_validated_model.rds"))
  plotModel(valArima, Model)
  sVec <- data.frame(model=Model,
                     household=house,
                     RMSE=sqrt(mean(valArima$residuals^2)),
                     fittingTime=as.numeric(fitTime),
                     memSize=as.numeric(object.size(fitArima)),
                     stringsAsFactors=FALSE)
  DFsummary <- rbind(DFsummary, sVec)
  
  Model <- "SARIMA"
  fitTime <- system.time(fitArima <- auto.arima(ts_fit$HWelec, 
                                                xreg = ts_fit$dHour))[3] # Fits model and returns time taken to do so
  dir.create(paste0(dFile, "models/", Model,"/"), showWarnings = FALSE)
  saveRDS(fitArima, file = paste0(dFile, "models/", Model,"/", house, "_fitted_model.rds"))
  valArima <- Arima(ts_val$HWelec, xreg = ts_val$dHour, model = fitArima)
  saveRDS(valArima, file = paste0(dFile, "models/", Model,"/", house, "_validated_model.rds"))
  plotModel(valArima, Model)
  sVec <- data.frame(model=Model,
                     household=house,
                     RMSE=sqrt(mean(valArima$residuals^2)),
                     fittingTime=as.numeric(fitTime),
                     memSize=as.numeric(object.size(fitArima)),
                     stringsAsFactors=FALSE)
  DFsummary <- rbind(DFsummary, sVec)
}

saveRDS(DFsummary, file = paste0(dFile, "allHouseModelStats.rds"))
allHouseSummary <- DFsummary %>%
  group_by(model) %>%
  summarise(RMSE = mean(RMSE), fittingTime = mean(fittingTime),
            memSize = mean(memSize))
saveRDS(allHouseSummary, file = paste0(dFile, "allModelSummaryStats.rds"))