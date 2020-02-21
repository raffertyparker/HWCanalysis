# This calculates an autoregression model for each household 
# with non-hot water electricity used as external regressor.
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

load(paste0(dFile, "houses.Rda"))

for (house in houses){
  dt_fit <- as.data.table(readr::read_csv(paste0(dFile,"households/fitting/",house,
                                                 "_at_30_min_for_fitting.csv")))
  dt_val <- as.data.table(readr::read_csv(paste0(dFile,"households/validating/",house,
                                                 "_at_30_min_for_validating.csv")))
  dt_fit$hHour <- lubridate::as_datetime(dt_fit$hHour, tz = 'Pacific/Auckland')
  dt_fit$dHour <- hour(dt_fit$hHour)
  dt_val$hHour <- lubridate::as_datetime(dt_val$hHour, tz = 'Pacific/Auckland')
  dt_val$dHour <- hour(dt_val$hHour)
  ts_fit <- as.xts(dt_fit)
  ts_val <- as.xts(dt_val)
  
  # Create ARIMAX model from training data
  # NOTE due to high frequency of time series this cannot be done using inbuilt seasonality function
  # instead we send hour of day as an x_reg dummy variable
  assign(paste0(house, "_30_min_ARIMAX"), 
         auto.arima(ts_fit$HWelec, 
                    stepwise = FALSE, approximation = FALSE, 
                    xreg = as.matrix(cbind(ts_fit$nonHWelec, 
                             ts_fit$nonHWshift1, 
                             ts_fit$nonHWshift2)))) %>% 
    save(file = paste0(dFile, "models/ARIMAX/", house, "_fitted_model.Rda"))
  # Validate model from test data
  assign(paste0(house, "_30_min_ARIMAX_validate"), 
         Arima(ts_val$HWelec, 
               xreg = as.matrix(cbind(ts_fit$nonHWelec, 
                                      ts_fit$nonHWshift1, 
                                      ts_fit$nonHWshift2)), 
               model = get(paste0(house, "_30_min_ARIMAX")))) %>%
    saveRDS(file = paste0(dFile, "models/ARIMAX/", house, "_validated_model.rds"))
  # Make plot for demonstration purposes
  plotModel(get(paste0(house, "_30_min_ARIMAX_validate")), "ARIMAX")
}