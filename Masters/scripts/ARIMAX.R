# This calculates a seasonal autoregression for each household 
# with non-hot water electricity used as external regressor

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
  assign(paste0(house, "_at_30_min_for_fitting"), 
         as.data.table(readr::read_csv(paste0("Masters/data/households/fitting/", 
                                              house,"_at_30_min_for_fitting.csv"))))
  assign(paste0(house, "_at_30_min_for_validating"), 
         as.data.table(readr::read_csv(paste0("Masters/data/households/validating/", 
                                              house,"_at_30_min_for_validating.csv"))))
  assign(paste0("ts_", house, "_fitting"), 
         as.xts(get(paste0(house, "_at_30_min_for_fitting"))))
  assign(paste0("ts_", house, "_testing"), 
         as.xts(get(paste0(house, "_at_30_min_for_validating"))))
  
  # Create ARIMAX model from training data
  assign(paste0(house, "_30_min_ARIMAX"), 
         auto.arima(get(paste0("ts_", house, "_fitting"))$HWelec, 
                    stepwise = FALSE, approximation = FALSE, 
                    xreg = get(paste0("ts_", house, "_fitting"))$nonHWelec)) %>% 
    save(file = paste0(dFile, "models/ARIMAX/", house, "_fitted_model.Rda"))
  # Validate model from test data
  assign(paste0(house, "_30_min_ARIMAX_validate"), 
         Arima(get(paste0("ts_", house, "_testing"))$HWelec, 
               xreg = get(paste0("ts_", house, "_testing"))$nonHWelec, 
               model = get(paste0(house, "_30_min_ARIMAX")))) %>%
    save(file = paste0(dFile, "models/ARIMAX/", house, "_validated_model.Rda"))
  # Make plot for demonstration purposes
  plotModel(get(paste0(house, "_30_min_ARIMAX_validate")), "ARIMAX")
}