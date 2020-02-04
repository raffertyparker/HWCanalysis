# This calculates a seasonal autoregression for each household and compares it
# Outdated and needs a tidy up
# Has been assimilated into modelling.R

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
library(forecast)
library(xts)
library(data.table)
library(ggplot2)

load(paste0(dFile, "houses.Rda"))

for (house in houses){
  assign(paste0(house, "_at_30_min_for_fitting"), 
         as.data.table(readr::read_csv(paste0("households/fitting/", 
                                       house,"_at_30_min_for_fitting.csv"))))
  assign(paste0(house, "_at_30_min_for_validating"), 
         as.data.table(readr::read_csv(paste0("Masters/data/households/validating/", 
                                       house,"_at_30_min_for_validating.csv"))))
  assign(paste0("ts_", house, "_fitting"), 
         as.xts(get(paste0(house, "_at_30_min_for_fitting")), frequency = 48*365))
  assign(paste0("ts_", house, "_testing"), 
         as.xts(get(paste0(house, "_at_30_min_for_validating")), frequency = 48*365))

  # Create ARIMA model from training data
  assign(paste0(house, "_30_min_ARIMA"), 
         auto.arima(get(paste0("ts_", house, "_fitting"))$HWelec, 
                    stepwise = FALSE, approximation = FALSE, D = 1))# %>% 
  #   saveRDS(file = paste0(dFile, "models/ARIMA/", house, "_fitted_model.rds"))
  # Validate model from test data
 # assign(house, 
  assign(paste0(house, "_30_min_ARIMA_validate"), 
         Arima(get(paste0("ts_", house, "_testing"))$HWelec, 
               model = get(paste0(house, "_30_min_ARIMA"))))# %>%
 #  saveRDS(file = paste0(dFile, "models/ARIMA/", house, "_validated_model.rds"))
  # Make plot for demonstration purposes
#  plotModel(get(paste0(house, "_30_min_ARIMA_validate")), "ARIMA")
}




