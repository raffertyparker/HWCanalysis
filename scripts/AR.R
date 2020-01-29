# This calculates a seasonal autoregression for each household.
# Could potentially be assimilated into modelling.R but may not be included

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
library(readr)

load(paste0(dFile, "houses.Rda"))

for (house in houses){
  assign(paste0(house, "_at_30_min_for_fitting"), 
         as.data.table(read_csv(paste0("Masters/data/households/fitting/", 
                                       house,"_at_30_min_for_fitting.csv"))))
  assign(paste0(house, "_at_30_min_for_validating"), 
         as.data.table(read_csv(paste0("Masters/data/households/validating/", 
                                       house,"_at_30_min_for_validating.csv"))))
  assign(paste0("ts_", house, "_fitting"), 
         as.xts(get(paste0(house, "_at_30_min_for_fitting"))))
  assign(paste0("ts_", house, "_testing"), 
         as.xts(get(paste0(house, "_at_30_min_for_validating"))))

  # Create AR model from training data
  assign(paste0(house, "_30_min_AR"), 
         modelAR(get(paste0("ts_", house, "_fitting"))$HWelec, 
                    p = 5, P = 1)) %>% 
    save(file = paste0(dFile, "models/AR/", house, "_fitted_model.Rda"))
  # Validate model from test data
  assign(paste0(house, "_30_min_AR_validate"), 
         Arima(get(paste0("ts_", house, "_testing"))$HWelec, 
               model = get(paste0(house, "_30_min_AR")))) %>%
    save(file = paste0(dFile, "models/AR/", house, "_validated_model.Rda"))
}

# Make plot for demonstration purposes
plotModel(get(paste0(house, "_30_min_AR_validate")), "AR")

