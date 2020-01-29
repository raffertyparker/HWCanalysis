# This creates the BATS model
# Not currently used but could be explored further if time permits

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
         as.data.table(readr::read_csv(paste0("Masters/data/households/fitting/", 
                                              house,"_at_30_min_for_fitting.csv"))))
  assign(paste0(house, "_at_30_min_for_validating"), 
         as.data.table(readr::read_csv(paste0("Masters/data/households/validating/", 
                                              house,"_at_30_min_for_validating.csv"))))

  assign(paste0("ts_", house, "_fitting"), 
         as.ts(get(paste0(house, "_at_30_min_for_fitting")), frequency = 48*365))
  assign(paste0("ts_", house, "_testing"), 
         as.ts(get(paste0(house, "_at_30_min_for_validating")), frequency = 48*365))
  
  # Create BATS model from training data
  assign(paste0(house, "_30_min_BATS"), 
         bats(get(paste0("ts_", house, "_fitting"))$HWelec, 
                    use.trend = FALSE, seasonal.periods = TRUE)) %>% 
    saveRDS(file = paste0(dFile, "models/BATS/", house, "_fitted_model.rds"))
  # Validate model from test data
#  assign(house, 
#         # assign(paste0(house, "_30_min_ARIMA_validate"), 
#         Arima(get(paste0("ts_", house, "_testing"))$HWelec, 
#               model = get(paste0(house, "_30_min_BATS")))) %>%
#   saveRDS(file = paste0(dFile, "models/BATS/", house, "_validated_model.rds"))
  # Make plot for demonstration purposes
  #  plotModel(get(paste0(house, "_30_min_ARIMA_validate")), "ARIMA")
}

