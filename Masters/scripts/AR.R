# This calculates a seasonal autoregression for each household 
# and compares it
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
         as.data.table(read_csv(paste0("Masters/data/households/fitting/", 
                                       house,"_at_30_min_for_fitting.csv"))))
  assign(paste0(house, "_at_30_min_for_validating"), 
         as.data.table(read_csv(paste0("Masters/data/households/validating/", 
                                       house,"_at_30_min_for_validating.csv"))))
  assign(paste0("ts_", house, "_fitting"), 
         as.xts(get(paste0(house, "_at_30_min_for_fitting"))))
  assign(paste0("ts_", house, "_testing"), 
         as.xts(get(paste0(house, "_at_30_min_for_validating"))))

  # Create ARIMA model from training data
  assign(paste0(house, "_30_min_ARIMA"), 
         auto.arima(get(paste0("ts_", house, "_fitting"))$HWelec, 
                    stepwise = FALSE, approximation = FALSE)) %>% 
    save(file = paste0(dFile, "models/ARIMA/", house, "_fitted_model.Rda"))
  # Validate model from test data
  assign(paste0(house, "_30_min_ARIMA_validate"), 
         Arima(get(paste0("ts_", house, "_testing"))$HWelec, 
               model = get(paste0(house, "_30_min_ARIMA")))) %>%
    save(file = paste0(dFile, "models/ARIMA/", house, "_validated_model.Rda"))
}

# Make plot for demonstration purposes
plotModel(get(paste0(house, "_30_min_ARIMA_validate")), "ARIMA")

# NEED TO GET LIST AS FULL FILENAME
# THEN LOAD FILES
# THEN APPLY plotModel()

#list1 <- list.files(paste0(dFile, "models/ARIMA/"), pattern = "_validated_model.Rda")
#list2 <- append(paste0(dFile, "models/ARIMA/"), list1)
#lapply(list, load)
#lapply(list, plotModel("ARIMA"))


