# This calculates a seasonal autoregression for each household 
# and compares it
if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}

library(forecast)
library(xts)

for (house in unique(DT_hh$linkID)){
  load(paste0(dFile, "households/fitting/", house, "_at_30_min_for_fitting.Rda"))
  assign(paste0("ts_", house, "_fitting"), as.xts(get(paste0(house, "_at_30_min_for_fitting"))))
  assign(paste0("ts_", house, "_testing"), as.xts(get(paste0(house, "_at_30_min_for_validating"))))

  # Create ARIMA model from training data
  assign(paste0(house, "_30_min_ARIMA"), auto.arima(get(paste0("ts_", house, "_fitting"))$HWelec, 
                                                  stepwise = FALSE, approximation = FALSE)) 
  # Validate model from test data
  assign(paste0(house, "_30_min_ARIMA_validate"), Arima(get(paste0("ts_", house, "_testing"))$HWelec, 
                                                      model = get(paste0(house, "_30_min_ARIMA"))))
}

test <- fitted(rf_06_30_min_ARIMA_validate)
test2 <- rf_06_30_min_ARIMA_validate$x
names(test2) <- "actual"
test2$fitted <- rf_06_30_min_ARIMA_validate$fitted

################# UP TO HERE ####################
ggplot2::ggplot()

plot(rf_06_30_min_ARIMA_validate$x,col="red")
lines(fitted(rf_06_30_min_ARIMA_validate),col="blue")

plot(fitted(rf_06_30_min_ARIMA_validate),col="blue")

fit2 <- ets(testdata, model=fit)
onestep <- fitted(fit2)