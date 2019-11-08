# This calculates a seasonal autoregression for each household 
# and compares it
# NOTE auto.arima does NOT consider seasonality

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

# Reload models for plotting
for (house in houses){
  assign(house, readRDS(paste0(dFile, "models/ARIMA/", house, "_validated_model.rds")))
}

############################################################################

fourHousesPlot <- function(house1, house2, house3, house4){
  require(lubridate)
#  houseNames <- c(deparse(substitute(house1)),
#                  deparse(substitute(house2)),
#                  deparse(substitute(house3)),
#                  deparse(substitute(house4))) 

 for (house in houses){
  pMdl <- as.data.table(get(house)$x)
  names(pMdl) <- c("Time", "Actual")
  pMdl$Fitted <- get(house)$fitted
  pMdl <- melt(pMdl, id = "Time")
#  pMdl <- as.data.frame(pMdl)
#  pMdl <- dplyr::arrange(pMdl, Time) # rearranges chronologically
  pMdl$Time <- as_datetime(pMdl$Time, # convert from UTC
                                    tz = 'Pacific/Auckland')
  pMdl$linkID <- house
  startTime <- as_datetime(paste(as.character( # startTime is midnight on first day of next month
    rollback(pMdl$Time[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)), "00:00:00"), 
    tz = 'Pacific/Auckland')
  pMdl <- pMdl[pMdl$Time %within% interval(startTime, startTime + days(1)), ]
#  q <- as.character(pMdl$Time)
#  p <- gsub(".* ","", q)
#  x <- as_datetime(p)
#  pMdl$Time <- hm(q)  
 # q <- as.character(rf_34_model$Time)
#  w <- gsub(".* ","",q)
#  e <- hms(w)
  assign(paste0(house, "_model"), pMdl)
 }

  RWplotDT <- rbind(get(paste0(house1, "_model")), get(paste0(house2, "_model")),
                    get(paste0(house3, "_model")), get(paste0(house4, "_model")))
  RWplotDT <- dplyr::arrange(RWplotDT, Time)
#RWplotexample <- RWplotDT[48:(48*9), ]
#  startTime <- as_datetime(paste(as.character( # startTime is midnight on first day of next month
#    rollback(RWplotDT$Time[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)), "00:00:00"), 
#    tz = 'Pacific/Auckland')
#  startTime <- rollback(RWplotDT$Time[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)

#  p <- ggplot(data = RWplotDT[RWplotDT$Time %within% interval(startTime, startTime + days(1)), ], # Select data 1 day from startTime, 
  p <- ggplot(data = RWplotDT, 
              aes(x = Time)) +
    geom_line(aes(y = value, colour = variable)) +
#    facet_grid(rows = vars(linkID), scales = "free")
  facet_wrap(. ~ linkID, scales = "free", nrow = 4)
  p + labs(y = "Power (W)", colour = "")

}

############################################################################



##RWplotDT <- rbind(get(paste0(houses[1], "_model")), get(paste0(houses[2], "_model")), 
#                  get(paste0(houses[3], "_model")), get(paste0(houses[4], "_model")))
#
#RWplotDT <- dplyr::arrange(RWplotDT, Time)
#RWplotexample <- RWplotDT[48:(48*9), ]
#startTime <- as_datetime(paste(as.character( # startTime is midnight on first day of next month
#  rollback(RWplotDT$Time[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)), "00:00:00"), 
#  tz = 'Pacific/Auckland')
#  startTime <- rollback(RWplotDT$Time[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)




# Make plot for demonstration purposes
#plotModel(get(paste0(house, "_30_min_ARIMA_validate")), "ARIMA")

# NEED TO GET LIST AS FULL FILENAME
# THEN LOAD FILES
# THEN APPLY plotModel()

#list1 <- list.files(paste0(dFile, "models/ARIMA/"), pattern = "_validated_model.Rda")
#list2 <- append(paste0(dFile, "models/ARIMA/"), list1)
#lapply(list, load)
#lapply(list, plotModel("ARIMA"))