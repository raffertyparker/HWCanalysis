if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}
if (!exists("sFile")){
  sFile <- "~/HWCanalysis/Masters/scripts/" 
}
if (!exists("DT_hh")){
  load(paste0(dFile, "DT_hh.Rda"))
}

library(dplyr)
library(data.table)
library(ggplot2)
library(xts)
library(lubridate)
source(paste0(sFile, "plot_model.R"))

#sdResRW <- list()
#DT_hh <- as.xts(DT_hh)
load(paste0(dFile, "houses.Rda"))
for (house in houses){
  mdl <- forecast::naive(as.xts(DT_hh[DT_hh$linkID == house])$HWelec, h = 1) # compute forecast
  mdl$x <- as.xts(DT_hh[DT_hh$linkID == house])$HWelec
  assign(paste0(house, "_model"), mdl) # assigns unique variable name
#  get(paste0(house, "_model")) %>%
#    save(file = paste0(dFile, "models/randomWalk/", house, "_at_30.Rda"))
  pMdl <- as.data.table(mdl$x)
  names(pMdl) <- c("Time", "Actual")
  pMdl$Fitted <- mdl$fitted
  pMdl <- melt(pMdl, id = "Time") 
  assign(paste0(house, "_model"), pMdl) # assigns unique variable name
#  plotModel(p, "randomWalk")
}
#sdResRW <- as.data.frame(sdResRW)
#sdResRW <- as.data.frame(t(sdResRW)) # transposes for ease of computation
#save(sdResRW, file = paste0(dFile, "models/randomWalk/sdResRW.Rda"))

# This times the model
proc.time() <- RWproc
for (house in houses){
  p <- forecast::naive(DT_hh$HWelec[DT_hh$linkID == house], h = 1)
}
RWproc <- proc.time()
save(RWproc, file=paste0(dFile, "models/randomWalk/procTime.Rda"))
# The following was an attempt to make a facet_wrap of 4 households to show
# variety of model fidelity. Unfortunately there is an unacceptable loss
# of detail in the plot output.

rf_40_model$linkID <- "rf_40"
rf_13_model$linkID <- "rf_13"
rf_22_model$linkID <- "rf_22"
rf_06_model$linkID <- "rf_06"

RWplotDT <- rbind(rf_40_model, rf_13_model, rf_22_model, rf_06_model)
RWplotDT <- dplyr::arrange(RWplotDT, Time)
#RWplotexample <- RWplotDT[48:(48*9), ]

startTime <- lubridate::as_datetime(paste(as.character( # startTime is midnight on first day of next month
  rollback(RWplotDT$Time[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)), "00:00:00"), 
  tz = 'Pacific/Auckland')

p <- ggplot(data = RWplotDT[RWplotDT$Time %within% interval(startTime, startTime + days(1)), ], # Select data 1 day from startTime, 
            aes(x = Time)) +
  geom_line(aes(y = value, colour = variable)) +
  facet_grid(rows = vars(linkID), scales = "free_y")
#facet_wrap(. ~ linkID, scales = "free")
p + labs(y = "Power (W)", colour = "")

ggplot_build(p)$data
ggsave(filename = paste0(pFile, "randomWalk/fourHouses.png"))


