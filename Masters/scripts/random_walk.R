if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}
if (!exists("DT_hh")){
load(paste0(dFile, "DT_hh.Rda"))
}

library(dplyr)
library(data.table)
library(ggplot2)

sdResRW <- list()
for (house in unique(DT_hh$linkID)){
  p <- forecast::naive(DT_hh$HWelec[DT_hh$linkID == house], h = 1) # compute forecast
  sdResRW[[house]] <- sd(resid(p), na.rm = TRUE)
  pMdl <- cbind(DT_hh$hHour[DT_hh$linkID == house], p$x, p$fitted) # create new df of time, actual, fitted
  pMdl <- data.table(pMdl)
  names(pMdl) <- c("Time", "Actual", "Fitted")
  pMdl$Time <- lubridate::as_datetime(pMdl$Time, tz = 'Pacific/Auckland')
  pMdl <- melt(pMdl, id = "Time")  # reshapes to format more conducive to ggplot
  pMdl <- dplyr::arrange(pMdl, Time) # rearranges chronologically
  assign(paste0(house, "_model"), p) # assigns unique variable name
  get(paste0(house, "_model")) %>%
    save(file = paste0(dFile, "models/randomWalk/", house, "_at_30.Rda"))
  assign(paste0(house, "_for_plotting"), pMdl)
  get(paste0(house, "_model")) %>%
    save(file = paste0(dFile, "models/randomWalk/plotting/", house, "_at_30_for_plotting.Rda"))
  p <- ggplot(data = get(paste0(house, "_for_plotting"))[48:(48*3), ], aes(x = Time)) +
    geom_line(aes(y = value, colour = variable)) +
    labs(y = "Power (W)", colour = "")
  ggsave(filename = paste0(pFile, "randomWalk/", house, "RW.pdf"))
}
sdResRW <- as.data.frame(sdResRW)
sdResRW <- as.data.frame(t(sdResRW)) # transposes for ease of computation
save(sdResRW, file = paste0(dFile, "models/randomWalk/sdResRW.Rda"))

# This times the model
proc.time() <- RWproc
for (house in unique(DT_hh$linkID)){
  p <- forecast::naive(DT_hh$HWelec[DT_hh$linkID == house], h = 1)
}
RWproc <- proc.time()
save(RWproc, file=paste0(dFile, "models/randomWalk/procTime.Rda"))
# The following was an attempt to make a facet_wrap of 4 households to show
# variety of model fidelity. Unfortunately there is an unacceptable loss
# of detail in the plot output.

rf_40$linkID <- "rf_40"
rf_13$linkID <- "rf_13"
rf_22$linkID <- "rf_22"
rf_11$linkID <- "rf_11"

RWplotDT <- rbind(rf_40, rf_13, rf_22, rf_11)
RWplotDT <- dplyr::arrange(RWplotDT, Time)
#RWplotexample <- RWplotDT[48:(48*9), ]

p <- ggplot(data = RWplotDT[48:(48*9), ], aes(x = Time)) +
  geom_line(aes(y = value, colour = variable)) +
  facet_grid(rows = vars(linkID), scales = "free_y")
#facet_wrap(. ~ linkID, scales = "free")
p + labs(y = "Power (W)", colour = "")

ggplot_build(p)$data
ggsave(filename = paste0(pFile, "randomWalk/fourHouses.pdf"))

