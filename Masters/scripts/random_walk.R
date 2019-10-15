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
source(paste0(sFile, "plot_model.R"))

#sdResRW <- list()
DT_hh <- as.xts(DT_hh)
load(paste0(dFile, "houses.Rda"))
for (house in houses){
  p <- forecast::naive(DT_hh$HWelec[DT_hh$linkID == house], h = 1) # compute forecast
  assign(paste0(house, "_model"), p) # assigns unique variable name
  get(paste0(house, "_model")) %>%
    save(file = paste0(dFile, "models/randomWalk/", house, "_at_30.Rda"))
  plotModel(p, "randomWalk")
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

