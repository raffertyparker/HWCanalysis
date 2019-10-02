# This script plots averages of hot water use over a day

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
library(ggplot2)

DT_hh$dayHour <- hour(DT_hh$hHour)
DT_hh$weekHour <- (lubridate::day(DT_hh$hHour)-1)*24 + DT_hh$dayHour

# day plot
p <- ggplot(DT_hh, aes(x=dayHour, y=HWelec))+
  stat_summary(fun.y="mean", geom="line") + 
  facet_wrap(~linkID, ncol = 4, scales = "free_y")
p + labs(x = "Hour of day", y = "Average demand (W)")
ggsave(filename = paste0(pFile, "averageDemand.pdf"))

# week plot
p <- ggplot(DT_hh, aes(x=weekHour, y=HWelec))+
  stat_summary(fun.y="mean", geom="line") + 
  facet_wrap(~linkID, ncol = 4, scales = "free_y")
p + labs(x = "Hour of week", y = "Average demand (W)")
ggsave(filename = paste0(pFile, "averageDemandWeek.pdf"))


