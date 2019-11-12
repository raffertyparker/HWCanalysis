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
library(lubridate)
library(data.table)

DT_hh$dayHour <- hour(DT_hh$hHour)

# NOTE this is inaccurate, it always begins the week hour at the first row per house,
# not at midnight Sunday. The current weekly average plot is therefore inaccurate.
DT_hh$weekHour <- (lubridate::day(DT_hh$hHour)-1)*24 + DT_hh$dayHour

# day plot (all households)
p <- ggplot(DT_hh, aes(x=dayHour, y=HWelec))+
  stat_summary(fun.y="mean", geom="line") + 
  facet_wrap(~linkID, ncol = 4, scales = "free_y")
p + labs(x = "Hour of day", y = "Average demand (W)")
ggsave(filename = paste0(pFile, "averages/averageDemand.pdf"))
ggsave(filename = paste0(pFile, "averages/averageDemand.png"))

# day plot (all households, other appliances)
p <- ggplot(DT_hh, aes(x=dayHour, y=nonHWelec))+
  stat_summary(fun.y="mean", geom="line") + 
  facet_wrap(~linkID, ncol = 4, scales = "free_y")
p + labs(x = "Hour of day", y = "Average demand (W)")
ggsave(filename = paste0(pFile, "averages/averageNONHWDemand.pdf"))
ggsave(filename = paste0(pFile, "averages/averageNONHWDemand.png"))

# week plot (all households)
p <- ggplot(DT_hh, aes(x=weekHour, y=HWelec))+
  stat_summary(fun.y="mean", geom="line") + 
  facet_wrap(~linkID, ncol = 4, scales = "free_y")
p + labs(x = "Hour of week", y = "Average demand (W)")
ggsave(filename = paste0(pFile, "averages/averageDemandWeek.pdf"))
ggsave(filename = paste0(pFile, "averages/averageDemandWeek.png"))

# day plot (individual households)
for (house in unique(DT_hh$linkID)){
  p <- ggplot(DT_hh[linkID == house], aes(x=dayHour, y=HWelec))+
    stat_summary(fun.y="mean", geom="line")
  p + labs(x = "Hour of day", y = "Average demand (W)")
  ggsave(filename = paste0(pFile, "averages/", house, "averageDemand.pdf"))
  ggsave(filename = paste0(pFile, "averages/", house, "averageDemand.png"))
}

# 2 day plot to contrast with averages
load(paste0(dFile, "houses.Rda"))
for (house in houses){
  assign("tmpDT", 
         as.data.table(readr::read_csv(paste0(dFile, "households/", 
                                              house,"_at_30_min.csv"))))

startTime <- lubridate::as_datetime(paste(as.character( # startTime is midnight on first day of next month
  rollback(tmpDT$hHour[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)), "00:00:00"), 
  tz = 'Pacific/Auckland')

tmpDT <- tmpDT[date(hHour) %in% c(date(startTime), date(startTime + ddays(1)))]
tmpDT$Hour <- hour(tmpDT$hHour)
tmpDT$Date <- as.character(date(tmpDT$hHour))
twoDay <- tmpDT %>%
  group_by(Date, Hour) %>%
  summarise(nonHWelec = mean(nonHWelec), HWelec = mean(HWelec))

p <- ggplot(data = twoDay, aes(x = Hour)) + 
              geom_line(aes(y = HWelec)) +
              facet_grid(rows = vars(Date), scales = "free_x")
            p + labs(x = "Hour of day", y = "Power (W)")
            ggsave(filename = paste0(pFile, "averages/two_day_", house, ".png"))
            ggsave(filename = paste0(pFile, "averages/two_day_", house, ".pdf"))
}
