# This script plots averages of hot water use over a day

if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/" 
}
if (!exists("DT_hh")){
  load(paste0(dFolder, "DT_hh.Rda"))
}

library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
theme_set(theme_minimal(base_size = 14))

DT_hh$dayHour <- hour(DT_hh$hHour)

# NOTE this is inaccurate, it always begins the week hour at the first row per house,
# not at midnight Sunday. The current *weekly* average plot is therefore inaccurate.
DT_hh$weekHour <- (lubridate::day(DT_hh$hHour)-1)*24 + DT_hh$dayHour

# day plot (all households)
p <- ggplot(DT_hh, aes(x=dayHour, y=HWelec))+
  stat_summary(fun.y="mean", geom="line") + 
  facet_wrap(~linkID, ncol = 3, scales = "free_y")
p + labs(x = "Hour of day", y = "Average demand (W)")
ggsave(filename = paste0(pFolder, "averages/averageDemand.pdf"), width = 210, height = 260, units = "mm")
ggsave(filename = paste0(pFolder, "averages/averageDemand.png"), dpi = 100)
ggsave(filename = paste0(pFolder, "averages/averageDemandHighRes.png"), width = 210, height = 260, units = "mm")

# day plot (all households, other appliances)
p <- ggplot(DT_hh, aes(x=dayHour, y=nonHWelec))+
  stat_summary(fun.y="mean", geom="line") + 
  facet_wrap(~linkID, ncol = 3, scales = "free_y")
p + labs(x = "Hour of day", y = "Average demand (W)")
ggsave(filename = paste0(pFolder, "averages/averageNONHWDemand.pdf"))
ggsave(filename = paste0(pFolder, "averages/averageNONHWDemand.png"))

# week plot (all households)
p <- ggplot(DT_hh, aes(x=weekHour, y=HWelec))+
  stat_summary(fun.y="mean", geom="line") + 
  facet_wrap(~linkID, ncol = 3, scales = "free_y")
p + labs(x = "Hour of week", y = "Average demand (W)")
ggsave(filename = paste0(pFolder, "averages/averageDemandWeek.pdf"))
ggsave(filename = paste0(pFolder, "averages/averageDemandWeek.png"))

# day plot (individual households)
for (house in unique(DT_hh$linkID)){
  p <- ggplot(DT_hh[linkID == house], aes(x=dayHour, y=HWelec))+
    stat_summary(fun.y="mean", geom="line", size = 1)
  p + labs(x = "Hour of day", y = "Average demand (W)")
  ggsave(filename = paste0(pFolder, "averages/", house, "averageDemand.pdf"))
  ggsave(filename = paste0(pFolder, "averages/", house, "averageDemand.png"))
}

# 2 day plot to contrast with averages
# Broken, easily fixed but no longer in use.

load(paste0(dFolder, "houses.Rda"))
for (house in houses){
  assign("tmpDT", 
         as.data.table(readr::read_csv(paste0(dFolder, "households/", 
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
              geom_line(aes(y = HWelec), size = 1) +
              facet_grid(rows = vars(Date), scales = "free_x")
            p + labs(x = "Hour of day", y = "Power (W)")
            ggsave(filename = paste0(pFolder, "averages/two_day_", house, ".png"))
            ggsave(filename = paste0(pFolder, "averages/two_day_", house, ".pdf"))
}

# This creates the boxplot of average demand
p <- ggplot(DT_hh, aes(x = linkID, y = HWelec, group = linkID)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape = 124, size=4, color="red") +
  labs(x = "Household", y = "Demand (W)")
p + coord_flip() 
ggsave(filename = paste0(pFolder, "averages/boxplot.png"), dpi = 100)
ggsave(filename = paste0(pFolder, "averages/boxplotHighRes.png"))
ggsave(filename = paste0(pFolder, "averages/boxplot.pdf"))

