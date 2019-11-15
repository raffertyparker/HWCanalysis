if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}
if (!exists("DT")){
  load(paste0(dFile, "DT.Rda"))
}
if (!exists("DT_hh")){
  load(paste0(dFile, "DT_hh.Rda"))
}
library(dplyr)
library(data.table)
library(ggplot2)

oneDay <- DT[dateTime_nz %between% c("2015-08-01", "2015-08-02") & linkID == "rf_06"]

#p <- ggplot(data = oneDay, aes(x = dateTime_nz)) +
#  geom_line(aes(y = HWelec, colour="#F8766D")) +
#  theme(legend.position="none")
#p + labs(y = "Power (W)", x = "Time")
#ggsave(filename = paste0(pFile, "oneDay.pdf"))



p <- ggplot(data = oneDay, aes(x = dateTime_nz)) +
  geom_line(aes(y = HWelec)) 
p + labs(y = "Power (W)", x = "Time")
ggsave(filename = paste0(pFile, "oneDay.pdf"))
ggsave(filename = paste0(pFile, "oneDay.png"))

twoDay <- DT[dateTime_nz %between% c("2015-08-01", "2015-08-03") & linkID == "rf_06"]
p <- ggplot(data = twoDay, aes(x = dateTime_nz)) +
  geom_line(aes(y = HWelec)) 
p + labs(y = "Power (W)", x = "Time")
#ggsave(filename = paste0(pFile, "twoDay.pdf"))
ggsave(filename = paste0(pFile, "twoDay.png"), width = 16, height = 5.7)

DT_hh <- as.data.table(DT_hh)
oneDay30min <- DT_hh[hHour %between% c("2015-08-01", "2015-08-02") & linkID == "rf_06"]

p <- ggplot(data = oneDay30min, aes(x = hHour)) +
  geom_line(aes(y = HWelec))# + , colour="#F8766D" 
#  theme(legend.position="none") Necessary for colour
p + labs(y = "Power (W)", x = "Time")
ggsave(filename = paste0(pFile, "oneDay30min.pdf"))
ggsave(filename = paste0(pFile, "oneDay30min.png"))

twoDay30min <- DT_hh[hHour %between% c("2015-08-01", "2015-08-03") & linkID == "rf_06"]

p <- ggplot(data = twoDay30min, aes(x = hHour)) +
  geom_line(aes(y = HWelec))
#  theme(legend.position="none") Necessary for colour
p + labs(y = "Power (W)", x = "Time")
ggsave(filename = paste0(pFile, "twoDay30min.pdf"))
ggsave(filename = paste0(pFile, "twoDay30min.png"), width = 16, height = 5.7)