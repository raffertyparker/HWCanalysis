# This plots the raw hot water data for one or two days to demonstrate the operation of the element
# and the difference between data at 1 min and 30 min resolutions

if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/" 
}
if (!exists("DT")){
  load(paste0(dFolder, "DT.Rda"))
}
if (!exists("DT_hh")){
  load(paste0(dFolder, "DT_hh.Rda"))
}
library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)

theme_set(theme_minimal(base_size = 14))
oneDay <- DT[dateTime_nz %between% c("2015-08-01", "2015-08-02") & linkID == "rf_06"]

# HW elec and other appliance elec plot
w <- DT[dateTime_nz %between% c("2015-08-24", "2015-08-25") & linkID == "rf_06"]
pData <- melt(w, id.vars = "dateTime_nz", measure.vars = c("HWelec", "nonHWelec"))
pData[variable == "HWelec", variable := "Hot water"]
pData[variable == "nonHWelec", variable := "Other appliances"]
p4 <- ggplot(data = pData, aes(x = dateTime_nz, y = value, colour = variable)) +
  geom_line()
p4 + labs(y = "Power (W)", x = "Time", colour = "")
ggsave(filename = paste0(pFolder, "bothElecPlot.pdf"))

# superceded by the above version
#p3 <- ggplot(data = oneDay, aes(x = dateTime_nz)) +
#  geom_line(aes(y = nonHWelec)) + 
#  labs(y = "Power (W)", x = "")
#bothElecPlot <- grid.arrange(p1,p3,ncol=1)



p1 <- ggplot(data = oneDay, aes(x = dateTime_nz)) +
  geom_line(aes(y = HWelec)) +
  labs(y = "Power (W)", x = "")
p1# + labs(y = "Power (W)", x = "Time")
ggsave(filename = paste0(pFolder, "oneDay.pdf"))
ggsave(filename = paste0(pFolder, "oneDay.png"))


twoDay <- DT[dateTime_nz %between% c("2015-08-01", "2015-08-03") & linkID == "rf_06"]
p <- ggplot(data = twoDay, aes(x = dateTime_nz)) +
  geom_line(aes(y = HWelec)) 
p + labs(y = "Power (W)", x = "Time")
ggsave(filename = paste0(pFolder, "twoDay.pdf"))
ggsave(filename = paste0(pFolder, "twoDay.png"), width = 16, height = 5.7)

DT_hh <- as.data.table(DT_hh)
oneDay30min <- DT_hh[hHour %between% c("2015-08-01", "2015-08-02") & linkID == "rf_06"]

p2 <- ggplot(data = oneDay30min, aes(x = hHour)) +
  geom_line(aes(y = HWelec)) +
  labs(y = "Power (W)", x = "Time") # + , colour="#F8766D" 
#  theme(legend.position="none") Necessary for colour
p2 #+ labs(y = "Power (W)", x = "Time")
ggsave(filename = paste0(pFolder, "oneDay30min.pdf"))
ggsave(filename = paste0(pFolder, "oneDay30min.png"))

# Create comparison plot

compPlot <- grid.arrange(p1,p2,ncol=1)
ggsave(compPlot, filename = paste0(pFolder, "elementComparisonPlot.pdf"))
ggsave(compPlot, filename = paste0(pFolder, "elementComparisonPlot.png"))


twoDay30min <- DT_hh[hHour %between% c("2015-08-01", "2015-08-03") & linkID == "rf_06"]

p <- ggplot(data = twoDay30min, aes(x = hHour)) +
  geom_line(aes(y = HWelec))
#  theme(legend.position="none") Necessary for colour
p + labs(y = "Power (W)", x = "Time")
ggsave(filename = paste0(pFolder, "twoDay30min.pdf"))
ggsave(filename = paste0(pFolder, "twoDay30min.png"), width = 16, height = 5.7)