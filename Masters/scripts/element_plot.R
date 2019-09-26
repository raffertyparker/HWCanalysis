if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}
if (!exists("DT")){
  load(paste0(dFile, "DT.Rda"))
}

# THIS IS RUBBISH 


library(dplyr)
library(data.table)


oneDay <- DT_hh[dateTime_nz %between% c("2015-08-01", "2015-08-02") & linkID == "rf_06"]

p <- ggplot(data = oneDay, aes(x = dateTime_nz)) +
  geom_line(aes(y = HWelec, colour="#F8766D")) +
  theme(legend.position="none")
p + labs(y = "Power (W)", x = "Time")
ggsave(filename = paste0(pFile, "oneDay.pdf"))


DT_hh <- as.data.table(DT_hh)
oneDay30min <- DT_hh[hHour %between% c("2015-08-01", "2015-08-02") & linkID == "rf_06"]

p <- ggplot(data = oneDay30min, aes(x = hHour)) +
  geom_line(aes(y = HWelec, colour="#F8766D")) +
  theme(legend.position="none")
p + labs(y = "Power (W)", x = "Time")
ggsave(filename = paste0(pFile, "oneDay30min.pdf"))