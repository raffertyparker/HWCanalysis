# This creates the acv plots
###############################################

if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/"
}
if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}

# Load presaved data to save time:
# load(paste0(dFolder, "acvDT.Rda"))

if (!exists("DT")){
  load(paste0(dFolder, "DT.Rda"))
}

library(ggplot2)
theme_set(theme_minimal(base_size = 14))
# house <- "rf_06"
for (house in unique(DT$linkID)){
  q <- forecast::Acf(DT$HWelec[DT$linkID == house], lag.max = 60*24*10, # 10 days
                     type = "correlation", 
                     plot = FALSE, na.action = na.pass)
  s <- as.data.frame(q$acf)
  names(s) <- "value"
  s$household <- house
  s$lag <- seq(from = 0, to = nrow(s)-1)
  # s$lag <- s$lag - 1441
  ifelse(house == unique(DT$linkID)[1],
         acvDT <- s,
         acvDT <- rbind(acvDT, s))
}

acvDT <- data.table::data.table(acvDT)
acvDT$lag <- as.numeric(acvDT$lag)/(60*24)

save(acvDT, file = paste0(dFolder, "acvDT.Rda"))

p <- ggplot(acvDT, aes(x=lag, y=value))+
  geom_line() + 
  facet_wrap(~household, ncol = 3) +
  scale_y_continuous(limits = c(-0.1,0.3)) +
  scale_x_continuous(breaks = seq(1:10))# +
#  coord_cartesian(xlim = c(0,10))
p + labs(x = "Lag (days)", y = "Autocovariance")
ggsave(filename = paste0(pFolder, "acfAllHouses.pdf"), width = 210, height = 260, units = "mm")
ggsave(filename = paste0(pFolder, "acfAllHouses.png"), width = 210, height = 260, units = "mm")

# lag > 1/48 selected to remove acv = 1 effect at lag = 0 and corresponding effect on plot
# can be gotten around by specifying y_lims as in the previous figure
ggplot(acvDT[lag > (1/48)], aes(lag,value,colour=household)) +
       geom_line() + 
  labs(x = "Lag (minutes)", y = "Autocovariance", 
       colour = "") +
  theme(legend.position="top") +
  guides(colour=guide_legend(nrow=3,byrow=TRUE))
ggsave(paste0(pFolder, "acfAllHousesColour.pdf"))