# CCV PLOTS #

if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/"
}
if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
#####################################################
# Load presaved resaved data files and then manually make plots
# makes things a lot faster!
#load(paste0(dFolder, "ccvDT.Rda"))
#load(paste0(dFolder, "ccv_max.Rda"))
#####################################################
if (!exists("DT")){
  load(paste0(dFolder, "DT.Rda"))
}

library(ggplot2)
theme_set(theme_minimal(base_size = 14))

for (house in unique(DT$linkID)){
  q <- forecast::Ccf(DT$HWelec[DT$linkID == house], DT$nonHWelec[DT$linkID == house], lag.max = 60*24,
                     type = "correlation",
                     plot = FALSE, na.action = na.pass)
  s <- as.data.frame(q$acf)
  names(s) <- "value"
  s$household <- house
  s$lag <- seq(from = -1440, to = 1440)
  ifelse(house == unique(DT$linkID)[1],
    ccvDT <- s,
    ccvDT <- rbind(ccvDT, s))
}

ccvDT <- data.table(ccvDT)

save(ccvDT, file = paste0(dFolder, "ccvDT.Rda"))

# The following finds the value of ccvDT that has the maximum correlation
# for each household and for lags greater than 30 mins

maxCor <- ccvDT %>%
  group_by(household) %>%
  filter(lag >= 30) %>%
  slice(which.max(value))

save(maxCor, file = paste0(dFolder, "ccv_max.Rda"))

ggplot(maxCor, aes(x = lag,fill=household)) +
  geom_histogram() + 
  labs(x = "Lag (minutes)", y = "Count", fill = "")
ggsave(paste0(pFolder, "maxXcorAllHouses.pdf"))


ccvDT$lag <- ccvDT$lag/60
# This could do with being edited to feed variables into a function
# Rather than just copy/pasted

ggplot(ccvDT[ccvDT$lag > -1 & ccvDT$lag < 8, ], aes(lag,value)) +
  geom_line() + 
  facet_wrap(~household, ncol = 3, scales = "free_y") +
  labs(x = "Lag (hours)", y = "Cross-covariance")
ggsave(paste0(pFolder, "ccfAllHouses.pdf"), width = 210, height = 260, units = "mm")
ggsave(paste0(pFolder, "ccfAllHouses.png"), width = 210, height = 260, units = "mm")

ggplot(ccvDT[ccvDT$lag > 0, ], aes(lag,value,colour=household)) +
  geom_line() + 
  labs(x = "Lag (minutes)", y = "Cross-covariance", colour = "") +
  theme(legend.position="top") +
  guides(colour=guide_legend(nrow=3,byrow=TRUE))
ggsave(paste0(pFolder, "ccfColourAllHouses.pdf"))
ggsave(paste0(pFolder, "ccfColourAllHouses.png"))

ggplot(ccvDT[ccvDT$lag > 0 & ccvDT$lag < 60, ], aes(lag,value,colour=household)) +
  geom_line() + 
  labs(x = "Lag (minutes)", y = "Cross-covariance", colour = "") +
  theme(legend.position="top") +
  guides(colour=guide_legend(nrow=3,byrow=TRUE))
ggsave(paste0(pFolder, "ccfAllHouses1Hour.pdf"))

ggplot(ccvDT[ccvDT$lag > 0 & ccvDT$lag < 120, ], aes(lag,value,colour=household)) +
  geom_line() + 
  labs(x = "Lag (minutes)", y = "Cross-covariance", colour = "") +
  theme(legend.position="top") +
  guides(colour=guide_legend(nrow=3,byrow=TRUE))
ggsave(paste0(pFolder, "ccfAllHouses2Hour.pdf"))
