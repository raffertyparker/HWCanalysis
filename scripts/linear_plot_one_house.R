# This creates a linear regression plot for a selected house
if (!exists("dFile")){
  dFile <- "~/HWCanalysis/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/plots/" 
}
# Load the processed dataframe
if (!exists("DT_hh")){
  load(paste0(dFile, "DT_hh.Rda"))
}

library(data.table)
library(ggpmisc)

DF_hh <- as.data.frame(DT_hh)
house <- "rf_13"  # Input appropriate house
dt <- DF_hh[DF_hh$linkID == house, ] 
# Select all except the first value of hot water electricity
HWminusOne <- tail(dt$HWelec, (length(dt$HWelec) - 1))  
# This gives the HW electricity half an hour "in advance" 
# of non hot water electricity
# Now select all except the last value of non HW electricity
nonHW <- head(dt$nonHWelec, (length(dt$HWelec) - 1))    
# Last value removed so it is same dimension as HWminusOne
linearRelData <- as.data.frame(cbind(HWminusOne,nonHW))
lmFormula <- HWminusOne ~ nonHW

p <- ggplot(data = linearRelData, aes(x = nonHW, y = HWminusOne)) +
  geom_point() + geom_smooth(method="lm", col = "red") +
  stat_poly_eq(formula = lmFormula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "right",
               rr.digits = 2, parse = TRUE)         
p + labs(x = "Other appliance demand (time = t)", 
         y = "Hot water demand (time = t+30 mins)", title = "")
ggsave(filename = paste0(pFile, house, "LinearPlot.pdf"))
ggsave(filename = paste0(pFile, house, "LinearPlot.png"))