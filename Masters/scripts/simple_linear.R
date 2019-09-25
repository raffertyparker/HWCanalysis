# Simple linear regression
# First we determine the equations for each household
# Next we model using this equation
# Then plot

if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}
if (!exists("DT_hh")){
  load(paste0(dFile, "DT_hh.Rda"))
}


##################################
head(DT_hh)

lmFormula <- DT_hh$HWelec ~ shift(DT_hh$nonHWelec)

DT_hh$HWminusOne <- shift(DT_hh$HWelec)

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
  geom_point() + geom_smooth(method="lm") +
  stat_poly_eq(formula = lmFormula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "right",
               rr.digits = 2, parse = TRUE)         
p + labs(x = "Non hot water electricity", 
         y = "Following half-hour hot water electricity", title = "")