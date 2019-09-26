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

DT_hh$nonHWshift <- shift(DT_hh$nonHWelec)
sdResSL <- list()
for (house in unique(DT_hh$linkID)){
  mdl <- lm(DT_hh$HWelec[DT_hh$linkID == house]~DT_hh$nonHWshift[DT_hh$linkID == house])
  sdResSL[[house]] <- sd(resid(mdl), na.rm = TRUE)
  pMdl <- cbind(DT_hh$hHour[DT_hh$linkID == house], DT_hh$HWelec[DT_hh$linkID == house], mdl$fitted.values)
  pMdl <- data.table(pMdl)
  names(pMdl) <- c("Time", "Actual", "Fitted")
  pMdl$Time <- lubridate::as_datetime(pMdl$Time, tz = 'Pacific/Auckland')
  pMdl <- melt(pMdl, id = "Time")
  pMdl <- dplyr::arrange(pMdl, Time)
  assign(house, mdl)
  get(house) %>%
    save(file = paste0(dFile, "models/simpleLinear/", house, "_at_30.Rda"))
  assign(house, pMdl)
  get(house) %>%
    save(file = paste0(dFile, "models/simpleLinear/", house, "_at_30_for_plotting.Rda"))
  p <- ggplot(data = get(house)[48:(48*3), ], aes(x = Time)) +
    geom_line(aes(y = value, colour = variable)) +
    labs(y = "Power (W)", colour = "")
  ggsave(filename = paste0(pFile, "simpleLinear/", house, "SL.pdf"))
}
sdResSL <- as.data.frame(sdResSL)
sdResSL <- as.data.frame(t(sdResSL)) # transposes for ease of computation
save(sdResSL, file = paste0(dFile, "models/simpleLinear/sdResSL.Rda"))

proc.time() <- SLproc
for (house in unique(DT_hh$linkID)){
  p <- lm(DT_hh$HWelec[DT_hh$linkID == house]~DT_hh$nonHWshift[DT_hh$linkID == house])
}
SLproc <- proc.time()
save(SLproc, file=paste0(dFile, "models/simpleLinear/procTime.Rda"))

# good range of houses: 31, 13, 45, 42
fourPlot <- function(modelName,house1,house2,house3,house4){
  house1$linkID <- deparse(substitute(house1))
  house2$linkID <- deparse(substitute(house2))
  house3$linkID <- deparse(substitute(house3))
  house4$linkID <- deparse(substitute(house4))

  plotDT <- rbind(house1,house2,house3,house4)

  plotDT <- dplyr::arrange(plotDT, Time)
  plotexample <- plotDT[48:(48*9), ]

  p <- ggplot(data = plotDT[48:(48*9), ], aes(x = Time)) +
    geom_line(aes(y = value, colour = variable)) +
    facet_grid(rows = vars(linkID), scales = "free_y")
#facet_wrap(. ~ linkID, scales = "free")
  p + labs(y = "Power (W)", colour = "")
  ggsave(filename = paste0(pFile, modelName, "/fourHouses.pdf"))
}

fourPlot("simpleLinear", rf_30, rf_13, rf_42, rf_44)


ggsave(filename = paste0(pFile, "simpleLinear/fourHouses.pdf"))

##################################
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


pMdl <- cbind(DT_hh$hHour[DT_hh$linkID == house], DT_hh$HWelec[DT_hh$linkID == house], mdl$fitted.values)
pMdl <- data.table(pMdl)
names(pMdl) <- c("Time", "Actual", "Fitted")
pMdl$Time <- lubridate::as_datetime(pMdl$Time, tz = 'Pacific/Auckland')
pMdl <- melt(pMdl, id = "Time")
pMdl <- dplyr::arrange(pMdl, Time)


fit1 <- lm(DT_hh$HWelec[DT_hh$linkID == house]~DT_hh$nonHWshift[DT_hh$linkID == house])
ggplotRegression(mdl)


pMdl <- DT_hh[DDT_hh$linkID == house, 

pMdl <- cbind(DT_hh$hHour[DT_hh$linkID == house], p$x, p$fitted)
              

ggplot(DT_hh, aes(x = nonHWshift, y = HWelec)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


DT_hh$HWelec ~ shift(DT_hh$nonHWelec)
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