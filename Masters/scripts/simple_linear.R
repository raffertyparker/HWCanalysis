# Simple linear regression bivariate models

if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}
if (!exists("sFile")){
  sFile <- "~/HWCanalysis/Masters/scripts/" 
}
if (!exists("DT_hh")){
  load(paste0(dFile, "DT_hh.Rda"))
}

DT_hh$nonHWshift <- shift(DT_hh$nonHWelec)
#sdResSL <- list()
load(paste0(dFile, "houses.Rda"))
for (house in houses){
  mdl <- lm(DT_hh$HWelec[DT_hh$linkID == house]~DT_hh$nonHWshift[DT_hh$linkID == house])
#  sdResSL[[house]] <- sd(resid(mdl), na.rm = TRUE)
  pMdl <- cbind(DT_hh$hHour[DT_hh$linkID == house], DT_hh$HWelec[DT_hh$linkID == house], mdl$fitted.values)
  pMdl <- data.table(pMdl)
  names(pMdl) <- c("Time", "Actual", "Fitted")
  pMdl$Time <- lubridate::as_datetime(pMdl$Time, tz = 'Pacific/Auckland')
  pMdl <- melt(pMdl, id = "Time")
  pMdl <- dplyr::arrange(pMdl, Time)
  assign(house, mdl)
  get(house) %>%
    saveRDS(file = paste0(dFile, "models/simpleLinear/", house, "_at_30.rds"))
  assign(house, pMdl)
  get(house) %>%
    saveRDS(file = paste0(dFile, "models/simpleLinear/", house, "_at_30_for_plotting.rds"))
  p <- ggplot(data = get(house)[48:(48*3), ], aes(x = Time)) +
    geom_line(aes(y = value, colour = variable)) +
    labs(y = "Power (W)", colour = "")
  ggsave(filename = paste0(pFile, "simpleLinear/", house, "SL.pdf"))
}

#sdResSL <- as.data.frame(sdResSL)
#sdResSL <- as.data.frame(t(sdResSL)) # transposes for ease of computation
#save(sdResSL, file = paste0(dFile, "models/simpleLinear/sdResSL.Rda"))

proc.time() <- SLproc
for (house in unique(DT_hh$linkID)){
  p <- lm(DT_hh$HWelec[DT_hh$linkID == house]~DT_hh$nonHWshift[DT_hh$linkID == house])
}
SLproc <- proc.time()
save(SLproc, file=paste0(dFile, "models/simpleLinear/procTime.Rda"))

source(paste0(sFile, "four_plot.R"))
# good range of houses: 31, 13, 45, 42
fourPlot("simpleLinear", rf_30, rf_13, rf_42, rf_44)


##################################################
# Below here is a mess, needs tidying or deleting

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)),
         x = "Other appliance demand (time = t)", 
         y = "Hot water demand (time = t+30 mins)")
}

ggplotRegression(mdl)
ggsave(paste0(pFile, "simpleLinear/rf_13_scatterplot.pdf"))


fit1 <- lm(DT_hh$HWelec[DT_hh$linkID == house]~DT_hh$nonHWshift[DT_hh$linkID == house])
ggplotRegression(fit1)

p <- ggplot(data = linearRelData, aes(x = nonHW, y = HWminusOne)) +
  geom_point() + geom_smooth(method="lm") +
  stat_poly_eq(formula = lmFormula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "right",
               rr.digits = 2, parse = TRUE)         
p + labs(x = "Non hot water electricity", 
         y = "Following half-hour hot water electricity", title = "")