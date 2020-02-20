# This script analysis the residuals to determine distributions and autocorrellation

if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/" 
}

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)


load(paste0(dFolder, "houses.Rda"))

models <- c("STLARIMA", "naive","seasonalNaive","simpleLinear", "ARIMA", "ARIMAX", "SVM")
#house <- "rf_06"
#model <- "seasonalNaive"
for (model in models){
  for (house in houses){
    mdl <- readRDS(paste0(dFolder, "models/", model, "/", house, "_validated_model.rds"))
    pdf(file=paste0(pFolder, model, "/", house, "_residual_acv.pdf"))
    q <- forecast::Acf(mdl$residuals, lag.max = 48*7*2, # 2 weeks
                       type = "correlation",
                       na.action = na.pass, 
                       main = "")
    #  save.image(file = paste0(pFolder, model, "/", house, "_residual_acv.png"))
    dev.off()
    w <- as.data.table(mdl$x)
    if(ncol(w) == 1){
      w$index <- mdl$hHour
    }
    w$dHour <- hour(w$index) + minute(w$index)/60
    w$residual <- mdl$residuals
    e <- w %>%
      group_by(dHour) %>%
      summarise(error = mean(residual, na.rm = TRUE))
    names(e) <- c("dHour", house)
    ifelse(house == houses[1], 
           avResDF <- e, 
           avResDF <- left_join(avResDF, e))
    saveRDS(avResDF, file = paste0(dFolder,"models/", model, "/dailyResiduals.rds"))
  }

  plotResDF <- melt(avResDF, id = "dHour")
  p <- ggplot(plotResDF, aes(x = dHour, y = value, colour = variable)) +
    geom_line() 
  p + labs(x = "Hour", y = "Residual", 
         colour = "Household")
  ggsave(paste0(pFolder, model, "/allHousesResidual.pdf"))
  ggsave(paste0(pFolder, model, "/allHousesResidual.png"))
  
  
  p <- ggplot(plotResDF, aes(x = dHour, y = value, group = variable)) +
    geom_boxplot() 
  p + labs(x = "Hour", y = "Residual")
  ggsave(paste0(pFolder, model, "/allHousesResidualBoxplot.pdf"))
  ggsave(paste0(pFolder, model, "/allHousesResidualBoxplot.png"))
}
