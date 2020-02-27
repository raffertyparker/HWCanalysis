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

theme_set(theme_minimal())

load(paste0(dFolder, "houses.Rda"))


# ---- define peak periods ----
# Add peaks function adapted from original written by Ben Anderson
# for project https://github.com/CfSOtago/evAnalysis/

amPeakStart <- 7
amPeakEnd <- 9
pmPeakStart <- 17
pmPeakEnd <- 20
# Functions ----
peaksAlpha <- 0.08
peaksCol <- "#0072B2" # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
addPeaks <- function(p){
  # takes a plot, assumes x is hms, adds peak period annotations
  # assumes you've set yMin & yMax already
  # breaks with facet_grid, scales = "free" so you have to build seperate plots if you want to do that
  # there is a complex solution (https://stackoverflow.com/questions/27898651/get-scales-range-of-facets-scales-free) but...
  p <- p + annotate("rect", xmin = amPeakStart, # set earlier
                    xmax = amPeakEnd, # set earlier
                    ymin = yMin, ymax = yMax, 
                    alpha = peaksAlpha, fill = peaksCol)
  p <- p + annotate("rect", xmin = pmPeakStart, # set earlier
                    xmax = pmPeakEnd, # set earlier
                    ymin = yMin, ymax = yMax, 
                    alpha = peaksAlpha, fill = peaksCol)
  return(p)
}

# ------------------------------------------



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
  
  yMin <- min(plotResDF$value, na.rm = TRUE)
  yMax <- max(plotResDF$value, na.rm = TRUE)

  
  p <- ggplot(plotResDF, aes(x = dHour, y = value, colour = variable)) +
    geom_line() 
  p <- p + labs(x = "Hour", y = "Residual", 
         colour = "Household")
  p <- addPeaks(p)
  
  ggsave(paste0(pFolder, model, "/allHousesResidual.pdf"))
  ggsave(paste0(pFolder, model, "/allHousesResidual.png"))
  
  
  p <- ggplot(plotResDF, aes(x = dHour, y = value, group = variable)) +
    geom_boxplot() 
  p <- p + labs(x = "Hour", y = "Residual")
  p <- addPeaks(p)
  ggsave(paste0(pFolder, model, "/allHousesResidualBoxplot.pdf"))
  ggsave(paste0(pFolder, model, "/allHousesResidualBoxplot.png"))
}
