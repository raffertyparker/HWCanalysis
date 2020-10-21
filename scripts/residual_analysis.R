#################################################
# This script analyses the residuals to determine
# distributions and autocorrellation
#################################################

if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/" 
}

# For quick loading + manusl plot creation
# (no data processing needed) 
#avResDF <- readRDS(avResDF, file = paste0(dFolder,"models/", model, 
#                                           "/dailyResiduals.rds"))

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)
library(forecast)
library(ggpubr)

theme_set(theme_minimal())

load(paste0(dFolder, "houses.Rda"))
for (house in houses){
  ifelse(house == houses[1],
         allHouseStr <- house, 
         allHouseStr <- paste(allHouseStr, house, sep = ","))
}

# ---- define peak periods ----
# Add peaks function adapted from original written by Ben Anderson
# for project https://github.com/CfSOtago/evAnalysis/

amPeakStart <- 7
amPeakEnd <- 9
pmPeakStart <- 17
pmPeakEnd <- 20
# Functions ----
peaksAlpha <- 0.08
peaksCol <- "#0072B2" 
addPeaks <- function(p){
  # takes a plot, assumes x is hms, adds peak period annotations
  # assumes you've set yMin & yMax already
  # breaks with facet_grid, scales = "free" so you have to build 
  # separate plots if you want to do that
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

models <- c("naive","seasonalNaive","simpleLinear", 
            "ARIMA", "ARIMAX","STLARIMA", "STLARIMAX", "SVM")
#house <- "rf_06"
#model <- "seasonalNaive"
###################################################################
# Combined ACV plots

for (model in models){
  for (house in houses){
    mdl <- readRDS(paste0(dFolder, "models/", model, "/", 
                          house, "_validated_model.rds"))
   # pdf(file=paste0(pFolder, model, "/", house, "_residual_acv.pdf"))
   # tst <- ggtsdisplay(mdl$residuals, lag.max = 48*7*2)
    p <- forecast::ggAcf(mdl$residuals, lag.max = 48*7, main = house)
   # theme(plot.title = element_text(size = 14, face = "bold"))
    p$labels$y <- NULL
    p$labels$x <- NULL
    p$labels$xend <- NULL
    
    assign(house, p)
   # save.image(file = paste0(pFolder, model, "/", 
   #                          house, "_residual_acv.png"))
   # dev.off()
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
    saveRDS(avResDF, file = paste0(dFolder,"models/", model, 
                                   "/dailyResiduals.rds"))
    ifelse(house == houses[1], 
           plotString <- "plot1",
           plotString <- append(plotString, 
                                paste0("plot", 
                                       as.character(
                                         which(house == houses)))))
  }

  allHouses <- grid.arrange(rf_06,rf_08,rf_13,rf_14,rf_22,rf_25,
                            rf_29,rf_30,rf_31,rf_32,rf_33,rf_34,
                            rf_35,rf_36,rf_37,rf_38,rf_39,rf_40,
                            rf_42,rf_44,rf_45, ncol = 3) 
  
  allHouses <- annotate_figure(allHouses, left = "Autocovariance", 
                               bottom = "Lag (half hours)")
  
  ggsave(allHouses,  width = 8.3, height = 11.7,
         filename = paste0(pFolder, model, "/allResiduals.pdf"))
  
  }
 

#############################################################
# Individual acv plots

for (model in models){
  for (house in houses){
    mdl <- readRDS(paste0(dFolder, "models/", model, "/", 
                          house, "_validated_model.rds"))
    pdf(file=paste0(pFolder, model, "/", 
                    house, "_residual_acv.pdf"))
    q <- forecast::Acf(mdl$residuals, lag.max = 48*7*2, # 2 weeks
                       type = "correlation",
                       na.action = na.pass, 
                       main = "")
    # save.image(file = paste0(pFolder, model, "/", 
    #                         house, "_residual_acv.png"))
    # dev.off()
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
    saveRDS(avResDF, file = paste0(dFolder,"models/", 
                                   model, "/dailyResiduals.rds"))
  }

  plotResDF <- melt(avResDF, id = "dHour")
  
  yMin <- min(plotResDF$value, na.rm = TRUE)
  yMax <- max(plotResDF$value, na.rm = TRUE)

  
  p <- ggplot(plotResDF, aes(x = dHour, y = value)) +
    geom_line() +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    facet_wrap(.~variable, ncol = 3)
  p <- p + labs(x = "Hour of day", y = "Residual")
  p <- addPeaks(p)
  
  ggsave(paste0(pFolder, model, "/allHousesResidual.pdf"), 
         width = 210, height = 260, units = "mm")
  ggsave(paste0(pFolder, model, "/allHousesResidual.png"), 
         width = 210, height = 260, units = "mm")
  

  p <- ggplot(plotResDF, aes(x = dHour, 
                             y = value, 
                             group = variable)) +
    geom_boxplot() 
  p <- p + labs(x = "Hour of day", y = "Residual")
  p <- addPeaks(p)
  ggsave(paste0(pFolder, model, "/allHousesResidualBoxplot.pdf"))
  ggsave(paste0(pFolder, model, "/allHousesResidualBoxplot.png"))
}
