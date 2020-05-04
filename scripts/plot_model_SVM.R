# This takes model outputs and plots a day worth
# No longer needed SVM can now be plotted with original plot_model.R
plotModelSVM <- function(dt, fitted){
  require(lubridate)
  nRows <- nrow(dt)
  pMdl <- as.data.table(cbind.data.frame(dt$hHour[3:nRows], dt$HWelec[3:nRows], fitted))
    
  #  as.data.table(ModelData$x)
  names(pMdl) <- c("Time", "Actual", "Fitted")
#  ifelse("fitted" %in% names(ModelData), pMdl$Fitted <- ModelData$fitted,
#         pMdl$Fitted <- ModelData$fitted.values)
  #  ifelse(exists("ModelData$fitted"), pMdl$Fitted <- ModelData$fitted,
  #         pMdl$Fitted <- ModelData$fitted.values)
  pMdl <- melt(pMdl, id = "Time") 
  pMdl <- dplyr::arrange(pMdl, Time) # rearranges chronologically
  pMdl$Time <- lubridate::as_datetime(pMdl$Time, # convert from UTC
                                      tz = 'Pacific/Auckland')
  startTime <- lubridate::as_datetime(paste(as.character( # startTime is midnight on first day of next month
    rollback(pMdl$Time[1] + dweeks(5), roll_to_first = TRUE, preserve_hms = FALSE)), "00:00:00"), 
    tz = 'Pacific/Auckland')
  ggplot(data = pMdl[pMdl$Time %within% interval(startTime, startTime + days(1)), ], # Select data 1 day from startTime
         aes(x = Time)) +
    geom_line(aes(y = value, colour = variable)) +
    labs(y = "Power (W)", colour = "")
  dir.create(paste0(pFolder, "SVM/"), showWarnings = FALSE)
  ggsave(filename = paste0(pFolder, "SVM/", house, ".pdf"))
  ggsave(filename = paste0(pFolder, "SVM/", house, ".png"))
}
