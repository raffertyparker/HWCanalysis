# This takes model outputs and plots a day worth
plotModel <- function(ModelData, ModelName){
  require(lubridate)
  theme_set(theme_minimal())
  pMdl <- as.data.table(ModelData$x)
  names(pMdl) <- c("Time", "Actual")
  ifelse("fitted" %in% names(ModelData), pMdl$Fitted <- ModelData$fitted,
         pMdl$Fitted <- ModelData$fitted.values)
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
  dir.create(paste0(pFile, ModelName,"/"), showWarnings = FALSE)
  ggsave(filename = paste0(pFile, ModelName,"/", house, ".pdf"))
  ggsave(filename = paste0(pFile, ModelName,"/", house, ".png"))
}
