# This takes model outputs and plots a day worth
plotModel <- function(ModelData, ModelName){
  pMdl <- as.data.table(ModelData$x)
  names(pMdl) <- c("Time", "Actual")
  pMdl$Fitted <- ModelData$fitted
  pMdl <- melt(for_plotting, id = "Time") 
  pMdl <- dplyr::arrange(pMdl, Time) # rearranges chronologically
  ggplot(data = pMdl[48:(48*3), ], aes(x = Time)) +
    geom_line(aes(y = value, colour = variable)) +
    labs(y = "Power (W)", colour = "")
  ggsave(filename = paste0(pFile, ModelName,"/", house, ".pdf"))
}


