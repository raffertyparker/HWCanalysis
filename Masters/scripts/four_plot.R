# Plots 4 houses
# Needs model in the environment to be named according to household (rf_XX)
# Could be improved

fourPlot <- function(modelName,house1,house2,house3,house4){
  house1$linkID <- deparse(substitute(house1))
  house2$linkID <- deparse(substitute(house2))
  house3$linkID <- deparse(substitute(house3))
  house4$linkID <- deparse(substitute(house4))
  
  plotDT <- rbind(house1,house2,house3,house4)
  
  plotDT <- dplyr::arrange(plotDT, Time)
  # plotexample <- plotDT[48:(48*9), ]
  
  p <- ggplot(data = plotDT[48:(48*9), ], aes(x = Time)) +
    geom_line(aes(y = value, colour = variable)) +
    facet_grid(rows = vars(linkID), scales = "free_y")
  #facet_wrap(. ~ linkID, scales = "free")
  p + labs(y = "Power (W)", colour = "")
  ggsave(filename = paste0(pFile, modelName, "/fourHouses.pdf"))
}