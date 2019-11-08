# Create model comparison plot

if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}


df <- readRDS(file = paste0(dFile, "AllModelSummaryStats.rds"))

df_adjusted <- df
df_adjusted$RMSE <- 1-(df$RMSE - min(df$RMSE))/(max(df$RMSE) - min(df$RMSE))
df_adjusted$fittingTime <- 1-(df$fittingTime - min(df$fittingTime))/(max(df$fittingTime) - min(df$fittingTime))
df_adjusted$memSize <- 1-(df$memSize - min(df$memSize))/(max(df$memSize) - min(df$memSize))

df_adjusted <- reshape2::melt(df_adjusted)

df_adjusted$variable <- factor(df_adjusted$variable, levels = c("RMSE", "fittingTime", "memSize"),
                               labels = c("Accuracy", "Speed", "Compactness"))
df_adjusted$model <- factor(df_adjusted$model, levels = c("naive","seasonalNaive","simpleLinear",
                                                          "ARIMA","ARIMAX","SARIMA"), 
                            labels = c("Naive","Seasonal Naive","Linear Regression",
                                       "ARIMA","ARIMAX","SARIMA"))

p <- ggplot(df_adjusted, aes(x = model, y = value,
                         fill = model)) +
  geom_bar(stat = "identity") + 
  facet_grid(variable ~ .) +
  guides(fill = FALSE) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  scale_fill_brewer()
p + labs(x = "", y = "")
ggsave(filename = paste0(pFile, "modelSummary.png"))
ggsave(filename = paste0(pFile, "modelSummary.pdf"))


p <- ggRadar(df_adjusted, aes(group = model), 
             alpha = 0, scales = "free",
             size = 0)
