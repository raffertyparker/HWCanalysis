# Create model comparison plot

if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("pFile")){
  pFile <- "~/HWCanalysis/Masters/plots/" 
}

# Plot of the variance of the models
DF <- readRDS(file = paste0(dFile, "allHouseModelStats.rds"))
DF$model <- factor(DF$model, levels = c("naive","seasonalNaive","simpleLinear",
                                                          "ARIMA","ARIMAX","SARIMA"), 
                            labels = c("Naive","Seasonal Naive","Linear Regression",
                                       "ARIMA","ARIMAX","SARIMA"))

p <- ggplot(DF, aes(x = RMSE, group = model)) +
  geom_density() +
  facet_grid(model ~.)
ggsave(filename = paste0(pFile, "modelRMSEdensity.png"))
ggsave(filename = paste0(pFile, "modelRMSEdensity.pdf"))

p <- ggplot(DF, aes(x = RMSE, group = model)) +
  geom_bar() +
  facet_grid(model ~.)
ggsave(filename = paste0(pFile, "modelRMSEbar.png"))
ggsave(filename = paste0(pFile, "modelRMSEbar.pdf"))


modelVariance <- DF %>%
  group_by(model) %>%
  summarise(variance = var(RMSE))
saveRDS(modelVariance, file = paste0(dFile, "modelVariance.rds"))



df <- readRDS(file = paste0(dFile, "AllModelSummaryStats.rds"))

# This inputs the qualitative values for fidelity/interpretability
# There is probably a better way of doing this
df$interp_fidelity <- rep(0, nrow(df))
df$interp_fidelity[df$model == "ARIMA"] <- 0.69
df$interp_fidelity[df$model == "SARIMA"] <- 0.35
df$interp_fidelity[df$model == "ARIMAX"] <- 0.25
df$interp_fidelity[df$model == "naive"] <- 1 
df$interp_fidelity[df$model == "seasonalNaive"] <- 1
df$interp_fidelity[df$model == "simpleLinear"] <- 0.66
  
    
df_adjusted <- df
df_adjusted$RMSE <- 1-(df$RMSE - min(df$RMSE))/(max(df$RMSE) - min(df$RMSE))
df_adjusted$fittingTime <- 1-(df$fittingTime - min(df$fittingTime))/(max(df$fittingTime) - min(df$fittingTime))
df_adjusted$memSize <- 1-(df$memSize - min(df$memSize))/(max(df$memSize) - min(df$memSize))

df_adjusted <- reshape2::melt(df_adjusted)

df_adjusted$variable <- factor(df_adjusted$variable, levels = c("RMSE", "fittingTime", "memSize", "interp_fidelity"),
                               labels = c("Accuracy", "Speed", "Compactness", "Fidelity/Interpretability"))
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


#p <- ggRadar(df_adjusted, aes(group = model), 
#             alpha = 0, scales = "free",
#             size = 0)
