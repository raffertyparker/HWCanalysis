# Differencing example

dFolder <- "~/HWCanalysis/data/" 
pFolder <- "/home/parra358/HWCanalysis/plots/"  
library(ggplot2)
library(gridExtra)
theme_set(theme_minimal(base_size = 14))

df <- readr::read_csv(paste0(dFolder, "households/validating/rf_22_at_30_min_for_validating.csv"))
df$total <- df$nonHWelec + df$HWelec
df <- df[1:(48*4+1), ]
df1 <- data.frame(df$hHour, df$total)
dfDiff <- diff(df$total)
df1 <- df1[1:(48*4), ]
df1$diff <- dfDiff
names(df1) <- c("hHour", "Original", "Differenced")
df1 <- data.table::melt(df1, id = "hHour")

p <- ggplot(df1, aes(x = hHour, y = value, group = variable)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free") +
  labs(y = "Watts", x = "Time", group = "")
  #facet_wrap(~variable, ncol = 1, scales = "free")
p 

ggsave(p, filename = paste0(pFolder, "differencingExample.pdf"))
ggsave(p, filename = paste0(pFolder, "differencingExample.png"))