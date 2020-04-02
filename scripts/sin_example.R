# This creates sinusoid example plot

library(ggplot2)
theme_set(theme_minimal())

t <- seq(1,20,0.1)
a <- sin(t)
b <- sin(t+1.5)
c <- 2*sin(t)
d <- sin(3*t)
e <- c+d

df <- as.data.frame(cbind(t,a,b,c,d,e))
names(df) <- c("t", "sin(t)", "sin(t+1.5)", "2*sin(t)", "sin(3*t)", "2*sin(t) + sin(3*t)")
df2 <- reshape2::melt(df, id = "t")
#df2$variable[df2$variable == "d"] <- "b + c"

ggplot(df2, aes(x = t, y = value, colour = variable, group = variable)) + 
  geom_path() +
  facet_wrap(~variable, ncol = 1) +
  theme(legend.position="none") +
  ylab("")
ggsave(paste0(pFolder, "sinExample.png"))
ggsave(paste0(pFolder, "sinExample.pdf"))