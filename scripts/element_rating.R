# This script estimates the element rating of each house
# in our dataset based on the maximum output
# Not actually accurate as it doesn't take anomolies into consideration.
# Consequently not currently in use.
###############################################

if (!exists("dFile")){
  dFile <- "~/HWCanalysis/data/" 
}
if (!exists("DT")){
  load(paste0(dFile, "DT.Rda"))
}

element_DF <- DT %>%
  group_by(linkID) %>%
  select(HWelec) %>%
  summarise_each(fun = max)
  
names(element_DF) <- c("house", "elementMax")  
save(element_DF, file = paste0(dFile, "element_DF.Rda"))