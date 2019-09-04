# This script estimates the element rating of each house
# in our dataset
# CCV PLOT #
###############################################
# If plot location pFile and data location dFile
# already exist in global variables
# make sure they point to correct paths
# If they are not, make sure the following lines
# define them correctly
###############################################
if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
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