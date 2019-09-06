# This processes data downloaded from the UK data service
# into the form needed for our analysis

# Note that preliminary analysis showed a number of households to
# be unsuitable for this project. For more information see the
# file "suitable_houses.txt"

# NOTE this process has changed dramatically since using the UKDS dataset.

# BEFORE THIS SCRIPT WILL WORK YOU MUST:

# Download and extract the files
# Delete houses 15 and 17
# Impute total electricity from the submeters using `imputeTotalPower.R`
# Extract total electricity from the imputed output files using 
# extractCircuitFromCleanGridSpy1min.R, circuit string "mputed"
# Extract hot water from the imputed files using 
# extractCircuitFromCleanGridSpy1min.R, circuit string "ater"

# THIS SCRIPT THEN:
# Deletes houses 07, 09, 10, 17b, 19, 21, 23, 26, 28, 41, 43, 46, 47
# as per suitable_houses.txt
# Combines hot water elec and total in new datatable
# Subtracts hot water from total

library(data.table)
library(lubridate)
library(dplyr)
#library(plyr)

# Set path to where we are keeping data
if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}

p <- fread(paste0(dFile, "mputed_2010-01-01_2020-01-01_observations.csv.gz"))
q <- fread(paste0(dFile, "ater_2010-01-01_2020-01-01_observations.csv.gz"))

# remove houses as per suitable_houses.txt
remove <- c("07", "09", "10", "17b", "19", "21", "23", "26", "28", "41", "43", "46", "47")
all_elec <- p[!grepl(paste(remove, collapse="|"), p$hhID),] 
hw_elec <- q[!grepl(paste(remove, collapse="|"), q$hhID),]

hw_elec <- hw_elec[,c("linkID","r_dateTime","powerW")]
names(hw_elec) <- c("linkID","r_dateTime","HWelec")
all_elec <- all_elec[,c("linkID","r_dateTime","powerW")]
names(all_elec) <- c("linkID","r_dateTime","nonHWelec")

hw_elec <- data.table(hw_elec)
all_elec <- data.table(all_elec)


#save(hw_elec, file = paste0(dFile, "hw_elec"))
#save(all_elec, file = paste0(dFile, "all_elec"))


DT <- dplyr::left_join(all_elec,hw_elec)


DT <- data.table(DT)
DT <- DT[, dateTime_nz := lubridate::as_datetime(r_dateTime, # stored as UTC
                                                 tz = 'Pacific/Auckland')] # so we can extract within NZ dateTime`

# Reorder columns
setcolorder(DT, neworder = 
              c("linkID","r_dateTime","dateTime_nz","HWelec","nonHWelec"))
# Remove rows with NA values
DT <- na.omit(DT)
# Remove HW elec from all elec
DT$nonHWelec <- DT$nonHWelec - DT$HWelec

#DT <- data.table(DT)

save(DT, file = paste0(dFile, "DT.Rda"))

# Note that some rows which had only one observation
# (i.e a nonHWelec value for a particular time with no
# corresponding HWelec value for that time) were dropped from DT
# The following calculates for how many values this ocurred

pc_rm <- (length(all_elec$linkID) - length(hw_elec$linkID))/length(DT$linkID)*100
save(pc_rm, file = paste0(dFile, "pc_rm"))
# This shows that less than 1% (~0.7%) of values have been removed
# by this process - we can live with this.

#load(paste0(dFile,"DT.Rda"))

# This gives the datetime as the START of each 15 min average
DT[, qHour := hms::trunc_hms(dateTime_nz, 15*60)]

# This creates the quarter-hour average data.table

DT_qh <- DT %>% 
  group_by(linkID, qHour) %>% 
  summarise (nonHWelec = mean(nonHWelec), HWelec = mean(HWelec))

save(DT_qh, file = paste0(dFile, "DT_qh.Rda"))

# This gives the datetime as the START of each 15 min average
DT[, hHour := hms::trunc_hms(dateTime_nz, 30*60)]

# Now we create the half-hour average data.table

DT_hh <- DT %>% 
  group_by(linkID, hHour) %>% 
  summarise (nonHWelec = mean(nonHWelec), HWelec = mean(HWelec))

save(DT_hh, file = paste0(dFile, "DT_hh.Rda"))

########################################################################
# The remainder of this script is generally unnecessary for the main body
# of analysis carried out in this thesis, but provided the means to 
# determine how to carry out the initial processing occurring above


# This creates a summary of each house

summary_DT <- NULL
for (i in houses){
  load(paste0(dFile, i, "_at_1.Rda"))
  p <- s %>%
    group_by(circuit, linkID) %>%
    summarise(meanW = mean(powerW))
  summary_DT <- bind_rows(summary_DT, p)
}
save(summary_DT, file = paste0(dFile, "summary_DT.Rda"))

# This gives all houses with PV

PV_houses <- summaryDT %>%
  subset(grepl("PV", circuit)) %>%
  select(linkID)
PV_houses <- PV_houses$linkID
save(PV_houses, file = paste0(dFile, "PV_houses.Rda"))

houses <- unique(DT$linkID)

# By manual inspection rf_39 was found to have negative power
# despite not having generating capabilities
# We now look for this property in other houses

load(paste0(dFile,"PV_houses.Rda"))
no_pv <- setdiff(houses, PV_houses)

neg_values <- DT %>%
  filter(linkID == no_pv & nonHWelec < 0 | HWelec < 0) %>% 
  group_by(linkID) %>%
  summarise(no_rows = length(linkID))

# This gives the houses that do not have seperate HW metering

seperate_HW <- summaryDT %>%
  subset(grepl("ater", circuit))

no_hw_metering <- setdiff(summaryDT$linkID, seperate_HW$linkID)
save(no_hw_metering, file = paste0(dFile, "no_hw_metering.Rda"))
