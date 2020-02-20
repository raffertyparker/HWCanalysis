# This processes data downloaded from the UK data service
# into the form needed for our analysis

# Note that preliminary analysis showed a number of households to
# be unsuitable for this project. For more information see the
# file "suitable_houses.txt"

# BEFORE THIS SCRIPT WILL WORK YOU MUST:

# Download and extract the files
# Delete houses 15 and 17 as per github instructions
# Impute total electricity from the submeters using `imputeTotalPower.R`
# Extract total electricity from the imputed output files using 
# extractCircuitFromCleanGridSpy1min.R, circuit string "mputed"
# Extract hot water from the imputed files using 
# extractCircuitFromCleanGridSpy1min.R, circuit string "ater"

# THIS SCRIPT THEN:
# Removes houses 07, 09, 10, 17b, 19, 21, 23, 26, 28, 41, 43, 46, 47
# Combines hot water elec and total in new data table
# Subtracts hot water from total
# Cleans 'holes' from some further households 
# Creates summary data tables

library(data.table)
library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)

# Set path to where we are keeping data
if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/" 
}

# If different extraction dates are used these need to be changed accordingly
p <- fread(paste0(dFolder, "mputed_2010-01-01_2020-01-01_observations.csv.gz"))
q <- fread(paste0(dFolder, "ater_2010-01-01_2020-01-01_observations.csv.gz"))

# remove unsuitable houses if they haven't been deleted already
remove <- c("01", "07", "09", "10", "11", "15b", "17a", "17b", "19", "21",
            "23", "24", "26", "27", "28", "41", "43", "46", "47")
all_elec <- p[!grepl(paste(remove, collapse="|"), p$linkID),] 
hw_elec <- q[!grepl(paste(remove, collapse="|"), q$linkID),]

hw_elec <- hw_elec[,c("linkID","r_dateTime","powerW")]
names(hw_elec) <- c("linkID","r_dateTime","HWelec")
all_elec <- all_elec[,c("linkID","r_dateTime","powerW")]
names(all_elec) <- c("linkID","r_dateTime","nonHWelec")

hw_elec <- data.table(hw_elec)
all_elec <- data.table(all_elec)

#save(hw_elec, file = paste0(dFolder, "hw_elec"))
#save(all_elec, file = paste0(dFolder, "all_elec"))

DT <- dplyr::left_join(all_elec,hw_elec)
DT <- data.table(DT)
DT <- DT[, dateTime_nz := lubridate::as_datetime(r_dateTime, # stored as UTC
                                                 tz = 'Pacific/Auckland')] # so we can extract within NZ dateTime`

houses <- unique(DT$linkID)
save(houses, file = paste0(dFolder, "houses.Rda"))
# Reorder columns
setcolorder(DT, neworder = 
              c("linkID","r_dateTime","dateTime_nz","HWelec","nonHWelec"))

# Remove HW elec from all elec
DT$nonHWelec <- DT$nonHWelec - DT$HWelec
DT[HWelec < 0, HWelec := 0]
save(DT, file = paste0(dFolder, "DT_no_houses_removed.Rda"))

# This gives the datetime as the start of each 30 min average
DT[, hHour := hms::trunc_hms(dateTime_nz, 30*60)]

# Now we create the half-hour average data.table
# this is done as the 1min DT was causing the machine to hang
# when attempting to generate plots
DT_hh <- DT %>% 
  group_by(linkID, hHour) %>% 
  summarise (nonHWelec = mean(nonHWelec), HWelec = mean(HWelec))

DT_hh <- as.data.table(DT_hh)
save(DT_hh, file = paste0(dFolder, "DT_hh_no_houses_removed.Rda"))

#####################################################################
# This section creates plots of the raw data to demonstrate anomolies
# and holes. The offending households are cleaned up after this block

p <- ggplot(DT_hh, aes(x = hHour, y = HWelec)) + 
  geom_point() + 
  facet_wrap(~linkID, ncol = 4)
p + labs(x = "Date", y = "Power (W)", 
         title = "")
ggsave(filename = paste0(pFolder, "prelim/allHousesNoHolesRemoved.png"))
ggsave(filename = paste0(pFolder, "prelim/allHousesNoHolesRemoved.pdf"))

for (house in houses){
  q <- DT[linkID == house]
  p <- ggplot(q, aes(x = hHour, y = HWelec)) +
    geom_point()
  p + labs(x = "Date", y = "Power (W)", 
           title = paste0("Household ", house))
  ggsave(filename = paste0(pFolder, "prelim/", house, "_test.pdf"))
}

#####################################################################

# remove date with negative values on rf_14
DT <- DT[!(linkID == "rf_14" & dateTime_nz < "2015-07-01")] 

# Keep largest 'section' of uninterrupted data per household
# Discard everything else
DT <- DT[!(linkID == "rf_34" & dateTime_nz < "2015-03-27")] 
DT <- DT[!(linkID == "rf_06" & (dateTime_nz < "2015-01-18" | dateTime_nz > "2017-02-27"))] 
DT <- DT[!(linkID == "rf_12" & dateTime_nz < "2015-01-04")] 
DT <- DT[!(linkID == "rf_15b" & dateTime_nz > "2015-08-28")] 
DT <- DT[!(linkID == "rf_31" & dateTime_nz > "2016-02-27")] 
DT <- DT[!(linkID == "rf_30" & dateTime_nz > "2015-10-08")] 
DT <- DT[!(linkID == "rf_33" & dateTime_nz > "2016-10-28")] 
DT <- DT[!(linkID == "rf_34" & dateTime_nz < "2015-03-27")] 
DT <- DT[!(linkID == "rf_35" & dateTime_nz < "2016-07-13")] 
DT <- DT[!(linkID == "rf_36" & dateTime_nz > "2017-12-04")] 
DT <- DT[!(linkID == "rf_38" & dateTime_nz < "2016-08-22")] 
DT <- DT[!(linkID == "rf_39" & (dateTime_nz < "2015-05-20" | dateTime_nz > "2017-11-19"))] 

# Now DT needs to be sorted chronologically by household
DT <- setkey(DT,linkID,dateTime_nz)
# This gives the datetime as the START of each 30 min average
DT[, hHour := hms::trunc_hms(dateTime_nz, 30*60)]
# This finds missing values and replaces with zeros
# by creating a datetime vector the length it should be
# based on beginning and end times at 1 min intervals
# Then exports as model training data and validating data
# (80% and 20% of the total data, respectively)
missingValues <- c()
for (house in houses) {
  q <- DT[linkID == house]
  nRows <- length(q$r_dateTime)
  nMins <- as.numeric(difftime(q$r_dateTime[nRows], q$r_dateTime[1],
                               units="mins")) 
  # Sequence of datetimes spanning from beginning to end of observations
  dateTime_nz <- seq.POSIXt(from = q$dateTime_nz[1], by = "mins", length.out = nMins)
  dt <- data.table(dateTime_nz)
  newDT <- full_join(dt,q) 
  newDT <- newDT[,c("dateTime_nz", "HWelec", "nonHWelec")]
  s <- nrow(newDT)
  # Keep track of missing values by number and percentage of total
  missingValues <- rbind(missingValues, c(house, sum(is.na(newDT)), 
                                          round(100*sum(is.na(newDT))/s, 2)))
  newDT[is.na(newDT)] <- 0
  newDT$dateTime_nz <- as_datetime(newDT$dateTime_nz, tz = 'Pacific/Auckland')
  assign(paste0(house, "_at_1_min"), newDT) %>%
    write_csv(path = paste0(dFolder, "households/", house, "_at_1_min.csv"))
  assign(paste0(house, "_at_1_min_for_fitting"), get(paste0(house, "_at_1_min"))[1:as.integer(0.8*s),]) %>%
    write_csv(path = paste0(dFolder, "households/fitting/", house, "_at_1_min_for_fitting.csv"))
  assign(paste0(house, "_at_1_min_for_validating"), get(paste0(house, "_at_1_min"))[as.integer(0.8*s):s,]) %>%
    write_csv(path = paste0(dFolder, "households/validating/", house, "_at_1_min_for_validating.csv"))
  newDT <- data.table(newDT)
  newDT[, hHour := hms::trunc_hms(dateTime_nz, 30*60)]
  newDT_hh <- newDT %>% 
    group_by(hHour) %>% 
    summarise (nonHWelec = mean(nonHWelec), HWelec = mean(HWelec))
  # Now create and save the half hour data
  newDT_hh <- as.data.table(newDT_hh)
  t <- nrow(newDT_hh)
  setcolorder(newDT_hh, c("hHour", "nonHWelec" ,"HWelec"))
  assign(paste0(house, "_at_30_min"), newDT_hh) %>%
    write_csv(path = paste0(dFolder, "households/", house, "_at_30_min.csv"))
  assign(paste0(house, "_at_30_min_for_fitting"), get(paste0(house, "_at_30_min"))[1:as.integer(0.8*t),]) %>%
    write_csv(path = paste0(dFolder, "households/fitting/", house, "_at_30_min_for_fitting.csv"))
  assign(paste0(house, "_at_30_min_for_validating"), get(paste0(house, "_at_30_min"))[as.integer(0.8*t):t,]) %>%
    write_csv(path = paste0(dFolder, "households/validating/", house, "_at_30_min_for_validating.csv"))
}

#DT_hh <- DT %>% 
#  group_by(linkID, hHour) %>% 
#  summarise (nonHWelec = mean(nonHWelec), HWelec = mean(HWelec))

#DT_hh$day <- weekdays(DT_hh$hHour)
#DT_hh$min <- gsub(".* ","", as.character(DT_hh$hHour))
#DT_hh$min <- gsub('.{3}$', '', DT_hh$min)

#DT_hh <- as.data.table(DT_hh)
#setcolorder(DT_hh, c("hHour", "min", "day", "linkID","nonHWelec" ,"HWelec"))

missingValues <- as.data.frame(missingValues)
names(missingValues) <- c("household", "NAs", "percent")
write_csv(missingValues, paste0(dFolder, "missingValues.csv"))

# Plenty of missing minutes unfortunately
# Households rf_14, rf_25 and rf_36 in particular
# all have more than 5% of values missing

#load(paste0(dFolder, "DT_before_holes_removed.Rda"))

######################################################
# This was used to manually locate larger holes in the data
#house <- "rf_36"
#q <- DT[linkID == house]
#p <- ggplot(q, aes(x = dateTime_nz, y = HWelec)) +
#  geom_point()
#p + labs(x = "Date", y = "Power (W)", 
#         title = paste0("Household ", house))
#tail(DT[linkID == house])
#ggsave(filename = paste0(pFolder, "prelim/", house, "_test.pdf"))
######################################################

DT$day <- weekdays(DT$dateTime_nz)
DT$min <- gsub(".* ","", as.character(DT$dateTime_nz))
DT$min <- gsub('.{3}$', '', DT$min)

save(DT, file = paste0(dFolder, "DT.Rda"))

# Now we create the half-hour average data.table

DT_hh <- DT %>% 
  group_by(linkID, hHour) %>% 
  summarise (nonHWelec = mean(nonHWelec), HWelec = mean(HWelec))

DT_hh <- as.data.table(DT_hh)
setcolorder(DT_hh, c("hHour", "linkID","nonHWelec" ,"HWelec"))

# Check for missing values again
for (house in houses) {
  s <- DT_hh[linkID == house]
  nRows <- length(s$hHour)
  nMins <- as.numeric(difftime(s$hHour[nRows], s$hHour[1],
                               units="hours"))
  
  assign(paste0(house, "_missing_hours"), (nMins - nRows/2))
}

save(DT_hh, file = paste0(dFolder, "DT_hh.Rda"))

houses <- unique(DT_hh$linkID)
save(houses, file = paste0(dFolder, "houses.Rda"))

p <- ggplot(DT_hh, aes(x = hHour, y = HWelec)) + 
  geom_point() + 
  facet_wrap(. ~ linkID, scales = "free", ncol = 4)
 # facet_wrap(~linkID, ncol = 4)
p + labs(x = "Date", y = "Power (W)", 
         title = "")
ggsave(filename = paste0(pFolder, "prelim/allHousesAfterRemoval.png"))

#load(paste0(dFolder, "DT_hh.Rda"))

# This creates our dummy variables for seasonality
# Converting
for (index in unique(DT_hh$day)) {
DT_hh[[index]] <- DT_hh$day == index
}
for(index in unique(DT_hh$min)){
  DT_hh[[index]] <- DT_hh$min == index
}
save(DT_hh, file = paste0(dFolder, "DT_hh_dummy.Rda"))

to.replace <- names(which(sapply(DT_hh, is.logical)))
for (var in to.replace) DT_hh[, (var):= as.numeric(get(var))]
head(DT_hh)

# This seperates data into training and test (fitting and validating)
for (house in unique(DT_hh$linkID)){
  assign(paste0(house, "_at_30_min"), DT_hh[linkID == house])
  s <- nrow(get(paste0(house, "_at_30_min")))
  assign(paste0(house, "_at_30_min_for_fitting"), get(paste0(house, "_at_30_min"))[1:as.integer(0.8*s),]) %>%
    readr::write_csv(path = paste0(dFolder, "households/fitting/", house, "_at_30_min_for_fitting.csv"))
  assign(paste0(house, "_at_30_min_for_validating"), get(paste0(house, "_at_30_min"))[as.integer(0.8*s):s,]) %>%
    write_csv(path = paste0(dFolder, "households/validating/", house, "_at_30_min_for_validating.csv"))
}

########################################################################
# The remainder of this script is generally unnecessary for the main body
# of analysis carried out in this thesis, but provided the means to 
# determine how to carry out the initial processing occurring above

# This creates a table of average power values of each house

meansDT <- DT_hh %>%
  group_by(linkID) %>%
  select(c(HWelec, nonHWelec)) %>%
  summarise_each(funs = mean)

save(meansDT, file = paste0(dFolder, "meansDT.Rda"))

# This gives all houses with PV

PV_houses <- summaryDT %>%
  subset(grepl("PV", circuit)) %>%
  select(linkID)
PV_houses <- PV_houses$linkID
save(PV_houses, file = paste0(dFolder, "PV_houses.Rda"))

houses <- unique(DT$linkID)

# By manual inspection rf_39 was found to have negative power
# despite not having generating capabilities
# We now look for this property in other houses

load(paste0(dFolder,"PV_houses.Rda"))
no_pv <- setdiff(houses, PV_houses)

neg_values <- DT %>%
  filter(linkID == no_pv & nonHWelec < 0 | HWelec < 0) %>% 
  group_by(linkID) %>%
  summarise(no_rows = length(linkID))

# This gives the houses that do not have seperate HW metering

seperate_HW <- summaryDT %>%
  subset(grepl("ater", circuit))

no_hw_metering <- setdiff(summaryDT$linkID, seperate_HW$linkID)
save(no_hw_metering, file = paste0(dFolder, "no_hw_metering.Rda"))
