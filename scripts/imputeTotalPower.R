print(paste0("#--------------- Processing NZ GREEN Grid Grid Power Data ---------------#"))

# -- Code to correctly sum (as far as possible) household power demand to get an overall total -- #
# Required because some circuits are seperate from the 'Incomer' - e.g. seperately controlled hot water
# Code (c) 2018 Jason Mair - jkmair@cs.otago.ac.nz 
# with amendments from ben.anderson@otago.ac.nz
# edit history:

# Notes:
# This code requires:
# - a csv file in the package /data folder which
# specifies the circuits to be used when calculating the total for each
# house. 
# - the cleaned safe household level data from http://reshare.ukdataservice.ac.uk/853334/

# The code below calculates per-house per-minute totals, and adds them to each household file as a new 'circuit type' for later use.
#
# The code assumes that the circuits in the circuits file are actually the ones to sum to get overall
# power demand. We have checked as best we can. If you notice errors please add an issue at:
#
# https://github.com/CfSOtago/GREENGridData/issues?q=is%3Aissue+is%3Aopen+label%3AdataIssue
#
# Note that the code attempts to check which circuits contribute to the total, but this
# method allows for further checking by using the circuit names.

# Run rarely - takes time

# Libraries ----

library(lubridate)
library(data.table)
library(readr) # use fread instead where possible
library(here)

# parameterise data path - edit for your set up ----

sysname <- Sys.info()[[1]]
login <- Sys.info()[[6]]
user <- Sys.info()[[7]]

# Select your circuits-to-sum file
#circuitsFile <- "circuitsToSum_v1.0" # JKM original
circuitsFile <- "circuitsToSum_v1.1" # make sure circuitsToSum_v1.1 is in the powerData folder

# localise data paths - edit for your context
DATA_PATH = "~/HWCanalysis/powerData" # <- Raff 
circuits_path <- paste0(DATA_PATH, "/", circuitsFile, ".csv") # <- Raff
#circuits_path <- sprintf("%s/circuitsToSum.csv", DATA_PATH) # <- Raff

if(user == "dataknut" & sysname == "Linux"){
  # CS RStudio server
  DATA_PATH <- "~/greenGridData/cleanData/safe/gridSpy/1min/data"
  circuits_path <- paste0(here::here(), "/data/", circuitsFile, ".csv") # in the package data folder
}
if(user == "ben" & sysname == "Darwin"){
  # Ben's laptop
  DATA_PATH <- "~/Data/NZ_GREENGrid/safe/gridSpy/1min/data"
  circuits_path <- paste0(here::here(), "/data/", circuitsFile, ".csv") # in the package data folder
}

message("Running on ", sysname, " under user ", user)
message("Loading data from ", DATA_PATH)
message("Using ", circuitsFile)

# set up ----

# Plot full numbers, not scientific
options(scipen=999)

# Set system timezone to UTC
Sys.setenv(TZ='UTC')

# Set the time period I want to get the per-house totals for ----
start_time <- as.POSIXct("2010-01-01 00:00", tz="Pacific/Auckland")
end_time <- as.POSIXct("2020-01-01 00:00", tz="Pacific/Auckland")

start_exec <- Sys.time()

message("Loading circuits from: ", circuits_path)
circuitsDF <- read.csv(circuits_path, header = TRUE) # if we switch to readr::read_csv the processing code breaks

dataL <- data.table() # data bucket to collect per-household per-minute imputed load

# process household files ----
processPowerFiles <- function(df){
  message("Using data from: ", DATA_PATH)
  for(house_id in colnames(df)){
    #house_id <- "rf_19" # for testing
    
    # input <- data.table::as.data.table(readr::read_csv(sprintf("%s/%s_all_1min_data.csv.gz", DATA_PATH, house_id), 
    #                                                    col_types=cols(hhID=col_character(), 
    #                                                                   linkID=col_character(), 
    #                                                                   dateTime_orig=col_character(), 
    #                                                                   TZ_orig=col_character(), 
    #                                                                   r_dateTime=col_character(), 
    #                                                                   circuit = col_character(), 
    #                                                                   powerW=col_double())
    #                                                    )
    #                                    )
    iF <- sprintf("%s/%s_all_1min_data.csv.gz", DATA_PATH, house_id)
    message(" -> Loading data for ", house_id, " from ", iF)
    inputDT <- data.table::fread(iF) # load the household data
    message("N rows: ", nrow(inputDT))
    message(" -> Parsing dates ", house_id)
    
    inputDT <- inputDT[, dateTime_nz := lubridate::as_datetime(r_dateTime, # stored as UTC 
                                                               tz = "Pacific/Auckland")] # so we can extract within NZ dateTime
    
    # select dates we want
    inputDT <- inputDT[dateTime_nz >= start_time & dateTime_nz < end_time, ]
    message("N rows after date extraction: ", nrow(inputDT))
    
    # Extract the circuit code numbers
    circuit_list <- circuitsDF[, house_id] # extracts the columns that match the house id
    circuit_list <- sub(".*\\$", "", circuit_list) # extracts the circuit numeric label (not the string)
    circuit_list <- circuit_list[circuit_list != ""] # makes sure no empty strings
    
    # Create a boolean array of rows/circuits to be extracted
    cond <- rep(FALSE, length(inputDT$circuit))
    
    for(circuit in circuit_list){
      message(" -> Checking circuit: ", circuit)
      cond <- cond | grepl(circuit, inputDT$circuit)
    }
    
    circuitsToSumDT <- inputDT[cond, 
                               c("hhID","linkID", "dateTime_orig","TZ_orig","r_dateTime", "powerW")] # uses cond to pull out just the circuits we're going to add up
    
    # make long verion (easier for data analysis)
    totDTl <- circuitsToSumDT[,.(powerW = sum(powerW)), 
                              keyby = .(hhID,linkID,dateTime_orig,TZ_orig,r_dateTime)] # so it has the same format as the original
    totDTl$circuit <- paste0("imputedTotalDemand_", circuitsFile) # add a label so we know what it is
    # add the total back to the input DT and save as new file
    inputDT$dateTime_nz <- NULL # not needed
    setcolorder(inputDT, c("hhID", "linkID", "dateTime_orig", "TZ_orig", "r_dateTime", "circuit", "powerW")) # make sure they match
    setcolorder(totDTl, c("hhID", "linkID", "dateTime_orig", "TZ_orig", "r_dateTime", "circuit", "powerW")) # make sure they match
    outDT <- rbind(inputDT,
                   totDTl) # add total to input
    oF <- paste0(DATA_PATH, "/imputed/",house_id, "_all_1min_data_withImputedTotal_",
                 circuitsFile, ".csv") # use the circuitsFile name so we know the definition of which circuits we summed
    message("Saving and gzipping: ", oF)
    data.table::fwrite(outDT, oF) # save the household data - use fwrite as it is FAST
    # gzip it
    cmd <- paste0("gzip -f ", oF)
    try(system(cmd)) # produces a warning on CS RStudio server but still works
    dataL <- rbind(totDTl, dataL)
    message("Done")
  }
  message("All households processed using: ", circuitsFile)
  ofile <- paste0(DATA_PATH, "/imputed/all_1min_data_withImputedTotal_",
                  circuitsFile, ".csv") # use the circuitsFile name so we know the definition of which circuits we summed
  message("Saving and gzipping: ", ofile)
  data.table::fwrite(dataL, ofile) # save the household data - use fwrite as it is FAST
  # gzip it
  cmd <- paste0("gzip -f ", ofile)
  try(system(cmd)) # produces a warning on CS RStudio server but still works
  message("Saving single file of imputed load only to: ", ofile)
}

processPowerFiles(circuitsDF)

# how long did it take?
end_exec <- Sys.time()
end_exec - start_exec