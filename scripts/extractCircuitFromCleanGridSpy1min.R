####- R script to extract and save all observations whose circuit labels match a given pattern between two dates -####
# If the pattern/dateFrom/dateTo file already exists, it skips and ends

# Best run using: "> Rscript extractCircuitFromCleanGridSpy1min.R"

# Header ----

#> Set start time ----
startTime <- proc.time()

#> Load GREENGrid package ----
library(GREENGridData)

#> Packages needed in this .Rmd file ----
rmdLibs <- c("data.table", # data munching
             "dplyr", # data munching
             "readr"
)
# load them
GREENGridData::loadLibraries(rmdLibs)

#> Local parameters ----
twoPatterns <- c("ater", "mputed")

#circuitPattern <- "mputed" # change this to change the circuit(s) extracted if doing manually

dateFrom <- "2010-01-01" # change this to change the (inclusive) UTC date to search from
dateTo <- "2020-01-01" # change this to change the (inclusive) UTC date to search to
# NB: searching in whole UTC dates will produce partial days in other timezones (e.g. NZ). We hate timezones.

# Automatically extract all circuits and HW circuit:
for (circuitPattern in twoPatterns){
  fullFb <- 0 # switch on (1) or off (0) full feedback
  localData <- 1 # test using local (1) or Otago HCS (0) data?
  refresh <- 1 # refresh data even if it seems to already exist

  b2Kb <- 1024 # http://whatsabyte.com/P1/byteconverter.htm
  b2Mb <- 1048576

  # >> Amend paths to suit your data storage location ----
  if(localData == 1){
    # local
    # amend these paths to suit your location
    # if the script fails it is most probably because the paths are incorrect
    iPath <- "~/HWCanalysis/powerData/imputed/" # downloaded from https://dx.doi.org/10.5255/UKDA-SN-853334
    oPath <- dFile # where the extract is saved. Create this before you run the script or it will fail
  } else {
    # HCS
    dPath <- "/Volumes/hum-csafe/Research Projects/GREEN Grid/cleanData/safe/gridSpy/1min/" # Otago HCS
    iPath <- paste0(dPath, "data/")
    oPath <- paste0(dPath, "dataExtracts/")
  }
  if(Sys.info()[4] == "gridcrawler"){
    # we're on the CS RStudio server
    dPath <- path.expand("~/greenGridData/cleanData/safe/gridSpy/1min/")
    iPath <- paste0(dPath, "data/")
    oPath <- paste0(dPath, "dataExtracts/")
  }
  
  # Code ----
  
  if(localData){
    msg <- paste0("#-> Test run using reduced data from ", iPath)
  } else {
    msg <- paste0("#-> Full run using all data from ", iPath)
  }

  print(msg)

  #> Set output file name ----

  oFile <- paste0(circuitPattern, "_", dateFrom, "_", dateTo, "_observations.csv")

  exFile <- paste0(oPath, oFile) # place to save them
  exFileTest <- paste0(exFile, ".gz")

  # check paths
  if(file.exists(iPath)){
    message("Good, path to safe data exists: ", iPath)
  } else {
    message("Oops. path to safe data does not exist: ", iPath)
    stop("Please check you have set the path correctly and it exists...")
  }

  if(file.exists(oPath)){
    message("Good, place to save extracts exists: ", oPath)
  } else {
    message("Oops. place to save extracts does not exist: ", oPath)
    stop("Please check you have set the path correctly and it exists...")
  }
  
  #>  Run the extraction ----

  if(file.exists(exFileTest) & refresh == 0){
    print(paste0(exFileTest, " exists and refresh = 0 so skipping.")) # prevents the file load in the function
    print(paste0("=> You may need to check your circuit label pattern (",
                 circuitPattern ,") and date filter settings (",
                 dateFrom, " - ", dateTo, ") if this is not what you expected."))
    
    
  } else {
    print(paste0(exFileTest, " does not exist or refresh == 1 so running extraction.")) # prevents the file load in the function
    extractedDT <- GREENGridData::extractCleanGridSpyCircuit(iPath,
                                                             exFile,
                                                             circuitPattern,
                                                             dateFrom,
                                                             dateTo)
    print(paste0("Done - look for data in: ", exFileTest))
    t <- proc.time() - startTime
    elapsed <- t[[3]]
    print(paste0("Extraction of ", circuitPattern," circuit data completed in ",
               round(elapsed,2),
                 " seconds ( ",
                 round(elapsed/60,2), " minutes) using ",
                 R.version.string , " running on ", R.version$platform , "."))
  }
}
