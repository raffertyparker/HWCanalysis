# This runs the scripts needed to do this analysis in the correct order.
# Note that some scripts may need to be altered before this will work 

#filepathsPath <- getwd()
#filepathsPath <- getwd()
source(paste0(getwd(), "/filepaths.R"))

# pFile <- "/home/parra358/HWCanalysis/Masters/plots/"  # Input plot folder location
# dFile <- "~/HWCanalysis/Masters/data/"                # Input data folder location
# sFile <- "~/HWCanalysis/Masters/scripts/"             # Input scripts folder

source(paste0(sFile, "installPackages.R"))
source(paste0(sFile, "extractCircuitFromCleanGridSpy1min.R")) # !!Change filepaths!!
source(paste0(sFile, "imputeTotalPower.R")) # !!Change filepaths!!
#source(paste0(sFile, "element_rating.R")) # 
source(paste0(sFile, "processing.R")) 
source(paste0(sFile, "acv_plot.R")) # This creates ACV plots
source(paste0(sFile, "freq_analysis.R")) # Calculates the frequency analysis
# source(paste0(sFile, "random_walk.R"))
source(paste0(sFile, "linear_plot_one_house.R"))
source(paste0(sFile, "average_demand.R"))
source(paste0(sFile, "modelling.R"))