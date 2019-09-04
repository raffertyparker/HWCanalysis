# This runs the scripts needed to do this analysis in the correct order.
# Note that some scripts may need to be altered before this will work 

pFile <- "/home/parra358/HWCanalysis/Masters/plots/"  # Input plot folder location
dFile <- "~/HWCanalysis/Masters/data/"                # Input data folder location

source("~/HWC-bookdown/Masters/scripts/installPackages.R")
source("~/HWC-bookdown/Masters/scripts/extractCircuitFromCleanGridSpy1min.R") # !!Change filepaths!!
source("~/HWC-bookdown/Masters/scripts/imputeTotalPower.R") # !!Change filepaths!!
source("~/HWC-bookdown/Masters/scripts/element_rating.R")
source("~/HWC-bookdown/Masters/scripts/processing.R") 
source("~/HWC-bookdown/Masters/scripts/acv_plot.R") # This creates ACV plots
source("~/HWC-bookdown/Masters/scripts/freq_analysis.R") # Calculates the frequency analysis
