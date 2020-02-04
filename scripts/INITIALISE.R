# This runs the scripts needed to do this analysis in the correct order.
# Note that some scripts may need to be altered before this will work 

# Make sure working directory is ".../HWCanalysis"
source(paste0(getwd(), "/filepaths.R"))

source(paste0(sFile, "installPackages.R"))
source(paste0(sFile, "imputeTotalPower.R")) 
source(paste0(sFile, "extractCircuitFromCleanGridSpy1min.R")) 
#source(paste0(sFile, "element_rating.R")) # Discontinued, not useful
source(paste0(sFile, "processing.R")) # Processes the data into the required format
source(paste0(sFile, "acv_plot.R")) # This creates ACV plots
source(paste0(sFile, "freq_analysis.R")) # Calculates the frequency analysis
source(paste0(sFile, "linear_plot_one_house.R")) # Creates linear regression plots
source(paste0(sFile, "average_demand.R")) # Creates average demand plots
source(paste0(sFile, "modelling.R")) # Creates the models
source(paste0(sFile, "four_plot.R")) # Creates model comparison plots of 4 households
source(paste0(sFile, "four_plot_SVM.R")) # Creates SVM plot of 4 households