# This runs the scripts needed to recreate this analysis in the correct order.
# Note that some scripts may need to be altered for this to work soothly

# Make sure working directory is ".../HWCanalysis"
source(paste0(getwd(), "/filepaths.R")) # Load filepaths into local environment
source(paste0(sFolder, "installPackages.R")) # Install required packages
source(paste0(sFolder, "imputeTotalPower.R")) # Calculate total power consumption of all households
source(paste0(sFolder, "extractCircuitFromCleanGridSpy1min.R")) # Extract hot water amd total power columns from all households
source(paste0(sFolder, "processing.R")) # Processes the data into the required format
source(paste0(sFolder, "acv_plot.R")) # This creates ACV plots
source(paste0(sFolder, "freq_analysis.R")) # Calculates the frequency analysis
source(paste0(sFolder, "linear_plot_one_house.R")) # Creates linear regression plots
source(paste0(sFolder, "average_demand.R")) # Creates average demand plots
source(paste0(sFolder, "modelling.R")) # Creates the models
source(paste0(sFolder, "four_plot.R")) # Creates model comparison plots of 4 households
source(paste0(sFolder, "four_plot_SVM.R")) # Creates SVM plot of 4 households
source(paste0(sFolder, "STL_plots.R")) # Creates STL plots
source(paste0(sFolder, "residual_analysis.R")) # Analyses model residuals
source(paste0(sFolder, "sin_example.R"))  # Creates sin example for explaining Rourier analysis