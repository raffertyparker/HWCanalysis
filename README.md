# HWCanalysis

Bookdown version of hot water electricity demand forecasting undertaken for my Masters thesis.

The thesis should be fully reproducible by carrying out the following:  
* Fork and/or clone this repository  
* Download [UKDS data](http://reshare.ukdataservice.ac.uk/853334/) and extract into `~/HWCanalysis/powerData/`  
* Define paths to where to store any generated plots ("pFolder") in `filepaths.R` (this appears to be unavoidable as LaTeX does not like the "~" notation)  
* Delete households `rf_15` and `rf_17` as per GREENGrid github instructions 
* Run `INITIALISE.R` to execute necessary scripts in the correct order  
 
To view the completed thesis, knit from `~/HWCanalysis/report/index.Rmd`.

Scripts `extractCircuitFromCleanGridSpy.R` and `imputeTotalPower.R` are taken from https://github.com/CfSOtago/GREENGridData. 
Their original authors are Ben Anderson and Jason Mair (respectively), and they are released under GNU General Public License v3.0.
Very minimal changes have been made to them in order to accommodate the change in directory and automate some steps necessary for this analysis.

Some of the scripts (especially those named after individual models) were used as exploratory tools, and have been superceded by their counterparts within `modelling.R`. The originals remain in case further exploration of individual models is required.
