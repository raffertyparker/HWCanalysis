# HWCanalysis

Bookdown version of hot water electricity demand forecasting undertaken for my Masters thesis

The report should be fully reproducible by carrying out the following:
  Define paths to where to store any generated plots ("pFolder") in `filepaths.R` (this appears to be unavoidable as LaTeX does not like the "~" notation)
  Download UKDS data and extract into `~/HWCanalysis/powerData/`
  Run `INITIALISE.R` to execute necessary scripts in the correct order
  Delete households `rf_15` and `rf_17` as per GREENGrid github instructions
  To view the completed thesis, knit from `~/HWCanalysis/report/index.Rmd`

Scripts `extractCircuitFromCleanGridSpy.R` and `imputeTotalPower.R` are taken from https://github.com/CfSOtago/GREENGridData
Their original authors are Ben Anderson and Jason Mair (respectively)
 
