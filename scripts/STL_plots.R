###################################
# This script creates the STL plots
###################################

if (!exists("dFolder")){
  dFolder <- "~/HWCanalysis/data/" 
}
if (!exists("pFolder")){
  pFolder <- "~/HWCanalysis/plots/" 
}
if (!exists("sFolder")){
  sFolder <- "~/HWCanalysis/scripts/" 
}

library(forecast)
library(xts)
library(data.table)
library(dplyr)
#library(tseries)

load(paste0(dFolder, "houses.Rda"))

for (house in houses){
  #   house <- houses[1]  
  dt_val <- as.data.table(readr::read_csv(paste0(dFolder,"households/validating/",house,
                                                 "_at_30_min_for_validating.csv")))
  dt_val$hHour <- lubridate::as_datetime(dt_val$hHour, tz = 'Pacific/Auckland')
  dt_val$dHour <- hour(dt_val$hHour) + minute(dt_val$hHour)/60
  dt_val$nonHWshift1 <- shift(dt_val$nonHWelec)
  dt_val$nonHWshift2 <- shift(dt_val$nonHWelec, 2)
  dt_val$HWshift1 <- shift(dt_val$HWelec)
  dt_val$HWshift2 <- shift(dt_val$HWelec, 2)
  dt_val$decDate <- decimal_date(dt_val$hHour)
  ts.HW <- ts(dt_val$HWelec, frequency = 48)
  #ts.HW <- ts(dt$HWelec, frequency = 48*7)
  decomp.HW <- decompose(ts.HW)
  t <- decomp.HW
  strt <- 48 # Start 1 day in
  len <- strt + 48*7*1 # Adjust according to number of weeks
  
  t$x <- as.ts(t$x[strt:len])
  t$seasonal <- as.ts(t$seasonal[strt:len])
  t$trend <- as.ts(t$trend[strt:len])
  t$random <- as.ts(t$random[strt:len])
  plot(t, xlab ="Time (half hours)")
  dev.print(pdf, paste0(pFolder, house, "_STL.pdf"))
}