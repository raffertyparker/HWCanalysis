# This is where we define paths to appropriate folders

# Determine which computer is being used
sysname <- Sys.info()[[1]]
login <- Sys.info()[[6]]
user <- Sys.info()[[7]]

if (sysname == "Windows" & login == 'ParkerR') { # Windows machine
  # path to scripts folder
  sFolder <- "C:/Users/ParkerR/HWCanalysis/scripts/"
  # path to data folder
  dFolder <- "C:/Users/ParkerR/HWCanalysis/data/" 
  # path to plot folder
  pFolder <- "C:/Users/ParkerR/HWCanalysis/plots/"
} else { # Linux machine
  sFolder <- "~/HWCanalysis/scripts/"
  dFolder <- "~/HWCanalysis/data/"
  pFolder <- "/home/parra358/HWCanalysis/plots/" # (laTeX did not like '~' notation)
}