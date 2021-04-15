#! /usr/bin/env Rscript

## execute all Kachemak Bay/Cook Inlet scripts, 2020
setwd ("~/myDocs/amyfiles/NOAA-LCI/")
rm (list = ls())

sT <- Sys.time()


if (0){ ## 2017 contract
  ## BUGS:
  ## bathymetry is read in as a raster. This creates a non-portable reference to the original file.
  ## Breaking this reference would be desirable, although it would increase file size of all temporary
  ## cache files.

  ## clear tmp directory first? CAREFUL with this!
  # rm -r ~/tmp/LCI_noaa/

  # source ("metaExtraction.R")
  source ("SeldoviaTemp.R")
  source ("dataSetup.R")
  source ("anaCTD.R")
  source ("ecoAn.R")
  source ("plotMaps.R")
  source ("commMap.R")

  source ("CTDwall.R")
}


if (0){ # Dec 2019 seasonality
  source ("dataSetup.R")
  source ("zoopCommunity.R")
  source ("phytopCommunity.R")

  source ("physOcean.R")
  q()
  source ("consensusTree.R")
}


## State of the Bay Report 2019

## plot SWMP weather data for annual state of the bay report
source ("SeldoviaTemp.R")
source ("annual-wind.R")

# source ("precipSalinity.R")  # calls the scripts below and makes a combined multi-panel PDF
source ("annual-rainy.R")
source ("annual-salinity.R")
source ("annual-airTemp.R")

source ("annual-snowpack.R")
source ("annual-stratification.R")
source ("annual-waves.R")  ## pull-out waves and surf?


cat ("all done\n")
print (Sys.time())
print (difftime(Sys.time(), sT, units = NULL)) ## not going to work here because of saved dumps

## EOF

