#! /usr/bin/env Rscript

## execute all Kachemak Bay/Cook Inlet scripts, 2020
## if this is a new installation, it may be necessary to disconnect from VPN
## to avoid timeouts
## to run up-to-date analysis, connect to VPN in order to download latest data from SWMP

if (.Platform$OS.type=="windows"){
  setwd ("~/myDocs/amyfiles/NOAA-LCI/")
}else{
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}
rm (list = ls())
sink (file="StateOfBay-runlog.txt", append=FALSE)
sT <- Sys.time()



pastYear <- FALSE  # plot currentYear-1 ?
ongoingY <- TRUE



# setRepositories(graphics=FALSE, ind=76)
# setRepositories(addURLs=c (CRAN="https://archive.linux.duke.edu/cran/"))
# chooseCRANmirror(graphics=FALSE, ind=76)

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


## set up required work environment
## gebco, gshhg, zimmerman bathymetry, CTD hex files, etc.
## source ("workEnvironment.R")

if (0){
## The Wall
source ("ctd_workflow.R")  ## should split ctd_workflow up into processing and wall
}

## State of the Bay Report

## plot SWMP weather data for annual state of the bay report
source ("SeldoviaTemp.R")
source ("annual-wind.R")

# source ("precipSalinity.R")  # calls the scripts below and makes a combined multi-panel PDF
source ("annual-rainy.R")
# source ("annual-salinity.R")
source ("annual-waterTempSal.R")
source ("annual-airTemp.R")

source ("annual-waves.R")  ## pull-out waves and surf?
source ("annual-snowpack.R")
# source ("annual-stratification.R")   ## projects/bottomTempTS ?

source ("CTDsyncGDwall.R")


cat ("all done\n")
print (Sys.time())
print (difftime(Sys.time(), sT, units = NULL)) ## not going to work here because of saved dumps
sink()

## EOF


