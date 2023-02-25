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

source ("FieldNotesDB.R") # first because it doesn't depend on anything else
source ("SeldoviaTemp.R")
if (1){
  ## The Wall
  source ("ctd_workflow.R")  ## should split ctd_workflow up into processing and wall
  # this also calls dataSetup.R -- could cause problems this early??
  # CTDwall, CTD-timeseries -- no need to call these here!
}else{
  source ("dataSetup.R")
  source ("CTD_timeseries.R")
  source ("CTDwall-setup.R")
  source ("CTDwall.R")
  source ("CTDwall-reportFigure.R")
}


if (0){ ## 2017 contract
  ## BUGS:
  ## bathymetry is read in as a raster. This creates a non-portable reference to the original file.
  ## Breaking this reference would be desirable, although it would increase file size of all temporary
  ## cache files.

  ## clear tmp directory first? CAREFUL with this!
  # rm -r ~/tmp/LCI_noaa/

  # source ("metaExtraction.R")
  source ("anaCTD.R")
  source ("ecoAn.R")
  source ("plotMaps.R")
  source ("commMap.R")

}


if (0){ # Dec 2019 seasonality
  source ("dataSetup.R")
  source ("zoopCommunity.R")
  source ("phytopCommunity.R")

  ## missing parts for EVOS 2023 final report
  source ("Nutrients_seasonality.R")


  source ("physOcean.R")
  q()
  source ("consensusTree.R")
}


## set up required work environment and external files/data
source ("EnvironmentSetup.R")

## State of the Bay Report
source ("AnnualStateOfTheBay.R")

source ("CTDsyncGDwall.R")

cat ("all done\n")
print (Sys.time())
print (difftime(Sys.time(), sT, units = NULL)) ## not going to work here because of saved dumps
sink()

## EOF


