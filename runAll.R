#! /usr/bin/env Rscript

## execute all Kachemak Bay/Cook Inlet scripts, 2020
## if this is a new installation, it may be necessary to disconnect from VPN
## to avoid timeouts
## to run up-to-date analysis, connect to VPN in order to download latest data from SWMP

if (.Platform$OS.type=="windows"){
  setwd ("~/myDocs/amyfiles/NOAA-LCI/")
  # set environment variable to avoid "no such file or directory errors"
  Sys.setenv(TMPDIR = "C:\tmp")
}else{
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}
rm (list = ls())
sT <- Sys.time()
cat ("\nStarting runAll.R at: ", as.character (Sys.time()), "\n")
## as of 2023-03-23 expect about 1:25 hours for CTD processing

## for SOB report -- quarterly vs annual -- clarify XXX
pastYear <- FALSE  # plot currentYear-1 ?
ongoingY <- TRUE   # for quarterly update



# setRepositories(graphics=FALSE, ind=76)
# setRepositories(addURLs=c (CRAN="https://archive.linux.duke.edu/cran/"))
# chooseCRANmirror(graphics=FALSE, ind=76)
sink (file="runAll.log", append=FALSE, split=FALSE)


source ("FieldNotesDB.R") # first because it doesn't depend on anything else
if (1){
  ## hex conversion and QAQC plots
  source ("ctd_workflow.R")
  source ("CTD_castQAQC.R")              ## CTD profiles keep QAQC separate from error correction
}


source ("SeldoviaTemp.R")

## pull together CTD and biological data.
## Also pull in external GIS data and produce data summaries
source ("datasetup.R")
## separate out CTD-specific stuff??
#; bathymetry
#; coastline
#; CTD data
## plot of seasonal-yearly matrix when samples were taken
source ("CTD_DataAvailability.R")
graphics.off()
cat ("\n## \n## finished with basic CTD processing\n##\n##\n")

sink ("wallpaper.log", append=FALSE, split=FALSE)
## the Wall
source ("CTDwall-setup.R")
source ("CTDwall_normals.R")
source ("CTDwall.R")


# source ("CTD_climatologies.R")  # sections over time, formerly "ctd_T9-anomaly.R" -- also see Jim's
source ("CTD_timeseries.R")   # sections and univariate summaries over time and anomalies.



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

if (0){ ## one-off projects
  source ("archive/CTDwall-reportFigure.R")
  source ("archive/OA-temps.R")
}

if (0){ # Dec 2019 seasonality
  source ("dataSetup.R")
  source ("zoopCommunity.R")
  source ("phytopCommunity.R")

  ## missing parts for EVOS 2023 final report
  source ("Nutrients_seasonality.R")

  source ("physOcean.R")
  source ("consensusTree.R")
}
sink()


sink (file="StateOfBay.log", append=FALSE, split=FALSE)

## set up required work environment and external files/data
source ("EnvironmentSetup.R")

## State of the Bay Report
try (source ("AnnualStateOfTheBay.R"))

## push to GoogleDrive
## requires rclone
## move aggregated CTD files to GISdata/LCI/ and WorkSpace manually
source ("CTDsyncGDwall.R")
## send email that run is completed
# source ("CTD_finishnotification.R")  # not yet ready or configured
cat ("all done\n")
print (Sys.time())
sink()


cat ("finished runAll.R at", Sys.time(), "\n")
cat ("Time taken for runAll.R:", difftime(Sys.time(), sT, units = NULL)) ## not going to work here because of saved dumps

## EOF


