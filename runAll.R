#! /usr/bin/env Rscript

## execute all Kachemak Bay/Cook Inlet scripts, 2020
## if this is a new installation, it may be necessary to disconnect from VPN
## to avoid timeouts
## to run up-to-date analysis, connect to VPN in order to download latest data from SWMP
## expect approximately 3 hours for a full run
## (2023-04 on Latitude 5420; 11th Gen Intel Core i7 1185G7 @3.0 GHz/1.8 GHz)


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
  sink (file = "ctdprocessinglog.log", append=FALSE, split = FALSE) # show output and write to file
  source ("ctd_workflow.R")              ## approx. 1:30 hours
  source ("CTD_castQAQC.R")              ## CTD profiles keep QAQC separate from error correction
  sink()
}




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
## only for SoB? -- mv down?
source ("SeldoviaTemp.R")
## the Wall
source ("CTDwall-setup.R")
source ("CTDwall_normals.R")
source ("CTDwall.R")
# source ("CTDwall-reportFigure.R")  ## not working, error when calling polygon (plot not called yet) -- XX fix later

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
## It may be necessary to restart R between above CTD processing and below Annual State of the Bay
## scripts? There may be an issue with temp files?

## set up required work environment and external files/data
source ("EnvironmentSetup.R")

## State of the Bay Report
try (source ("AnnualStateOfTheBay.R"))
sink()

## push to GoogleDrive
## requires rclone
## move aggregated CTD files to GISdata/LCI/ and WorkSpace manually
cat ("all done\n")
print (Sys.time())
if (grep ("[M|m]artin", getwd())){
  source ("CTDsyncGDwall.R")
  ## send email that run is completed
  source ("CTD_finishnotification.R")
}

cat ("Finished runAll.R at ", as.character (Sys.time()), "\n\n")


sink()
cat ("finished runAll.R at", Sys.time(), "\n")
# cat ("Time taken for runAll.R:", difftime(Sys.time(), sT, units = NULL)) ## not going to work here because of saved dumps

## EOF


