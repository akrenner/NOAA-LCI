#! /usr/bin/env Rscript

## Execute all Kachemak Bay/Cook Inlet scripts, 2020
## If this is a new installation, it may be necessary to disconnect from VPN
## to avoid timeouts. To run up-to-date analysis, connect to VPN in order to
## download latest SWMP data from CDMO. Expect approximately 3 hours for a full
## run (2023-04 on Latitude 5420; 11th G Intel Core i7 1185G7 @3.0 GHz/1.8 GHz).


# add to time series (signature data)
# freshwater of first 30 m / bottom
# salinity at T9-6 at 50 m (seasonal influence of ACC?)
# boyyancy profile over time
# max buoyancy over time
# position of max buoyancy over time


rm (list = ls())

if (.Platform$OS.type=="windows"){
  setwd ("~/myDocs/amyfiles/NOAA-LCI/")
  # set environment variable to avoid "no such file or directory errors"
  Sys.setenv(TMPDIR = "C:\tmp")
}else{ ## Linux or macOS platform
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}

sT <- Sys.time()
cat ("\nStarting runAll.R at: ", as.character (Sys.time()), "\n")
## as of 2023-03-23 expect about 1:25 hours for CTD processing

## for SOB report -- quarterly vs annual -- clarify XXX
pastYear <- FALSE  # plot currentYear-1 ?
ongoingY <- TRUE   # for quarterly update




if(!file.exists(".initialized.rds")){
  source("InitialSetup.R")
  saveRDS(Sys.Date(), file=".initialized.rds")
}
hd <- getwd()
setwd("~/GISdata/LCI/")
system("git pull")
setwd(hd); rm (hd)


if(0) {
  ## to update packages: 0-- trouble on MacOS?
  # require(usethis) ## for github rate limits
  usethis::create_github_token()
  gitcreds::gitcreds_set()
  # usethis::edit_r_environ()

  ## set up AI helper claudeR
  # if(!require("claudeR")) {
  #   # install.packages("devtools")
  #   devtools::install_github("yrvelez/claudeR")
  #   require("claudeR")
  # }
  # Sys.setenv(ANTHROPIC_API_KEY = "MYAPI KEY")  ## consider at $5 to start


  ## troubleshoot dependencies used in the past:
  badP <- c("rgdal", "rgeos", "maptools", "rnoaa", "rtide", "SDraw")
  badP <- c("lubridate", "tidyr", "gsw", "openssl", "parallel")
  deps <- renv::dependencies()
  for(i in seq_along(badP)) {
    cat("\n\n##", badP [i], "##\n")
    print(deps [which(deps$Package == badP[i]), 1])
  }
  rm(badP, deps)

  # renv::update(exclude = c("oce")) ## rerun for all/specific packages to update
  # renv::install("~/src/oce_1.7-10.tar.gz")
  renv::update()
  renv::clean()
  renv::snapshot()
  renv::status()
}



if(1) {
  ## run the first script interactively! :
  # source("I-ctd_uneditedHexFiles.R")

  ## hex conversion and QAQC plots
  sink(file = "ctdprocessing.log", append = FALSE, split = FALSE)
  cat("Started CTD hex conversion at", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  source("FieldNotesDB.R") # first because it doesn't depend on anything else
  source("ctd_workflow.R")              ## approx. 1:30 hours
  source("CTD_castQAQC.R")              ## CTD profiles keep QAQC separate from error correction
  cat("Finished CTD hex conversion and processing at: ", as.character(Sys.time()), "\n")
  sink()
}


sink(file = "runAll.log", append = FALSE)
## pull together CTD and biological data.
## Also pull in external GIS data and produce data summaries
source("datasetup.R")


## plot of seasonal-yearly matrix when samples were taken
source("CTD_DataAvailability.R")


## the Wall


## move CTDwall-setup.R forward, use some output in CTD_timeseries.R ?
## use CTDwall_normals.R in CTD_timeseries.R ?

source("CTDwall-setup.R")
source("CTDwall_normals.R")
source("CTD_anomaly-helpers.R")
source("CTD_timeseries.R")   # sections and univariate summaries over time and anomalies. -- Signature Datasets
indivPlots <- FALSE; source("CTDsections.R", local = TRUE)
indivPlots <- TRUE;  source("CTDsections.R", local = TRUE); rm(indivPlots)
source("CTDwall.R")
# source("CTDwall-reportFigure.R")  ## not working, error when calling polygon (plot not called yet) -- XX fix later
# source("CTD_climatologies.R")  # sections over time, formerly "ctd_T9-anomaly.R" -- also see Jim's

## only for SoB? -- mv down?
## source("SeldoviaTemp.R") ## -- already called by AnnualStateofTheBay.R



sink(file = "StateOfBay-run.log", append = FALSE, split = FALSE)
## State of the Bay Report
source("AnnualStateOfTheBay.R")
sink()



## 2017 contract
if(0) { ## 2017 contract
  ## BUGS:
  ## bathymetry is read in as a raster. This creates a non-portable reference to the original file.
  ## Breaking this reference would be desirable, although it would increase file size of all temporary
  ## cache files.

  ## clear tmp directory first? CAREFUL with this!
  # rm -r ~/tmp/LCI_noaa/

  # source("metaExtraction.R")
  source("anaCTD.R")
  source("ecoAn.R")
  source("plotMaps.R")
  source("commMap.R")

}

if (0){ ## one-off projects
  source ("archive/CTDwall-reportFigure.R")
  source ("archive/OA-temps.R")
}

## 2019 seasonality
if(0) { # Dec 2019 seasonality
  source("zoopCommunity.R")
  source("phytopCommunity.R")

  ## missing parts for EVOS 2023 final report
  source("Nutrients_seasonality.R")

  source("physOcean.R")
  q()
  source("consensusTree.R")
}

## one-offs
if(0) {
  source("Currents/bathymetry-merge.R")
  source("Currents/ciofs_maxCurrent.r")
  source("Currents/drifter.R")
  source("Currents/plotDrifter.R")

  source("CTD_timeseries_freshwater.R")
}



## update metadata
source("metaDataCompilation.R")


## push to GoogleDrive
## requires rclone
## move aggregated CTD files to GISdata/LCI/ and WorkSpace manually
if(length(grep("[M|m]artin", getwd())) > 0) {
  ## sync all data to GoogleDrive -- better with GoogleDriveDesktop now?
  source("CTDsyncGDwall.R")
  ## send email that run is completed
  source("CTD_finishnotification.R")
}

cat("Finished runAll.R at ", as.character(Sys.time()), "\n\n")
sink()
cat("Finished runAll.R at ", as.character(Sys.time()), "\n\n")
write(as.character(Sys.time()), file = "finish_runAll.log", append = TRUE)

## EOF
