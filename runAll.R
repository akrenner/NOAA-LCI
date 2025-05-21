#! /usr/bin/env Rscript

## execute all Kachemak Bay/Cook Inlet scripts, 2020
## if this is a new installation, it may be necessary to disconnect from VPN
## to avoid timeouts
## to run up-to-date analysis, connect to VPN in order to download latest data from SWMP
## expect approximately 3 hours for a full run
## (2023-04 on Latitude 5420; 11th Gen Intel Core i7 1185G7 @3.0 GHz/1.8 GHz)

rm (list = ls())
sT <- Sys.time()

# pastYear <- FALSE  # plot currentYear-1 ?
# ongoingY <- TRUE

if (.Platform$OS.type=="windows"){
  setwd ("~/myDocs/amyfiles/NOAA-LCI/")
}else{ ## Linux or macOS platform
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}


if (!require ("oce")){
  source ("InitialSetup.R")
}


if (0){
  ## to update packages: 0-- trouble on MacOS?
  require (usethis) ## for github rate limits
  usethis::create_github_token()
  gitcreds::gitcreds_set()
  # usethis::edit_r_environ()


  ## troubleshoot dependencies:
  # x <- renv::status()
  # names (x$library$Packages) [which (!names (x$library$Packages) %in% names (x$lockfile$Packages))]
  # names (x$lockfile$Packages) [which (!names (x$library$Packages) %in% names (x$lockfile$Packages))]
  badP <- c("maptools", "rgdal", "rgeos", "rnoaa", "rtide", "SDraw", "GVI")
  deps <- renv::dependencies()
  for (i in 1:length (badP)){
    cat ("\n\n##", badP [i], "##\n")
    print (deps [which (deps$Package==badP[i]),1])
  }
  rm (badP, deps)

  renv::update(exclude=c("oce")) ## rerun for all/specific packages to update
  # renv::install ("~/src/oce_1.7-10.tar.gz")
  renv::clean()
  renv::snapshot()
  renv::status()
}



if (1){
  ## run the first script interactively! :
  # source ("I-ctd_uneditedHexFiles.R")

  ## hex conversion and QAQC plots
  sink (file = "ctdprocessinglog.txt", append=FALSE, split = FALSE) # show output and write to file
  cat ("Started CTD hex conversion and processing at: ", Sys.time(), "\n")
  source ("FieldNotesDB.R") # first because it doesn't depend on anything else
  source ("ctd_workflow.R")              ## approx. 1:30 hours
  source ("CTD_castQAQC.R")              ## CTD profiles keep QAQC separate from error correction
  cat ("Finished CTD hex conversion and processing at: ", as.character (Sys.time()), "\n")
}
sink()


sink (file="StateOfBay-runlog.txt", append=FALSE, split=FALSE)
sink()
## pull together CTD and biological data.
## Also pull in external GIS data and produce data summaries
source ("datasetup.R")
## separate out CTD-specific stuff??
#; bathymetry
#; coastline
#; CTD data

## set up required work environment and external files/data (bathymetry)
source ("EnvironmentSetup.R")

## plot of seasonal-yearly matrix when samples were taken
source ("CTD_DataAvailability.R")


## the Wall
source ("CTD_timeseries.R")   # sections and univariate summaries over time and anomalies. -- Signature Datasets
source ("CTDwall-setup.R")
indivPlots <- FALSE; source ("CTDsections.R", local=TRUE)
indivPlots <- TRUE; source ("CTDsections.R", local=TRUE); rm (indivPlots)
source ("CTDwall_normals.R")
source ("CTDwall.R")
# source ("CTDwall-reportFigure.R")  ## not working, error when calling polygon (plot not called yet) -- XX fix later
# source ("CTD_climatologies.R")  # sections over time, formerly "ctd_T9-anomaly.R" -- also see Jim's


## 2017 contract
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


## 2019 seasonality
if (0){ # Dec 2019 seasonality
  source ("zoopCommunity.R")
  source ("phytopCommunity.R")

  ## missing parts for EVOS 2023 final report
  source ("Nutrients_seasonality.R")

  source ("physOcean.R")
  q()
  source ("consensusTree.R")
}



## only for SoB? -- mv down?
## source ("SeldoviaTemp.R") ## -- already called by AnnualStateofTheBay.R

## State of the Bay Report
source ("AnnualStateOfTheBay.R")


## how to execute report?
# source ("MonthlyUpdates/MonthlyTemplate.qmd")


## one-offs
if (0){
  source ("Currents/bathymetry-merge.R")
  source ("Currents/ciofs_maxCurrent.r")
  source ("Currents/drifter.R")
  source ("Currents/plotDrifter.R")
}



## update metadata
source ("metaDataCompilation.R")


## push to GoogleDrive
## requires rclone
## move aggregated CTD files to GISdata/LCI/ and WorkSpace manually
if (length (grep ("[M|m]artin", getwd()))>0){
  source ("CTDsyncGDwall.R")
  ## send email that run is completed
  source ("CTD_finishnotification.R")
}

cat ("Finished runAll.R at ", as.character (Sys.time()), "\n\n")
sink()
cat ("Finished runAll.R at ", as.character (Sys.time()), "\n\n")

## EOF
