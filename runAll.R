#! /usr/bin/env Rscript

## To optain this and all other scripts and data for this project, paste the
## code between the braces into a R console:
if(0) {
  ## required software: R, RStudio, RTools, git, seabird data processing,
  ## set up folder for R SCRIPTS and pull scripts from github
  rFolder <- "~/myDocs/R-scripts" # or any other folder name you like
  dir.create(rFolder, recursive = TRUE)
  setwd(rFolder)
  system("git clone https://github.com/akrenner/NOAA-LCI.git")
  setwd(paste0(rFolder, "/NOAA-LCI"))
  source("runAll.R")  ## which will run this script and everything else.
  ## Be patient
}

## Execute all Kachemak Bay/Cook Inlet scripts. In a new installation, it's
## recommended to disconnect from VPN first, to avoid network timeouts. Expect
## over 3 hours for the initial run, which needs to download many external files
## (2023-04 on Latitude 5420; 11th G Intel Core i7 1185G7 @3.0 GHz/1.8 GHz).
## To update to the latest SWMP data, be sure to be connected to VPN NCCOS-West.

## If you want to make your own changes to any of these scripts, learn about
## git, and create a new fork.

## See the Recurrent_Oceanographic_Survey-Manual for more instructions.



rm(list = ls())

if(length(grep("/NOAA-LCI$", getwd())) < 1) {
  stop("Please open the R Project in NOAA-LCI/")
  if(.Platform$OS.type=="windows"){
    setwd ("~/myDocs/amyfiles/NOAA-LCI/")
    # set environment variable to avoid "no such file or directory errors"
    Sys.setenv(TMPDIR = "C:\tmp")
    system ("git pull --force")
  }else{ ## Linux or macOS platform
    setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
  }
}


sT <- Sys.time()
cat ("\nStarting runAll.R at: ", as.character (Sys.time()), "\n")
## as of 2023-03-23 expect about 1:25 hours for CTD processing

## for SOB report -- quarterly vs annual -- clarify XXX
pastYear <- FALSE  # plot currentYear-1 ?
ongoingY <- TRUE   # for quarterly update



## This will only run once, getting required data and packages
if(!file.exists(".initialized.rds")){
  #rJava issues? try
  # Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/temurin-26.jdk/Contents/Home/")

  source("InitialSetup.R")
  saveRDS(Sys.Date(), file=".initialized.rds")
}


#######################################################
## For collaborators -- get the latest updates       ##
## This will overwrite any changes you may have made ##
#######################################################

if(length(grep("[M|m]artin", getwd())) < 1) {
  ## for collaborators: pull latest versions from git and sync packages
  system("git pull --force")
  system ("git checkout main")
  if(!renv::restore()$synchronized) {
    renv::restore(prompt=FALSE, clean= TRUE)
  }
  hd <- getwd()
  setwd("~/GISdata/LCI/")  ## fetch latest CTD data
  system("git pull")
  setwd(hd); rm (hd)
}




###########################################
## Test dependencies and update packages ##
###########################################


if(0) {
  ## to update packages: 0-- trouble on MacOS?
  # require(usethis) ## for github rate limits
  usethis::create_github_token()
  gitcreds::gitcreds_set()
  # usethis::edit_r_environ()

  ## set up AI helper claudeR
  # if(!require("claudeR")) {
  #   # renv::install("yrvelez/claudeR")
  #   require("claudeR")
  # }
  # Sys.setenv(ANTHROPIC_API_KEY = "MYAPI KEY")  ## consider at $5 to start

  ## troubleshoot dependencies used in the past:
  badP <- c("rgdal", "rgeos", "maptools", "rnoaa", "rtide", "SDraw")
  badP <- c("GVI", "yaml", "randomForest", "stinepack")
  deps <- renv::dependencies()
  for(i in seq_along(badP)) {
    if(length(deps[which(deps$Package == badP[i]), 1]) > 0) {
      cat("\n\n##", badP [i], "##\n")
      print(deps [which(deps$Package == badP[i]), 1])
    } else {cat("No dependencies found for: ", badP[i], "\n")}
  }
  rm(badP, deps)

  ## temp until CRAN is updated
  if(packageVersion("worldmet") != '1.1.0.9000') {
    renv::install("openair-project/worldmet")
    # stop("Package worldmet needs a different verion. Try \n renv::restore('worldmet')")
  }

  # renv::update(exclude = c("oce")) ## rerun for all/specific packages to update
  # renv::install("~/src/oce_1.7-10.tar.gz")

  ## Delete left-over lock files if package installation is stuck
  # unlink(list.files(.libPaths(), pattern = "^00LOCK", full.names = TRUE), recursive = TRUE)

  renv::update()
  renv::clean()
  renv::snapshot()
  renv::status()
}





###########################
## Process CTD HEX files ##
###########################


if(.Platform$OS.type != "unix") {
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
} else {
  cat("Need to upate aggregated CTD files from ResearchWorkSpace or GoogleDrive\n")
}

sink(file = "runAll.log", append = FALSE)



########################################################
## Analyse and plot oceanographic and biological data ##
########################################################

## pull together CTD and biological data.
## Also pull in external GIS data and produce data summaries
source("datasetup.R")

## plot of seasonal-yearly matrix when samples were taken
source("CTD_DataAvailability.R")


## Plot The Wall

## move CTDwall-setup.R forward, use some output in CTD_timeseries.R ?
## use CTDwall_normals.R in CTD_timeseries.R ?

source("CTDwall-setup.R")
source("CTDwall_normals.R")  # climatologies
source("CTD_anomaly-helpers.R")
source("CTD_timeseries.R")   # sections and univariate summaries over time and anomalies. -- Signature Datasets
indivPlots <- FALSE; source("CTDsections.R", local = TRUE)
indivPlots <- TRUE;  source("CTDsections.R", local = TRUE); rm(indivPlots)
# quickPlot <- !as.numeric(format(Sys.time(), "%H")) %in% c(0:6,18:24)
if(as.numeric(format(Sys.time(), "%H")) %in% c(0:6, 18:24)) {
  quickPlot <- TRUE
} else {quickPlot <- TRUE}
source("CTDwall.R", local = TRUE); rm(quickPlot)
sink()



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

## one-offs -- drifters
if(0) {
  source("Currents/bathymetry-merge.R")
  source("Currents/ciofs_maxCurrent.r")
  source("Currents/drifter.R")
  source("Currents/plotDrifter.R")
}
if (0){ ## more one-off projects
  source ("archive/CTDwall-reportFigure.R")
  source ("archive/OA-temps.R")
}

source("CTD_timeseries_freshwater.R")



## update metadata
source("metaDataCompilation.R")



#############################################
## Push new plots to Martin's GoogleDrive ##
#############################################

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
write(as.character(Sys.time()), file = "runAll.log", append = TRUE)
cat("Finished runAll.R at ", as.character(Sys.time()), "\n\n")

## EOF
