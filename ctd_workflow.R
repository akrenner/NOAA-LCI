##
## missing features: consider library (renv) -- see https://rstudio.github.io/renv/articles/renv.html


rm (list = ls())

print (Sys.time())
# CTD processing
if (length (grep ("martin", tolower (getwd()))) > 0){
  if (.Platform$OS.type != "windows"){
    setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
  }else{
    setwd ("~/myDocs/amyfiles/NOAA-LCI/")
  }
}else{
  # setwd("~/GISdata/LCI/CTDprocessing/")
  setwd("~/myDocs/R-scripts/NOAA-LCI")
}

# sink (file = "~/tmp/LCI_noaa/cache/ctdprocessinglog.txt", split = TRUE) # show output and write to file
sink (file = "ctdprocessinglog.txt", split = TRUE) # show output and write to file

## load missing packages
if (!require("pacman")) install.packages("pacman"
                                         , repos = "http://cran.fhcrc.org/", dependencies = TRUE)
Require <- pacman::p_load

## set-up renv
## do all renv work manually to avoid clash between base::load() and renv::load()
# Require ("renv")  ## do NOT load this here in script!
# renv::init()

## pre-load widely needed packages. Do this here for new users.
Require ("tools")
Require ("dplyr")
Require ("oce")
Require ("openssl")
Require ("parallel")
Require ("lubridate")
Require ("geosphere")
Require ("zip")
# Require ("LakeMetabolizer")

hexFileD <- "~/GISdata/LCI/CTD-processing/WorkspaceTest/"
hexFileD <- "~/GISdata/LCI/CTD-processing/Workspace/"


source ("CTD_file_management.R") ## QCQA, match hex with con file -- some time error checking: name and meta
source ("CTD_hexconversion.R")   ## call SEABIRD to do hex to cnv conversion
source ("CTD_cnv-Import.R")      ## still has QAQC in here; runs for 17 min
source ("CTD_cleanup.R")         ## move error corrections into here. Produce aggregate CTD file (data product)



## pull together CTD and biological data.
## Also pull in external GIS data and produce data summaries
source ("datasetup.R")
## separate out CTD-specific stuff??
#; bathymetry
#; coastline
#; CTD data


## plot of seasonal-yearly matrix when samples were taken
source ("CTD_DataAvailability.R")
# source ("CTD_castQAQC.R")              ## CTD profiles keep QAQC separate from error correction


## the Wall
source ("CTDwall-setup.R")
source ("CTDsections.R")
source ("CTDwall.R")


# source ("CTD_climatologies.R")  # sections over time, formerly "ctd_T9-anomaly.R" -- also see Jim's
source ("CTD_signatureData.R")

## push to GoogleDrive
## requires rclone
## move aggregated CTD files to GISdata/LCI/ and WorkSpace manually
source ("CTDsyncGDwall.R")  # sync to GoogleDrive -- requires rclone

## send email that run is completed
source ("CTD_finishnotification.R")


## save snapshot of current package versions
# renv::snapshot()


sink() # end console logging
cat ("open log file to examine output")
print (Sys.time())
#EOF
