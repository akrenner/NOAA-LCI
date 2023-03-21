#!/usr/bin/env Rscript

## missing features:
## - consider library (renv) -- see https://rstudio.github.io/renv/articles/renv.html
## - cross-platform hex-file processing

rm (list = ls())

print (Sys.time())
# CTD processing
if (length (grep ("martin", tolower (getwd()))) > 0){
  # MR personal setup
  if (.Platform$OS.type != "windows"){
    setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
  }else{
    setwd ("~/myDocs/amyfiles/NOAA-LCI/")
  }
}else{
  # Generic setup
  setwd("~/myDocs/R-scripts/NOAA-LCI")
}
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
cat ("## Finished hex conversion of CTD files\n\n")
source ("CTD_cnv-Import.R")      ## still has QAQC in here; runs for 17 min
cat ("## Finished CNV import of CTD files\n\n")
source ("CTD_cleanup.R")         ## move error corrections into here. Produce aggregate CTD file (data product)
cat ("## Finished CTD_cleanup.R\n\n")

## save snapshot of current package versions
# renv::snapshot()

sink() # end console logging
cat ("open log file to examine output")
print (Sys.time())
#EOF
