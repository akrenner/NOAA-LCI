#!/usr/bin/env Rscript

##
## missing features:
## - consider library (renv) -- see https://rstudio.github.io/renv/articles/renv.html
## - cross-platform hex-file processing
##

# rm (list = ls())
print (Sys.time())
# CTD processing
if (length (grep ("martin", tolower (getwd()))) > 0) {
  if (.Platform$OS.type != "windows") {
    setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
  } else {
    setwd ("~/myDocs/amyfiles/NOAA-LCI/")
  }
} else {
  # Generic setup
  setwd("~/myDocs/R-scripts/NOAA-LCI")
}
cat ("\n\nStarting ctdprocessing at", as.character (Sys.time()), "\n\n")

## load missing packages
# if (!require("pacman")) install.packages("pacman"
#                                         , repos = "http://cran.fhcrc.org/", dependencies = TRUE)
# Require <- pacman::p_load


# ## pre-load widely needed packages. Do this here for new users.
# require ("tools")
# require ("dplyr")
# require ("oce")
# require ("openssl")
# require ("parallel")
# require ("lubridate")
# require ("geosphere")
# require ("zip")
# require ("LakeMetabolizer")

hexFileD <- "~/GISdata/LCI/CTD-processing/WorkspaceTest/"
hexFileD <- "~/GISdata/LCI/CTD-processing/Workspace/"


source ("CTD_file_management.R") ## QCQA, match hex with con file -- some time error checking: name and meta
source ("CTD_hexconversion.R")   ## call SEABIRD to do hex to cnv conversion
cat ("## Finished hex conversion of CTD files\n\n")
source ("CTD_cnv-Import.R")      ## still has QAQC in here; runs for 17 min
cat ("## Finished CNV import of CTD files\n\n")
source ("CTD_cleanup.R")         ## move error corrections into here. Produce aggregate CTD file (data product)
cat ("## Finished CTD_cleanup.R at ", as.character (Sys.time()), "\n\n")
# source ("CTD_notesQAQC.R")       ## unfinished; need to retune. Merge into CTD_cleanup.R??
# cat ("## Finished CTD_notesQAQC.R\n\n")

cat ("## Finished ctd_workflow at ", as.character (Sys.time()), "\n")

# EOF
