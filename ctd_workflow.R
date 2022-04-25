##

rm (list = ls())

print (Sys.time())
# CTD processing
if (length (grep ("Martin", getwd())) > 0){
   setwd("~/myDocs/amyfiles/NOAA-LCI/")
}else{
   # setwd("~/GISdata/LCI/CTDprocessing/")
   setwd("~/myDocs/R-scripts/NOAA-LCI")
}


## load missing packages
if (!require("pacman")) install.packages("pacman"
                                         , repos = "http://cran.fhcrc.org/", dependencies = TRUE)
Require <- pacman::p_load
Require ("tools")
Require ("dplyr")
Require ("oce")
Require ("openssl")
Require ("parallel")
Require ("lubridate")
Require ("geosphere")
Require ("zip")
Require ("LakeMetabolizer")

hexFileD <- "~/GISdata/LCI/CTD-processing/WorkspaceTest/"
hexFileD <- "~/GISdata/LCI/CTD-processing/Workspace/"

source ("CTD_file_management.R") ## QCQA, match hex with con file -- some time error checking: name and meta
cat ("\n# END CTD_file_management.R #\n")
source ("CTD_hexconversion.R")   ## call SEABIRD to do hex to cnv conversion
cat ("\n# END CTD_hexconversion.R #\n")
source ("CTD_cnv-Import.R")      ## still has QAQC in here; runs for 17 min
cat ("\n# END CTD_cnv-Import.R #\n")
source ("CTD_cleanup.R")         ## move error corrections and QAQC into here. Also aggregate-output
cat ("\n# END CTD_cleanup.R #\n")



source ("datasetup.R")
cat ("\n# END datasetup.R #\n")

## anaCTD.R needs updates/work -- not currently working -- multiplatform issue?
# source ("anaCTD.R")              ## CTD profiles?
# cat ("\n# END anaCTD.R #\n")
cat ("\n# SKIPPED anaCTD.R #\n")


## the Wall
source ("CTDwall-setup.R")
source ("CTDsections.R")
# source ("CTD-testSectionPlot.R")  # look for plots
source ("CTDwall.R")
cat ("\n# END CTDwall.R #\n", Sys.time())

# source ("ctd_T9-anomaly.R")

## replot The Wall
## produce 2019 aggregate file (and others as well)

print (Sys.time())
#EOF
