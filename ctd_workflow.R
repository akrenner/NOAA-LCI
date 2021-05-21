##

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


source ("CTD_file_management.R") ## QCQA, match hex with con file
source ("CTD_hexconversion.R")   ## call SEABIRD to do hex to cnv conversion
source ("CTD_cnv-Import.R")
source ("CTD_cleanup.R")         ## move error corrections and QAQC into here. Also aggregate-output
source ("datasetup.R")
source ("anaCTD.R")              ## CTD profiles?
source ("CTDwall.R")
source ("ctd_T9-anomaly.R")


## replot The Wall
## produce 2019 aggregate file (and others as well)
