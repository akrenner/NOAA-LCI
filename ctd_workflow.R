##

# CTD processing
if (length (grep ("Martin", getwd())) > 0){
   setwd("~/myDocs/amyfiles/NOAA-LCI/")
}else{
   setwd("~/GISdata/LCI/CTDprocessing/")
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


source ("CTD_file_management.R")   ## QCQA, match hex with con file
source ("CTD_hexconversion.R")     ## call SEABIRD to do hex to cnv conversion
source ("CTD_cnv-Import.R")        ## yes, all that... QCQA -- fix final step
# source ("datasetup.R")
# source ("anaCTD.R")
# source ("CTDwall.R")
