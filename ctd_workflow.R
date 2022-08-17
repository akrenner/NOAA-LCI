##

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
source ("CTD_hexconversion.R")   ## call SEABIRD to do hex to cnv conversion
source ("CTD_cnv-Import.R")      ## still has QAQC in here; runs for 17 min
source ("CTD_cleanup.R")         ## move error corrections into here. Produce aggregate CTD file (data product)

## plot of seasonal-yearly matrix when samples were taken
source ("CTD_DataAvailability.R")


source ("datasetup.R")   ## crash -- ror in `$<-.data.frame`(`*tmp*`, SSS, value = c(28.5532333333333, 31.4265333333333,  :
# replacement has 3833 rows, data has 3834

# source ("CTD_castQAQC.R")              ## CTD profiles keep QAQC separate from error correction


## the Wall
source ("CTDwall-setup.R")
source ("CTDsections.R")
source ("CTDwall.R")
source ("CTDsyncGDwall.R")  # sync to GoogleDrive -- requires rclone

# source ("CTD_climatologies.R")  # was: source ("ctd_T9-anomaly.R") -- also see Jim's

sink() # end console logging
cat ("open log file to examine output")
print (Sys.time())
#EOF
