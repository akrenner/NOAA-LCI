##

# CTD processing
setwd("~/myDocs/amyfiles/NOAA-LCI/")
source ("CTD_file_management.R")  ## QCQA, match hex with con file
source ("CTD_hexconversion.R")     ## call SEABIRD to do hex to cnv conversion
source ("CTD_cnv-Import.R")       ## yes, all that... QCQA -- fix final step
# source ("datasetup.R")
# source ("anaCTD.R")
# source ("CTDwall.R")
