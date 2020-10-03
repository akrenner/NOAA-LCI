##

# CTD processing

source ("CTD_file_management.R")  ## QCQA, match hex with con file
source ("ctdhexconversion.R")     ## call SEABIRD to do hex to cnv conversion
source ("CTD_cnv-Import.R")       ## yes, all that... QCQA
source ("datasetup.R")
source ("anaCTD.R")
source ("CTDwall.R")
