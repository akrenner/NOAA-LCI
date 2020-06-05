## replot CTD wallpaper for office
## provide line-graph alternatives


## load data
## start with file from dataSetup.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
## link physOc and stn
## should be poSS and stn -- check!

## set-up plot and paper size


## plot CTD-profiles along transect
require ("oce")
#for (i in length (levels (stn$Line))){
# xC <- subset (physOc, stn$Line == )
#}



## plot CTD-profiles of station over time



# EOF