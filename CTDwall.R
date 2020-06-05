## replot CTD wallpaper for office
## provide line-graph alternatives


## load data
## start with file from dataSetup.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")  # contains physOc -- raw CTD profiles


# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") ## this contains poSS -- CTD summaries
## link physOc and stn
## should be poSS and stn -- check!

## set-up plot and paper size


## plot CTD-profiles along transect
require ("oce")
physOc$transDate <- factor (with (physOc, paste (Date, Transect, sep = "-")))

for (i in length (levels (physOc$transDate))){
# xC <- subset (physOc, stn$Line == )

}



## plot CTD-profiles of station over time



# EOF