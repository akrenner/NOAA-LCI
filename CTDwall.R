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

## find ctd data and arrange them into sections


   # "section" -- that's what I want there
    define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html
    plot (ctdSection)

    oceColorsTemperature(n)
    oce.colorsSalinity
    oce.colorsPAR

}



## plot CTD-profiles of station over time



# EOF
