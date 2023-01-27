##
## plot time series of CTD data and derivitives -- signature data-sets
##
## Martin Renner
##
##
## also see CTD_climatologies.R for inspiration
##

rm (list=ls())
base::load ("~/tmp/LCI_noaa/cache/CTDcasts.RData") ## from dataSetup.R: stn, physOc, poSS


## section-plot of T9_6 over time -- get Jim's to work or CTD_climatologies.R or where it was

## 3-panel figure:
## buoyancy-profile over time (and climatology)
## line-graph of max-buoyancy over time
## line-graph of position of max-byoyancy over time?

# pdf ("~/tmp/LCI_noaa/media/CTDsections/signature_buoyancy.pdf", height=11,width=8.5)
# par (mfrow = c(3,1))
# dev.off()



## freshwater contents of first 30 m (strenght of the freshwater lens)

## salinity at T9_6 over time at 50 m -> seasonal influence of ACC

##