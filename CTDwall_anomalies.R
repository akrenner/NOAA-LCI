#!/usr/bin/env Rscript

## CTD anomaly
## for each station location, calculate averages for all
## oceanographic parameters

rm (list=ls())
load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R


## calculate monthly and quarterly means
##
## output: as poAll, but with month as factor
poAll$month <- factor (format (poAll$isoTime, "%m"))
oM <- as.matrix (poAll [,which (names (poAll) == "Pressure..Strain.Gauge..db.")
             : which (names (poAll) == "bvf")])

##
#monthly mean
poNorm <- aggregate (oM~Match_Name+month, data=poAll, FUN=mean, na.rm=TRUE)
rm (oM)



## quarterly means
# seasonB <- c(0,3,5,7,11)


save.image ("~/tmp/LCI_noaa/cache/ctdwallAnomalies.RData")   # from CTDwallSetup.R
