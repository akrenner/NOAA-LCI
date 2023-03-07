#!/usr/bin/env Rscript

## CTD anomaly
## for each station location, calculate averages for all
## oceanographic parameters

rm (list=ls())
load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R


## copied from dataSetup.R (should have a package)
Seasonal <- function (month){           # now using breaks from zoop analysis -- sorry for the circularity
  month <- as.numeric (month)
  cut (month, breaks = c(-13,0,2,4,8,10, 25)
       , labels = c ("fall", "winter", "spring", "summer", "fall", "winter"))
}


## calculate monthly and quarterly means
##
## output: as poAll, but with month as factor
poAll$month <- factor (format (poAll$isoTime, "%m"))
oM <- as.matrix (poAll [,which (names (poAll) == "Pressure..Strain.Gauge..db.")
             : which (names (poAll) == "bvf")])

##
#monthly mean
poNorm <- aggregate (oM~Match_Name+month, data=poAll, FUN=mean, na.rm=TRUE)

## add stn data
sidx <- match (poNorm$Match_Name, stn$Match_Name)
poNorm$latitude_DD <- stn$Lat_decDegree [sidx]
poNorm$logitude_DD <- stn$Lon_decDegree [sidx]
rm (sidx)

## quarterly means
season <- Seasonal (poAll$month)
poSNorm <- aggregate (oM~Match_Name+season, data=poAll, FUN=mean, na.rm=TRUE)
sidx <- match (poSNorm$Match_Name, stn$Match_Name)
poSNorm$latitude_DD <- stn$Lat_decDegree [sidx]
poSNorm$logitude_DD <- stn$Lon_decDegree [sidx]
rm (sidx)
rm (oM)

save (poNorm, poSNorm, file="~/tmp/LCI_noaa/cache/ctdwallAnomalies.RData")   # from CTDwallSetup.R
# EOF
