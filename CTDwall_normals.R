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

## plot normals
source ("CTDsectionFcts.R")
dir.create ("~/tmp/LCI_noaa/media/CTDsections/CTDwall-normals/", showWarnings=FALSE, recursive=TRUE)


poNorm$Transect <- factor (stn$Line [match (poNorm$Match_Name, stn$Match_Name)])
for (tn in 1:length (levels (poNorm$Transect))){
  ## doubly-used stations:
  stn$Line <- flexTransect (levels (poAll$Transect)[tn], stn)  ## function from CTDsectionFcts.R
  poNorm$Transect <- stn$Line [match (poAll$Match_Name, stn$Match_Name)]
sect <- subset (stn, subse)
  ## get bathymetry
  Require ("sf")
  sect <- st_as_sf(sect, coords=c("loni", "lati"))
  sf::st_crs(sect) <- 4326  ## WGS84 definition
  Require ("stars")
  sectP <- sf::st_transform(sect, st_crs (bathyZ))
  bottomZ <- stars::st_extract(bathyZ, at=sectP)$w001001.adf



  pdf (paste0 ("~/tmp/LCI_noaa/media/CTDsections/CTDwall-normals/"
              , levels (poNorm$Transect)[tn], ".pdf"), width=10, height=7.5)
  for (mo in 1:12){
      ## construct oce-object
    require ("oce")


  }
  dev.off()
}


# EOF
