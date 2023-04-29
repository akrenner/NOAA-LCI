#!/usr/bin/env Rscript

## CTD anomaly
## for each station location, calculate averages for all
## oceanographic parameters
rm (list=ls())

normDir <- "~/tmp/LCI_noaa/media/CTDsections/CTDwall-normals/"


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
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwallAnomalies.RData")



## plot normals
source ("CTDsectionFcts.R")
dir.create (normDir, showWarnings=FALSE, recursive=TRUE)

if (0){  ## not ready yet! XXX

poNorm$Transect <- factor (stn$Line [match (poNorm$Match_Name, stn$Match_Name)])
levels (poNorm$Transect) <- c (levels (poNorm$Transect), "ABext")

for (tn in 1:length (levels (poNorm$Transect))){
  tranN <- levels (poNorm$Transect)[tn]

  monthly <- TRUE

  ## doubly-used stations:
  stn$Line <- flexTransect (tranN, stn)  ## function from CTDsectionFcts.R
  transect <- stn$Line [match (poNorm$Match_Name, stn$Match_Name)]
  #  sect <- subset (stn, subse)
  ## get bathymetry
  # bottom <- getBathy(tranN, stn)
  bottom <- getBathy (subset (stn, Line==tranN))
  poNormT <- subset (poNorm, Transect==transect)


  ## build CTD object
  require ("oce")

  pdf (paste0 (normDir, levels (poNorm$Transect)[tn], ".pdf")
       , width=10, height=7.5)
  if (monthly){
    layoutM <- matrix (1:12, 1, byrow=TRUE)
    omText <- month.name
  }
  for (mo in nrow (layoutM)){
      ## construct oce-object
    require ("oce")

    ## plotting code

  }
  dev.off()
}
}

cat ("#\n#end of CTDwall_normals.R\n\n")
# EOF
