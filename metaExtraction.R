#!/usr/bin/env Rscript

## (c) Martin Renner
## licence: GPL-3

## NOAA/KBRR LCI study
## read in plankton and oceanographic data
## geographically link with seabird data
## interannual comparison in relation to SST


## file structure:
## source files in ~/GISdata/LCI/

## check processed 'rawish' files on workspace for bad profiles, see whether these look ok
## deep-water stability:  50m vs bottom : where and when does this occur? (difference, not ratio)
## GAK-1 50 m sensor:  time series compared to LCI


rm (list = ls())

## CAREFUL with this!!
# system ("rm -r ~/tmp/LCI_noaa/")


## hard-code location to AK -- how?
system ("mkdir -p ~/tmp/LCI_noaa/cache")

require (parallel)
nCPUs <- detectCores()



############################
## define basic functions ##
############################

PDF <- function(fn, ...) {
  pdf (paste ("~/tmp/LCI_noaa/media/", fn, sep = ""), ...)
}


Seasonal <- function(month) {
  month <- as.numeric (month)
  cut (month, breaks = c(0, 3, 6, 9, 13)
    , labels = c ("winter", "spring", "summer", "fall"))
}




#####################################
## read aggregated files           ##
## best source of lat/lon metadata ##
#####################################


fN <- list.files ("~/GISdata/LCI/CTD/4_aggregated/", "_LowerCookInlet_ProcessedCTD.csv", full.names = TRUE)

regStr <- "^([a-zA-Z.0]{3})([a-zA-Z0-9_.]+)"


for (i in seq_along(fN)) {
  agF <- read.csv (fN [i], na.strings = "N/A")
  names (agF) <- gsub (regStr, "\\1", names (agF))
  names (agF)[ncol(agF)] <- "O2Sat"
  if (i == 1) {
    physOc <- agF
  } else {
    agF <- agF [, match (names (agF), names (physOc))] # fix column order
    physOc <- rbind (physOc, agF)
  }
}
rm (agF, i, fN)


## this is what field names look like after importing CSV file into R
## apply these names again to keep export consistent and to avoid having to
## change names throughout this script now.
newNames <- c ("File.Name", "Date", "Transect", "Station", "Time", "CTD.serial"
  , "latitude_DD", "longitude_DD", "Bottom.Depth", "Depth.saltwater..m."
  , "Temperature_ITS90_DegC", "Salinity_PSU", "Density_sigma.theta.kg.m.3"
  , "Fluorescence_mg_m3"
  , "Oxygen_SBE.43..mg.l.", "PAR.Irradiance", "Pressure..Strain.Gauge..db."
  , "Oxygen.Saturation.Garcia.Gordon.mg.l.")
if (all (gsub (regStr, "\\1", newNames)[1:17] == names (physOc)[1:17])) {
  names (physOc) <- newNames
} else {
  print (cbind (gsub (regStr, "\\1", newNames), names (physOc)))
  error ("Field names don't match") # in case of future troubles
}
rm (fieldNames, newNames, regStr)



## physOc data clean-up
## physOc <- cbind (isoDate = strptime (physOc$Date, format = "%m/%d/%Y"), physOc)
## summary (physOc)

physOc$Time <- ifelse (is.na (physOc$Time), "12:00", as.character (physOc$Time)) # avoid bad timestamps
physOc$Time <- ifelse (physOc$Time == "", "12:00", as.character (physOc$Time)) # avoid bad timestamps
physOc <- cbind (isoTime = as.POSIXlt (paste (physOc$Date, physOc$Time), format = "%m/%d/%Y %H:%M"
  #                     , tz = "AKDT")
  , tz = "America/Anchorage")
, physOc)

## fix bad/missing time/date info
# physOc$File.Name [is.na (physOc$isoTime)]
if (any (is.na (physOc$isoTime))) {
  print (physOc$Time [which (is.na (physOc$isoTime))])
  print (physOc$Date [which (is.na (physOc$isoTime))])
  print (physOc$File.Name [which (is.na (physOc$isoTime))])
  stop ("still having bad isoTime")
}

physOc$CTD.serial <- factor (physOc$CTD.serial)



### reduce to meta-data table
print (names (physOc))
mdat <- physOc [, 1:10]
mdat <- unique (mdat)

write.csv (mdat, file = "~/tmp/LCI_noaa/media/ctd_metadata.csv"
  , row.names = FALSE, quote = FALSE, na = "")

q()




poSS <- with (physOc, data.frame (File.Name = levels (File.Name)))
poSS$Match_Name <- physOc$Match_Name [match (poSS$File.Name, physOc$File.Name)]
poSS$latitude_DD <- physOc$latitude_DD [match (poSS$File.Name, physOc$File.Name)]
poSS$longitude_DD <- physOc$longitude_DD [match (poSS$File.Name, physOc$File.Name)]
poSS$timeStamp <- physOc$isoTime [match (poSS$File.Name, physOc$File.Name)]
poSS$Date <- format (poSS$timeStamp, format = "%Y-%m-%d", usetz = FALSE)
poSS$Time <- format (poSS$timeStamp, format = "%H:%M:%S", usetz = FALSE)
poSS$year <- as.numeric (format (poSS$timeStamp, "%Y"))
poSS$month <- as.numeric (format (poSS$timeStamp, "%m"))
poSS$season <- Seasonal (poSS$month)
poSS$SampleID <- with (poSS, paste (Match_Name
  , format (timeStamp, format = "%Y-%m-%d"
    , usetz = FALSE)
))   # no need to spec by H

## replace any/everything that's more than 1 km off
## poSS$longitude_DD <- ifelse (StDis > 1
##                             ,SCo [,1]
## #                             stn$Lon_decDegree [match (poSS$Match_Name, stn$Match_Name)]
##                            , poSS$longitude_DD)
## poSS$latitude_DD <- ifelse (StDis > 1
##                             , SCo [,2]
## #                           , stn$Lat_decDegree [match (poSS$Match_Name, stn$Match_Name)]
##                            , poSS$latitude_DD)




cat ("\n\n#\n#\n#", format (Sys.time(), format = "%Y-%m-%d %H:%M"
  , usetz = FALSE)
, " \n# \n# End of dataSetup.R\n#\n#\n")
## EOF
