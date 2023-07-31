#!/usr/bin/env Rscript

## phytoplankton data update
## translate taxa
## aggregate taxa
## reshape to wide
## merge with existing table
## translate stations to match_name
## merge with reference station list
## output data product to share

rm (list=ls())

phyt <- read.csv("~/GISdata/LCI/phytoplankton/2019-2021 phyto xiuning.csv")
tax <- read.csv ("~/GISdata/LCI/phytoplankton/phytoTaxonomy.csv")
phyt$newTax <- tax$lowresTax [match (phyt$Taxon, tax$hiresTax)]

## QAQC
summary (duplicated (phyt))


if (0){
  ## one-off -- make translation table
  levels (factor (phyt$Taxon))
  levels (factor (phyt$Taxon.1))
  ## generate taxonomy lookup table
  x <- levels (factor (phyt$Taxon))
  y <- phyt$Taxon.1 [match (x, phyt$Taxon)]
  tax <- data.frame (hiresTax=x, lowresTax=y)
  write.csv (tax, file="~/GISdata/LCI/phytoplankton/phytoTaxonomy.csv"
               , row.names=FALSE, quote=FALSE)
  rm (x, y)
}


phyt$StatDate <- with (phyt, paste (Station, Date, sep="_"))  ## define sample-identifier
ph2 <- aggregate (Abundance..cells.L.~Taxon.1+StatDate, phyt, FUN=sum, na.rm=TRUE)
pW <- reshape (ph2, direction="wide", timevar="Taxon.1", idvar="StatDate")

## clean-up wide data frame
names (pW) <- gsub ("Abundance..cells.L.", "", names (pW))
names (pW) <- gsub ("^\\.", "", names (pW))
## all NAs are implied 0s
for (i in 2:ncol (pW)){
  pW [,i] <- ifelse (is.na (pW [,i]), 0, pW [,i])
}
dT <- strsplit (pW$StatDate, split="_", fixed=TRUE)
dT <- do.call (rbind, dT)
pWout <- data.frame (Date=format (as.Date (dT [,2], "%m/%d/%Y"), "%Y-%m-%d")
                     , Sampling.location=dT[,1], pW [,2:ncol (pW)])
rm (pW, ph2)



## merge with existing table
bigP <- read.csv ("~/GISdata/LCI/phytoplankton/phytodata-kbl-Oct2022.csv")
## QAQC -- any new taxa?
levels (factor (tax$lowresTax)) [is.na (match (gsub ("[\ \\(\\)-]", ".", levels (factor (tax$lowresTax)))
                                               , names (bigP)))]
## duplicate records
summary (duplicated (bigP))
bigP <- subset (bigP, !duplicated (bigP))


## merge both DFs
require ("plyr")
bOut <- rbind.fill (bigP, pWout)
## all NAs are implied 0s
for (i in 3:ncol (bOut)){
  bOut [,i] <- ifelse (is.na (bOut [,i]), 0, bOut [,i])
}
## re-sort columns
bOut <- bOut [,c(2,1, (3:ncol (bOut))[order (names (bOut)[3:ncol (bOut)])])]

## fix station names, split-up to transect and station
# levels (factor (bOut$Sampling.location))
bOut$Sampling.location <- trimws(bOut$Sampling.location)
bOut$Sampling.location <- gsub ("^Transect ", "T", bOut$Sampling.location)
bOut$Sampling.location <- gsub (", Station ", "-", bOut$Sampling.location)
bOut$Sampling.location <- gsub ("^KB[0]*", "AlongBay-", bOut$Sampling.location)
bOut$Sampling.location <- gsub ("^Kachemak Bay ", "AlongBay-", bOut$Sampling.location)
bOut$Sampling.location <- gsub ("-", "_", bOut$Sampling.location)
for (i in c(3,4,6,7,9)){
  bOut$Sampling.location <- gsub (paste0 ("T",i), i, bOut$Sampling.location)
}
bOut$Sampling.location <- gsub ("Cove ", "", bOut$Sampling.location)
bOut$Sampling.location <- gsub ("Bay ", "", bOut$Sampling.location)
bOut$Sampling.location <- gsub ("Lab Dock$", "Dock", bOut$Sampling.location)
bOut$Sampling.location <- gsub ("^China Poot", "Chinapoot", bOut$Sampling.location)
bOut$Sampling.location <- gsub (" ", "_", bOut$Sampling.location)
# x <- levels (factor (bOut$Sampling.location))

## match columsn to CTD data
## station, date, time, LatDD, lonDD, transect, station
msl <- read.csv("~/GISdata/LCI/MasterStationLocations.csv")
# msl$Match_Name
# match (levels (factor (bOut$Sampling.location)), msl$Match_Name)
# 1 non-match: "unknown"
# bOut <- subset (bOut, Date != "unknown 2021")


## look up Transect, station, geographic coordinates
mslR <- match (bOut$Sampling.location, msl$Match_Name)
phytoOut <- with (bOut, data.frame (Station=Sampling.location
                                    , Date
                                    , Time=rep ("", nrow (bOut))
                                    , Latitude_DD=msl$Lat_decDegree [mslR]
                                    , Longitude_DD=msl$Lon_decDegree [mslR]
                                    , Transect=msl$Line [mslR]
                                    , StationN=msl$Station [mslR]
                                    , bOut [,3:ncol (bOut)]
))
rm (mslR, bOut, bigP, dT, i, msl, phyt, pWout, tax)

## find duplicate records
StnDate <- with (phytoOut, paste (Station, Date))
summary (duplicated(StnDate))
summary (duplicated (phytoOut))
rm (StnDate)
# phytoOut <- phytoOut [order (phytoOut$Date),]


## -------------------
##    look up times from CTD data
## -------------------

## assume that ctd data has already been processed and dataSetup.R has run
load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
pSID <- with (phytoOut, paste (Station, Date))
phytoOut$Time <- poSS$Time [match (pSID, poSS$SampleID)]


outF <- "~/tmp/LCI_noaa/data-products/phytoplankton.csv"
## concatenate and output merged table
write (paste0 ("Collected as part of GulfWatch on predefined stations in Kachemak Bay ",
               "and lower Cook Inlet concurrent with zooplankton and CTD. "
               , paste (range (format (as.Date (phytoOut$Date), "%Y"), na.rm=TRUE), collapse="-")
)
, file=outF)
write.table (phytoOut, outF, row.names=FALSE, append=TRUE, quote=FALSE
             , sep=",", col.names=TRUE, na="")


## for GWA final data
outF <- "~/tmp/LCI_noaa/media/EVOS/phytoplankton.csv"
phytoOut <- subset (phytoOut, !Transect %in% c ("subbay", "land"))
write (paste0 ("Collected as part of GulfWatch on predefined stations in Kachemak Bay ",
               "and lower Cook Inlet concurrent with zooplankton and CTD. "
               , paste (range (format (as.Date (phytoOut$Date), "%Y"), na.rm=TRUE), collapse="-")
)
, file=outF)
write.table (phytoOut, outF, row.names=FALSE, append=TRUE, quote=FALSE
             , sep=",", col.names=TRUE, na="")

## EOF
