#!/usr/bin/env Rscript

## consolidate as much error-checking and QAQC as possible in here
## This has to come after basic checking of file names to have date correct
## check here: stations and transects, dates and times
## also do final link of notebook-DB and files-DB

## start where CTD_cnv-Import.R left off
## produce file to be read by datasetup.R


#####################################################
##
## 1. QAQC
##    times and dates in file names consistent with metadata -> latlon from notebook
##    notebook latlon consistent with master-list
##    transect-stations correct -> match with master-list
# -> fill in from masterlist
# check consistency of metadata and notebook time and date -- do this earlier? 
# that station names are consistent with notebook and file names
# 
# make sure matchName for link to other datasets is correct
# 
#
##
## 2. produce annual aggregates
#     zip-up aggregates for export
#
#####################################################


rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV2.RData")



## fix bad/missing time/date info
# physOc$File.Name [is.na (physOc$isoTime)]
if (any (is.na (physOc$isoTime))){
  print (physOc [which (is.na (physOc$isoTime))
                 , which (names (physOc) %in% c("Time", "Date", "File.Name"))])
  ## print (physOc$Time [which (is.na (physOc$isoTime))])
  ## print (physOc$Date [which (is.na (physOc$isoTime))])
  ## print (physOc$File.Name [which (is.na (physOc$isoTime))])
  stop ("still having bad isoTime")
}


## levels (factor (physOc$File.Name [grep ("\\s", physOc$Density_sigma.theta.kg.m.3)]))
## levels (factor (physOc$File.Name [grep ("\\s", physOc$PAR.Irradiance)]))
## levels (factor (physOc$File.Name [grep ("\\s", physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.)]))
## levels (factor (physOc$File.Name [grep ("\\s", physOc$Fluorescence_mg_m3)]))
## grep ("\\s", factor (physOc$PAR.Irradiance), value = TRUE)
## grep ("\\s", factor (physOc$Density_sigma.theta.kg.m.3), value = TRUE)
## grep ("\\s", factor (physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.), value = TRUE)
## physOc <- physOc [grep ("^\\s", physOc$PAR.Irradiance),] # without spaces
## physOc <- physOc [grep ("^\\s", physOc$Density_sigma.theta.kg.m.3),]





stn <- read.csv ("~/GISdata/LCI/MasterStationLocations.csv")
stn <- subset (stn, !is.na (Lon_decDegree))
stn$Plankton <- grepl ("Y", stn$Plankton) # force into logical

## fix Match_Name
showBad <- function (po){
  ## print out currently missmatching station names
  x <- match (po$Match_Name, stn$Match_Name)
  y <- levels (factor (po$Match_Name [which (is.na (x))]))
  return (y)
}

oldMatch <- physOc$Match_Name
oldMatch <- paste (physOc$Transect, physOc$Station, sep = "_")
Match_Name <- paste (physOc$Transect, physOc$Station, sep = "_") # ignore all prev fixings!
physOc <- cbind (Match_Name, physOc); rm (Match_Name)
# summary (stn)
# y1 <- showBad (physOc)
physOc$Match_Name <- gsub (" (part of multiple transects)", "", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("^AlongBay_", "", physOc$Match_Name) #, fixed = TRUE)
physOc$Match_Name <- gsub ("^Subbays_", "", physOc$Match_Name) #, fixed = TRUE)
physOc$Match_Name <- gsub ("Sadie0", "Sadie_", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("Bear ", "Bear_", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("^J[bB]ay", "Jakolof", physOc$Match_Name)
physOc$Match_Name <- gsub ("^K[bB]ay", "Kasitsna", physOc$Match_Name)
physOc$Match_Name <- gsub ("^KB([0-9])", "AlongBay_\\1", physOc$Match_Name)
physOc$Match_Name <- gsub ("_0", "_", physOc$Match_Name, fixed = TRUE) # as per naming convention
physOc$Match_Name <- gsub ("_(\\d+)[ab]$", "_\\1", physOc$Match_Name)
physOc$Match_Name <- gsub ("^9andTutka_Tutka", "Tutka_", physOc$Match_Name)
physOc$Match_Name <- gsub ("^9andTutka_", "9_", physOc$Match_Name)
physOc$Match_Name <- gsub ("9_Tutka", "9_", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("_13to10m$", "_13", physOc$Match_Name)
physOc$Match_Name <- gsub ("^T4S02$", "4_2", physOc$Match_Name)
physOc$Match_Name <- gsub ("^T7S20$", "7_20", physOc$Match_Name)
physOc$Match_Name <- gsub ("^T9S06$", "9_6", physOc$Match_Name)
physOc$Match_Name <- gsub ("\\s([AB])$", "_\\1", physOc$Match_Name) # " A" or " B"
## all remaining spaces should be in transect names and should be removed (no underscore)
physOc$Match_Name <- gsub ("\\s", "", physOc$Match_Name)
physOc$Match_Name <- gsub ("(Bear|ChinaPoot|Halibut|Jakolof|Kasitsna|Peterson|Sadie|Seldovia|Tutka)([ABC])$", "\\1_\\2", physOc$Match_Name) # "TutkaA" or " B"
physOc$Match_Name <- gsub ("^Tutka0", "Tutka_", physOc$Match_Name)
physOc$Match_Name <- gsub ("^Tutka1", "Tutka_1", physOc$Match_Name)
physOc$Match_Name <- gsub ("PogiPoint|Pt\\.Pogi|Pt\\.KBlandPogi", "Pogibshi", physOc$Match_Name)
physOc$Match_Name <- gsub ("extra$", "", physOc$Match_Name)

## fixing a few individual stations
physOc$Match_Name <- gsub ("9_North", "9_1", physOc$Match_Name, fixed = TRUE)
physOc$Match_Name <- gsub ("9_South", "9_1", physOc$Match_Name)
## could also match some of the Tutka_1-9, but probably little point in doing that
print (badStn <- showBad (physOc))
# print (stn$Match_Name)


## XXX 2019-12-20 -- duplicate cast? unique issue?
# physOc <- subset (physOc, physOc$Match_name != "9_4south")
physOc <- physOc [physOc$Station != "4south",]
###




## fix casts with missing geographic coordinates, if possible
## bad/missing coordinates
# summary (factor (physOc$Match_Name [x]))
# physOc$longitude_DD <- ifelse (physOc$longitude_DD == "NA", "", physOc$longitude_DD)
physOc$longitude_DD <- ifelse (is.na (physOc$longitude_DD)
                               , stn$Lon_decDegree [match (physOc$Match_Name, stn$Match_Name)]
                               , physOc$longitude_DD)
physOc$latitude_DD <- ifelse (is.na (physOc$latitude_DD)
                              , stn$Lat_decDegree [match (physOc$Match_Name, stn$Match_Name)]
                              , physOc$latitude_DD)
## print (sort (levels (factor (physOc$longitude_DD))))
## physOc [which (physOc$longitude_DD == "NA"),]
physOc$longitude_DD <- ifelse (physOc$longitude_DD > 0, physOc$longitude_DD * -1
                               , physOc$longitude_DD)
# summary (factor (physOc$File.Name [which (is.na (physOc$longitude_DD))]))
# summary (physOc$longitude_DD)



## bad densities (some are not sigma theta, up to 1024)
Require ("oce")
physOc$Density_sigma.theta.kg.m.3 <- with (physOc, swRho (Salinity_PSU, Temperature_ITS90_DegC
                                                          , Pressure..Strain.Gauge..db.
                                                          , eos = "unesco"))-1000
## O2 Saturation is off -- sometimes reported as ml/l, others as mg/l.
## no obvious way in oce, but could recalculate it using
## LakeMetabolizer::o2.at.sat()
Require ("LakeMetabolizer")
## # physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.
O2 <- with (
  physOc, o2.at.sat.base (temp = Temperature_ITS90_DegC
                          #                          , baro = Pressure..Strain.Gauge..db. *100 + 1000
                          , salinity = Salinity_PSU
                          , model= "garcia"))
## plot (O2, physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.)
physOc$Oxygen.Saturation.Garcia.Gordon.mg.l. <- O2
rm (O2)
physOc$O2perc <- with (physOc, Oxygen_SBE.43..mg.l. / Oxygen.Saturation.Garcia.Gordon.mg.l.)

physOc$Spice <- with (physOc, swSpice (Salinity_PSU
                                       , Temperature_ITS90_DegC
                                       , Pressure..Strain.Gauge..db.
))
summary (physOc)
## any other data cleaning on physOc data...




## check whether still needed? 
if (0){
## easier to re-export all CTD data?
## Re-assembly of inconsistent data is a bit of a mess
## output translation table
transT <- cbind (Trans_Station = oldMatch, Match_Name = physOc$Match_Name)[!duplicated (oldMatch),]
## XXX Warning number or ros or results is not multiple of vector length (arg 2).
## does this need a fix/is a bug ?? XXX

transT <- transT [order (transT [,1]),]
write.csv (subset (transT, transT [,1] != transT [,2])
           , file = "~/tmp/LCI_noaa/media/PO_stationNames.csv"
           , row.names = FALSE, quote = FALSE)
write.csv (badStn, file = "~/tmp/LCI_noaa/media/BadStationNames.txt"
           , row.names = FALSE, quote = FALSE)
rm (badStn, transT)
}

## -- check this -- still relevant?? 
## fix up station and transect names from Match_Name
physOc$Transect <- stn$Line [match (physOc$Match_Name, stn$Match_Name)]
# physOc$Station <- stn$New.Station.Name [match (physOc$Match_Name, stn$Match_Name)]





#####################################
## 2. annual aggregates for export ##
#####################################

Require ("zip")
yr <- factor (format (physOc$isoTime, "%Y"))
for (i in 1:length (levels (yr))){
  ctdA <- subset (physOc, yr == levels (yr)[i])
  write.csv (ctdA, file = paste0 ("~/tmp/LCI_noaa/cache/", levels (yr)[i], "processedCTD.csv")
             , row.names = FALSE, quote = FALSE)
  zipr (paste0 ("../media/CTDaggregate", levels (yr)[i], ".zip")
        , files = paste0 ("~/tmp/LCI_noaa/cache/", levels (yr)[i], "processedCTD.csv")
        , recurse = FALSE, include_directories = FALSE)
}

## zip-up result files
# Require ("zip")
# unlink ("~/tmp/LCI_noaa/media/processedCTD_annual.zip", force = TRUE)
wD <- getwd()
dir.create("~/tmp/LCI_noaa/data-products", recursive = TRUE)
setwd ("~/tmp/LCI_noaa/data-products/")
zFiles <- list.files ("~/tmp/LCI_noaa/cache/", pattern = "*processedCTD.csv", full.names = FALSE)
zipr ("processedCTD_annual.zip"
      , files = zFiles, recurse = FALSE, include_directories = FALSE)
unlink (zFiles, force = TRUE)
rm (zFiles)
setwd (wD); rm (wD)
### system ("zip -jm ~/tmp/LCI_noaa/media/processedCTD_annual.zip ~/tmp/LCI_noaa/cache/*processedCTD.csv")
## write.csv (physOc, file = "~/tmp/LCI_noaa/media/CTD_aggregate_allYears.csv"
##          , row.names = FALSE, quote = FALSE)
rm (showBad, oldMatch, ctdA, yr)



save (physOc, file = "~/tmp/LCI_noaa/cache/CNV1.RData")  ## this to be read by dataSetup.R


## link with ACCESS (c) database (migrate enventually), to get actual coordinates
## use file-name to link with database tables
## -- do that in dataSetup.R -- or BETTER: more that step and annual aggregates into this
## file to have all CDT stuff together here
