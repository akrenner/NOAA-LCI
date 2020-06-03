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

PDF <- function (fn, ...){
    pdf (paste ("~/tmp/LCI_noaa/media/", fn, sep = ""), ...)
}

Require <- function (package, ...){
    ## package <- as.character(substitute(package))
    ext <- require (package)
    if (ext){
        install.packages (package, repos = "http://cran.fhcrc.org/", dependencies = TRUE)
    }
}
Require <- require

Seasonal <- function (month){
    month <- as.numeric (month)
    cut (month, breaks = c(0,3,6,9,13)
            , labels = c ("winter", "spring", "summer", "fall"))
}



# ctd <- read.csv ("~/Downloads/2015_02-23_T6_S15_cast233.csv")

##############
## CTD data ##
##############


## There are 3 different approaches here, relying in different stages of processing with
## SeaBird software:
## 1. cnv files: minimal processing by SeaBird software. 2012 largely missing.
## 2. processed csv files: binned files, output by SeaBird software. Hierachical folders = issue
## 3. aggregated files: missing turbidity. All other issues resolved now. 

## need to process HEX files using Seabird BATCH files! See
## http://www.comm-tec.com/Training/Training_Courses/SBE/Module%203%20Basic%20Data%20Processing.pdf



## read in cnv files
Require (oce)
fN <- list.files ("~/GISdata/LCI/CTD/1_cnv/", ".cnv", full.names = TRUE)

fN <- list.files ("~/GISdata/LCI/CTD/CTD-casts", ".zip", full.names = TRUE)
readAnnZip <- function (fn){            # annual zip files of all cnv data
    ##  good in theory, but turbidity is missing on all but 2012
    ##  
    td <- tempdir()
    suppressMessages (system (paste ("unzip", fn, " -d ", td))) # cut-out messages
    fList <- list.files (td, ".cnv", recursive = TRUE, full.names = TRUE)
    fListG <- grep ("2\\sCNV", fList, value = TRUE)
    require (oce)
    rCTD <- function (cN){
        ctdF <- suppressMessages (read.ctd (cN))
        x <- summary (ctdF)
        return (x)
    }
    meta <- rCTD (fListG [1])
    for (i in 2:length (fListG)){
        meta <- rCTD (fListG [i])
    }
    unlink (td, recursive = TRUE)
    return (meta)
}

if (0){ # most desirable way, but need to reprocess CNV files to add turbidity
ctdF <- bind_rows (mclapply (fN, readAnnZip, mc.cores = nCPUs), id = fN)
}
rm (readAnnZip)


## read in processed files of individual CTD casts
if (0){
fN <- list.files ("~/GISdata/LCI/CTD/3_processed/", ".csv", full.names = TRUE)

if (0){ # troubleshooting
fieldNames <- sapply (1:length (fN), FUN = function (i){
    tF <- read.fwf (fN [i], width = 550)
    strtLine <- grep ("^\"Temperature", tF$V1)-1
    if (length (strtLine) == 0){
        strtLine <- grep ("^\"Depth", tF$V1)-1
    }
    agF <- read.csv (fN [i], na.string = "-9999", skip = strtLine)
    return (names (agF))
})
sapply (1:length (fieldNames), function (i){length (fieldNames [[i]])})

sapply (1:length (fieldNames), function (i){grep ("sigma", fieldNames [[i]], value = TRUE)})
}

for (i in 1:length (fN)){
    tF <- read.fwf (fN [i], width = 550)
    strtLine <- grep ("^\"Temperature", tF$V1)-1
    if (length (strtLine) == 0){
        strtLine <- grep ("^\"Depth", tF$V1)-1
    }
    agF1 <- read.csv (fN [i], na.string = "-9999", skip = strtLine)
    
    File.Name = gsub ("/Users/martin/GISdata/LCI/CTD/3_processed/", "", fN [i])

    ## dPat <-  "^[0-9]{4}_[0-9]{2}-[0-9]{2}"
    ## m <- regexpr (dPat, File.Name)
    ## Date <- regmatches (File.Name, m)

    fStrS <- strsplit (File.Name, "_")
    Date <- paste (fStrS [[1]][1], fStrS[[1]][2], sep = "-")
    Transect <- fStrS [[1]][3]
    Station <- fStrS [[1]][4]
    cat (File.Name, "\n")
    agF <- with (agF1, data.frame (File.Name
                                 , Date = as.POSIXct (gsub ("_", "-", Date))
                                 , Transect
                                 , Station
                                 , Temperature..ITS.90..deg.C.
                         #       , Conductivity..uS.cm.
                                 , Pressure..Strain.Gauge..db.
                                 , Oxygen..SBE.43..mg.l.
                                 , PAR.Irradiance..Biospherical.Licor
                         #       , Density..density..kg.m.3.
                         #       , Specific.Conductance..uS.cm.
                                 , Salinity..Practical..PSU.
                         #       , Oxygen.Saturation..Garcia...Gordon..ml.l.
                         #       , Potential.Temperature..ITS.90..deg.C.
                         #        , Density..sigma.t..kg.m.3..
                         #       , Descent.Rate..m.s...WS...2
                         #       , flag
                                   )
                 )
    agF$Fluorescence.mg.m.3 <- agF1 [,grep ("^Fluorescence", names (agF1))]
    agF$Depth.salt.water <- agF1 [,grep ("^Depth", names (agF1))]
    agF$Density..sigma.theta..kg.m.3. <- agF1 [,grep ("sigma", names (agF1))]

    if ("Oxygen.Saturation..Garcia...Gordon..mg.l." %in% names (agF1)){
        agF$Oxygen.Saturation..Garcia...Gordon..mg.l. <-
            agF1$Oxygen.Saturation..Garcia...Gordon..mg.l.
    }else{
        agF$Oxygen.Saturation..Garcia...Gordon..mg.l. <- agF1$Oxygen.Saturation..Garcia...Gordon..ml.l. * 1.42905 # per USGS memo 2011.03
        }

    if ("Upoly.0..ECO.FLNTUS" %in% names (agF1)){
        agF$Upoly.0..ECO.FLNTUS <- agF1$Upoly.0..ECO.FLNTUS
    }else{
        agF$Upoly.0..ECO.FLNTUS <- NA
    }
    
    if (i == 1){
        physOc <- agF
    }else{
        physOc <- rbind (physOc, agF)
    }
}


rm (agF, agF1, i, fN)
}

## read HEX files -- need seabird software for that! oce will not do that, only
## read .cvn files. 
## fN <- list.files ("~/GISdata/LCI/CTD/2_editedHEX/", ".hex", full.names = TRUE)
## require (oce)
## ctd <- read.oce (fN [5])
## temp, only to get turbidity? 


fN <- list.files ("~/GISdata/LCI/CTD/6\ DERIVE/", ".cnv", full.names = TRUE)
readCNV <- function (i){
    require (oce, quietly = TRUE)
    cN <- fN [i]
    ctdF <- suppressMessages (read.ctd (cN)) # NAs introduced by coercion... = ? 
    ## cat (i, " ")
    ## if (i %% 10 == 0){cat ("\n")}

                                        #    if ("Air" %in% ctdF@metadata$Station)

    if (! "station" %in% ctdF@metadata){ # temp eliminate cast with missing station metadata. XXX fix these HEX/CNV files! 
        return()
    }else if (grepl ("Air", ctdF@metadata$station)){ # eleminate air casts
        return()
    }else{
        
        ## more CTD import processing steps
        ## zero-depth
        ## cut-out surface, up-cast?
                                        # cut bad flags! 
        ## aggregate depth bins
        ## derived variables ??
        ## add metadata from elsewhere (later -- latlon, Match_Name, tide,..)
        ## most should be here (paste into aggregated data or separate table)
        ##   .... find others!

        ## best: manually inspect and read-in from separate table
                                        # ?plotScan
        
        ctdF <- ctdTrim (ctdF, method = "downcast") # there's also a method seabird
        
        ## aggregate
        ctdF <- handleFlags (ctdF)
        ctdF <- ctdDecimate (ctdF, p = 1, method = "boxcar", e = 1.5) # later?

        if ("turbidity" %in% names (ctdF@data)){ # some called "turbidity", not "upoly"
            names (ctdF@data)[which (names (ctdF@data) == "turbidity")] <- "upoly"
        }
        if (!"fluorescence" %in% names (ctdF@data)){
            ctdF@data$fluorescence <- rep (NA, length (ctdF@data$sigmaTheta))
                                        # cat (gsub ("/Users/martin/GISdata/LCI/CTD/6 DERIVE//", "", fN [i]), "\n")
        }
        
        meta <- function (x){rep (x, length (ctdF@data$sigmaTheta))}
        
        cDF <- data.frame (File.Name =
                               gsub ("^/Users/martin/GISdata/LCI/CTD/6\ DERIVE//", ""
                                   , fN [i])
                           ## , latitude = meta (ctdF@metadata$latitude)
                           ## , longitude = meta (ctdF@metadata$longitude)
                         , timestamp = meta (ctdF@metadata$startTime)
                         , depth_bottom = meta (ctdF@metadata$waterDepth)
                           ## , transect = meta (ctdF@metadata$station)
                         , Match_Name = meta (ctdF@metadata$station)
                         , CTDserial = meta (ctdF@metadata$serialNumberTemperature)
                         , density = ctdF@data$sigmaTheta
                         , depth = ctdF@data$depth
                         , O2 = ctdF@data$oxygen
                         , par = ctdF@data$par
                         , salinity = ctdF@data$salinity
                         , temperature = ctdF@data$temperature
                         , pressure = ctdF@data$pressure
                         , fluorescence = ctdF@data$fluorescence ## often missing
                         , turbidity = ctdF@data$upoly
                         , conductivity = ctdF@data$conductivity
                                        # , depth2 = ctdF@data$depth2
                           )
        
        ##   x <- summary (ctdF)
        ##   return (x)
        return (cDF)
        ## return (length (ctdF@data))
    }
}


## CTD1 <- readCNV (120)                   # Air Cast
## CTD1 <- readCNV (520)                   # missing station metadata
## for (i in 1:length (fN)){cat (i, "\n"); CTD1 <- readCNV (i)}


CTD1 <- readCNV (235)
CTD1 <- readCNV (236)

require (dplyr)
CTD1 <- mclapply (1:length (fN), readCNV, mc.cores = nCPUs)
# CTD1 <- bind_rows (CTD1, id = fN)
CTD1 <- do.call (rbind, CTD1)
rm (fN, readCNV)
  

## ctdF@metadata$latitude <- 
## ctdF@metadata$longitude <-

save.image ("~/tmp/LCI_noaa/cache/CNV1.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV1.RData")


q()


###########################
## read aggregated files ##
###########################

fN <- list.files ("~/GISdata/LCI/CTD/4_aggregated/", "_LowerCookInlet_ProcessedCTD.csv", full.names = TRUE)

## field names in _Aggregatedfiles.csv are not consistent, neither is their order consistent.
## reduce all field names to make the consistent, then sort by field names to get order right
## algorithm: last one is always O2 saturation. Others should be diagnostic by first 3 letters
regStr <- "^([a-zA-Z.0]{3})([a-zA-Z0-9_.]+)"

# agF <- read.csv (fN [4])
fieldNames <- sapply (1:length (fN), FUN = function (x){
    names (read.csv (fN [x], na.strings = "N/A"))
})

 sapply (1:length (fN), FUN = function (x){
    fnms <- names (read.csv (fN [x], na.strings = "N/A"))
    c (grep ("Sat", fnms, value = TRUE), fN [x])
#    c (grep ("Temperature", fnms, value = TRUE), fN [x])
})


# fieldNames
# print (gsub (regStr, "\\1",fieldNames))

for (i in 1:length (fN)){
    agF <- read.csv (fN [i], na.strings = "N/A")
#    plot (Oxygen.Saturation..Garcia...Gordon..mg.l. ~ Temperature..ITS.90..deg.C., agF)
    names (agF) <- gsub (regStr, "\\1",names (agF))    
    names (agF)[ncol(agF)] <- "O2Sat"
    ## agF <- data.frame (FNag = fN [i], agF) -- not needed
    if (i == 1){
        physOc <- agF
    }else{
        ## print (names (agF))
        agF <- agF [,match (names (agF),names (physOc))] # fix mixed-up column orders
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
if (all (gsub (regStr, "\\1",newNames)[1:17] == names (physOc)[1:17])){
    names (physOc) <- newNames
}else{
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
if (any (is.na (physOc$isoTime))){
    print (physOc$Time [which (is.na (physOc$isoTime))])
    print (physOc$Date [which (is.na (physOc$isoTime))])
    print (physOc$File.Name [which (is.na (physOc$isoTime))])
    stop ("still having bad isoTime")
}

physOc$CTD.serial <- factor (physOc$CTD.serial)


## levels (factor (physOc$File.Name [grep ("\\s", physOc$Density_sigma.theta.kg.m.3)]))
## levels (factor (physOc$File.Name [grep ("\\s", physOc$PAR.Irradiance)]))
## levels (factor (physOc$File.Name [grep ("\\s", physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.)]))
## levels (factor (physOc$File.Name [grep ("\\s", physOc$Fluorescence_mg_m3)]))
## grep ("\\s", factor (physOc$PAR.Irradiance), value = TRUE)
## grep ("\\s", factor (physOc$Density_sigma.theta.kg.m.3), value = TRUE)
## grep ("\\s", factor (physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.), value = TRUE)
## physOc <- physOc [grep ("^\\s", physOc$PAR.Irradiance),] # without spaces
## physOc <- physOc [grep ("^\\s", physOc$Density_sigma.theta.kg.m.3),]

save.image ("~/tmp/LCI_noaa/cache/cachePO1.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/cachePO1.RData")
Require ("XLConnect")
stn <- readWorksheetFromFile ("~/GISdata/LCI/MasterStationLocations.xlsx", sheet = 1)
stn <- subset (stn, !is.na (Lon_decDegree))


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


## ## ID bad station names using distance from stn$Match_Name
if (0){                                 # not worth it -- leave rest as-is
relD <- function (stNum, yName){
    disT <- sqrt (2*(stn$Lon_decDegree [stNum] -
                   physOc$longitude_DD [which (physOc$Match_Name == yName)[1]])^2 +
                  (stn$Lat_decDegree [stNum] -
                   physOc$latitude_DD [which (physOc$Match_Name == yName)[1]])^2)
    ## Require ("oce")
    ## dist <- geod.dist (lat1 = stn$Lat_decDegree [stNum]
    ##                  , lon1 = stn$Lon_decDegree [stNum]
    ##                  , lat2 = latitude_DD [which (physOc$Match_Name == yName)[1]]
    ##                  , lon2 = physOc$longitude_DD [which (physOc$Match_Name == yName)[1]]
    ##                    )
    return (disT)
}

badMatch <- as.data.frame (t (sapply (1:length (y), function (i){
    mD <- sapply (1:nrow (stn), function (j){relD (j, y [i])})    
    return (cbind (y [i], stn$Match_Name [which.min (mD)], min (mD, na.rm = TRUE)))
})))
badMatch [,3] <- as.numeric (as.character (badMatch [,3]))
print (badMatch [order (badMatch [,3]),])
rm (badMatch)
badO <- subset (physOc, Match_Name %in% y)
badO <- badO [!duplicated (badO$Match_Name),]
plot (latitude_DD~longitude_DD, badO, type = "n")
text (latitude_DD~longitude_DD, badO, labels = Match_Name)
text (Lat_decDegree~Lon_decDegree, stn, labels = Match_Name, col = "red")
}


## fix casts with missing geographic coordinates, if possible
## bad/missing coordinates
# summary (factor (physOc$Match_Name [x]))
physOc$longitude_DD <- ifelse (is.na (physOc$longitude_DD)
                              , stn$Lon_decDegree [match (physOc$Match_Name, stn$Match_Name)]
                              , physOc$longitude_DD)
physOc$latitude_DD <- ifelse (is.na (physOc$latitude_DD)
                              , stn$Lat_decDegree [match (physOc$Match_Name, stn$Match_Name)]
                              , physOc$latitude_DD)
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
require (LakeMetabolizer)
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



## output translation table
transT <- cbind (Trans_Station = oldMatch, Match_Name = physOc$Match_Name)[!duplicated (oldMatch),]
transT <- transT [order (transT [,1]),]
write.csv (subset (transT, transT [,1] != transT [,2])
         , file = "~/tmp/LCI_noaa/media/PO_stationNames.csv"
         , row.names = FALSE, quote = FALSE)
write.csv (badStn, file = "~/tmp/LCI_noaa/media/BadStationNames.txt"
         , row.names = FALSE, quote = FALSE)
rm (badStn)
## fix up station and transect names from Match_Name
physOc$Transect <- stn$Line [match (physOc$Match_Name, stn$Match_Name)]
# physOc$Station <- stn$New.Station.Name [match (physOc$Match_Name, stn$Match_Name)]



yr <- factor (format (physOc$isoTime, "%Y"))
for (i in 1:length (levels (yr))){
    ctdA <- subset (physOc, yr == levels (yr)[i])
    write.csv (ctdA, file = paste ("~/tmp/LCI_noaa/cache/", levels (yr)[i]
                                 , "processedCTD.csv", sep = "")
               , row.names = FALSE, quote = FALSE)
}
system ("zip -jm ~/tmp/LCI_noaa/media/processedCTD_annual.zip ~/tmp/LCI_noaa/cache/*processedCTD.csv")
## write.csv (physOc, file = "~/tmp/LCI_noaa/media/CTD_aggregate_allYears.csv"
##          , row.names = FALSE, quote = FALSE)
rm (showBad, transT, oldMatch, ctdA, yr)

save.image ("~/tmp/LCI_noaa/cache/POcean.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/POcean.RData"); require (parallel)



#####################################################
## trouble-shooting density and depth calculations ##
#####################################################

## a fair number of casts have highest density/salinity at the surface. Remove those impossible data points.
badDens <- function (i){
    ctd <- subset (physOc, File.Name == levels (physOc$File.Name)[i])
    cntx <- subset (1:nrow (physOc), physOc$File.Name == levels (physOc$File.Name)[i])
    if (ctd$Density_sigma.theta.kg.m.3 [1] >
        ctd$Density_sigma.theta.kg.m.3 [2]){
        bCntr <- cntx [1]
    }else{
        bCntr <- numeric()
    }
    return (bCntr)
}
bCntr <- unlist (mclapply (1:length (levels (physOc$File.Name)), FUN = badDens
                         , mc.cores = nCPUs))
cat ("\n\nRemoved first data point from ", length (bCntr), "CTD casts because density \nwas higher than in subsequent samples.\n\n")
physOc <- subset (physOc, !(1:nrow (physOc)) %in% bCntr)
rm (bCntr, badDens)



png ("~/tmp/LCI_noaa/media/testPlots%02d.png")
xY <- factor (format (physOc$isoTime, "%Y"))
plot (physOc$Temperature_ITS90_DegC, physOc$Oxygen.Saturation.Garcia.Gordon.mg.l.
#    , col = physOc$CTD.serial
    , col = as.numeric (xY) # YEAR!!
      , pch = 19
      )
legend ("topright", col = 1:length (levels (xY)), pch = 19, legend = levels (xY))
rm (xY)
        
plot (physOc$Temperature_ITS90_DegC, physOc$Oxygen_SBE.43..mg.l.
    , col = physOc$CTD.serial)
## plot (physOc$Oxygen.Saturation.Garcia.Gordon.mg.l., physOc$Oxygen_SBE.43..mg.l.
##     , col = physOc$CTD.serial)
## plot (physOc$Depth.saltwater..m., physOc$Oxygen_SBE.43..mg.l.
##     , col = physOc$CTD.serial)
dev.off()

## dF <- factor (physOc$File.Name)         # Date ?
## dPc <- unlist (mclapply (1:length (levels (dF)), function (i){
##     x <- subset (physOc, dF == levels (dF)[i])       
##     r <- cor (x$Pressure..Strain.Gauge..db.,x$Depth.saltwater..m.)
##     return (r)
## }
## , mc.cores = nCPUs))
## summary (dPc)

if (0){
is.monotone <- function (ts){
    tsd <- ts [2:length (ts)]
    difV <- ts [1:(length (ts)-1)]-tsd
    all (difV <= 0)
    }
badPAR <- sapply (levels (physOc$File.Name), FUN = function (fn){
    cast <- subset (physOc, File.Name == fn)
    is.monotone (cast$PAR.Irradiance)
})
badDens <- sapply (levels (physOc$File.Name), FUN = function (fn){
    cast <- subset (physOc, File.Name == fn)
    is.monotone (cast$Density_sigma.theta.kg.m.3)
})
}

## exclude bad casts, cast that don't include top water layer
gC <- levels (factor (subset (physOc, Depth.saltwater..m. <= 3)$File.Name))
physOc <- subset (physOc, File.Name %in% gC)
physOc <- physOc [order (physOc$File.Name, physOc$Depth.saltwater..m.),] # for match
physOc$File.Name <- factor (physOc$File.Name)
rm (gC)







save.image ("~/tmp/LCI_noaa/cache/CTD.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData"); require (parallel)



## plotting and analyis of CTD data outsourced from here to anaCTD.R



## Kris:
## - presistance of mixing across seasons and tides
## 
## use webtide to add tidal cycle: HF, HE, LE, LF
## make CTD-cast table:  isoDate, File.Name, Time, latitude_DD, longitude_DD, Bottom.depth, posDate
## webtide installed in /usr/local/
## see http://dankelley.github.io/r/2013/12/29/installing-webtide-in-osx.html

## tid <- webtide ("predict", lon=-151.6, lat=59
##               , time=Sys.time()  #as.POSIXct ("2017-06-21 23:44 UTC")
##               , plot = FALSE)
## with (tid, ifelse (elevation > 0 && u > 0, "HF"
##         , ifelse (elevation > 0 && u < 0, "HE"
## , ifelse (elevation < 0 && u > 0, "LF", "LE"))))

## more to ask Kris:
## fluorescence in total water column?


############################################
## univariate summaries for each CTD cast ##
############################################

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
## poSS$SampleID_H = with (poSS, paste (Match_Name
##                                    , format (timeStamp, format = "%Y-%m-%d_%H", usetz = FALSE)
##                                      ))
## multiple samples of same station on one day! 
## x <- summary (factor (poSS$SampleID_H), maxsum = 10000)
## sort (poSS$File.Name [poSS$SampleID_H %in% names (which (x>1))])
## x <- summary (factor (poSS$SampleID), maxsum = 10000)
## sort (poSS$File.Name [poSS$SampleID %in% names (which (x>1))])
poSS$maxDepth <- aggregate (Depth.saltwater..m.~File.Name
                          , data = physOc, FUN = max)$Depth.saltwater..m.
dMean <- function (fn, fldn){
    ## calculate mean of field for the last 5 m above the bottom
    cast <- subset (physOc, File.Name == fn)
    lLay <- c (-5, 0) + max (cast$Depth.saltwater..m.)
    outM <- mean (subset (cast [,which (names (cast) == fldn)]
                        , (lLay [1] < cast$Depth.saltwater..m.)
                          ))
    return (outM)
}

poSS$SST <- aggregate (Temperature_ITS90_DegC~File.Name, data = physOc
                 , subset = Depth.saltwater..m. <= 3
                 , FUN = mean)$Temperature_ITS90_DegC
poSS$aveTemp <- aggregate (Temperature_ITS90_DegC~File.Name, data = physOc
                         , FUN = mean)$Temperature_ITS90_DegC
poSS$minTemp <- aggregate (Temperature_ITS90_DegC~File.Name, data = physOc
                         , FUN = min)$Temperature_ITS90_DegC
poSS$deepTemp <- unlist (mclapply (poSS$File.Name, FUN = dMean, fldn = "Temperature_ITS90_DegC"
                                , mc.cores = nCPUs))
poSS$SSS <- aggregate (Salinity_PSU~File.Name, data = physOc
                 , subset = Depth.saltwater..m. <= 3
                 , FUN = mean)$Salinity_PSU
poSS$aveSalinity <- aggregate (Salinity_PSU~File.Name, data = physOc
                             , FUN = mean)$Salinity_PSU
poSS$deepSal_old <- aggregate (Salinity_PSU~File.Name, data = physOc, FUN = function (x){
    if (length (x) > 50){mean (x [50:length (x)])}else{NA}
})$Salinity_PSU
poSS$deepSal <- unlist (mclapply (poSS$File.Name, FUN = dMean, fldn = "Salinity_PSU"
                                , mc.cores = nCPUs))
# poSS$aveSpice <- aggregate (Spice~File.Name, data = physOc, FUN = mean)$Spice
## almost the same als salinity. skip it

minPAR <- function (fn){
    cast <- subset (physOc, File.Name == fn)
    PARscl <- cast$PAR.Irradiance / max (cast$PAR.Irradiance, na.rm = TRUE)
    return (min (PARscl))
}
mP <- unlist (mclapply (poSS$File.Name, FUN =minPAR, mc.cores = 4))
print (quantile (mP, probs = seq (0.8, 0.95, by = 0.05), na.rm = TRUE))
rm (mP)
thresPAR <- function (fn){
    ## depth at which PAR is 1% of surface (max) PAR
    Pperc <- 0.01
    Pperc <- 0.05
    cast <- subset (physOc, File.Name == fn)
    PARscl <- cast$PAR.Irradiance / max (cast$PAR.Irradiance, na.rm = TRUE)
    if (length (which (PARscl < Pperc)) == 0){
        return (NA)
    }else{
            thresDepth <- cast$Depth.saltwater..m. [min (which (PARscl < Pperc))]
            return (thresDepth)
    }
}
poSS$PARdepth5p<- unlist (mclapply (poSS$File.Name, FUN = thresPAR, mc.cores = nCPUs))
rm (thresPAR, minPAR)
## poSS$PARsum <- aggregate (PAR.Irradiance~File.Name, data = physOc
##                      , FUN = sum)$PAR.Irradiance # sum or mean? 
## poSS$PARave <- aggregate (PAR.Irradiance~File.Name, data = physOc
##                      , FUN = mean)$PAR.Irradiance # sum or mean? 
poSS$Fluorescence <- aggregate (Fluorescence_mg_m3~File.Name, data = physOc
                              , FUN = sum)$Fluorescence_mg_m3
minO2 <- aggregate (Oxygen_SBE.43..mg.l.~File.Name, data = physOc, FUN = min)
poSS$minO2 <- minO2$Oxygen_SBE.43..mg.l. [match (poSS$File.Name, minO2$File.Name)] # to deal with NAs in O2 data.
O2perc <- aggregate (O2perc~File.Name, data = physOc, FUN = mean)
poSS$O2perc <- O2perc$O2perc [match (poSS$File.Name, O2perc$File.Name)]
rm (minO2, O2perc)

Require ("parallel")
wcStab <- function (fn){
    ## calculate water column stability for a given File.Name
    uLay <- c (0,3)
   # lLay <- c (30,35)
    cast <- subset (physOc, File.Name == fn)
    lLay <- c (-5, 0) + max (cast$Depth.saltwater..m.) # lowest 5 m instead of fixed depth
                                        # some casts only 2 or 5 m deep -- then what?    
    uDens <- mean (subset (cast, (uLay [1] < Depth.saltwater..m.) &
                                 (Depth.saltwater..m. < uLay [2]), na.rm = TRUE)$Density_sigma.theta.kg.m.3)    
    lDens <- mean (subset (cast, (lLay [1] < Depth.saltwater..m.) &
                                 (Depth.saltwater..m. < lLay [2]), na.rm = TRUE)$Density_sigma.theta.kg.m.3)
    stabIdx <- (lDens - uDens)/(mean (lLay) - mean (uLay))
    stabIdx <- ifelse (is.infinite (stabIdx), NA, stabIdx)
    return (stabIdx)
#    return ((lDens - uDens)/(mean (lLay) - mean (uLay)))
#     return (lDens - uDens)
}
poSS$stability <- unlist (mclapply (poSS$File.Name, FUN = wcStab, mc.cores = nCPUs))
summary (poSS$stability)
Require ("oce")
wcStab2 <- function (fn){
    ## similar to http://www.sciencedirect.com/science/article/pii/S0967063711000884 !
    ## \citep{Bourgain:2011}
    cast <- subset (physOc, File.Name == fn)
        ctd <- with (cast, as.ctd (Salinity_PSU, Temperature_ITS90_DegC
                                 , Pressure..Strain.Gauge..db.))
    N2 <- swN2 (ctd, derivs = "smoothing")
    return (mean (N2))
}
poSS$stability2 <- unlist (mclapply (poSS$File.Name, FUN = wcStab2, mc.cores = nCPUs))
rm (wcStab2)
Require ("oce")
## physOc$N2 <- median, max, mean N2 in deep-water section -- this should be a
## good indicator of presence of deep-water salinity gradient
deepPyc <- function (fn){
    cast <- subset (physOc, File.Name == fn)
    dThres <- 30
    if (max (cast$Depth.saltwater..m.) < dThres){
        return (NA)
    }else{
        ## midDens<-mean(subset(cast,40<Depth.saltwater..m.)&(Depth.saltwater..m.<50))
        ## depDens<-mean(subset(cast,80<Depth.saltwater..m.)&(Depth.saltwater..m.<200))
        ## return (depDens - midDens)
        ## use max N2 of smoothed CTD object
        ctd <- with (cast, as.ctd (Salinity_PSU, Temperature_ITS90_DegC
                                 , Pressure..Strain.Gauge..db.))
        N2 <- swN2 (ctd, derivs = "smoothing")
        dPC <- max (subset (N2, cast$Depth.saltwater..m. < dThres), na.rm = TRUE)
        dPC <- ifelse (is.na (dPC), 0, dPC) # shouldn't need that? [unless sensor bad]
        return (dPC)
    }
}
poSS$deepPyc <- unlist (mclapply (poSS$File.Name, FUN = deepPyc, mc.cores = nCPUs))
rm (deepPyc)
pcDepth <- function (fn){
    ## also see https://saltydrip.wordpress.com/tag/halocline/  (derivative of spline)
    ## same code here? http://dankelley.github.io/r/2014/01/11/inferring-halocline-depth.html
    cast <- subset (physOc, File.Name == fn)
        ctd <- with (cast, as.ctd (Salinity_PSU, Temperature_ITS90_DegC
                                 , Pressure..Strain.Gauge..db.))
    N2 <- swN2 (ctd, derivs = "smoothing")
    return (cast$Depth.saltwater..m. [which.max (N2)])
}
poSS$pcDepth <- unlist (mclapply (poSS$File.Name, FUN = pcDepth, mc.cores = nCPUs))
rm (pcDepth)
tRange <- function (tstmp){
    ## look up tidal range for a given date for Seldovia
    ## use this as quantitative measure of neap vs spring tide
    Require ("oce")
    ## also require that webtide has been installed! See XXX
    ## Foreman, M.G.G., W.R. Crawford, J.Y. Cherniawsky, R.F. Henry and M.R. Tarbottom. 2000. A high-resolution assimilating tidal model for the northeast Pacific Ocean. Journal of Geophysical Research. 105: 28,629-28,652.
## see http://www.bio.gc.ca/science/research-recherche/ocean/webtide/nepacific-nepacifique-en.php
    tid <- webtide ("predict", lat = 59.438889, lon = -151.7125
                  , time = seq (tstmp -12*3600, tstmp + 12*3600, by = 1800) # per station or per day? 
                  , plot = FALSE)
    return (diff (range (tid$elevation)))
}
Require ("rtide")
tRange <- function (tstmp){
    ## uses NOAA tide table data
    ## only available for US, but 8x faster than webtide (with oce)
    Require ("rtide")    
    kasbay <- tide_stations ("Kasitsna.*")    
    timetable <- data.frame (Station = kasbay, DateTime = seq (tstmp -12*3600, tstmp + 12*3600
                                                             , by = 600)
                             , stringsAsFactors = FALSE)
    ## return (diff (range (timetable$DateTime)))
    tid <- tide_height_data (timetable)
    return (diff (range (tid$TideHeight)))
}
## this step takes a while! [approx 5 min, depending on computer]
if (.Platform$OS.type == "unix"){
    poSS$tideRange <- unlist (mclapply (poSS$timeStamp, FUN = tRange, mc.cores = nCPUs))
}else{
    poSS$tideRange <- unlist (lapply (poSS$timeStamp, FUN = tRange))
}
## rerun tideRange
poSS$tideRange <- as.numeric (poSS$tideRange)
if (any (is.na (poSS$tideRange))){
    for (i in which (is.na (poSS$tideRange))){
        print (poSS$timeStamp [i])
        if (!is.na (poSS$timeStamp [i])){
            poSS$tideRange [i] <- tRange (poSS$timeStamp [i])
            print (poSS$tideRange [i])
        }else{
            print (poSS [i,])
        }
    }
}
rm (tRange, wcStab, i, deepM)
## tidal phase
tPhase <- function (tstmp, lat, lon){
    ## return radians degree of tidal phase during cast
}
Require (suncalc)
poSS$sunAlt <- with (poSS, getSunlightPosition (data = data.frame (date = timeStamp, lat = latitude_DD, lon = longitude_DD)))$altitude # , keep = "altitude")) -- in radians
## Require (oce)
## poSS$sunAlt <- with (poSS, sunAngle(timeStamp, longitude = longitude_DD, latitude = latitude_DD, useRefraction = FALSE)
is.na (poSS$PARdepth1p) <- poSS$sunAlt < 0
is.na (poSS$PARdepth1p) <- poSS$sunAlt < 0




#######################
## troubleshoot poSS ##
#######################

# if (1){
    ## troubleshoot densities!
    badDens <- physOc$File.Name [which (physOc$Density_sigma.theta.kg.m.3 > 100)]
    badDens <- physOc$File.Name [which (physOc$Density_sigma.theta.kg.m.3 < 0)]
    badStrat <- poSS$File.Name [which (poSS$stability < -500)]
    # 2012_02-14_T9_S10_cast273
    # 2016_02-14_T3_S07_cast106
    rm (badDens, badStrat)


## test tracer for deep-water origin:
## compare poSS variables between
## 2013_03-15_T9_S08_cast055  (distinct picnocline at 65 m, below sal = 31.7)
## 2013_04-19_T6_S20_cast067  (mixed from 20 m down, although with weird temperature signal)
## 2013_04-19_T6_S23_cast064  (mixed to 100 m, then faint picnocline)
## 2013_04-20_T7_S12_cast093  (mixed!)
## 2013_07-21_T6_S23_cast055  (picnocline at 30 m and 160 m, shear inbetween)

## "2013_03-15_T9_S08_cast055
## 2013_04-19_T6_S20_cast067
## 2013_04-19_T6_S23_cast064
## 2013_04-20_T7_S12_cast093
## 2013_07-21_T6_S23_cast055
## " -> testF
## read.csv (testF)



save.image ("~/tmp/LCI_noaa/cache/troublesPO.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/troublesPO.RData")


## bad geographic positions: on land, too far south, ..
## calc distance between actual and planned station position
SCo <- stn[match (poSS$Match_Name, stn$Match_Name), names (stn) %in% c("Lon_decDegree"
                                                                     , "Lat_decDegree")]
SCo <- as.matrix (cbind (SCo, cbind (poSS$longitude_DD, poSS$latitude_DD)))
Require ("fields")
StDis <- sapply (1:nrow (SCo), FUN = function (i){
    rdist.earth (matrix (SCo [i,1:2], nrow = 1), matrix (SCo [i,3:4], nrow = 1), miles = FALSE)
})
StDis <- ifelse (is.na (StDis), 0, StDis) # NAs are being weird, ignore them! 
StDisA <- data.frame (poSS$File.Name, StDis)[StDis > 1,]
StDisA [order (StDisA$StDis, decreasing = TRUE),]

poSS$lonM <- SCo [,1] # use this farther down to map distance off station
poSS$latM <- SCo [,2] 
poSS$distOff <- StDis

print (subset (poSS, poSS$distOff > 5)[,names (poSS) %in% c("File.Name", "Match_Name", "distOff"
                                , "longitude_DD","latitude_DD"
                                 ,"lonM", "latM")])

## based on below map, only stations on transect 9 are of concern
## print-out T9 for everything off by more than 500 m
b9 <- poSS [grep ("^9_", poSS$Match_Name),]
b9 <- rbind (b9, poSS [grep ("^4_", poSS$Match_Name),])
b9 <- subset (poSS, distOff > 1)        # 500 m

write.csv (b9 [order (b9$distOff, decreasing = TRUE)
              ,names (b9) %in% c("File.Name", "Match_Name", "distOff"
                                , "longitude_DD","latitude_DD"
                                 ,"lonM", "latM")]
           , file = "~/tmp/LCI_noaa/media/badPos.csv")
rm (b9)

## replace any/everything that's more than 1 km off
## poSS$longitude_DD <- ifelse (StDis > 1
##                             ,SCo [,1]
## #                             stn$Lon_decDegree [match (poSS$Match_Name, stn$Match_Name)]
##                            , poSS$longitude_DD)
## poSS$latitude_DD <- ifelse (StDis > 1
##                             , SCo [,2]
## #                           , stn$Lat_decDegree [match (poSS$Match_Name, stn$Match_Name)]
##                            , poSS$latitude_DD)
rm (SCo, StDis, StDisA)



## plot ALL CTD stations -- now in anaCTD.R

head (poSS$tideRange)
poSS$tideRange <- as.numeric (poSS$tideRange)
poSS$File.Name [which (is.na (poSS$tideRange))]
poSS$File.Name [which (is.na (poSS$stability))]
poSS$File.Name [which.min (poSS$stability)]
length (which (0 > poSS$stability))
                                        # poSS$File.Name [which (0 > poSS$stability)]
# }





print (summary (poSS))
write.csv (poSS, file = "~/tmp/LCI_noaa/media/castTable.csv"
           , row.names = FALSE, na = "NA", quote = FALSE)
##  write meta-data separately
save.image ("~/tmp/LCI_noaa/cache/sampleTable.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/sampleTable.RData")


if (0){## aggregate poSS to SampleID file (one cast per day) -- necessary = ??
poID <- poSS [!duplicated (poSS$SampleID),1:8]
# poID <- subset (poID, !is.na (poID$SampleID)) # needed?? 
poAg <- aggregate (as.formula (paste ("cbind ("
                                    , paste (names (poSS)[10:ncol (poSS)], collapse = ",")
                                    , ")~SampleID"))
                   , FUN = mean, na.rm = TRUE, data = poSS)
poID <- cbind (poID, poAg [match (poID$SampleID, poAg$SampleID),2:ncol (poAg)]) # avoid duplication of SampleID
## cut out tidal-range here
poID <- poID [,-which (names (poID) == "tideRange")]
rm (poAg, poSS)
poSS <- poID
}


rm (physOc)                             # no needed any more, poSS takes it place



###################
## phytoplankton ##
###################

phyp <- read.csv ("~/GISdata/LCI/KBL-Phytoplankton-Feb2017.csv")
phyp <- cbind (Match_Name = trimws (phyp$Sampling.location), phyp)
phyp$Match_Name <- gsub ("Transect\\s([1-9]),\\sStation\\s([0-9]+)", "\\1_\\2", phyp$Match_Name)
# phyp$Match_Name <- gsub ("\\s([A-D])$", "_\\1", phyp$Match_Name) # space to underscore
phyp$Match_Name <- gsub ("\\sBay", "", phyp$Match_Name) # del Cove
phyp$Match_Name <- gsub ("\\sCove", "", phyp$Match_Name) # del Bay
phyp$Match_Name <- gsub ("\\sLab", "", phyp$Match_Name) # del Lab

phyp$Match_Name <- gsub ("^Kachemak\\s", "AlongBay_", phyp$Match_Name)
phyp$Match_Name <- gsub ("^China\\sPoot", "ChinaPoot", phyp$Match_Name)
phyp$Match_Name <- gsub ("\\s", "_", phyp$Match_Name) # space to underscore


levels (factor (phyp$Match_Name [which (is.na (match (phyp$Match_Name, stn$Match_Name)))]))
phyp <- cbind (stn [match (phyp$Match_Name
                         , stn$Match_Name), which (names (stn) %in% c("Lon_decDegree", "Lat_decDegree"))]
               , timeStamp = as.POSIXlt (phyp$Date)
             , phyp)
names (phyp)[1:2] <- c("lon", "lat")

## add: month, year, SampleID
phyp <- cbind (SampleID = paste (phyp$Match_Name, " "
                         , format (phyp$timeStamp, format = "%Y-%m-%d", usetz = FALSE)
                         , sep = "")
               , month = as.numeric (format (phyp$timeStamp, format = "%m"))
             , year = as.numeric (format (phyp$timeStamp, format = "%Y"))
             , season = Seasonal (format (phyp$timeStamp, format = "%m"))
               , phyp)



#################
## zooplankton ##
#################

## zoop <- read.csv ("~/GISdata/LCI/Kachemak\ Bay\ Zooplankton\ Masterfile_26Apr16_ProcessedData.csv")
zoop <- read.csv ("~/GISdata/LCI/Kachemak\ Bay\ Zooplankton\ Masterfile_26Apr16_RawData.csv"
                  , as.is = TRUE)
## Require ("XLConnect")
## stn <- readWorksheetFromFile ("~/GISdata/LCI/MasterStationLocations.xlsx", sheet = 1)
## stn <- subset (stn, !is.na (Lon_decDegree))

## georeference zoop table
# zoop$Transect <- ifelse (zoop$Station == "Sadie C", "subbay", zoop$Transect)
zoop$Time <- ifelse (zoop$Time == "", "12:00", zoop$Time)
zoop <- cbind (Match_Name = paste (zoop$Transect, zoop$Station, sep = "_")
               , timeStamp = as.POSIXlt (paste (zoop$Date, zoop$Time)
                                       , format = "%d-%b-%y %H:%M")
             , zoop)
zoop$Match_Name <- as.character (zoop$Match_Name)
zoop$Match_Name <- ifelse (zoop$Station == "Sadie C", "Sadie_C", zoop$Match_Name)
zoop$Match_Name <- ifelse (zoop$Transect == "TKBay", "Tutka_A", zoop$Match_Name)
zoop$Match_Name <- gsub ("^KB_", "AlongBay_", zoop$Match_Name)
zoop$Match_Name <- gsub ("Peterson Bay_", "Peterson_", zoop$Match_Name, fixed = TRUE)
zoop$Match_Name <- gsub ("^_$", "Halibut_B", zoop$Match_Name) # or A,C?
                                        # no matching CTD data on that date
refN <- match (zoop$Match_Name, stn$Match_Name)
## zoop$Match_Name [is.na (refN)]    # bad zoop stations that don't match master list
# add geographic coordinates from stn
zoop <- cbind (stn[refN, match (c ("Lon_decDegree", "Lat_decDegree"), names (stn))]
               , isoDate = strptime (zoop$Date, format = "%d-%b-%y") # output with tz? 
             , zoop)
rm (refN)
zoop <- cbind (SampleID= paste (zoop$Match_Name, " "
                         , format (zoop$timeStamp, format = "%Y-%m-%d", usetz = FALSE)
                         , sep = "")
               ##             , SampleID_H= paste (zoop$Match_Name, " ", format (zoop$timeStamp, format = "%Y-%m-%d_%H", usetz = FALSE), sep = "")
               , season = Seasonal (format (zoop$timeStamp, "%m"))
               , zoop)
if (any (is.na (zoop$Lon_decDegree))){
    zoop <- zoop [!is.na (zoop$Lon_decDegree),] # cut out samples without coordinates
}

#### zoop <- zoop [,match (zoop$SampleID, poSS$SampleID)]



save.image ("~/tmp/LCI_noaa/cache/fileDump.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/fileDump.RData")

allZoo <- aggregate (RTotal ~ SampleID, data = zoop, FUN = sum)
# samp <- unique (data.frame (zoop [,1:7]))
allZoo <- allZoo [order (allZoo$SampleID),]
## print (allZoo)
rm (allZoo)

## standardize samples by volumne to get density
zooL <- with (zoop, data.frame (SampleID, Species, RTotal))
zoolF <- aggregate (RTotal~SampleID+Species, zooL, FUN = sum) # aggregate multiple entries
zooC <- reshape (zoolF, v.names = "RTotal", timevar = "Species"
                 , idvar = "SampleID", direction = "wide")
rm (zooL, zoolF)

## causes corruption, -- fix Mueller's larvae in source file  XXX  
names (zooC) <- gsub ("^RTotal.", "", names (zooC))
for (i in 2:ncol (zooC)){
    zooC [,i] <- ifelse (is.na (zooC [,i]), 0, zooC [,i])
}

rownames (zooC) <- zooC [,1]
zooC <- zooC [,2:ncol (zooC)]
if (any (colSums (zooC) < 1)){          # doesn't seem to be the case
    warning ("empty species!")
    zooC <- zooC [,colSums (zooC) > 0]
}
zooC <- zooC [order (row.names (zooC)),]

## csv file of zooC community matrix to Kim
print (row.names (zooC))
write.csv (zooC, "~/tmp/LCI_noaa/media/community.csv")
print (summary (zooC))

## merge zoop with poSS
zooCenv <- zoop [match (row.names (zooC), zoop$SampleID)
                ,c (1:12, which (names (zoop) == "Water.Sampled..m3."))]
zooCenv$year <- as.numeric (format (zooCenv$timeStamp, "%Y"))

rm (zoop)
## check that poSS data is available for every zooCenv sample
summary (zooC$SampleID %in% poSS$SampleID)


##  spplot (zoop, "RTotal")
## cluster analysis and/or DCA of along-bay vs transect 9




save.image ("~/tmp/LCI_noaa/cache/zoopEnd.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopEnd.RData")


##############
## seabirds ##
##############

stnB <- c (1,5,10,20,50)*1e3           # buffer -- at different scales
stnB <- 10e3                           # buffer -- 10 km

# pj4str <- "+proj=lcc +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +datum=WGS84 +units=m +no_defs +ellps=WGS84"
LLprj <- "+proj=longlat +datum=WGS84 +ellps=WGS84"


## bounding-box for LCI
lonL <- c(-154.2,-151.2)
latL <- c(58.8,60.6)


Require ("sp"); Require ("rgdal"); Require ("rgeos") # for gBuffer


spTran <- function (x, p4){
    # proj4string (x) <- CRS ("+proj=longlat +datum=WGS84 +ellps=WGS84")
    Require ("rgdal")
    suppressWarnings (y <- spTransform (x, CRS (p4)))
    return (y)
}


## find all seabird observations within XX km of station at the same date as zoop station
load ("~/tmp/NPPSDv2countW_-1.RData")
NPPSD2 <- subset (NPPSD2, tArea > 0)
NPPSD2 <- subset (NPPSD2 # subset to LCI to reduce size and speed up things
                , (lonL [1]-5 < lon) & (lon < lonL [2]+5) & (latL [1]-5 < lat) &
                  (lat < latL [2]+5)
                  )


## geographically overlay seabirds and physical oceanography stations
coordinates (stn) <- ~Lon_decDegree+Lat_decDegree
coordinates (poSS) <- ~longitude_DD+latitude_DD
coordinates (phyp) <- ~lon+lat
coordinates (zooCenv) <- ~Lon_decDegree+Lat_decDegree
coordinates (NPPSD2) <- ~lon+lat
proj4string (stn) <- LLprj
proj4string (poSS) <- LLprj
proj4string (phyp) <- LLprj
proj4string (zooCenv) <- LLprj
proj4string (NPPSD2) <- LLprj



# coastline from gshhs
Require ("maptools")
system (paste ("unzip ~/GISdata/data/coastline/gshhg.zip -d", (tD <- tempdir())))
coast <- getRgshhsMap (paste (tD, "/gshhs_h.b", sep = "") # portable??
                     , xlim = lonL+360+c(-10,10), ylim = latL+c(-5,5)
                     , verbose = FALSE)
unlink (tD, TRUE); rm (tD)
## coast <- getRgshhsMap ("/Users/martin/GISdata/data/coastline/gshhs/gshhs_h.b" # portable??
##                      , xlim = lonL+360+c(-10,10), ylim = latL+c(-5,5)
##                      , verbose = FALSE)

save.image ("~/tmp/LCI_noaa/cache/mapPlot.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/mapPlot.RData")

badPO <- !is.na (over (poSS, coast))
PDF ("testSamplesites.pdf")
plot (coast, col = "beige", axes = TRUE, xaxs = "i", yaxs = "i", xlim = lonL, ylim = latL
      )
# plot (NPPSD2, pch = 1, add = TRUE)
plot (stn, col = "red", pch =2, add = TRUE)
plot (poSS, col = ifelse (badPO, "red", "yellow")
    , pch = ifelse (badPO, 19, 1)
    , cex = ifelse (badPO, 2, 1), add = TRUE)
plot (zooCenv , col = "blue", pch = 3, add = TRUE)
## plot (coast, col = "beige", add = TRUE)
dev.off()

PDF ("badPositions.pdf")
## new plot -- bad positions
plot (coast, col = "beige", axes = TRUE, xaxs = "i", yaxs = "i", xlim = lonL, ylim = latL)
for (i in (1:nrow (poSS))[order (poSS$distOff, decreasing = TRUE)]){
    lines (c (poSS$longitude_DD [i], poSS$lonM [i])
         , c (poSS$latitude_DD [i], poSS$latM [i])
         , lwd = sqrt (poSS$distOff [i]) * 2
         , col = ifelse (poSS$distOff [i] > 1, "red", "blue")
           )
}

dev.off()
## subset (poSS, badPO)$File.Name --- keep data.frame version for export
if (any (badPO)){
##     poSS$latitude_DD <- ifelse (badPO
##                               , stn$Lat_decDegree [match (poSS$Match_Name, stn$Match_Name)]
##                               , poSS$latitude_DD)
##     poSS$longitude_DD <- ifelse (badPO
##                                , stn$Lon_decDegree [match (poSS$Match_Name, stn$Match_Name)]
##                                , poSSOc$longitude_DD)
poSS <- subset (poSS, !badPO)
}
rm (badPO)
                                        # still have 16 records of 3_1 in poSS. lost where?? 
                                        # nrow (subset (poSS, Match_Name == "3_1"))




## bathymetry from AOOS, Zimmerman
Require ("raster")
Require ("rgdal")
## system ("cd /Users/martin/GISdata/LCI/ && unzip Cook_bathymetry_grid.zip")
bath <- raster ("/Users/martin/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
                                        # use getValues() to ditch source file?
## tD <- tempdir()
## system (paste ("unzip /Users/martin/GISdata/LCI/Cook_bathymetry_grid.zip -d", tD))
## bath <- raster (paste (tD, "/ci_bathy_grid/w001001.adf", sep = ""))

## read bathymetry from netcdf rather than proprietary ArcGIS raster
## Require ("ncdf4")
## system ("gunzip ~/GISdata/LCI/ci_bathy_20090101.nc.gz")
## bNC <- nc_open ("~/GISdata/LCI/ci_bathy_20090101.nc")
## bath <- ncvar_get (bNC)
## rm (bNC)
## system ("gzip ~/GISdata/LCI/ci_bathy_20090101.nc.gz")


bathCont <- rasterToContour (bath, levels = c(50, 100, 200, 500))
## system (paste ("rm -r", tD)); rm (tD)


ch <- chull(coordinates (NPPSD2))
chCoor <- coordinates (NPPSD2)[c(ch, ch[1]), ]  # closed polygon
sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(chCoor)), ID=1))
                         , proj4string = CRS (proj4string (NPPSD2)))
grd <- spsample (sp_poly, n = 100^2, "regular") # ok, but grid size somewhat mysterious
## grd <- makegrid (sp_poly, cellsize = 1e3)
## coordinates (grd) <- ~x1+x2
## proj4string (grd) <- CRS (proj4string (NPPSD2))
## gridded (grd) <- TRUE
rm (ch, chCoor, sp_poly)

pr <- proj4string (bath)

stn <- spTran (stn, pr)
poSS <- spTran (poSS, pr)
zooCenv <- spTran (zooCenv, pr)
NPPSD2 <- spTran (NPPSD2, pr)
grd <- spTran (grd, pr)
coast <- spTran (coast, pr)
bathCont <- spTran (bathCont, pr)
rm (spTran, pr)

## AOOS model data:
## - Tidal current speed
if (0){
    library (ncdf4)
    url <- "http://thredds.aoos.org/thredds/dodsC/NOAA_CSDL_ROMS.nc?lon[0:1:787][0:1:145],lat[0:1:787][0:1:145],u[0:1:532][0][0:1:787][0:1:145],v[0:1:532][0][0:1:787][0:1:145],time[0:1:532]" # 500 MB? -- only need climatology; how to do that?
    ## or could actually look up individual pixels 
    
con <- nc_open (url)
print (con)
lon <- ncvar_get (con, "lon")
lat <- ncvar_get (con, "lat")
u <- ncvar_get (con, "u")
v <- ncvar_get (con, "v")
}
    
## match stn and birds at different levels of buffer
# for (i in length (stnB)){
i <- 1
    lBuff <- gBuffer (stn, width = stnB [i], byid = TRUE)
    Require ("parallel")
    findBirds <- function (x){
        stnBuf <- over (NPPSD2, lBuff [x,])$Match_Name
        birdD <- cbind (Match_Name = stnBuf, NPPSD2@data)
        ##  what if stnBuf = "NULL" ? 
        stnBird <- subset (birdD, !is.na (stnBuf))
        ## for (k in 1:ncol (stnBird)){
        ##     if (class (stnBird [,k]) == "factor"){
        ##         stnBird [,k] <- as.character (stnBird [,k])
        ##     }
        ## }
        return (stnBird)
    }
    xo <- mclapply (1:length (lBuff), FUN = findBirds
                  , mc.cores = nCPUs)

## save.image ("~/tmp/LCI_noaa/cache/birdRef.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/birdRef.RData")

## birds <- do.call (rbind, xo)  # 20 s

require (dplyr)
birds <- bind_rows (xo)                 # 10 s
## difftime (Sys.time(), sP)
rm (sP)
print (warnings())
#}

birds <- with (birds, cbind (SampleID = paste (Match_Name
                                             , format (startTime, format = "%Y-%m-%d"
                                                     , usetz = FALSE))
                           , birds))
birdS <- subset (birds, birds$SampleID %in% poSS$SampleID) # specific to sample time
birdS <- birds []    ### what's this for??? FIX this XXX  

rm (j,i,xo, findBirds, lBuff, birds)
rm (stnB)

## birdS: 
## these are specific to samples! Samples may not be independent, i.e. may contain overlapping
## bird observations, if sample sites within buffer distance of each other!  Do NOT use this for
## spatial interpolation.


## define grid for interpolation
if (0){                                 # old version
    ch <- chull(coordinates (NPPSD2))
    chCoor <- coordinates (NPPSD2)[c(ch, ch[1]), ]  # closed polygon
    sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(chCoor)), ID=1))
                             , proj4string = CRS (proj4string (NPPSD2)))
    grd <- spsample (sp_poly, n = 100^2, "regular") # ok, but grid size somewhat mysterious
    ## grd <- makegrid (sp_poly, cellsize = 1e3)
    ## coordinates (grd) <- ~x1+x2
    ## proj4string (grd) <- CRS (proj4string (NPPSD2))
    ## gridded (grd) <- TRUE
    rm (ch, chCoor, sp_poly)
}


## fix up poSS to essential variables
poSS@data <- poSS@data [,-which ((names (poSS) %in% c("lonM", "latM", "distOff")))]

## main data sets now:
#  poSS, zooC (and zooCenv), birdS, NPPSD2, grd, coast, bath



print (ls())
save.image ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")


cat ("\n\n#\n#\n#", format (Sys.time(), format = "%Y-%m-%d %H:%M"
                          , usetz = FALSE)
, " \n# \n# End of dataSetup.R\n#\n#\n")
## EOF
