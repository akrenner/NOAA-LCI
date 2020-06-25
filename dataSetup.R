#!/usr/bin/env Rscript

## (c) Martin Renner
## licence: GPL-3

## NOAA/KBRR LCI study
## read in plankton and oceanographic data
## geographically link with seabird data
## interannual comparison in relation to SST


rm (list = ls())
## CAREFUL with this!!
# system ("rm -r ~/tmp/LCI_noaa/")
# unlink ("~/tmp/LCI_noaa/", recursive = TRUE)



## file structure:
## source files in ~/GISdata/LCI/
GISF <- "~/GISdata/LCI/"
tmpF <- "~/tmp/LCI_noaa/"
mediaF <- paste0 (tmpF, "media")
cacheF <- paste0 (tmpF, "cache")
dirL <- c (GISF=GISF, tmpF=tmpF, mediaF=mediaF, cacheF=cacheF)
rm (GISF, tmpF, mediaF, cacheF)
x <- lapply (dirL, dir.create, showWarnings = FALSE, recursive = TRUE); rm (x)

## check processed 'rawish' files on workspace for bad profiles, see whether these look ok
## deep-water stability:  50m vs bottom : where and when does this occur? (difference, not ratio)
## GAK-1 50 m sensor:  time series compared to LCI

## better solution:
## put all this code into a package
## make required packages a dependency -> they will install automatically, if missing, during package install



## hard-code location to AK -- how?
# system ("mkdir -p ~/tmp/LCI_noaa/cache")
# dir.create("~/tmp/LCI_noaa/cache/", showWarnings = FALSE, recursive = TRUE)
# dir.create("~/tmp/LCI_noaa/media/2019/", showWarnings = FALSE, recursive = TRUE)


############################
## define basic functions ##
############################

PDF <- function (fN, ...){
#    pdf (paste0 ("~/tmp/LCI_noaa/media/", fN, ".pdf"), ...)
  # if (width < 50){
  #   width <- 100*width
  #   height <- 100*height
  # }else{
  #   width <- 480; height <- 480
  # }
     png (paste0 ("~/tmp/LCI_noaa/media/", fN, ".png"), ...) # automatically adjust scale?
}

if (!require("pacman")) install.packages("pacman"
  , repos = "http://cran.fhcrc.org/", dependencies = TRUE)
# pacman::p_load(package1, package2, package_n)
# pacman::p_load ("parallel")
Require <- pacman::p_load


Seasonal <- function (month){           # now using breaks from zoop analysis -- sorry for the circularity
    month <- as.numeric (month)
    cut (month, breaks = c(-13,0,2,4,8,10, 25)
            , labels = c ("fall", "winter", "spring", "summer", "fall", "winter"))
    ## cut (month, breaks = c(-13,3,6,9,12, 25)-0
    ##         , labels = c ("winter", "spring", "summer", "fall", "winter"))
}


Require ("parallel")
nCPUs <- detectCores()
if (.Platform$OS.type != "unix"){
    nCPUs <- 1
}
# require (doParallel)
# cl <- makeCluster(nCPUs)
# registerDoParallel(cl)
# ## foreach (i = 1:3) %dopar% sqrt (i)
# stopCluster(cl)



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


### read cnv files for turbidity

if (0){                                 # read in cnv files for turbidity
  Require (oce)
  ## read in processed files of individual CTD casts
  ## --- abandon this for now. Still in dataSetup_1.R, should there ever be a need to go back to it.

  fN <- list.files ("~/GISdata/LCI/CTD/6\ DERIVE/", ".cnv", full.names = FALSE)
#  fN <- list.files ("~/GISdata/LCI/CTD")
  print (length (fN))

  readCNV <- function (i){
    Require (oce)
    cN <- fN [i]
    ctdF <- suppressMessages (read.ctd (paste ("~/GISdata/LCI/CTD/6\ DERIVE/", cN, sep = "")))# NAs introduced by coercion... = ?
    ## cat (i, " ")
    ## if (i %% 10 == 0){cat ("\n")}

    ## if ("station" %in% names (ctdF@metadata)) if (grepl ("Air", ctdF@metadata$station)){
    ##     cat ("Air Cast:", i, fN [i], "\n")
    ##     return()
    ## }

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
    # ctdF <- ctdTrim (ctdF, method = "sbe") # there's also a method seabird
    # could/should specify min soak times, soak depth
    #    41, 2012_05-02_T3_S01_cast026.cnv fails at ctdTrim "sbe"


    ## aggregate
    # ctdF <- initializeFlagScheme(ctdF, name = "ctd")
    # ctdF <- handleFlags (ctdF)
#    ctdF <- handleFlags (ctdF, flags = defaultFlags(ctdF))
    ctdF <- ctdDecimate (ctdF, p = 1, method = "boxcar", e = 1.5) # later?

    if ("turbidity" %in% names (ctdF@data)){ # some called "turbidity", not "upoly"
      names (ctdF@data)[which (names (ctdF@data) == "turbidity")] <- "upoly"
    }
    if (!"fluorescence" %in% names (ctdF@data)){
      ctdF@data$fluorescence <- rep (NA, length (ctdF@data$sigmaTheta))
      # cat (gsub ("/Users/martin/GISdata/LCI/CTD/6 DERIVE//", "", fN [i]), "\n")
    }

    meta <- function (x){rep (x, length (ctdF@data$sigmaTheta))}

    cDF <- data.frame (File.Name = gsub (".cnv$", "", fN [i])
                       ## , latitude = meta (ctdF@metadata$latitude)
                       ## , longitude = meta (ctdF@metadata$longitude)
                       , timestamp = meta (ctdF@metadata$startTime)
                       , depth_bottom = meta (ctdF@metadata$waterDepth)
                       ## , transect = meta (ctdF@metadata$station)
                       ## , Match_Name = meta (ctdF@metadata$station)
                       #, CTDserial = meta (ctdF@metadata$serialNumberTemperature)
                       , density = ctdF@data$sigmaTheta
                       , depth = ctdF@data$depth
                       , O2 = ctdF@data$oxygen
                       , par = ctdF@data$par
                       , salinity = ctdF@data$salinity
                       , temperature = ctdF@data$temperature
                       , pressure = ctdF@data$pressure
                       , fluorescence = ctdF@data$fluorescence ## often missing
                       , turbidity = ctdF@data$upoly
                       #                        , conductivity = ctdF@data$conductivity
                       # , depth2 = ctdF@data$depth2
    )
    cDF <- subset (cDF, density > 0)
    return (cDF)
  }


  ## CTD1 <- readCNV (120)                   # Air Cast
  ## CTD1 <- readCNV (520)                   # missing station metadata
  ## for (i in 1:length (fN)){cat (i, "\n"); CTD1 <- readCNV (i)}
  ## CTD1 <- readCNV (235)
  ## CTD1 <- readCNV (236)

  for (k in 1:length (fN)){print (k); readCNV (k)}  ## for troubleshooting

    CTD1 <- mclapply (1:length (fN), readCNV, mc.cores = nCPUs)
  # require (dplyr)
  # CTD1 <- bind_rows (CTD1, id = fN)
  CTD1 <- as.data.frame (do.call (rbind, CTD1))
  rm (fN, readCNV)

  # "bad" in station?
  save.image ("~/tmp/LCI_noaa/cache/CNV1.RData")
  # rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV1.RData")


  ## start-over this part -- read metadata/field notes from KBRR ACCESS database
  Require ("odbc")
  Require ("DBI")
  odbc <- dbConnect (odbc::odbc(), dsn = "MicrosoftAccess")


  ## read in metadata and match based on File.Name
  ## metadata currently harvested from aggregated files.
  ## In future, should be kept from field-notes DB
  mdata <- read.csv ("~/GISdata/LCI/CTD/ctd_metadata_m.csv")

  dim (mdata)
  dim (CTD1)
  head (CTD1$File.Name)
  head (mdata$File.Name)

  summary (as.factor (format (CTD1$timestamp [grep ("_T9_", CTD1$File.Name, invert = FALSE)], "%m")))
  summary (as.factor (format (CTD1$timestamp, "%Y")))

  needCNV <- mdata$File.Name [!mdata$File.Name %in% CTD1$File.Name]
  print (length (needCNV))
  print (length (levels (factor (CTD1$File.Name))))
  print (nrow (mdata))


  xmatch <- match (as.character (CTD1$File.Name), as.character (mdata$File.Name))
  ## finding bad matches
  summary (xmatch)
  badFileNames <- as.character (unique (CTD1$File.Name [which (is.na (xmatch))]))
  length (badFileNames)                   # < 26 -- not too bad

  print (badFileNames)

  mBad <- function (fstr){
    cat ("non-matching File.Names of CNV files\n")
    print (grep (fstr, badFileNames, value = TRUE))
    cat ("\nNames in aggregated\n")
    print (grep (fstr, mdata$File.Name, value = TRUE))
  }

  ## failed to turn-off CTD?  field notes?
  # mBad ("2012_04-26_T9_S")

  ## bad casts?  No metadata available?  Field notes?
  mBad ("2012_05-31")
  # mBad ("2017_12-14_T9_S04south_cast123"


  # mBad ("2012_08-15_AlongBay")

  ## PARTIAL -- bad, del
  # mBad ("2014_03-28_AlongBay")

  ## bad casts repeated, keep both?
  # mBad ("2015_06-26_T9")

  # mBad ("2015_08-14_AlongBay")
  # mBad ("2015_11-03_T4")
  # mBad ("2015_11-04_T3_S03")

  ## missing metadata? field notes?
  # mBad ("2015_11-04")

  ## Station KB10 or KB09?
  # mBad ("2016_06-16_AlongBay")

  # mBad ("2016_01-07_AlongBay")



  # print (summary (mdata))
  # print (summary (CTD1))

  physOc <- data.frame (mdata [xmatch,], CTD1) # watch out for isoTime
  # physOc <- data.frame (mdata [xmatch,2:ncol (mdata)], CTD1)
  rm (xmatch)
  # print (summary (physOc))

  names (physOc)
  testFN <- as.character (physOc$File.Name) == as.character (physOc$File.Name.1)
  summary (testFN)
  physOc <- subset (physOc, testFN, select = -File.Name.1)
  rm (testFN, CTD1, mBad, mdata)
  # print (summary (physOc))

  save.image ("~/tmp/LCI_noaa/cache/CNV2.RData")
  # rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV2.RData")

  names (physOc) <- c ("isoTime",
                       "File.Name", "Date", "Transect", "Station"
                       , "Time", "CTD.serial", "latitude_DD", "longitude_DD"
                       , "Bottom.Depth", "comments"
                       , "timestamp", "depth_bottom" # , "CTDserial"
                       , "Density_sigma.theta.kg.m.3"
                       , "Depth.saltwater..m."
                       , "Oxygen_SBE.43..mg.l."
                       , "PAR.Irradiance"
                       , "Salinity_PSU"
                       , "Temperature_ITS90_DegC"
                       , "Pressure..Strain.Gauge..db."
                       , "Fluorescence_mg_m3"
                       , "turbidity"
  )
  # print (summary (physOc))

  ## link with ACCESS (c) database (migrate enventually), to get actual coordinates



}else{


  ###########################
  ## read aggregated files ##
  ###########################

  fN <- list.files ("~/GISdata/LCI/CTD/4_aggregated/", "_LowerCookInlet_ProcessedCTD.csv", full.names = TRUE)
  fN <- c (fN, list.files ("~/GISdata/LCI/CTD/2017-21/4_ctd-aggregated-files/", "_Aggregatedfiles.csv", full.names = TRUE))

  ## field names in _Aggregatedfiles.csv are not consistent, neither is their order consistent.
  ## reduce all field names to make the consistent, then sort by field names to get order right
  ## algorithm: last one is always O2 saturation. Others should be diagnostic by first 3 letters
  regStr <- "^([a-zA-Z.0]{3})([a-zA-Z0-9_.]+)"

  # agF <- read.csv (fN [4])
  fieldNames <- sapply (1:length (fN), FUN = function (x){
    names (read.csv (fN [x], na.strings = "N/A"))
  })

  sapply (1:length (fN), FUN = function (x){  # should this go somewhere? just for info or del?
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
      for (j in 1:length (names (physOc))){
        if (!names (physOc)[j] %in% names (agF)){
          print (names (physOc [j]))
          print (fN [i])
          print (names (agF))
        }
      }
      agF <- agF [,match (names (agF),names (physOc))] # fix mixed-up column orders
      physOc <- rbind (physOc, agF)
    }
  }
  rm (agF, i, fN, j)
}

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
# }


## physOc data clean-up
## physOc <- cbind (isoDate = strptime (physOc$Date, format = "%m/%d/%Y"), physOc)
## summary (physOc)

physOc$Time <- ifelse (is.na (physOc$Time), "12:00", as.character (physOc$Time)) # avoid bad timestamps
physOc$Time <- ifelse (physOc$Time == "", "12:00", as.character (physOc$Time)) # avoid bad timestamps

if (!exists ("physOc$isoTime")){
  physOc <- cbind (isoTime = as.POSIXlt (paste (physOc$Date, physOc$Time)
                                         , format = "%m/%d/%Y %H:%M"
                                         #                     , tz = "AKDT")
                                         , tz = "America/Anchorage")
                   , physOc)
}


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
## Require ("XLConnect")
## stn <- readWorksheetFromFile ("~/GISdata/LCI/MasterStationLocations.xlsx", sheet = 1)
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
## zip-up result files
Require ("zip")
# unlink ("~/tmp/LCI_noaa/media/processedCTD_annual.zip", force = TRUE)
wD <- getwd()
setwd ("~/tmp/LCI_noaa/cache/")
zFiles <- list.files ("~/tmp/LCI_noaa/cache/", pattern = "*processedCTD.csv", full.names = FALSE)
zipr ("../media/processedCTD_annual.zip"
     , files = zFiles, recurse = FALSE, include_directories = FALSE)
unlink (zFiles, force = TRUE)
rm (zFiles)
setwd (wD); rm (wD)
### system ("zip -jm ~/tmp/LCI_noaa/media/processedCTD_annual.zip ~/tmp/LCI_noaa/cache/*processedCTD.csv")
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
  if (!is.na (sum (ctd$Density_sigma.theta.kg.m.3[c(1,2)]))){ # bad/missing values Density_sigma.theta.kg.m.3
    if (ctd$Density_sigma.theta.kg.m.3 [1] >
        ctd$Density_sigma.theta.kg.m.3 [2]){
      bCntr <- cntx [1]
    }else{
      bCntr <- numeric()
    }
  }else{bCntr <- numeric()
  }
  return (bCntr)
}
# bCntr <- unlist (mclapply (1:length (levels (physOc$File.Name)), FUN = badDens
#                          , mc.cores = nCPUs))
bCntr <- unlist (lapply (1:length (levels (physOc$File.Name)), FUN = badDens))

cat ("\n\nRemoved first data point from ", length (bCntr), " out of ",
    length (levels (physOc$File.Name)), "CTD casts ("
    , round (length (bCntr)/length (levels (physOc$File.Name))*100)
    ,"%) because surface density \nwas higher than in subsequent samples.\n\n")
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
poSS$Year <- as.numeric (format (poSS$timeStamp, "%Y"))
poSS$month <- as.numeric (format (poSS$timeStamp, "%m"))
poSS$season <- Seasonal (poSS$month)
poSS$SampleID <- with (poSS, paste (Match_Name
                                  , format (timeStamp, format = "%Y-%m-%d"
                                          , usetz = FALSE)
                                   ))   # no need to spec by H
poSS$SampleID_H = with (poSS, paste (Match_Name # some days >1 sample (tide cycle)
                                   , format (timeStamp, format = "%Y-%m-%d_%H", usetz = FALSE)
                                     ))
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
poSS$tideRange <- unlist (mclapply (poSS$timeStamp, FUN = tRange, mc.cores = nCPUs))
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
rm (tRange, i)
## tidal phase
tPhase <- function (tstmp, lat, lon){
    ## return radians degree of tidal phase during cast
}
rm (tPhase)
Require ("suncalc")
poSS$sunAlt <- with (poSS, getSunlightPosition (data = data.frame (date = timeStamp, lat = latitude_DD, lon = longitude_DD)))$altitude # , keep = "altitude")) -- in radians
## Require (oce)
## poSS$sunAlt <- with (poSS, sunAngle(timeStamp, longitude = longitude_DD, latitude = latitude_DD, useRefraction = FALSE)

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
## poSS$deepSal_old <- aggregate (Salinity_PSU~File.Name, data = physOc, FUN = function (x){
##     if (length (x) > 50){mean (x [50:length (x)])}else{NA}
## })$Salinity_PSU
poSS$deepSal <- unlist (mclapply (poSS$File.Name, FUN = dMean, fldn = "Salinity_PSU"
                                , mc.cores = nCPUs))
rm (dMean)
## poSS$aveSpice <- aggregate (Spice~File.Name, data = physOc, FUN = mean)$Spice
## almost the same als salinity. skip it

minPAR <- function (fn){
    cast <- subset (physOc, File.Name == fn)
    PARscl <- cast$PAR.Irradiance / max (cast$PAR.Irradiance, na.rm = TRUE)
    return (min (PARscl))
}
mP <- unlist (mclapply (poSS$File.Name, FUN =minPAR, mc.cores = nCPUs))
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
sAgg <- function (varN, data = physOc, FUN = sum, ...){
    aDF <- aggregate (formula (paste (varN, "File.Name", sep = "~"))
                    , data, FUN, ...)
    return (aDF [match (poSS$File.Name, aDF$File.Name),2])
}
poSS$Fluorescence <- sAgg ("Fluorescence_mg_m3")
poSS$minO2 <- sAgg ("Oxygen_SBE.43..mg.l.")
poSS$O2perc <- sAgg ("O2perc")
if ("turbidity" %in% names (physOc)){
    ## poSS$turbidity <- sAgg ("turbidity", FUN = mean)
    ## print (summary (poSS$turbidity))
    poSS$logTurb <- sAgg ("turbidity", FUN = function (x){mean(log10(x))}
                        , subset = physOc$Depth.saltwater..m. < 20)
}else{
    cat ("\nno turbidity here\n")
}
rm (sAgg)

## derived summary statistics
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
rm (wcStab)
## summary (poSS$stability)
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
is.na (poSS$PARdepth1p) <- poSS$sunAlt < 0
is.na (poSS$PARdepth1p) <- poSS$sunAlt < 0

print (summary (poSS))


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
save.image ("~/tmp/LCI_noaa/cache/sampleTable.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/sampleTable.RData")



###################
## phytoplankton ##
###################

phyp <- read.csv ("~/GISdata/LCI/KBL-Phytoplankton-2020-04.csv")
# phyp <- read.csv ("~/GISdata/LCI/KBL-Phytoplankton-2017-02.csv")
# phyp <- read.csv ("~/GISdata/LCI/KBL-Phytoplankton-2018-10-sorted.csv")
## current 2018 version does not have date
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
               , timeStamp = as.POSIXlt (phyp$Date) ## XXX should have time as well!!! XXX temp!!
               , phyp)
names (phyp)[1:2] <- c("lon", "lat")

## add: month, year, SampleID
# Require ("tidyverse")
Require (magrittr) # for pipe!
trnsct <- strsplit (phyp$Match_Name, "_", fixed = TRUE) %>%
  unlist () %>%
      matrix (ncol =2 , byrow = TRUE)

phyp <- cbind (SampleID = paste (phyp$Match_Name
                                 , format (phyp$timeStamp, format = "%Y-%m-%d", usetz = FALSE))
               , SampleID_H = paste (phyp$Match_Name, format (phyp$timeStamp, format = "%Y-%m-%d_%H", usetz = FALSE))
               , Match_Name = phyp$Match_Name
               , Transect = trnsct [,1]
#               , Station =
               , season = Seasonal(format (phyp$timeStamp, format = "%m"))
               #, isoDate
               , month = as.numeric (format (phyp$timeStamp, format = "%m"))
               , Year = as.numeric (format (phyp$timeStamp, format = "%Y"))
               , season = Seasonal (format (phyp$timeStamp, format = "%m"))
               , phyp)
rm (trnsct)
phyCenv <- phyp
phyCenv <- phyp [,c(1:which (names (phyCenv) == "Sampling.location")
                    , which (names (phyp) == "Total.cell.count"))]
phyC <- phyp [,which (names (phyp) == "Alexandrium.spp.") :
                     which (names (phyp) == "unknown.pennate.diatom")]
## check that phyC are integers?!
sort (apply (phyC, 2, function (x){
  sum (x %% 1)
}))
rm (phyp)



#################
## zooplankton ##
#################

## zoop <- read.csv ("~/GISdata/LCI/Kachemak\ Bay\ Zooplankton\ Masterfile_26Apr16_RawData.csv", as.is = TRUE)
zoop <- read.csv ("~/GISdata/LCI/Kachemak\ Bay\ Zooplankton\ Masterfile_15Dec19_RawData.csv"
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
zoop <- cbind (SampleID= paste (zoop$Match_Name
                         , format (zoop$timeStamp, format = "%Y-%m-%d", usetz = FALSE))
                          # XXX some days are sampled twice for differences in tidal cycle XXX
             , SampleID_H= paste (zoop$Match_Name, format (zoop$timeStamp, format = "%Y-%m-%d_%H", usetz = FALSE))
             , season = Seasonal (format (zoop$timeStamp, "%m"))
             , zoop)
if (any (is.na (zoop$Lon_decDegree))){
    zoop <- zoop [!is.na (zoop$Lon_decDegree),] # cut out samples without coordinates
}

#### zoop <- zoop [,match (zoop$SampleID, poSS$SampleID)]


### fix species names
## fix Mueller's larvae in source!
zoop$Species <- tolower (as.character (zoop$Species))
substring (zoop$Species, 1,1) <- toupper (substring (zoop$Species, 1,1))
print (sort (levels (factor (zoop$Species))))

save.image ("~/tmp/LCI_noaa/cache/fileDump.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/fileDump.RData")

allZoo <- aggregate (RTotal ~ SampleID, data = zoop, FUN = sum) # ouch -- sample ID!!
# samp <- unique (data.frame (zoop [,1:7]))
allZoo <- allZoo [order (allZoo$SampleID),]
## print (allZoo)
rm (allZoo)

## standardize samples by volumne to get density --- NO, don't!
## zooL <- with (zoop, data.frame (SampleID, Species, RTotal)) #, Water.Sampled..m3., Split))
## zoolF <- aggregate (RTotal~SampleID+Species, zooL, FUN = sum) # aggregate multiple entries -- from different tides!!
## zooC <- reshape (zoolF, v.names = "RTotal", timevar = "Species"
##                  , idvar = "SampleID", direction = "wide")
zooL <- with (zoop, data.frame (SampleID_H, Species, RTotal)) # High/Low tide samples on one day
zoolF <- aggregate (RTotal~SampleID_H+Species, zooL, FUN = sum) # aggregate multiple entries -- from different tides!!
zooC <- reshape (zoolF, v.names = "RTotal", timevar = "Species"
                 , idvar = "SampleID_H", direction = "wide")
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
zooC <- zooC [order (row.names (zooC)),] # not naturally ordered; but harmless?

## csv file of zooC community matrix to Kim
## print (row.names (zooC))
write.csv (zooC, "~/tmp/LCI_noaa/media/community.csv")
## print (summary (zooC))


## corrected water volume to use as off-set
## density = count * split / volume
zoop$volSample <- with (zoop, Split * Water.Sampled..m3.)
## merge zoop with poSS
## zooCenv <- zoop [!duplicated (zoop$SampleID_H), c(1:12, which (names (zoop) == "volSample"))]
zooCenv <- zoop [match (row.names (zooC), zoop$SampleID_H)
                ,c (1:12, which (names (zoop) == "volSample"))]
# zooCenv$zoopDensity <-
zooCenv$Year <- as.numeric (format (zooCenv$timeStamp, "%Y"))
rm (zoop)
## check that poSS data is available for every zooCenv sample
summary (zooC$SampleID %in% poSS$SampleID)
summary (zooC$SampleID_H %in% poSS$SampleID_H)

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
bD <- 1                  # buffer distance
NPPSD2 <- subset (NPPSD2 # subset to LCI to reduce size and speed up things
                , (lonL [1]-bD < lon) & (lon < lonL [2]+bD) & (latL [1]-bD < lat) &
                  (lat < latL [2]+bD)
                  )
rm (bD)

## remove species that are not in the sample
bC <- NPPSD2 [,which (names (NPPSD2) == "ALTE"):ncol (NPPSD2)]
bC <- bC [,which (colSums (bC) > 0)]
NPPSD2 <- cbind (NPPSD2 [,1:which (names (NPPSD2) == "jdate")]
               , bC); rm (bC)     # doesn't seem to remove as expected






## geographically overlay seabirds and physical oceanography stations
print (poSS [which (is.na (poSS$longitude_DD) | is.na (poSS$latitude_DD)),])

coordinates (stn) <- ~Lon_decDegree+Lat_decDegree
coordinates (poSS) <- ~longitude_DD+latitude_DD
coordinates (phyCenv) <- ~lon+lat
coordinates (zooCenv) <- ~Lon_decDegree+Lat_decDegree
coordinates (NPPSD2) <- ~lon+lat
proj4string (stn) <- LLprj
proj4string (poSS) <- LLprj
proj4string (phyCenv) <- LLprj
proj4string (zooCenv) <- LLprj
proj4string (NPPSD2) <- LLprj



## coastline from gshhs
## migrate from Rghhg to shape file for windows compatibility
Require ("maptools")
Require ("zip")
tD <- tempdir()
unzip ("~/GISdata/data/coastline/gshhg-shp-2.3.7.zip"
  , junkpaths = TRUE, exdir = tD)

Require ("sf")
coastSF <- read_sf (dsn = tD, layer = "GSHHS_f_L1") ## select f, h, i, l, c
## clip to bounding box: larger Cook Inlet area
b <- st_bbox (coastSF)
b[c(1,3)] <- lonL+c(-8,11)
b[c(2,4)] <- latL+c(-5,2)
bP <- as (st_as_sfc (b), "Spatial") # get spatial polygon for intersect

coastSP <- as (coastSF, "Spatial")
Require ("raster")
Require ("rgeos")
coast <- raster::intersect(coastSP, bP)
# plot (coastC)
unlink (tD, TRUE); rm (tD)
rm (b, bP, coastSP, coastSF)

save.image ("~/tmp/LCI_noaa/cache/mapPlot.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/mapPlot.RData")


suppressWarnings (proj4string (coast) <- CRS (proj4string (poSS))) # XX execute without warning
badPO <- !is.na (over (poSS, coast)$id)
PDF ("testSamplesites")
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

PDF ("badPositions")
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
rm (i)

if (any (badPO)){
  poSS <- subset (poSS, !badPO)
}
rm (badPO)
# still have 16 records of 3_1 in poSS. lost where??
# nrow (subset (poSS, Match_Name == "3_1"))




## bathymetry from AOOS, Zimmerman
# need to clean up source files -- get rid of obsolete/redundant data sets
Require ("raster")
Require ("rgdal")

## need to supply absolute pass because raster object is just a pointer.
## still needs uncompressed raster file accessible.
if (.Platform$OS.type == "windows"){
  bath <- raster ("~/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
}else{
  bath <- raster ("/Users/martin/GISdata/LCI/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
}
bathCont <- rasterToContour (bath, levels = c(50, 100, 200, 500))


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

Require (rgeos); Require (parallel)
bDist <- function (stnL){
    ## stnL is SpatialPointsDataFrame
    ## buffDist <- unlist (mclapply (1:nrow (stnL), FUN = function (i){
    buffDist <- sapply (1:nrow (stnL), FUN = function (i){
        sDis <- gDistance (stnL [i,], stnL, byid = TRUE)
        bufD <- min (subset (sDis, sDis > 0)) / 2
        return (bufD)
    }
    )
    ## )
}
## stnB <- mean (bDist (stn))
## stnB <- mean (bDist (subset (stn, stn$Plankton)))

stnT <- subset (stn, grepl ("[1-9]|AlongBay", stn$Line)) # excl one-off stations
stnT <- subset (stn, stn$Plankton)

lBuff <- gBuffer (stnT, width = bDist (stnT), byid = TRUE)
## i <- 1
##     lBuff <- gBuffer (stn, width = stnB [i], byid = TRUE)
findBirds <- function (x){
    stnBuf <- over (NPPSD2, lBuff [x,])$Match_Name
    birdD <- cbind (Match_Name = stnBuf, NPPSD2@data)
    stnBird <- subset (birdD, !is.na (stnBuf))
    return (stnBird)
}
xo <- mclapply (1:length (lBuff), FUN = findBirds
              , mc.cores = nCPUs)

## save.image ("~/tmp/LCI_noaa/cache/birdRef.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/birdRef.RData")

## birds <- do.call (rbind, xo)  # 20 s

Require ("dplyr")
birds <- bind_rows (xo)                 # 10 s
print (warnings())

birds <- with (birds, cbind (SampleID = paste (Match_Name
                                             , format (startTime, format = "%Y-%m-%d"
                                                     , usetz = FALSE))
                    #      , Match_Name
                           , season = Seasonal (format (startTime, format = "%m"))
                           , birds))
birdS <- subset (birds, birds$SampleID %in% poSS$SampleID) # specific to sample time
birdS <- birds []    ### what's this for??? FIX this XXX

## further trim species list in birdS
bC <- birdS [,which (names (birdS) == "ALTE"):ncol (birdS)]
bC <- bC [,which (colSums (bC) > 0)]
birdS <- cbind (birdS [,1:which (names (birdS) == "jdate")]
               , bC); rm (bC)
## pro-rate unidentified spp  XXXX

rm (j,i,xo, findBirds, lBuff, birds)
rm (stnB)

## birdS:
## these are specific to samples! Samples may not be independent, i.e. may contain overlapping
## bird observations, if sample sites within buffer distance of each other!  Do NOT use this for
## spatial interpolation.


## fix up poSS to essential variables
poSS@data <- poSS@data [,-which ((names (poSS) %in% c("lonM", "latM", "distOff")))]

## main data sets now:
#  poSS, zooC (and zooCenv), birdS, NPPSD2, grd, coast, bath


print (dim (poSS@data))
print (summary (poSS))
## print (summary (physOc@data))
print (ls())


save.image ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")


cat ("\n\n#\n#\n#", format (Sys.time(), format = "%Y-%m-%d %H:%M"
                          , usetz = FALSE)
, " \n# \n# End of dataSetup.R\n#\n#\n")
## EOF
