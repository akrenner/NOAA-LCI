#!/usr/bin/env Rscript

## (c) Martin Renner
## licence: GPL-3

## NOAA/KBRR LCI study
## based on dataSetup.R -- pulled-out CTD import section.
## new dataSetup.R to read-in output file produced here instead of doing the
## heavy lifting of CTD import and plotting itself.



## missing issues:
# metadata dates and times
# duplicate station names (need sym-links, something like that)
#    - need algorithm on how to find station in Q for either transect
#      on any date X
#    - delete duplicate file (only one)
# fix meta-data dates? -- in script
# fix medata-data? times 2012-04-26_T9  (notes are good)
# x done x  fix file-names: 2019-15-14 to 2019-05-14
# anything that can/should be done about missing notes or missing files?
# check that station numbers within transects are in chronological order


## replot The Wall
## produce 2019 aggregate file (and others as well)



rm (list = ls())
## CAREFUL with this!!
# system ("rm -r ~/tmp/LCI_noaa/")
# unlink ("~/tmp/LCI_noaa/", recursive = TRUE)



## file structure:
## source files in ~/GISdata/LCI/
GISF <- "~/GISdata/LCI/"
tmpF <- "~/tmp/LCI_noaa/"
mediaF <- paste0 (tmpF, "media/")
cacheF <- paste0 (tmpF, "cache/")
dirL <- c (GISF=GISF, tmpF=tmpF, mediaF=mediaF, cacheF=cacheF)
rm (GISF, tmpF, mediaF, cacheF)
x <- lapply (dirL, dir.create, showWarnings = FALSE, recursive = TRUE); rm (x)

set.seed(7)

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

# read in cnv files for turbidity
Require (oce)
## read in processed files of individual CTD casts
## --- abandon this for now. Still in dataSetup_1.R, should there ever be a need to go back to it.

## start-over
fNf <- list.files("~/GISdata/LCI/CTD-startover/allCTD/CNV/", ".cnv"
# fNf <- list.files("~/GISdata/LCI/CTD-startover/allCTD/CNV--homogene/", ".cnv"
#fNf <- list.files("~/GISdata/LCI/CTD-startover/allCTD/CNV--turbid/", ".cnv"
                  , full.names = TRUE, ignore.case = TRUE)
fN <- gsub ("^.*/", "", fNf)
print (length (fN))


#####################
## file accounting ##
#####################

## matching hex file for each CNV? -- no longer needed or relevant
## now that all is reprocessed
if (0){
hFN <- list.files ("~/GISdata/LCI/CTD-new/", pattern = ".hex"
                   , ignore.case = TRUE, full.names = FALSE
                   , recursive = TRUE)
# start-over
hList <- function (f){list.files (f, pattern = ".hex", ignore.case = TRUE, full.names = FALSE, recursive = TRUE)}
hFN <- hList ("~/GISdata/LCI/CTD-startover/allCTD/edited_hex/")
hFN <- c (hFN, hList ("~/GISdata/LCI/CTD-startover/ctd-data2012-16/"))
hFN <- c (hFN, hList ("~/GISdata/LCI/CTD-startover/ctd-data2017-21/"))
rm (hList)
hDB <- data.frame (path = hFN, file = gsub ("^.*/", "", hFN)); rm (hFN)
# hFN <- gsub ("^.*/", "", hFN) # keep only file, strip out directories
# any (duplicated (hFN)) # no duplicates

## check that all CNV have matching HEX
hDB$hStub <- gsub (".hex$", "", tolower (hDB$file))
hDB <- hDB [order (hDB$path),]

## check for duplicate HEX files
# hDB$hDupl <- duplicated (hDB$hStub)
if (any (duplicated(hDB$hStub))){
  print (hDB [order (hDB$hStub, hDB$hDupl),])
  stop ("duplicated hex file names")
}else{
  Require ("openssl") # alternative: digest
  hDB$sha256 <- sha256 (paste0 ("~/GISdata/LCI/CTD-new/", hDB$path))
  if (any (duplicated (hDB$sha256))){
    print (subset (hDB, duplicated (hDB$sha256)))
    stop ("duplicated sha256 checksums")
  }else{cat ("No duplicate HEX files found\n")}
}


## do all HEX files have matching CNV files
cFN <- gsub (".cnv$", "", tolower (fN))
# summary (cFN %in% hFN)
hDB$CNV <- character (nrow (hDB)) #cFN [grep ()]
for (i in 1:nrow (hDB)){
  cMatch <- fN [grep (hDB$hStub [i], cFN, value = FALSE)]
  if (length (cMatch) == 1){
    hDB$CNV [i] <- cMatch
    # }else if (length (cMatch > 1)){print (i)}else{
    #   hDB$CNV [i] <- "NA"
  }
}; rm (cMatch)
summary (hDB$CNV == "")
if (any (hDB$CNV == "")){
  ## identify HEX that need processing
  hDBx <- subset (hDB, CNV == "")
  write.csv (hDBx, file = "~/tmp/LCI_noaa/cache/hexFiles.csv", row.names = FALSE)
  stop ('see hexFiles.csv for ', nrow (hDBx),' hex that need processing or renaming')
  rm (hDBx)
}


## reverse: which CNV filenames have no matching HEX
cDB <- data.frame (fN, cStub = cFN, hex = character (length (fN)))
for (i in 1:length (fN)){
  hMatch <- hDB$path [grep (cDB$cStub [i], hDB$hStub)]
  if (length (hMatch) == 1){
    cDB$hex [i] <- hMatch
  }
}; rm (hMatch)
if (any (cDB$hex == "")){
  cDB <- cDB [order (cDB$hex),]
  write.csv (cDB, file = "~/tmp/LCI_noaa/cache/cnvFiles.csv", row.names = FALSE)
  stop ("see cnvFiles.csv for ",nrow (cDB), " cnv files that missmatch")
rm (cFN, cDB)}
}

#####################
#####################


## TESTING ONLY xxxx ###
#fN <- fN [grep ("2019", fN, value = FALSE)]
#fNf <- fNf [grep ("2019", fNf, value = FALSE)]
## END TESTING ONLY ####

## exclude negative pressure: 114: "C:/Users/Martin.Renner/Documents/GISdata/LCI/CTD-new/4141/2_CNV/2012_10-29_T4_S07_cast065.cnv"
# fNf <- fNf [-114] # dirty trick XXX



## deem file-names inherently unreliable and go with CTD metadata-dates instead
## match time-stamps to closest timestamps in notebooks and hope for the best
fileDB <- lapply (1:length (fNf), FUN = function (i){  # slow and inefficient to read files twice, once just for metadata -- still cleaner?
  Require (oce)
  ctd <- suppressWarnings (read.ctd (fNf[i])) ## still warning for missing values and NAs introduced by coercion
  cT <- ctd@metadata$startTime   # fix time zone later, because import is slow
  ## , latitude = meta (ctdF@metadata$latitude)
  ## , longitude = meta (ctdF@metadata$longitude)
  #, depth_bottom = meta (ctdF@metadata$waterDepth)
  ## , transect = meta (ctdF@metadata$station)
  ## , Match_Name = meta (ctdF@metadata$station)
  #, CTDserial = meta (ctdF@metadata$serialNumberTemperature)

  return (data.frame (time = ctd@metadata$startTime, file = fN [i], path = fNf [i]
                      , instSerNo = ctd@metadata$serialNumberTemperature # serial number of CTD instrument
  ))
})
fileDB <- as.data.frame (do.call (rbind, fileDB)) # CTD metadata database
## ok to ignore warnings regarding NAs introduced by coersion

save.image ("~/tmp/LCI_noaa/cache/CNVx0.RData")  ## this to be read by dataSetup.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVx0.RData")




readCNV <- function (i){
  Require (oce)
  ctdF <- read.ctd (fNf [i])
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
  ## add metadata from notebook (later -- latlon, Match_Name, tide,..)
  ## most should be here (paste into aggregated data or separate table)
  ##   .... find others!

  ## best: manually inspect and read-in from separate table
  # ?plotScan

  ## attempt to use SEABIRD method "sbe". If that fails,
  ## revert to "downcast"
  cTrim <- try (ctdTrim (ctdF, method = "sbe"), silent = TRUE) # some fail
  if (class (cTrim) == "try-error"){
    ctdF <- ctdTrim (ctdF, method = "downcast") # specify soak time/depth
    # ctdF <- ctdTrim (ctdF, method = "sbe") # there's also a method seabird
    # could/should specify min soak times, soak depth -- min soak time = 40s
    #    41, 2012_05-02_T3_S01_cast026.cnv fails at ctdTrim "sbe"
  }
  ## derived variables?

  ## aggregate
  # ctdF <- initializeFlagScheme(ctdF, name = "ctd")
  # ctdF <- handleFlags (ctdF)
  #    ctdF <- handleFlags (ctdF, flags = defaultFlags(ctdF))
  ctdF <- ctdDecimate (ctdF, p = 1, method = "boxcar", e = 1.5) # later? -- really needed?

  ## fix-up missing fields
  meta <- function (x){rep (x, length (ctdF@data$temperature))}
  # if (length (grep ("upoly", names (ctdF@data))) == 0){
  if (!"upoly" %in% names (ctdF@data)){
        ctdF@data$upoly <- meta (NA)
  }
  if (!"fluorescence" %in% names (ctdF@data)){
    ctdF@data$fluorescence <- meta (NA)
    # cat (gsub ("/Users/martin/GISdata/LCI/CTD/6 DERIVE//", "", fN [i]), "\n")
  }
  if ("turbidity" %in% names (ctdF@data)){ # some called "turbidity", not "upoly"
    names (ctdF@data)[which (names (ctdF@data) == "turbidity")] <- "upoly"
  }

  # turbidity and fluorescence missing throughout??
  cDFo <- data.frame (File.Name = meta (gsub (".cnv$", "", fN [i]))
                     , timestamp = meta (ctdF@metadata$startTime)
                     , depth_bottom = meta (ctdF@metadata$waterDepth)
                     #, CTDserial = trimws (meta (ctdF@metadata$serialNumberTemperature))
                     , density = ctdF@data$sigmaTheta # use sigmaTheta or sigmaT?
                     , depth = ctdF@data$depth
                     , O2 = ctdF@data$oxygen
                     , par = ctdF@data$par
                     , salinity = ctdF@data$salinity
                     , temperature = ctdF@data$temperature
                     , pressure = ctdF@data$pressure
                     # nitrogen
                     #, fluorescence = ctdF@data$fluorescence ## often missing
                     #, turbidity = ctdF@data$upoly
  )
  cDF <- subset (cDFo, density > 0)
  return (cDF)
}


## bad? cast: 2012_10-29_T4_S07_cast065 -- ctdDecimate fails. All depth = negative
# i=113 2012_10-29_T4_S06_cast067.cnv
for (i in 1:length (fNf)){
  print (paste (i, fileDB [i,c(2,4)]))
  ctdX <- readCNV (i)
  if (i == 1){
    CTD1 <- ctdX
  }else{
    CTD1 <- rbind (CTD1, ctdX)
  }
}
CTD1 <- mclapply (1:length (fNf), readCNV, mc.cores = nCPUs) # read in measurements
# require (dplyr); CTD1 <- bind_rows (CTD1, id = fN)
CTD1 <- as.data.frame (do.call (rbind, CTD1))
rm (readCNV)
rm (fN, fNf)
# "bad" in station?

save.image ("~/tmp/LCI_noaa/cache/CNVx.RData")  ## this to be read by dataSetup.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVx.RData")


# ideal: read-in data from ACCESS database via ODBC -- may be not worth the troubles
if (0){
  # Require ("odbc")
  # odbc <- dbConnect (odbc::odbc(), dsn = "MicrosoftAccess")
  Require ("DBI")
  dbC <- dbConnect (odbc::odbc()
                    , driver = "Microsoft Access Driver"
                    , database = "/Users/Martin.Renner/Documents/GISdata/LCI/EVOS_LTM.accdb"
  )


  dbC <- dbConnect (odbc::odbc(), dsn = "MicrosoftAccess", driver = )
  channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/Martin.Renner/Documents/GISdata/LCI/EVOS_LTM.accdb")
  data <- sqlQuery( channel , paste ("select * from CUSTOMERS"))
  odbcCloseAll()




  dbq_string <- paste0 ("DBQ=", "locatoin of my file")
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <- paste0 ()
  dC <- dbConnect (odbc::odbc(), .connection_string = paste0 (driver_string, dbq_string))
  ## appropriate SQL call to link station and transect, as below...

}

# manually exported tables from note-book Access DB, read-in those data and link to existing tables
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVx.RData")
stationEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblStationEvent.csv")
transectEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblTransectEvent.csv")
#  sampleEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblSampleEvent.txt")

## temporary fix of transects -- fix this in Access DB!!
transectEv$Transect <- ifelse (transectEv$Transect == 1, "AlongBay"
                               , ifelse (transectEv$Transect == "KB", "AlongBay"
                                         , transectEv$Transect))
transectEv$Transect <- ifelse (transectEv$Transect == "0", "SubBay"
                               , transectEv$Transect) ## confirm?!?


## clean up dates/times
stationEv$Date <- ifelse (stationEv$Date == "", "1900-01-01", stationEv$Date)
stationEv$Time <- ifelse (stationEv$Time == "", "1900-01-01 00:00", stationEv$Time)
# stationEv$timeStamp <- as.POSIXct (paste (format (as.POSIXct (stationEv$Date), "%Y-%m-%d")
#                                           , format (as.POSIXct (stationEv$Time), "%H:%M")), tz = "UTC" )#, tz = "America/Anchorage")
Require (lubridate) # for time-zone adjustment
stationEv$timeStamp <- ymd_hms (paste (gsub (" .*", '', stationEv$Date)
                                       , gsub (".* ", '', stationEv$Time))
                                , tz = "America/Anchorage")
stationEv [is.na (stationEv$timeStamp), c (8, 10, 5, 6)]  ## 40 bad timestamps -- ignore if no files affected
## ignore these, if there are no matching CTD files available



## make relational DB links
tM <- match (stationEv$TransectEvent, transectEv$TransectEvent) ## assuming dates are all correct

## 20 NAs -- why??
if (any (is.na (tM))){
  print (stationEv [which (is.na (tM)), c (21, 8, 9, 10)])
  stop ("no missing transectEvents allowed")
}

## this may be the only needed field from TransectEvent
stationEv$Transect <- factor (transectEv$Transect [tM])
stationEv$Match_Name <- paste0 (transectEv$Transect [tM], "_", stationEv$Station)
rm (transectEv, tM)

stationEv$LonNotes <- with (stationEv, LongitudeDeg - abs (LongitudeMins)/60)
stationEv$LatNotes <- with (stationEv, LatitudeDeg + LatitudeMins/60)

badLon <- subset (stationEv, (-160 > LonNotes)|(LonNotes > -130))
if (nrow (badLon) > 1){
  stop (print (badLon, c(21, 5, 6)))
  #    stationEv <- subset (stationEv, !(StationEvent %in% badLon$StationEvent))
}
rm (badLon)
# summary (stationEv [,c(1:4,22:23)])



stnMaster <- read.csv ("~/GISdata/LCI/MasterStationLocations.csv")
sMatch <- match (stationEv$Match_Name, stnMaster$Match_Name)
stationEv$LonMast <- stnMaster$Lon_decDegree [sMatch]
stationEv$LatMast <- stnMaster$Lat_decDegree [sMatch]
# stationEv$Transect <- stnMaster$Line [sMatch]
# stationEv$Station <- stnMaster$Station [sMatch]
rm (stnMaster)

Require (geosphere)
stationErr <- data.frame (posError = with (stationEv, distHaversine (
  cbind (LonNotes, LatNotes), cbind (LonMast, LatMast)
)))
stationErr$lonErr <- with (stationEv, LonNotes - LonMast)
stationErr$latErr <- with (stationEv, LatNotes - LatMast)

# summary (stationErr$lonErr)
# stationEv [which.max (stationErr$posError), c(21,5,6, 22:25)]

stationEv <- stationEv [order (stationErr$posError, decreasing = TRUE),]
stationErr <- stationErr [order (stationErr$posError, decreasing = TRUE),]
stationErr$nm <- stationErr$posError/1852

x <- data.frame (stationEv, stationErr) [which (stationErr$posError > 1852*1.0),]
if (nrow (x) > 3){
  ## notebook positions that deviate from original positions -> Jim to check
  print (x [order (x$timeStamp)[1:nrow (x)], c(21,23,24,25,26,27,29,30, 31, 28, 7)])
  write.csv(x [order (x$timeStamp)[1:nrow (x)], c(21,23,24,25,26,27,29,30, 31, 28, 7)]
            , file = paste0(dirL [[3]], "badDBPos.csv"), row.names = FALSE)
}
rm (x)

# pdf ("~/tmp/LCI_noaa/media/badPositions_CTD.pdf")
# plot (LonNotes~LonMast, stationEv, asp = 1)
# plot (LatNotes~LatMast, stationEv, asp = 1)
#
# plot (LatNotes~LonNotes, stationEv, asp = 1)
# plot (LatMast~LonMast, stationEv, asp = 1)
# dev.off()


## TEMPORARY!!! XXXX -- replace bad notebook positions with defaults XXX
stationEv$LonNotes <- with (stationEv, ifelse (stationErr$nm > 1, LonMast, LonNotes))
stationEv$LatNotes <- with (stationEv, ifelse (stationErr$nm > 1, LatMast, LatNotes))
## END TEMPORARY XXX
rm (stationErr)
## fill-in missing coordinates in Notebook with generic station positions
stationEv$LonNotes <- with (stationEv, ifelse (is.na (LonNotes), LonMast, LonNotes))
stationEv$LatNotes <- with (stationEv, ifelse (is.na (LatNotes), LatMast, LatNotes))


## remove surplus fields
stationEv <- stationEv [,-which (names (stationEv) %in%
                                   c("LatitudeDeg", "LatitudeMins"
                                     , "LongitudeDeg", "LongitudeMins"
                                     , "LatMast", "LonMast"))]
## just as below with metadata, link cnv names to relevant notebook cast records
## BUT, as long as positions are not great, use master positions


save.image ("~/tmp/LCI_noaa/cache/CNVy.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVy.RData")




## match Access DB tables with CTD data using timestamps (ignore filenames!!)
## find a file for each record in Access table --- not much point in doing that
# stationEv$CTDfile <- character (nrow (stationEv))
# stationEv$metaTime <- character (nrow (stationEv))
# for (i in 1:nrow (stationEv)){
#   dT <- as.numeric (difftime (stationEv$timeStamp [i], fileDB$time, units = "mins"))
#   tMatch <- which.min (dT^2)
#   if (dT [tMatch] > 30){
#     warning ("Difftime = ",dT [tMatch], "min for ", stationEv$timeStamp [tMatch])
#   }else{
#     stationEv$CTDfile [i] <- fileDB$file [tMatch]
#   }
#   stationEv$metaTime [i] <- as.character (fileDB$time [tMatch])
# }
# ## QAQC ##
# tErr <- difftime (stationEv$timeStamp
#                   , fileDB$time [match (stationEv$CTDfile, fileDB$file)]
#                   , units = "days")
# hist (as.numeric (tErr))
# stationEv [which.max (as.numeric (tErr)^2),]
#
#
# summary (nchar (stationEv$CTDfile)>0)  # 2037 FALSE, 935 TRUE ##|| records but no CTD file



# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVy.RData")
## this is the ROOM where it's happening

## match CTD data to Access DB -- this is the one that matters!
fileDB$recNo <- as.numeric (rep (NA, nrow (fileDB))) # numeric(nrow (fileDB))
fileDB$tErr <- as.numeric (nrow (fileDB))


## casts to skip for now, until added to database:
# fileDBx <- fileDB [which (! fileDB$file %in% c (
# "2012_06-25_Subbays_BearA_cast115.cnv"
# , "2012_08-15_AlongBay_S06_inner-cast011.cnv"
# , "2012_06-25_Subbays_BearB_cast114.cnv"
# , "2012_06-25_Subbays_ChinaPootA_cast132.cnv"
# , "2012_06-25_Subbays_HalibutA_cast128.cnv"
# , "2012_06-25_Subbays_HalibutB_cast127.cnv"
#  ## ... [and many more Subbay !]
# , "2012_08-15_AlongBay_S07_inner-cast012.cnv" # all AlongBay for that day missing in DB!
# , "2014_02-15_T9_S01_cast170.cnv" # all 2014-02-15 T9  missing
# , '2014_05-28_AlongBay_SKB03_cast037.cnv' # et al.
# # 2012_08-15_T4_S01_cast123.cnv  -- notebook: AlongBay_S06 to _S13
# )),]


## exclude Subbay samples for now XXXX TEMP!!!
nrow (fileDB)
# fileDB <- fileDB [-grep ("Subbay", fileDB$file),]
nrow (fileDB)  # removed 62 casts :(  [missing in notebook] -- standardize filenames

## correct time zone in fileDB
require (lubridate)
fileDB$localTime <- as.POSIXlt (ifelse (fileDB$time > as.POSIXct("2012-05-01 09:00")
                                        , force_tz(fileDB$time, "America/Anchorage")
                                        , with_tz (fileDB$time, "America/Anchorage"))
                                , tz = "America/Anchorage"
                                , origin = as.POSIXct ("1969-12-31 14:00:00", tz = "America/Anchorage")
)

## IMPORTANT FIX: meta-data clock is wrong at times.
# bad metadata/time-calibration 2017-12-14, 2018-01-17 -- confirmed by notebooks
tEr <- as.numeric (difftime (as.POSIXct ("2017-12-14 10:38 AKST")
                             , as.POSIXct("2017-04-18 10:38 AKST")
                             , units = "secs"))
fileDB$localTime [grep ("2017_12-14_", fileDB$file)] <- fileDB$localTime [grep ("2017_12-14_", fileDB$file)] + tEr

tEr <- as.numeric (difftime (as.POSIXct ("2018-01-17 10:45:25 AKST")
                             , as.POSIXct("2017-05-22 10:45:25 AKST")
                             , units = "secs"))
fileDB$localTime [grep ("2018_01-17_", fileDB$file)] <- fileDB$localTime [grep ("2018_01-17_", fileDB$file)] + tEr

## 2012-04-26 -- clock appears to be stuck -- fix here (rather than mess with files)
fixN <- c ("2012_04-26_T9_S05_cast299.cnv", "2012_04-26_T9_S06_cast299.cnv"
           , "2012_04-26_T9_S07_cast299.cnv", "2012_04-26_T9_S08_cast299.cnv"
           , "2012_04-26_T9_S09_cast299.cnv", "2012_04-26_T9_S10_cast299.cnv")
fixT <- as.POSIXct (paste ("2012-04-26"
                           , c ("18:50", "18:58", "19:20", "19:30", "19:39", "19:57")
                           , "AKDT"))
for (i in 1:length (fixN)){
  fileDB$localTime [which (fileDB$file == fixN [i])] <- fixT [i]
}
rm (fixN, fixT)

save.image ("~/tmp/LCI_noaa/cache/CNVy3.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVy3.RData")


############################################
## run the actual matching by time stamps ##
############################################
dbLog <- data.frame (i = numeric(), fn = character(), err = numeric()) # for QAQC
for (i in 1: nrow (fileDB)){ # could/should use sapply
  fDt <- fileDB$localTime [i]
  dT <- as.numeric (difftime (fDt, stationEv$timeStam, units = "mins"))
  tMatch <- which.min (ifelse (dT > 0, dT, -1*dT +5)) ## station recorded before CTD. find smallest positive value (with penalty)
  fileDB$tErr [i] <- dT [tMatch]
  if (0){  ## debugging
    ## Pacific TS for CTD?
    fDt <- with_tz (force_tz (fDt, "America/Los_Angeles")
                    , "America/Anchorage")

    tS <- c(17,18,6) #, 22)
    print (fDt)
    print (stationEv$timeStamp [tMatch])
    print (fileDB$file [i])
    print (stationEv [tMatch, tS])
    pic <- stationEv [grep (gsub (" .*$", '', as.character (fDt))
                            , stationEv$Date), tS]  # better to look in stationEv$timeStamp ??
    # pic <- stationEv [grep ("^2014-02-16", stationEv$Date), tS]
    pic$err <- difftime (fDt, pic$timeStamp, units = "mins")
    print (pic [order (pic$timeStamp),])
  }
  ## end debugging

  ## stationEv$tErr <- dT   ## XXX tmp -- debugging only!
  if (abs (dT)[tMatch] > 14){
    #        if (length (grep ("Subbay", fileDB$file [i])) < 1){
    if (abs (dT)[tMatch] < 10){  # flag discrepancies larger than -- 180 = 3 h
      #   print (paste0 ("i: ", i, ", Difftime: ", round (dT [tMatch], 1), "min for "
      #         , fileDB$file [i]))
      dbLog <- rbind (dbLog, data.frame (i, fn = fileDB$file [i], err = dT [tMatch]))
    } # fileDB$recNoT <- NA
  }else{
    fileDB$recNo [i] <- tMatch
  }
}
# warnings() #[1:10]
save.image ("~/tmp/LCI_noaa/cache/CNVy4.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVy4.RData")


## QCQA of timestamp matching
dbLog [order (dbLog$err^2, decreasing = FALSE)[1:10],]  ## dbLog abandoned?

# flag mismatch between metadata and file-name date -- resolve above rather than manually in files
fnDate <- gsub ("_", "-", fileDB$file)
fnDate <- strsplit (fnDate, "-")

## find bad file names -- loop over names
fnDDF <- data.frame (y=integer(), m=integer(), d=integer()
                     , T=character(), S=character())
for (i in 1:length (fnDate)){
  fnDDF [i,] <- fnDate [[i]][1:5]
}
# fnDate <- do.call (rbind, fnDate)  # fails
fnDate <- fnDDF; rm (fnDDF)
fnDate <- paste (fnDate [,1], fnDate [,2], fnDate [,3], sep = "-")
fnDate <- gsub ("[A-Z,a-z]*$", "", fnDate) # remove any letters at end of date
mDate <- gsub (" .*$", "", as.character (fileDB$localTime))
fileDB$dateErr <- ifelse (mDate == fnDate, FALSE, TRUE)
rm (fnDate, mDate)
x <- subset (fileDB, dateErr)[,c(2,6)]
x [order (x$file),]  ## files where dates in filename and CTD-metadata mismatch. Any shortly after midnight are ok.
if (any (as.numeric (format (x$localTime, "%H")) > 2)){ # allow up to 2 am
  stop ("There are mismatches between metadata and file names")
}
rm (x)
# x$year <- factor (as.numeric (format (x$localTime, "%Y")))
# subset (x, year !=2012) # 2012 records all refer to night samples -- safe to use metadata
## as of 2020-08-03, all discrepancies are now accounted for. 2012 and 2017 have a 1-day discrepancy due to
## surveys going on past mid-night.

# 2017-12-14 AlongBay, T9: notebook indicates that's right, metadata claims 2017-04-18 (also sampled, but other transects)
# 2018-01-17 AlongBay, T9; notebook indicates that's right, metadata claims 2017-05-22
## resolved above and through changes in filenames
## possible causes:
# missing notebook entry!
# bad time zone
# bad time/date in notebook
# -> need to ascertain first whether station is matching!


summary (fileDB$recNo)  ## 526 still missing an entry -- assign station to those by filename? XXX end of editsXXX
summary (fileDB$time [is.na (fileDB$recNo)]) # distribution of missing matches




############################################
##      run date + station matching       ##
############################################
## match by date and station -- to find time zone errors and fix remainders
## use date from CTD rather than file name. Use T-station from file name
fileDB$matchN <- sapply (1:nrow (fileDB), FUN = function (i){
  stn <- gsub ("-", "_", fileDB$file [i])
  stn <- strsplit(stn, "_")[[1]] # returned as list

  sN <- stn [5] # trim leading zero and trailing letters (if any)
  #  sN <- gsub ("^S", "", sN) # for regular station
  sN <- gsub ("^[A-Z]*", "", sN) # covering SKB and similars as well
  sN <- gsub ("[a-z,A-Z]*$", "", sN)  ## this will also kill "ChinaPootB_", etc.
  stn <- paste0 (stn [4], "-S", as.numeric (sN)) # introduces NAs by coercion
  stn <- paste0 (format (fileDB$localTime [i], "%Y-%m-%d"), "_", stn)

  nbName <- paste0 (format (stationEv$timeStamp, "%Y-%m-%d"), "_"
                    , ifelse (stationEv$Transect == "AlongBay", "", "T")
                    , stationEv$Transect, "-S"
                    , stationEv$Station # no leading zero
  )
  match (stn, nbName)
})

save.image ("~/tmp/LCI_noaa/cache/CNVyb.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVyb.RData")

## QAQC:
## issue: concurrent surveys, notebook entries only for one of them
## solutions:
## a. remove recNo from records duplicate records with mismatching station
##    would need to run 2x (1:nrow, nrow:1)
## b. add missing records to notebook
## c. skip and just use matching station names XX!
##

## QAQC
# summary (fileDB$matchN)
fileDB$chsm <- ifelse (is.na (fileDB$recNo), 0,2) +
  ifelse (is.na (fileDB$matchN), 0,1)
fileDB$chsm <- with (fileDB, ifelse ((chsm == 3) & (matchN != recNo), -1, chsm))
fileDB$chsm <- factor (fileDB$chsm)
summary (fileDB$chsm)
row.names(fileDB) <- 1:nrow (fileDB) # reset for troubleshooting
## 1. check that existing matches are correct
##   chsm == 3 -- trust those.
##   chsm == 2: time match, station doesn't. -- concurrent surveys? nudge times?
##   chsm == 1: station-match, time doesn't (tzone?)
##   chsm == 0: missing notebook?? -- use station-list only
##   chsm == -1: conflict -- trust file name IF time-error small

# fileDB$consMatch <- with (fileDB, ifelse (chsm == -1, matchN
#                                           , ifelse (chsm == 3, matchN
# , ifelse ()                                                    ))


## export chsm == 0 for Jim to look for notebook entries
x <- subset (fileDB, chsm == 0)
x <- x [order (x$localTime),]
write.csv (x [,c(6, 2)], file = "~/tmp/LCI_noaa/cache/noNotebookEntries.csv", row.names = FALSE)


##  Investigate chsm == 2 (263). More time-zone issues?
x <- subset (fileDB, chsm == 2)
j <- 5;     tS <- c(17,18,6) #, 22)

for (j in 1:nrow (x)){
  i <- as.numeric (row.names(x)[j])
  row.names(x) <- 1:nrow (x)
  x [, c(6,2,3,4,5)]
  cat ("\n\n###\n", j, "\n")
  print (x$localTime [j]) # ctd meta
  print (stationEv$timeStamp [x$recNo [j]]) # notebook -- should be slightly earlier
  print (x$file [j]) # file name
  x [j,] # full record
  # instSerNo
  stationEv [x$recNo [j], tS]
  pic <- stationEv [grep (gsub (" .*$", '', as.character (x$localTime [j])) # all notes from that day
                          , stationEv$Date), tS]  # better to look in stationEv$timeStamp ??
  pic$err <- difftime (fDt, pic$timeStamp, units = "mins")
  print (pic [order (pic$timeStamp),]) # all notebook entries for that date in chron order


  fx <- fileDB [grep (gsub (" .*$", "", as.character (x$localTime [j]))
                      , fileDB$localTime),] ## all files from day in question
  print (fx [order (fx$localTime), c(6,2,3,5)]) # all files from that date
}

## 2. find missing matches: chsm %in% c(0,1)



## debugging
fileDB [which (is.na (fileDB$matchN))[1],]





## root-out duplicate files
dF <- fileDB [which (duplicated(fileDB$localTime)),]
lapply (1:length (dF), FUN = function (i){
  fileDB [which (fileDB$localTime == dF$localTime [i]),c(2,6,9)]
})
## there are a few -- delete cnv file!
unlink (paste0 (dir, "/", fileDB$path [dF]))




if (0){ ## MATCH file names to database --- WHAT to do about doubles??? (same station sampled 2x+ per day)
  fNDB <- with (stationEv, paste0 (format (as.POSIXct (timeStamp), "%Y_%m-%d")
                                   , "_"
                                   , ifelse (Transect %in% as.character (3:9), "T", "")
                                   , Transect
                                   , "_S", formatC (Station, width = 2, flag = "0")
  ))
  fNDB2 <- with (stationEv, paste0 (format (as.POSIXct (timeStamp), "%m_%d_%Y")
                                    , "_"
                                    , ifelse (Transect %in% as.character (3:9), "T", "")
                                    , Transect
                                    , "_S", formatC (Station, width = 2, flag = "0")
  ))


  ## grep-link with actual cnv file-names
  fNEnd <- unlist (strsplit (fN, "/"))
  fNEnd <- fNEnd [which ((1:length (fNEnd)) %% 3 == 0)]
  ## problem: dates in filenames are inconsistent: 2018_10-17_T9_S08_cast072.hex vs 04_24_2019_AlongBay_

  ## test with small example first
  stDB <- "T9_S08"



  ## reverse -- identify surveys in notebook with no matching CTD-CNV file
  for (i in 1:length (fNDB)){ # move to apply
    tS <- as.POSIXct(stationEv$timeStamp)[i]
    stDB <- with (stationEv [i,], paste0 (ifelse (Transect %in% as.character (3:9), "T", "")
                                          , Transect
                                          , "_S", formatC (Station, width = 2, flag = "0")))
    xM <- grep (paste0 (format (tS, "%Y"), "[-_]", format (tS, "%m"), "[-_]"
                        , format (tS, "%d"), stDB)
                , fNEnd, ignore.case = TRUE) #, fixed = TRUE)
    if (length (xM) == 0){
      xM <- grep (fNDB2 [i], fNEnd, ignore.case = TRUE)
    }
    if (length (xM) > 1){stop (fNDB [i], "is matched to", fNEnd [xM])}



  }
}




## need alternative to mdata here!
## END OF EDITS


save.image ("~/tmp/LCI_noaa/cache/CNVyc.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVyc.RData")


# mdata <- .....    # cook-up from notebooks

if (0){
  ## read in metadata and match based on File.Name
  ## metadata currently harvested from aggregated files.
  ## In future, should be kept from field-notes DB
  mdata <- read.csv ("~/GISdata/LCI/CTD/ctd_metadata_m.csv")  # currently 2012-02-04 to 2016-12-13 (as of 2020-07-02)

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
}


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





save.image ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## this to be read by dataSetup.R

cat ("\n\n#\n#\n#", format (Sys.time(), format = "%Y-%m-%d %H:%M"
                            , usetz = FALSE)
     , " \n# \n# End of dataSetup.R\n#\n#\n")
## EOF

