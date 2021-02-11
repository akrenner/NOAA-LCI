#!/usr/bin/env Rscript

## (c) Martin Renner
## licence: GPL-3
## NOAA/KBRR LCI study

## import CNV files, generated from CTD by CTD_hexprocessing.R
## import notebook database tables
## QAQC on notebook tables: match with master list

## based on dataSetup.R -- pulled-out CTD import section.
## new dataSetup.R to read-in output file produced here instead of doing the
## heavy lifting of CTD import and plotting itself.
##
## abandoned earlier attempts reading in various .CNV files
## new read .CNV files freshly reprocessed by ctd_workflow.R -> CTD_hexprocessing.R
## this guarantees that there's a 1:1 match between HEX and CNV files
## Outsource as much of the post-merge QAQC to CTD_cleanup.R, as possible.



## missing issues:
# metadata dates and times
# duplicate station names (need sym-links, something like that)
#    - need algorithm on how to find station in Q for either transect
#      on any date X
#    - delete duplicate file (only one)
# fix meta-data dates? -- in script
# fix medata-data? times 2012-04-26_T9  (notes are good)
# x done x  fix file-names: 2019-15-14 to 2019-05-14  -- redo in code?
# anything that can/should be done about missing notes or missing files?
# check that station numbers within transects are in chronological order
# x negative pressures: delete. Aircasts?  Calibration file is correct!

## 2020-10-13: resolve metadata-filename date mismatches!

## make SURE, fileDB alsoways has station, transect, lat, lon
##            lat lon can be from notebook or master list -- cannot be NA




rm (list = ls())
## CAREFUL with this!!
# system ("rm -r ~/tmp/LCI_noaa/")
# ## don't  ##  unlink ("~/tmp/LCI_noaa/", recursive = TRUE)

sTime <- Sys.time()


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


if (!require("pacman")){
  install.packages("pacman", repos = "http://cran.fhcrc.org/", dependencies = TRUE)}
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

Require (oce)
## read in processed files of individual CTD casts
# fNf <- list.files("~/GISdata/LCI/CTD-processing/allCTD/CNV--homogene/", ".cnv"
# fNf <- list.files("~/GISdata/LCI/CTD-processing/allCTD/CNV--turbid/", ".cnv"
fNf <- list.files("~/GISdata/LCI/CTD-processing/allCTD/CNV/", ".cnv"
                  , full.names = TRUE, ignore.case = TRUE)
## cut-out bad files for now -- fix this later
fNf <- fNf [-c(195, 720, 762, 900, 1858, 1864, 3527)] ## temporary hack
## to make things run with SEABIRD loopedit


fN <- gsub ("^.*/", "", fNf)
print (length (fN))




## find dates to link to notebook DB
## deem file-names inherently unreliable and go with CTD metadata-dates instead
## match time-stamps to closest timestamps in notebooks and hope for the best
fileDB <- lapply (1:length (fNf), FUN = function (i){  # slow and inefficient to read files twice, once just for metadata -- still cleaner?
# for (i in 1:length (fNf)){print (i)
    Require ("oce")
  ctdF <- suppressWarnings (try (read.ctd (fNf[i]))) ## still warning for missing values and NAs introduced by coercion
  if (class (ctdF) == "try-error"){
    print (i)
    print (fNf[i])
    ## as of 2021-02-02, this affects 7 casts could address this in hex-conversion or drop them
    # fNf [c (195, 720, 762, 900, 1858, 1864, 3527)]
    ## return empty DF
    cT <- NA
    instSerNo <- NA
    depth_bottom <- NA
  }else{
    cT <- ctdF@metadata$startTime   # fix time zone later, because import is slow
    time <- ctdF@metadata$startTime
    instSerNo <- ctdF@metadata$serialNumberTemperature # serial number of CTD instrument
    depth_bottom <- ctdF@metadata$waterDepth
  }
  outDF <- data.frame (time = cT
                       , file = fN [i], path = fNf [i]
                       , instSerNo, depth_bottom
                       )
  return (outDF)
})
fileDB <- as.data.frame (do.call (rbind, fileDB)) # CTD metadata database
fileDB <- subset (fileDB, !is.na (time))
## ok to ignore warnings regarding NAs introduced by coersion


save.image ("~/tmp/LCI_noaa/cache/CNVx0.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVx0.RData")



## read CTD data from CNV file and apply basic processing:
## trim (keep only down-cast)
## apply basic QAQC: skip files with negative pressure (air-casts?)
## build flat table combining data and some metadata

unlink ("~/tmp/LCI_noaa/cache/badCTDfile.txt")
readCNV <- function (i){
  Require (oce)
  ctdF <- try (read.ctd (fNf [i]))
  if (class (ctdF) == "try-error"){
  } #else{
  ## more CTD import processing steps
  ## zero-depth
  ## cut-out surface, up-cast?
  # cut bad flags!
  ## aggregate depth bins
  ## add derived variables ??

  ## best: manually inspect and read-in from separate table
  # ?plotScan


  ## loop edit and binning -- here or in SEABIRD?
  if (0){ ##  do all this in SEABIRD now

    ## attempt to use SEABIRD method "sbe". If that fails,
    ## revert to "downcast"
    cTrim <- try (ctdTrim (ctdF, method = "sbe"), silent = TRUE) # this is the seabird method -- some fail
    if (class (cTrim) == "try-error"){
      ctdF <- ctdTrim (ctdF, method = "downcast") # specify soak time/depth
      # could/should specify min soak times, soak depth -- min soak time = 40s
      #    41, 2012_05-02_T3_S01_cast026.cnv fails at ctdTrim "sbe"
    }


    if (median (ctdF@data$pressure) < 0){
      cat ("Negative pressure: ", fN [i], "\n")
      # log bad files
      write (fNf [i], file = "~/tmp/LCI_noaa/cache/badCTDfile.txt", append = TRUE)
      return()
    }else{

      ## bin CTD profile by depth, not pressure XXX -- this needs fixing later? -- really needed?
      # ctdF <- ctdDecimate (ctdF, p = 1, method = "boxcar", e = 1.5) # this is by pressure
      ## depth values for pressures
      dP <- swPressure (0:200, latitude = 59, eos = "unesco")
      ctdF <- ctdDecimate (ctdF, p = dP, method = "unesco")
      ## keeping both at 'unesco' seems to keep depth closest to prescribed depth
      rm (dP)

      # Require ("vprr")
      # ctdFx <- bin_calculate (ctdF, binSize = 1, imageVolume = 1, rev = FALSE) # 1 meter
    }
  }


  ## fix-up missing fields
  meta <- function (x){rep (x, length (ctdF@data$temperature))}
  if (!"beamAttenuation" %in% names (ctdF@data)){
    ctdF@data$beamAttenuation <- meta (NA)
  }
  if (!"turbidity" %in% names (ctdF@data)){
    ctdF@data$turbidity <- meta (NA)
  }


    # if (length (grep ("upoly", names (ctdF@data))) == 0){
  # if (!"upoly" %in% names (ctdF@data)){
  #   ctdF@data$upoly <- meta (NA)
  # }
  # if (!"fluorescence" %in% names (ctdF@data)){
  #   ctdF@data$fluorescence <- meta (NA)
  # }
  # if ("turbidity" %in% names (ctdF@data)){ # some called "turbidity", not "upoly"
  #   names (ctdF@data)[which (names (ctdF@data) == "turbidity")] <- "upoly"
  # }

  cDFo <- data.frame (File.Name = meta (gsub (".cnv$", "", fN [i]))
                      , path = meta (fNf [i])
                      #, timestamp = meta (ctdF@metadata$startTime)  ## NOT needed here -- cut!
                      , depth_bottom = meta (ctdF@metadata$waterDepth)
                      #, CTDserial = trimws (meta (ctdF@metadata$serialNumberTemperature))
                      , density = ctdF@data$sigmaTheta # use sigmaTheta preferable when comparing samples from different depth
                      , depth = ctdF@data$depth
                      , O2 = ctdF@data$oxygen
                      , O2GG = ctdF@data$oxygen2
                      , par = ctdF@data$par
                      , salinity = ctdF@data$salinity
                      , temperature = ctdF@data$temperature
                      , pressure = ctdF@data$pressure
                      , nitrogen = ctdF@data$nitrogenSaturation
                      , fluorescence = ctdF@data$fluorescence
                      , turbidity = ctdF@data$turbidity
                      , attenuation = ctdF@data$beamAttenuation
  )
  cDF <- subset (cDFo, density > 0) ## still necessary?
  return (cDF)
}


## for troubleshooting
# for (i in 1:length (fNf)){
# # for (i in 193:length (fNf)){  ## speed-up for testing
#   print (paste (i, fileDB [i,c(2)]))
#   ctdX <- readCNV (i)
#   # if (i == 1){
#   if (!exists ("CTD1")){
#     CTD1 <- ctdX
#   }else{
#     CTD1 <- rbind (CTD1, ctdX)
#   }
# }
# rm (ctdX)
## more efficient, but doesn't ID errors -- so redundant
CTD1 <- mclapply (1:length (fNf), readCNV, mc.cores = nCPUs) # read in measurements
# require (dplyr); CTD1x <- bind_rows (CTD1x, id = fN)
CTD1 <- as.data.frame (do.call (rbind, CTD1))
rm (readCNV)
rm (fN, fNf)

save.image ("~/tmp/LCI_noaa/cache/CNVx.RData")  ## this to be read by dataSetup.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVx.RData")







##################################################
### get metadata from notebook ACCESS database ###
## QCQA (outsource!) of notebook
## check station lat-lon vs master list
## check timestamps vs. metadata
##################################################

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
stationEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblStationEvent.csv")
transectEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblTransectEvent.csv")
#  sampleEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblSampleEvent.txt")

## temporary fix of transects names -- fix this in Access DB!!
transectEv$Transect <- ifelse (transectEv$Transect == 1, "AlongBay"
                               , ifelse (transectEv$Transect == "KB", "AlongBay"
                                         , transectEv$Transect))
transectEv$Transect <- ifelse (transectEv$Transect == "0", "SubBay"
                               , transectEv$Transect) ## confirm?!?


## clean up dates/times
stationEv$Date <- ifelse (stationEv$Date == "", "1900-01-01", stationEv$Date)
stationEv$Time <- ifelse (stationEv$Time == "", "1900-01-01 00:00", stationEv$Time)
Require ("lubridate") # for time-zone adjustment
stationEv$timeStamp <- ymd_hms (paste (gsub (" .*", '', stationEv$Date)
                                       , gsub (".* ", '', stationEv$Time))
                                , tz = "America/Anchorage")
stationEv [is.na (stationEv$timeStamp), c (8, 10, 5, 6)]  ## 40 notebook records with missing timestamps
## ignore these, if there are no matching CTD files available. No point trying to interpolate them?



## make relational DB links within notebook DB
tM <- match (stationEv$TransectEvent, transectEv$TransectEvent) ## assuming dates are all correct
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



## DB-link to master list for positions -- do QAQC first?? -- move to CTD_cleanup.R??
stnMaster <- read.csv ("~/GISdata/LCI/MasterStationLocations.csv")
sMatch <- match (stationEv$Match_Name, stnMaster$Match_Name)
stationEv$LonMast <- stnMaster$Lon_decDegree [sMatch]
stationEv$LatMast <- stnMaster$Lat_decDegree [sMatch]
# stationEv$Transect <- stnMaster$Line [sMatch]
# stationEv$Station <- stnMaster$Station [sMatch]
rm (stnMaster, sMatch)



## QAQC: distance between master-position and note-book position

#################  ---- move this part to cleanup -----------  ###############
# or better to keep here after all???
# pro move: all QAQC to cleanup, keep this one simple
# con: logical to do it right after match, before it's forgotton

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
  print ("Stations with large positional error, exported to badDBpos.csv")
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
## Remove notebook position that are > 1 nautical mile from master-list position
is.na (stationEv$LonNotes)[which (stationErr$nm > 1)] <- TRUE
is.na (stationEv$LatNotes)[which (stationErr$nm > 1)] <- TRUE
# stationEv$LonNotes <- with (stationEv, ifelse (stationErr$nm > 1, LonMast, LonNotes)) ## old version
# stationEv$LatNotes <- with (stationEv, ifelse (stationErr$nm > 1, LatMast, LatNotes)) ## delete after test
## END TEMPORARY XXX
rm (stationErr)
## fill-in missing coordinates in Notebook with master-list positions
stationEv$LonNotes <- with (stationEv, ifelse (is.na (LonNotes), LonMast, LonNotes))
stationEv$LatNotes <- with (stationEv, ifelse (is.na (LatNotes), LatMast, LatNotes))


## remove surplus fields
stationEv <- stationEv [,-which (names (stationEv) %in%
                                   c("LatitudeDeg", "LatitudeMins"
                                     , "LongitudeDeg", "LongitudeMins"
                                     , "LatMast", "LonMast"))]
## -- this comment still relevant/true?? XXX del??
## just as below with metadata, link cnv names to relevant notebook cast records
## BUT, as long as positions are not great, use master positions


save.image ("~/tmp/LCI_noaa/cache/CNVy.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVy.RData")
## this is the ROOM where it's happening

## match CTD data to Access DB by timestamp -- this is the one that matters!
fileDB$match_time <- as.numeric (rep (NA, nrow (fileDB))) # numeric(nrow (fileDB))
fileDB$tErr_min <- as.numeric (nrow (fileDB))


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
rm (fixN, fixT, i, tEr)

save.image ("~/tmp/LCI_noaa/cache/CNVy3.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVy3.RData")


############################################
## run the actual matching by time stamps ##
############################################
#dbLog <- data.frame (i = numeric(), fn = character(), err = numeric()) # for QAQC
for (i in 1: nrow (fileDB)){ # could/should use sapply
  fDt <- fileDB$localTime [i]
  dT <- as.numeric (difftime (fDt, stationEv$timeStam, units = "mins"))
  tMatch <- which.min (ifelse (dT > 0, dT, -1*dT +5)) ## station recorded before CTD. find smallest positive value (with penalty)
  fileDB$tErr_min [i] <- dT [tMatch]
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
    # rm (tS)
  }
  ## end debugging

  # if (abs (dT)[tMatch] > 14){
  #   #        if (length (grep ("Subbay", fileDB$file [i])) < 1){
  #   if (abs (dT)[tMatch] < 10){  # flag discrepancies larger than -- 180 = 3 h
  #     #   print (paste0 ("i: ", i, ", Difftime: ", round (dT [tMatch], 1), "min for "
  #     #         , fileDB$file [i]))
  #      dbLog <- rbind (dbLog, data.frame (i, fn = fileDB$file [i], err = dT [tMatch]))
  #   }
  # }else{
  #   fileDB$match_time [i] <- tMatch
  # }
  if (abs (dT)[tMatch] < 14){
    fileDB$match_time [i] <- tMatch
  }
}
# warnings() #[1:10]
rm (tMatch, dT)
save.image ("~/tmp/LCI_noaa/cache/CNVy4.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVy4.RData")




## QCQA of timestamp matching
# print (dbLog [order (dbLog$err^2, decreasing = FALSE)[1:10],])  ## dbLog abandoned?

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
x <- subset (fileDB, dateErr) #[,c(2,6,7)]  ## files with mismatch in file-name and metadata date


if (any (as.numeric (format (x$localTime, "%H")) > 2)){ # allow up to 2 am
#  stop ("There are mismatches between metadata and file names")
  print ("There are date-mismatches between metadata and file names")
  print (x [order (x$file),c(2,6,7)])  ## files where dates in filename and CTD-metadata mismatch. Any shortly after midnight are ok.
}
rm (x)
## as of 2020-08-03, all discrepancies are now accounted for. 2012 and 2017 have a 1-day discrepancy due to
## surveys going on past mid-night.
# 2017-12-14 AlongBay, T9: notebook indicates that's right, metadata claims 2017-04-18 (also sampled, but other transects)
# 2018-01-17 AlongBay, T9; notebook indicates that's right, metadata claims 2017-05-22
## resolved above and through changes in filenames

## 2020-08-14: all notes in degrees decimal-degrees. -- fixed in Access

summary (fileDB$match_time)  ## 526 still missing an entry -- assign station to those by filename? XXX end of editsXXX
summary (fileDB$time [is.na (fileDB$match_time)]) # distribution of missing matches


## to be resolved above!!
## manually assign a date/time for these mismatches
## make the change in either notebook-DB or in CNV1(?) table?







############################################
##      run date + station matching       ##
############################################
## match by date and station -- to find time zone errors and fix remainders
## use date from CTD rather than file name. Use T-station from file name
fileDB$matchN <- sapply (1:nrow (fileDB), FUN = function (i){
  stn <- gsub ("-", "_", fileDB$file [i])
  stn <- strsplit(stn, "_")[[1]] # returned as list
  stnL <- tolower (stn)

  # sN <- stn [5] # trim leading zero and trailing letters (if any)
  # sN <- gsub ("^S", "", sN) # for regular station
  # sN <- gsub ("^[A-Z]*", "", sN) # covering SKB and similars as well
  sN <- grep ("^s", stnL, value = TRUE)
  sN <- gsub ("[a-z,A-Z]*$", "", sN)  ## this will also kill "ChinaPootB_", etc.
  sN <- as.integer (gsub ("^[a-z]+", "", sN))
  sN <- paste0 ("S", sN) # no leading zero

  if (length (grep ("along", stnL)) > 0){
    tN <- stn [grep ("alongbay", stnL)]
  }else if (length (grep ("sub", stnL)) > 0){
    tN <- stn [grep ("subbay", stnL)]
  }else{
    tN <- grep ("^t", stnL, value = TRUE)
    tN <- as.integer (gsub ("^[a-z]+", "", tN))
    tN <- paste0 ("T", tN)
  }
  fixFileN <- paste0 (format (fileDB$localTime [i], "%Y-%m-%d"), "_", tN, "-", sN) # fixed-up file-name

  nbName <- paste0 (format (stationEv$timeStamp, "%Y-%m-%d"), "_"
                    , ifelse (stationEv$Transect == "AlongBay", "", "T")  ## no SubBay here so far
                    , stationEv$Transect, "-S"
                    , stationEv$Station # no leading zero
  )
  match (fixFileN, nbName)
})
## deal with multiple matches -- all from subbays and one-offs
y <- sapply (1:nrow (fileDB), function (i){length (fileDB$matchN [[i]])})
fileDB$file [which (y > 1)]  ## multiple matches are all subbays and similar one-offs
is.na (fileDB$matchN)[which (y > 1)] <- TRUE
fileDB$matchN <- unlist (fileDB$matchN)
rm (y)

save.image ("~/tmp/LCI_noaa/cache/CNVyb.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVyb.RData")



## QAQC:
## issue: concurrent surveys, notebook entries only for one of them
## solutions:
## a. remove match_time from records duplicate records with mismatching station
##    would need to run 2x (1:nrow, nrow:1)
## b. add missing records to notebook
## c. skip and just use matching station names XX!
##

## QAQC
# summary (fileDB$matchN)
fileDB$chsm <- ifelse (is.na (fileDB$match_time), 0,2) +
  ifelse (is.na (fileDB$matchN), 0,1)
fileDB$chsm <- with (fileDB, ifelse ((chsm == 3) & (matchN != match_time), -1, chsm))
fileDB$chsm <- factor (fileDB$chsm)
summary (fileDB$chsm)
row.names(fileDB) <- 1:nrow (fileDB) # reset for troubleshooting
## 1. check that existing matches are correct
##   chsm == -1: conflict -- trust file name IF time-error small  (still resolve correct time)
##   chsm == 0: missing notebook?? -- use station-list only
##   chsm == 1: station-match, time doesn't (tzone?)
##   chsm == 2: time match, station doesn't. -- concurrent surveys? nudge times?
##   chsm == 3 -- trust those.

## -1 -- conflict
fileDB$consensNo <- ifelse ((fileDB$chsm == -1) && (fileDB$tErr_min < 10), fileDB$matchN, NA)


## export chsm == 0 for Jim to look for notebook entries
x <- subset (fileDB, chsm == 0)
x <- x [order (x$localTime),]
write.csv (x [,c(6, 2)], file = "~/tmp/LCI_noaa/cache/noNotebookEntries.csv", row.names = FALSE)
print ("chsm == 0, i.e. no match_time and no matchN -- no notebook entry? Re-investigate station-match")
## subbays have not been entered -- exclude here
print (x$file[-grep ("su(b|bb)ay", x$file)])
fileDB$consensNo <- ifelse (fileDB$chsm == 0, fileDB$matchN, fileDB$consensNo)

##  Investigate chsm == 2 time ok, station isn't. More time-zone issues?
x <- subset (fileDB, chsm == 2)
j <- 5;     tS <- c(17,18,6) #, 22)
rm (x, j)
fileDB$consensNo <- ifelse (fileDB$chsm == 2, NA, fileDB$consensNo)
## alternative strategy -- trust times, not location


## all good: 3
fileDB$consensNo <- fileDB$match_time







## hundreds -- need a different fix -- looking for which issue again??
if (0){
for (j in 1:nrow (x)){  ## hundreds -- need a different fix
  i <- as.numeric (row.names(x)[j])
  row.names(x) <- 1:nrow (x)
  x [, c(6,2,3,4,5)]
  cat ("\n\n###\n", j, "\n")
  print (x$localTime [j]) # ctd meta
  print (stationEv$timeStamp [x$match_time [j]]) # notebook -- should be slightly earlier
  print (x$file [j]) # file name
  x [j,] # full record
  # instSerNo
  stationEv [x$match_time [j], tS]
  pic <- stationEv [grep (gsub (" .*$", '', as.character (x$localTime [j])) # all notes from that day
                          , stationEv$Date), tS]  # better to look in stationEv$timeStamp ??
  pic$err <- difftime (fDt, pic$timeStamp, units = "mins")
  print (pic [order (pic$timeStamp),]) # all notebook entries for that date in chron order


  fx <- fileDB [grep (gsub (" .*$", "", as.character (x$localTime [j]))
                      , fileDB$localTime),] ## all files from day in question
  print (fx [order (fx$localTime), c(6,2,3,5)]) # all files from that date
}
}
rm (fDt)
## 2. find missing matches: chsm %in% c(0,1)



## debugging
# fileDB [which (is.na (fileDB$matchN))[1],]
# fileDB [which (is.na (fileDB$consensNo))[1],]
# print (length (which (is.na (fileDB$matchN))))




## root-out duplicate files -- do that much earlier!! -- top of this file? or even earlier?
## lots! 304 files!!
print ("files with matching time stamps (duplicates)")
dF <- fileDB [which (duplicated(fileDB$localTime)),]
dubFiles <- lapply (1:nrow (dF), FUN = function (i){
  fileDB [which (fileDB$localTime == dF$localTime [i]),c(2,6,9)]
})
print (dubFiles)
 fX <- fileDB
 cX <- CTD1
## speed-up by using sqlite-DF
#  if (0){ ## XXX not working yet === CTD1 ends up empty   XXX
for (i in 1:length (dubFiles)){ ## remove one of dubFiles-pair from CDT1
  killCand <- fileDB$file [which (fileDB$localTime == dF$localTime [i])]
  ## pick best file to drop
  killName <- killCand [2] # default to keeping 2nd name
  if (diff (nchar(killCand)) > 0){killName <- killCand [1]} # keep shorter name
  # grep (substr (killName, 1,30), CTD1$File.Name)
  # which (paste0 (CTD1$File.Name, ".cnv") == killName)
  ctdOut <- which (paste0 (CTD1$File.Name, ".cnv") == killName)
  ## account for negative pressures?
  if (length (ctdOut) > 0){
    CTD1 <- CTD1 [-ctdOut,]
    fileDB <- fileDB [-which (fileDB$file == killName),]
  }
  rm (killName, killCand, ctdOut)
  if (nrow (CTD1) == 0){stop(print (i), " messed up")}
}
#}
## there are a few -- delete cnv file!
# unlink (paste0 (dir, "/", fileDB$path [dF]))
rm (dF, dubFiles)
rm (fX, cX)


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
rm (tS)




save.image ("~/tmp/LCI_noaa/cache/CNVyc.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVyc.RData")

#  ls()
## where's notebook data? need lat-lon. Also need masterlist
# "CTD1"      "cX"        "dirL"      "fileDB"    "fX"        "i"         "nCPUs"
# "Require"  "stationEv" "sTime"
## need: CTD1, fileDB?



### what's below here is based on confusing legacy code -- clean-up needed
## still to do:
##  - match link notebook into physOc
##  - fix missing values from Master_Station




# mdata <- .....    # cook-up from notebooks
mdata <- with (fileDB, data.frame (isoTime = localTime
                                   , File.Name = gsub (".cnv", "", file, fixed = TRUE)
                                   , Date = format (localTime, "%Y-%m-%d") #??
                                   , Transect = stationEv$Transect [consensNo]
                                   , Station = stationEv$Station [consensNo]
                                   , Time = format (localTime, "%H:%M")
                                   , CTD.serial = instSerNo
                                   , latitude_DD = stationEv$LatNotes [consensNo]
                                   , longitude_DD = stationEv$LonNotes [consensNo]
                                   , Bottom.Depth = depth_bottom # rep (NA, length (consensNo))## put into FileDB from metatdata/masterstation -- should also be in stationEv, but isn't
                                   , comments = stationEv$Comments [consensNo]
))
summary (mdata)
summary (fileDB$consensNo)
rm (stationEv)

## FIX here: lat-long: all NA!


  ## glue it all together: add transect, station, lat, lon to CTD1
  # stationMatch <- stationEv [match (),]
  # fileDB <- cbind (fileDB, )
  #
  #
  # physOc <- with (CTD1, data.frame (timestamp, File.Name,
  #                                   stationEv$Station
  #                                   )

if (0){
  ## read in metadata and match based on File.Name
  ## metadata currently harvested from aggregated files.
  ## In future, should be kept from field-notes DB
  mdata <- read.csv ("~/GISdata/LCI/CTD/ctd_metadata_m.csv")  # currently 2012-02-04 to 2016-12-13 (as of 2020-07-02)
}

xmatch <- match (as.character (CTD1$File.Name), as.character (mdata$File.Name))
## finding bad matches
summary (xmatch)
badFileNames <- as.character (unique (CTD1$File.Name [which (is.na (xmatch))]))
length (badFileNames)                   # < 26 -- not too bad
print (badFileNames)
rm (badFileNames)

physOc <- data.frame (mdata [xmatch,]
                      , CTD1 [,which (names (CTD1) == "density"):ncol (CTD1)]) # watch out for isoTime
# physOc <- data.frame (mdata [xmatch,2:ncol (mdata)], CTD1)
rm (xmatch, CTD1, mdata)



## DANGEROUS -- REVERSE?!?
print (names (physOc))
names (physOc) <- c ("isoTime",
                     "File.Name", "Date", "Transect", "Station"
                     , "Time", "CTD.serial", "latitude_DD", "longitude_DD"
                     , "Bottom.Depth", "comments"
                     # , "timestamp"
                     # , "depth_bottom" # , "CTDserial"
                     , "Density_sigma.theta.kg.m.3"
                     , "Depth.saltwater..m."
                     , "Oxygen_SBE.43..mg.l."  # verify which is exported!!
                     , "Oxygen.Saturation.Garcia.Gordon.mg.l."
                     , "PAR.Irradiance"
                     , "Salinity_PSU"
                     , "Temperature_ITS90_DegC"
                     , "Pressure..Strain.Gauge..db."
                     , "Nitrogen.saturation..mg.l."
                     , "Fluorescence_mg_m3"
                     , "turbidity"
                     , "attenuation"
)
# print (summary (physOc))
rm (i)



cat ("\n\n#\n#\n#\n# ")
print (difftime(Sys.time(), sTime))
cat ("\n# ", format (Sys.time(), format = "%Y-%m-%d %H:%M"
                            , usetz = FALSE)
     , " \n# \n# End of CTD_cnv-Import.R\n#\n#\n")
rm (sTime)


save.image ("~/tmp/LCI_noaa/cache/CNV2.RData")   ## to be used by CTD_cleanup.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV2.RData")


## EOF
