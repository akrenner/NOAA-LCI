## CTD_notesQAQC

## Legacy code from CTD_cnv-Import.R: use notes database for QAQC.
## Main test: any bad station labels in file names?
## This is code in flux as migration from Access to FileMaker Pro db is underway.

## Also check date of filename against metadata date.



# rm (list = ls()); base::load ("~/tmp/LCI_noaa/cache/CNVx2.RData")

rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNV2.RData") ## from CTD_cnv-Import.R








##################################################
### get metadata from notebook ACCESS database ###
## QCQA (outsource!) of notebook
## check station lat-lon vs master list
## check timestamps vs. metadata
##################################################


## update Access tables from Notebook database
## may need to adapt this to make this portable
## ---  move all this to FieldNotesDB.R?
## Migrate to FileMaker and automated export of tables

if (.Platform$OS.type != "unix") {
  ## need to call 32-bit version of R to use ODBC -- until 64-bit Access ODBC driver installed
  ## windows-version of mdb-export?
  if (0) {
    system("cmd.exe"
      , input = paste('"C:/Users/Martin.Renner/Applications/R-4.1.3/bin/i386/Rscript.exe" C:/Users/Martin.Renner/Documents/myDocs/amyfiles/NOAA-LCI/ctd_odbc-export.R'))
  }
} else {
  if (0) {
    system ("mdb-export ~/GISdata/LCI/EVOS_LTM.accdb tblStationEvent > ~/GISdata/LCI/EVOS_LTM_tables/tblStationEvent.csv")
    system ("mdb-export ~/GISdata/LCI/EVOS_LTM.accdb tblTransectEvent > ~/GISdata/LCI/EVOS_LTM_tables/tblTransectEvent.csv")
    system ("mdb-export ~/GISdata/LCI/EVOS_LTM.accdb tblSampleEvent > ~/GISdata/LCI/EVOS_LTM_tables/tblSampleEvent.txt")
  }
}

## which set of tables is best to import? -- MDBTools vs ODBC
tblDir <- "~/GISdata/LCI/EVOS_LTM_tables/"  ## ODBC via 32-bit R-4.1.3
tblDir <- "~/GISdata/LCI/EVOS_LTM_tables/manualExport/" ## manual export from Access
tblDir <- "~/GISdata/LCI/EVOS_LTM_tables/mdbtools/"  ## mdbtools
# manually exported tables from note-book Access DB, read-in those data and link to existing tables

stationEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblStationEvent.csv")
transectEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblTransectEvent.csv")
#  sampleEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblSampleEvent.txt")

stationEv <- read.csv(paste0 (tblDir, "tblStationEvent.txt"))
transectEV <- read.csv (paste0 (tblDir, "tblTransectEvent.txt"))

## temporary fix of transects names -- fix this in Access DB!!
transectEv$Transect <- ifelse (transectEv$Transect == 1, "AlongBay"
  , ifelse (transectEv$Transect == "KB", "AlongBay"
    , transectEv$Transect))
transectEv$Transect <- ifelse (transectEv$Transect == "0", "SubBay"
  , transectEv$Transect) ## confirm?!?


## clean up dates/times
stationEv$Date <- ifelse (stationEv$Date == "", "1900-01-01", stationEv$Date) %>%
  word(start = 1L, end = 1L) # only date part
stationEv$Time <- ifelse (stationEv$Time == "", "1900-01-01 00:00", stationEv$Time) %>%
  word (start = 2L)  # only time part
require ("lubridate") # for time-zone adjustment
stationEv$timeStamp <- ymd_hms (paste (stationEv$Date, stationEv$Time, ":00")
  , tz = "America/Anchorage")

stationEv [is.na (stationEv$timeStamp), c (8, 10, 5, 6)]  ## 40 notebook records with missing timestamps
## ignore these, if there are no matching CTD files available. No point trying to interpolate them?



## make relational DB links within notebook DB
tM <- match (stationEv$TransectEvent, transectEv$TransectEvent) ## assuming dates are all correct
if (any (is.na (tM))) {
  print (stationEv [which (is.na (tM)), which (names (stationEv) %in% c("timeStamp", "TransectEvent", "StationEvent", "Station"))])
  #  stop ("no missing transectEvents allowed")  ## not an issue until January 2023 -- Access DB missing transect entries?
  warning ("no missing transectEvents allowed")
  ## trouble-shooting
  x <- stationEv [which (is.na (tM)), ]
  print (summary (factor (x$timeStamp)))
}

## this may be the only needed field from TransectEvent
stationEv$Transect <- factor (transectEv$Transect [tM])
stationEv$Match_Name <- paste0 (transectEv$Transect [tM], "_", stationEv$Station)
rm (transectEv, tM)

stationEv$LonNotes <- with (stationEv, LongitudeDeg - abs (LongitudeMins) / 60)
stationEv$LatNotes <- with (stationEv, LatitudeDeg + LatitudeMins / 60)

badLon <- subset (stationEv, (-160 > LonNotes) | (LonNotes > -130))
if (nrow (badLon) > 1) {
  stop (print (badLon, c(21, 5, 6)))
  #    stationEv <- subset (stationEv, !(StationEvent %in% badLon$StationEvent))
}
rm (badLon)
# summary (stationEv [,c(1:4,22:23)])



## DB-link to master list for positions -- do QAQC first?? -- move to CTD_cleanup.R??
sMatch <- match (stationEv$Match_Name, stnMaster$Match_Name)
stationEv$LonMast <- stnMaster$Lon_decDegree [sMatch]
stationEv$LatMast <- stnMaster$Lat_decDegree [sMatch]
# stationEv$Transect <- stnMaster$Line [sMatch]
# stationEv$Station <- stnMaster$Station [sMatch]
rm (sMatch)





save.image ("~/tmp/LCI_noaa/cache/cnvImportQAQCnotes.RData")
## rm (list=ls()); base::load ("~/tmp/LCI_noaa/cache/cnvImportQAQCnotes.RData")

## QAQC: distance between master-position and note-book position

#################  ---- move this part to cleanup -----------  ###############
# or better to keep here after all???
# pro move: all QAQC to cleanup, keep this one simple
# con: logical to do it right after match, before it's forgotton

require (geosphere)
stationErr <- data.frame (posError = with (stationEv, distHaversine (
  cbind (LonNotes, LatNotes), cbind (LonMast, LatMast)
)))
stationErr$lonErr <- with (stationEv, LonNotes - LonMast)
stationErr$latErr <- with (stationEv, LatNotes - LatMast)
# summary (stationErr$lonErr)
# stationEv [which.max (stationErr$posError), c(21,5,6, 22:25)]

stationEv <- stationEv [order (stationErr$posError, decreasing = TRUE), ]
stationErr <- stationErr [order (stationErr$posError, decreasing = TRUE), ]
stationErr$nm <- stationErr$posError / 1852


x <- data.frame (stationEv, stationErr) [which (stationErr$posError > 1852 * 1.0), ]
if (nrow (x) > 3) {
  ## notebook positions that deviate from original positions -> Jim to check
  print ("Stations with large positional error, exported to badDBpos.csv")
  print (x [order (x$timeStamp)[seq_len(nrow (x))],
    which (names (x) %in% c("timeStamp", "Match_Name", "nm"))])
  # c(21,23,24,25,26,27,29,30, 31, 28, 7)])
  write.csv(x [order (x$timeStamp)[seq_len(nrow (x))], c(21, 23, 24, 25, 26, 27, 29, 30, 31, 28, 7)]
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
stationEv <- stationEv [, -which (names (stationEv) %in%
  c("LatitudeDeg", "LatitudeMins"
    , "LongitudeDeg", "LongitudeMins"
    , "LatMast", "LonMast"))]
## -- this comment still relevant/true?? XXX del??
## just as below with metadata, link cnv names to relevant notebook cast records
## BUT, as long as positions are not great, use master positions


save.image ("~/tmp/LCI_noaa/cache/CNVy.RData")
# rm (list = ls()); base::load ("~/tmp/LCI_noaa/cache/CNVy.RData")
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

## 2012-04-26 -- clock appears to be stuck -- fix here (rather than mess with files) ????
fixN <- c ("2012_04-26_T9_S05_cast299.cnv", "2012_04-26_T9_S06_cast299.cnv"
  , "2012_04-26_T9_S07_cast299.cnv", "2012_04-26_T9_S08_cast299.cnv"
  , "2012_04-26_T9_S09_cast299.cnv", "2012_04-26_T9_S10_cast299.cnv")
fixT <- as.POSIXct (paste ("2012-04-26"
  , c ("18:50", "18:58", "19:20", "19:30", "19:39", "19:57")
  , "AKDT"))
for (i in seq_along(fixN)) {
  fileDB$localTime [which (fileDB$file == fixN [i])] <- fixT [i]
}
rm (fixN, fixT, i, tEr)

save.image ("~/tmp/LCI_noaa/cache/CNVy3.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVy3.RData")


############################################
## run the actual matching by time stamps ##
############################################
# dbLog <- data.frame (i = numeric(), fn = character(), err = numeric()) # for QAQC
for (i in seq_len(nrow(fileDB))) { # could/should use sapply
  fDt <- fileDB$localTime [i]
  dT <- as.numeric (difftime (fDt, stationEv$timeStam, units = "mins"))
  tMatch <- which.min (ifelse (dT > 0, dT, -1 * dT + 5)) ## station recorded before CTD. find smallest positive value (with penalty)
  fileDB$tErr_min [i] <- dT [tMatch]
  if (0) {  ## debugging
    ## Pacific TS for CTD?
    fDt <- with_tz (force_tz (fDt, "America/Los_Angeles")
      , "America/Anchorage")

    tS <- c(17, 18, 6) # , 22)
    print (fDt)
    print (stationEv$timeStamp [tMatch])
    print (fileDB$file [i])
    print (stationEv [tMatch, tS])
    pic <- stationEv [grep (gsub (" .*$", '', as.character (fDt))
      , stationEv$Date), tS]  # better to look in stationEv$timeStamp ??
    # pic <- stationEv [grep ("^2014-02-16", stationEv$Date), tS]
    pic$err <- difftime (fDt, pic$timeStamp, units = "mins")
    print (pic [order (pic$timeStamp), ])
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
  if (abs (dT)[tMatch] < 14) {
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
fnDDF <- data.frame (y = integer(), m = integer(), d = integer()
  , T = character(), S = character())
for (i in seq_along(fnDate)) {
  fnDDF [i, ] <- fnDate [[i]][1:5]
}
# fnDate <- do.call (rbind, fnDate)  # fails
fnDate <- fnDDF; rm (fnDDF)
fnDate <- paste (fnDate [, 1], fnDate [, 2], fnDate [, 3], sep = "-")
fnDate <- gsub ("[A-Z,a-z]*$", "", fnDate) # remove any letters at end of date
mDate <- gsub (" .*$", "", as.character (fileDB$localTime))
fileDB$dateErr <- ifelse (mDate == fnDate, FALSE, TRUE)
rm (fnDate, mDate)
x <- subset (fileDB, dateErr) # [,c(2,6,7)]  ## files with mismatch in file-name and metadata date


if (any (as.numeric (format (x$localTime, "%H")) > 2)) { # allow up to 2 am
  #  stop ("There are mismatches between metadata and file names")
  print ("There are date-mismatches between metadata and file names")
  print (x [order (x$file), c(2, 6, 7)])  ## files where dates in filename and CTD-metadata mismatch. Any shortly after midnight are ok.
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
fileDB$matchN <- sapply (seq_len(nrow(fileDB)), FUN = function(i) {
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

  if (length (grep ("along", stnL)) > 0) {
    tN <- stn [grep ("alongbay", stnL)]
  } else if (length (grep ("sub", stnL)) > 0) {
    tN <- stn [grep ("subbay", stnL)]
  } else {
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
y <- sapply (seq_len(nrow(fileDB)), function(i) {length (fileDB$matchN [[i]])})
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
fileDB$chsm <- ifelse (is.na (fileDB$match_time), 0, 2) +
  ifelse (is.na (fileDB$matchN), 0, 1)
fileDB$chsm <- with (fileDB, ifelse ((chsm == 3) & (matchN != match_time), -1, chsm))
fileDB$chsm <- factor (fileDB$chsm)
summary (fileDB$chsm)
row.names(fileDB) <- seq_len(nrow(fileDB)) # reset for troubleshooting
## 1. check that existing matches are correct
##   chsm == -1: conflict -- trust file name IF time-error small  (still resolve correct time)
##   chsm == 0: missing notebook?? -- use station-list only
##   chsm == 1: station-match, time doesn't (tzone?)
##   chsm == 2: time match, station doesn't. -- concurrent surveys? nudge times?
##   chsm == 3 -- trust those.

fileDB$consensNo <- ifelse ((fileDB$chsm == -1) & (fileDB$tErr_min < 10), fileDB$matchN, NA)


## export chsm == 0 for Jim to look for notebook entries
x <- subset (fileDB, chsm == 0)
x <- x [order (x$localTime), ]
write.csv (x [, c(6, 2)], file = "~/tmp/LCI_noaa/cache/noNotebookEntries.csv", row.names = FALSE)
print ("chsm == 0, i.e. no match_time and no matchN -- no notebook entry? Re-investigate station-match")
## subbays have not been entered -- exclude here
print (x$file[-grep ("su(b|bb)ay", x$file)])
fileDB$consensNo <- ifelse (fileDB$chsm == 0, fileDB$matchN, fileDB$consensNo)

##  Investigate chsm == 2 time ok, station isn't. More time-zone issues?
x <- subset (fileDB, chsm == 2)
j <- 5;     tS <- c(17, 18, 6) # , 22)
rm (x, j)
fileDB$consensNo <- ifelse (fileDB$chsm == 2, NA, fileDB$consensNo)
## alternative strategy -- trust times, not location


## all good: 3
fileDB$consensNo <- fileDB$match_time







## hundreds -- need a different fix -- looking for which issue again??
if (0) {
  for (j in seq_len(nrow(x))) {  ## hundreds -- need a different fix
    i <- as.numeric(row.names(x)[j])
    row.names(x) <- seq_len(nrow(x))
    x [, c(6, 2, 3, 4, 5)]
    cat ("\n\n###\n", j, "\n")
    print (x$localTime [j]) # ctd meta
    print (stationEv$timeStamp [x$match_time [j]]) # notebook -- should be slightly earlier
    print (x$file [j]) # file name
    x [j, ] # full record
    # instSerNo
    stationEv [x$match_time [j], tS]
    pic <- stationEv [grep (gsub (" .*$", '', as.character (x$localTime [j])) # all notes from that day
      , stationEv$Date), tS]  # better to look in stationEv$timeStamp ??
    pic$err <- difftime (fDt, pic$timeStamp, units = "mins")
    print (pic [order (pic$timeStamp), ]) # all notebook entries for that date in chron order


    fx <- fileDB [grep (gsub (" .*$", "", as.character (x$localTime [j]))
      , fileDB$localTime), ] ## all files from day in question
    print (fx [order (fx$localTime), c(6, 2, 3, 5)]) # all files from that date
  }
}
rm (fDt, tS)
## 2. find missing matches: chsm %in% c(0,1)



## debugging
# fileDB [which (is.na (fileDB$matchN))[1],]
# fileDB [which (is.na (fileDB$consensNo))[1],]
# print (length (which (is.na (fileDB$matchN))))




## root-out duplicate files -- do that much earlier!! -- top of this file? or even earlier?
## lots! 304 files!!
print ("files with matching time stamps (duplicates)")
dF <- fileDB [which (duplicated(fileDB$localTime)), ]
dubFiles <- unlist (lapply (seq_len(nrow(dF)), FUN = function(i) {
  fileDB [which (fileDB$localTime == dF$localTime [i]), c(2, 6, 9)]
}))
print (dubFiles)
## speed-up by using sqlite-DF
#  if (0){ ## XXX not working yet === CTD1 ends up empty   XXX
## need to check by hand!! not plausible
if (0) {
  fX <- fileDB
  cX <- CTD1
  if (length (dubFiles) > 0) {
    for (i in seq_along(dubFiles)) { ## remove one of dubFiles-pair from CDT1
      killCand <- fileDB$file [which (fileDB$localTime == dF$localTime [i])]
      ## pick best file to drop
      killName <- killCand [2] # default to keeping 2nd name
      if (diff (nchar(killCand)) > 0) {killName <- killCand [1]} # keep shorter name
      # grep (substr (killName, 1,30), CTD1$File.Name)
      # which (paste0 (CTD1$File.Name, ".cnv") == killName)
      ctdOut <- which (paste0 (CTD1$File.Name, ".cnv") == killName)
      ## account for negative pressures?
      if (length (ctdOut) > 0) {
        CTD1 <- CTD1 [-ctdOut, ]
        fileDB <- fileDB [-which (fileDB$file == killName), ]
      }
      rm (killName, killCand, ctdOut)
      if (nrow (CTD1) == 0) {stop(print (i), " messed up")}
    }
  }
  rm (fX, cX)
  ## there are a few -- delete cnv file!
  # unlink (paste0 (dir, "/", fileDB$path [dF]))
  rm (dF, dubFiles)
}


if (0) { ## MATCH file names to database --- WHAT to do about doubles??? (same station sampled 2x+ per day)
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
  fNEnd <- fNEnd [which ((seq_along(fNEnd)) %% 3 == 0)]
  ## problem: dates in filenames are inconsistent: 2018_10-17_T9_S08_cast072.hex vs 04_24_2019_AlongBay_

  ## test with small example first
  stDB <- "T9_S08"


  ## reverse -- identify surveys in notebook with no matching CTD-CNV file
  for (i in seq_along(fNDB)) { # move to apply
    tS <- as.POSIXct(stationEv$timeStamp)[i]
    stDB <- with (stationEv [i, ], paste0 (ifelse (Transect %in% as.character (3:9), "T", "")
      , Transect
      , "_S", formatC (Station, width = 2, flag = "0")))
    xM <- grep (paste0 (format (tS, "%Y"), "[-_]", format (tS, "%m"), "[-_]"
      , format (tS, "%d"), stDB)
    , fNEnd, ignore.case = TRUE) # , fixed = TRUE)
    if (length (xM) == 0) {
      xM <- grep (fNDB2 [i], fNEnd, ignore.case = TRUE)
    }
    if (length (xM) > 1) {stop (fNDB [i], "is matched to", fNEnd [xM])}
  }
  rm (tS)
}




save.image ("~/tmp/LCI_noaa/cache/CNVyc.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVyc.RData")

#  ls()
## where's notebook data? need lat-lon. Also need masterlist
# "CTD1"      "cX"        "dirL"      "fileDB"    "fX"        "i"         "nCPUs"
# "stationEv" "sTime"
## need: CTD1, fileDB?



### what's below here is based on confusing legacy code -- clean-up needed
## still to do:
##  - match link notebook into physOc
##  - fix missing values from Master_Station
