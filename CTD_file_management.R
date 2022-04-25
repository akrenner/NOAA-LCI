#!/usr/bin/env Rscript

## automate movement and reoganization of CTD hex and CNV files
## still need Jim to reprocess files for alignment


## BUGS and open issues
#
## streamline: don't need to copy files twice:
# create fDB, edit file names in there (newname), copy to final directory with newname
## QAQC
# move comparison with Notebook-DB from CTD_cnv-Import to here?
# ignore FixMeta completely?
## finding best con-file: always use con-file in the past or whatever is closest?


cat (rep ("##\n", 4), "reassertain whether metadata or filename is used for fixing date -- file name should take precedent! \n##\n")



# rm (list = ls())

if (!exists ("hexFileD")){hexFileD <- "~/GISdata/LCI/CTD-processing/Workspace/"}


# TESTrun <- TRUE
# TESTrun <- FALSE


## make this independent from ctd_workflow.R
if (!exists ("hexFileD")){
  rm (list = ls())
  hexFileD <- "~/GISdata/LCI/CTD-processing/Workspace/"
}


# unlink ("~/GISdata/LCI/CTD-startover/allCTD/", recursive = TRUE)  ## careful!! -- overkill
unlink ("~/GISdata/LCI/CTD-processing/allCTD/edited_hex", recursive = TRUE)
unlink ("~/GISdata/LCI/CTD-processing/allCTD/hex2process", recursive = TRUE)
set.seed (8)


## define new destinations
nD <- "~/GISdata/LCI/CTD-processing/allCTD/edited_hex/"
instL <- c ("4141", "5028", "8138")  # do this from data?
instL <- c ("4141", "5028")  # do this from data?
# XXX add new CTD to this list!!!
## clean slate -- do this by hand!

# x <- unlink (nD, recursive = TRUE, force = TRUE) # not working on Win??
x <- c (unlink (paste0 (nD, instL [1], "/"))
        , unlink (paste0 (nD, instL [2], "/")))
# print (x)
for (i in 1:length (instL)){
 dir.create (paste0 (nD, instL [i]), recursive = TRUE)
}
rm (x, instL)
## mkdir?




rL <- function (f, p = NULL){ # recursive listing of files
  ## NULL because some files are .txt -- gobble all up
  x <- list.files(paste0 (hexFileD, f)
#  "~/GISdata/LCI/CTD-processing/Workspace/", f)
             , pattern = p, ignore.case = TRUE, recursive = TRUE
             , full.names = TRUE)
  # print (length (x))
  return (x)
  }




## move about HEX files
fL <- rL("ctd-data_2012-2016/2_Edited\ HEX/") #, p = ".hex")
fL <- c(fL, rL ("ctd-data_2017-ongoing/2_Edited\ .hex\ files/"))
fL <- c(fL, rL ("ctd-data-KBL_Interns_and_Partners/Updated\ Text\ Files\ CTD\ 2012-2013", p = ".txt"))
fL <- c(fL, rL ("YSI-2016", p = ".hex")) # Steve Kibler
## add unedited files -- those would be marked as duplicate, coming in 2nd, if concerning the
## same cast and still having the same filename.
fL <- c(fL, rL ("ctd-data_2016/1_Unedited\ HEX"))
fL <- c(fL, rL ("ctd-data_2017-21/1_Unedited .hex files/"))
# print (length (fL))
rm (rL)


## bad files out
bF <- grep ("Troubleshooting", fL)
if (length (bF) > 0){
  cat ("removing ", length (bF), " bad files\n")
  fL <- fL [-bF]
}


## manually remove duplicates -- if any -- none found
## check duplicates by name or sha256
fDB <- data.frame (file = fL
                   , fN = gsub ("^.*/", "", fL)
)
rm (fL)
# fDB <- fDB[with (fDB, order (file, fN)),]
fDB$isD <- duplicated (fDB$fN)

if (0){
  require ("openssl")
  fDB$sha <- sapply (1:nrow (fDB), function (i){
    hF <- file (fDB$file [i], open = "r", raw = TRUE)
    require (openssl)
    x <- sha256 (hF)
    close (hF)
    return (as.character (x))
  })
  if (any (duplicated(fDB$sha))){
    print (sum (duplicated (fDB$sha)), "files are duplicate")
    warning ("fix duplicates first")
  }
}# tail (fDB)
##





##################################################################
## move files into new folder structure -- the main action item ##
##################################################################

fDB$copy <- logical (nrow (fDB))
fDB$file <- as.character (fDB$file)

for (i in 1:nrow (fDB)){
  hF <- file (fDB$file [i], open = "r", raw = TRUE)
  hx <- readLines(hF, n = 10)
  close (hF)

  hx <- gsub ("^.* ", "", grep ("Conductivity SN", hx, value = TRUE))
  fDB$instN [i] <- hx
  fDB$copy [i] <- file.copy (from = fDB$file [i]
                              , to = paste0 (nD, fDB$instN [i], "/")
                              , recursive = FALSE
             , overwrite = FALSE, copy.date = TRUE)
}
rm (hF)
print (summary (factor (fDB$copy)))

## rename any .txt files to .hex
iN <- list.files (nD, pattern = ".txt$", ignore.case = TRUE
                  , recursive = TRUE, full.names = TRUE)
cpCk <- file.copy (from = iN, to = gsub (".txt", ".hex", iN, fixed = TRUE)
           #, copy.date = TRUE
           , recursive = FALSE)
if (any (!cpCk)){print (summary (cpCk))} # all should be TRUE
rm (cpCk)
unlink (iN)


print (length (list.files (nD, pattern = ".hex", recursive = TRUE, ignore.case = TRUE)))
print (nrow(fDB))


## weed out undesirables, bad/air casts
iN <- list.files (nD, pattern = "bad", ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
iN <- c (iN, list.files (nD, pattern = "air", ignore.case = TRUE, recursive = TRUE, full.names = TRUE))
unlink (iN)
rm (iN, i, fDB)  # fDB is no longer valid, after bad/aircasts removed, build new

## end of first, main step
## next step:
## fix-up malformed file names and metadata
## place hex files with their matching conf/xmlconf file
# save.image ("~/tmp/LCI_noaa/cache/ctdHex1.RData")
## rm (list = ls()); load  ("~/tmp/LCI_noaa/cache/ctdHex1.RData")







#####
## check that file names are consistent with meta-data date -- move this elsewhere?
##   -- need this to make sure conf-file folders are right
## treat iN location as a cache (move it elsewhere, then unlink?)
## modify files in iD, then move to final destination
#####


## manually fix-up files -- here or on original files?
fixMeta <- function (patternfind, goodDate){
  if (0){ ## do nothing -- no longer needed
  badMeta <- list.files (nD, pattern = patternfind # need to fix files before starting fDB
                       ,recursive = TRUE, full.names = TRUE)
  for (i in 1:length (badMeta)){
    hF <- file (badMeta [i], open = "r", raw = TRUE)
    hX <- readLines (hF)
    close (hF)
#    badDate <- grep ("Date: ", hX)  ## doesn't alway say "Date"
#    hX [badDate] <- gsub (badDate, goodDate)  ## could skip "badDate, just write new
    hX [8] <- paste ("** Date:", goodDate)
    write (hX, file = badMeta [i])
  }
  rm (badMeta, hF, hX)
  }
}
fixFN <- function (patternfind, goodpattern){
  badFN <- list.files (nD, pattern = patternfind # need to fix files before starting fDB
                       ,recursive = TRUE, full.names = TRUE)
  goodFN <- gsub (patternfind, goodpattern, badFN)
  cpCk <- file.copy (from = badFN, to = goodFN#, copy.date = TRUE
                     , recursive = FALSE)
  unlink (badFN)
  #  return (cpCk) # make this silent/invisible?
}
fixBoth <- function (patternfind, goodpattern, goodDate){
 fixMeta (patternfind, goodDate)
 fixFN (patternfind, goodpattern)
}
delFile <- function (filename){
  ## patternfind <- gsub ("-", "\\\\-", filename)
  badFN <- list.files (nD, pattern = filename, recursive = TRUE, full.names = TRUE)
  unlink (badFN, force = TRUE)
}
inspFile <- function (patternfind, nr = 80){
  badFN <- list.files (nD, pattern = patternfind, recursive = TRUE, full.names = TRUE)
  if (length (badFN)>1){print (gsub ("^.*/", "", badFN))}else{
    print (gsub ("^.*/", "", badFN))
    hF <- file (badFN, open = "r")
    # print (readLines (hF, n = 50))
    hX <- readLines (hF, n = nr)
    close (hF)
    # print (hX)
    return (hX)
  }
}


## 20125_04 -- impossible date. Wrong metadata and FN
fixBoth ("20125_04-12", "2015_04-12", "2015-04-12")  # make sure all on 12th!
# fixBoth ("04_24_2019", "2019-04-24", "2019-04-24")
fixFN ("04(_|-)24(_|-)2019", "2019_04-24")
fixFN ("04(_|-)29(_|-)2019", "2019_04-24")
fixFN ("2019(_|-)15(_|-)14", "2019_05-14")


## manually fix-up files with discrepancies metadata vs filename

## bad metadata date, filename is correct. Start-time matches correct notebook
## Date given in metadata was also sampled, BUT already has file that checks out ok
## T9 was sampled on 2017-01-11 AND 2017-02-07.
fixMeta ("2017_02-07_T9_S", "2017-02-07")

## no documentation, no times, no station -- unable to use
## (UNLESS shifting all stations one forward??!?) XXX!!!  XXX --> Jim/Kris
## 1-13 is in hex files, notebook has 1:14  XXX!!!XXX
## metdata times also MESSED UP (ignore for now)

# delFile ("2012_02-12_AlongBay_NotinFieldNotes_cast033")
# ## rest of that day-transect have the wrong date/year
# fixFN (patternfind = "2012_02-12_AlongBay_SKB", "2013_02-12_AlongBay_SKB")
for (i in 1:13){
  fixFN (paste0 ("2012_02-12_AlongBay_SKB"  # station has leading zero 0
                 , formatC (i, width = 2, format = "d", flag = "0"))
         , paste0 ("2013_02-12_AlongBay_SKB"
                   , formatC (i+1, width = 2, format = "d", flag= "0"))
  )
}
fixFN ("2012_02-12_AlongBay_NotinFieldNotes_cast033"
       , "2013_02-12_AlongBay_SKB01_cast033")

## 2018_07-29_T3_S16_cast189 -- Notebook and metadata confirms date to be 2018-07-26
for (i in 2:6){
  fixFN (paste0 ("2018_07-29_T3_S1", i, "_cast1", 94-i)
       , paste0 ("2018_07-26_T3_S1", i, "_cast1", 94-i)
  )
}
# fixFN ("2018_07-29_T3_S12_cast193", "2018_07-26_T3_S12_cast193")
# fixFN ("2018_07-29_T3_S13_cast192", "2018_07-26_T3_S13_cast192")
# fixFN ("2018_07-29_T3_S14_cast191", "2018_07-26_T3_S14_cast191")
# fixFN ("2018_07-29_T3_S15_cast190", "2018_07-26_T3_S15_cast190")
# fixFN ("2018_07-29_T3_S16_cast189", "2018_07-26_T3_S16_cast189")

## 2017_12-14_AlongBay_SKB03_cast127, recorded by sys-clock as 2017-4-18 -- investigate!

## Required END line not found: -- SEABIRD XXX can fix this?? -- examine! XXX
delFile ("2012-07-30-CookInlet-Tran6-cast091")  ## must be RegEx
delFile ("2012_07-30_T6_S21_cast091.hex")


## bad / surface or otherwise empty casts -- those that fail in CTD_cnv-Import.R
## better to cut those out here (where other file management is done)
## or in CTD_cnv-Import.R where failure occurs?
## the later might give a chance to fix these ??
# => move to CTD_cnv-Import.R!
# delFile ("2012-10-29-cookinlet-tran4-cast065-s07_4141")
# delFile ("2012_10-29_t4_s07b_cast065_4141")
# delFile ("2013-04-19-cookinlet-tran6-cast076-s05b_5028")
# delFile ("2013_04-19_t6_s05b_cast076_5028")
# delFile ("2015_06-26_t9_s01b_cast117_5028")
# delFile ("2015_06-26_t9_s06b_cast111_5028")
# delFile ("2021_05-01_ab_s06_surface-duplicate_cast036_5028")



for (i in 1:10){## ambiguous file -- fix this!! ask Jim?
#  fixFN (paste0 ("2018_01-17_T9_S", formatC (i, width = 2, format = "d", flag = "0"))
#         , paste0 ("2018-01-"))
}

## 2014_02-15_T9_S03 to ..S10
## no cruise on 2013-02-15 (as metadata). Cruise on 2014-02-15 (as metadata),
## but not this transect/station. However, this was sampled on 2014-02-14,
## according to notebook. No files so far on 2014-02-14. Go with notebook.
fixBoth ("2014_02-15_T9_S", "2014_02-14_T9_S", "2014-02-14")

## date ambigous -- force it one way
fixBoth ("2012-10-28-and-29-CookInletOcean-Tran7"
         , "2012-10-28_T7", "2012-10-28")
fixBoth ("2012-10-28-CookInlet-Tran6-cast\\d*-", "2012-10-28_T6-", "2012-10-28")
fixMeta ("2012_10-28_T7_", "2012-10-28")


#if (0){
## 2013_06-19-T9, metadata: 2013-06-06. Notebook: no survey on 2013-06-06
# 2013_06-06_T9_S01_cast161 also has bad metadata -> fix both
# surveys on both dates -- metadata stuck from previous?
fixMeta("2013_06-19_T9_S", "2013-06-19")

## T9andTutka 2013-07-09 -- no cruise on 2017-07-05 as metadata claims
fixMeta ("2013_07-09_T9andTutka_", "2013-07-09")

## 2012_05-31a_T9_S01a_cast002 -- metadata not filled in ("xxxx Tide (x of 2 lines)")
fixMeta ("2012_05-31(a|b)_T9", "2012-05-31")
fixMeta ("2012_06-05(a|b)_T9", "2012-06-05")

## no year in metadata
fixMeta ("2012_08-15_Subbays_", "2012-08-15")
fixMeta ("2014_07-21", "2014-07-21")
fixMeta ("2014_07-22", "2014-07-22")
fixMeta ("2014_07-23", "2014-07-23")
fixMeta ("2014_07-24", "2014-07-24")
fixMeta ("2014_08-13", "2014-08-13")
fixMeta ("2014_08-14", "2014-08-14")
fixMeta ("2014_10-13", "2014-10-13")
fixMeta ("2014_10-18", "2014-10-18")
fixMeta ("2014_10-19", "2014-10-19")
fixMeta ("2014_11-25_T9_", "2014-11-25")
fixMeta ("2014_12-17_T9_", "2014_12-17")
fixMeta ("2015_01-16_T9_", "2015_01-16")
fixMeta ("2015_02-12", "2015_02-12")

fixMeta ("2016_02-16_T7", "2016-02-16")
#}


## end of file manipulations
# rm (inspFile)
# save.image ("~/tmp/LCI_noaa/cache/ctdHex2.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdHex2.RData")



## gather metadata-dates and filename dates
## setup file-database
fL <- list.files (nD, pattern = ".hex$", ignore.case = TRUE, recursive = TRUE
                  , full.names = TRUE)
fDB <- data.frame (file = fL); rm (fL)
fDB$shortFN <- gsub ("^.*/", "", fDB$file)
fDB$metDate <- as.POSIXct(rep (NA, nrow (fDB)))
fDB$instN <- character (nrow (fDB))


# ## quick test-run for prototyping
# if (TESTrun == TRUE){
#   # fDB <- fDB [sample (1:nrow (fDB), size = nrow (fDB)/10, replace = FALSE)]
#   fDB <- fDB [1:(nrow (fDB)/10),]
# }


## filename-dates
x <- substr (fDB$shortFN, 1, 10)  ## extract dates
x <- gsub ("_+", "-", x)          ## fix up messes
x <- gsub ("-+", "-", x)
# x <- gsub ("-$", "", x)
fDB$fnDate <- as.POSIXct(x)
if (any (is.na (fDB$fnDate))){
  cat (subset (fDB$shortFN, is.na (fDB$fnDate)))
  stop ("are bad file names")
}
rm (x)


## gather metadata dates
## use these to check against filename dates

for (i in 1:nrow (fDB)){
  hF <- file (fDB$file [i], open = "r", raw = TRUE)
  hX <- readLines(hF, n = 90)
  close (hF)
  #eL <- grep (" S>$", trimws (hX))
  #hD <- hX [eL - 1]
  # hD <- hX [69]  ## try system clock instead, as per Jim's suggestion
  hX <- trimws (hX)
  hD <- hX [grep ("mag switch$", hX)]
  if (length (hD) < 1){hD <- hX [grep ("low batt$", hX)]}
  hD <- trimws (substr (hD, 11, 23))
  x <- try (as.POSIXct (hD, format = "%d %b %Y"), silent = TRUE)
  if (0){  # old stuff -- manually entered header data -- a mess
    hD <- hX [8] # grep ("** Date:", hX, fixed = TRUE, value = TRUE) # always #8?
    hD <- gsub ("^.*: ", "", hD) # for a myriad of different forms
    hD <- gsub ("**", "", hD, fixed = TRUE)
    #hD <- gsub ("^.* ", "", hD) # can't delete white space: "28 Oct 2012"
    hD <- gsub ("_", "-", hD)
    x <- try (as.POSIXct (hD), silent = TRUE)

    xT <- function (y){((class (y)[[1]] == "try-error") || is.na (y))} # test date format for creativity

    if (xT (x)){  # date in metadata is inconsistent
      x <- try (as.POSIXct (hD, format = "%d %b %Y"))
    }
    if (xT (x)){
      x <- try (as.POSIXct (gsub ("\\*\\* ", "", hX [7]), format = "%Y_%M-%d"))
    }
    rm (xT)
  }
  fDB$metDate [i] <- x
  hX <- gsub ("^.* ", "", grep ("Conductivity SN", hX, value = TRUE))
  fDB$instN [i] <- hX
}
rm (hF, hD, hX, i)
save.image ("~/tmp/LCI_noaa/cache/ctdHexConv2.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdHexConv2.RData")





## QAQC
summary (fDB$metDate)
# if (any (is.na (fDB$metDate))){   ## too many to go through 1x1
#   i <- min (which (is.na (fDB$metDate)))
#   stop ("NAs in meta data date")
# }
## show files with bad metDate -- investigate and fix above! XXX
# fDB$shortFN [is.na (fDB$metDate)][1:50]
# tail (fDB$shortFN [is.na (fDB$metDate)])
#   inspFile("2013_06-28_T9andTutka_S07_cast199", n = 75)[8]
# inspFile ("2018_07-29_T3_S16_cast189", n = 75)


lBad <- length (fDB$shortFN [is.na (fDB$metDate)])

if (lBad > 0){
  warning ("Still got files with bad metadata dates.
           Inspect these manually and fix in code above.")
  for (i in 1:length (lBad)){
    fixMeta (fDB$shortFN [i], fDB$fnDate)
  }
  # fDB$metDate <- ifelse (is.na (fDB$metDate), fDB$fnDate, fDB$metDate) # messes up POSIXct -- BUG!?
  # attributes (fDB$metDate) <- attributes (fDB$fnDate) # unfortunate behaviour of ifelse on POSIXct
  # bI <- which (is.na (fDB$metDate))
  # fDB$metDate [bI] <- fDB$fnDate [bI]
  require (dplyr)
  fDB$metDate <- if_else (is.na (fDB$metDate), fDB$fnDate, fDB$metDate) # avoids POSIXct trouble of ifelse
}
rm (lBad)

## apply fixMeta?? -- yes, to avoid trouble down the road?

rm (fixBoth, fixFN, fixMeta, delFile)



#####
## check that file names are consistent with meta-data date
##   -- need this to make sure conf-file folders are right
#####


fDB$tErr <- as.numeric (difftime(fDB$fnDate, fDB$metDate, units = "days"))
summary (fDB$tErr)
if (any (abs (fDB$tErr) > 1)){
  x <- subset (fDB [abs (fDB$tErr) >1 , 2:ncol (fDB)])
  print (x[order (abs (x$tErr), decreasing = TRUE)[1:30],1:5]); rm (x)

  #  stop ("fix date discrepancies between metadata and file names")
  warning ("fix date discrepancies between metadata and file names") # -- do do it later in CTD_cnv-Import.R
#  for (i in 1:nrow (fDB))
#    if (fDB$tErr[i] > 1){
#      fixMeta (fDB$shortFN [i], fDB$fnDate)
#    }
#  fDB$metDate <- if_else (fDB$tErr > 1, fDB$fnDate, fDB$metDate) # avoids POSIXct trouble of ifelse
  ## FIX Filename!!
}
## fix metDate above to make this unneccessary,
## or fix this so metDate does not become numeric

## for now, fix discrepancy in favor of filename (here?)


## thought: cut-out date-filename, replace with metDate

# save.image ("~/tmp/LCI_noaa/cache/ctdHexConv3.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdHexConv3.RData")



## manipulate shortF, short filename to implement corrections
## (ultimately, skip all the in-file corrections above)
# add instrument-serial to filename for troubleshooting later (temp?)
fDB$shortFNx <- sapply (1:nrow (fDB), function (i){
  gsub (".hex$", paste0 ("_", fDB$instN [i], ".hex"), fDB$shortFN [i])
  })





####################################################
## conf files
## create directories named after conf files
## copy conf files to those directories
## copy respective hex files to those directories
## clean up caches
####################################################

cDir <- "~/GISdata/LCI/CTD-processing/Workspace/conFiles/"
cFDB <- data.frame (file = list.files (cDir, full.names = FALSE))
cFDB$date <- gsub (".(xmlcon|CON)", "", cFDB$file)
cFDB$dir <- paste0 ("~/GISdata/LCI/CTD-processing/allCTD/hex2process/"
                    , cFDB$date, "/")
cFDB$inst <- substr (x = cFDB$file, start = 11, stop = 14)

for (i in 1:nrow (cFDB)){ # copy config files to their folders
  dir.create (cFDB$dir [i], showWarnings = FALSE, recursive = TRUE)
  file.copy (from = paste0 (cDir, cFDB$file [i]), to = cFDB$dir [i]) #, copy.date = TRUE)
}
cFDB$date <- gsub ("^SBE19plus_(4141|5028)_", "", cFDB$date)
cFDB$date <- as.POSIXct (paste0 ("1-", cFDB$date), format = "%d-%b-%Y")
cFDB$inst <- substr (cFDB$file, start = 11, 14)
cFDB <- cFDB [order (cFDB$date),]

if (0){ ## read calibration date
for (i in 1:nrow (cFDB)){
  hX <- file (paste (cFDB$dir [i], cFDB$file [i], sep = "/"))
  hD <- readLines(hX)
  close (hX)
  hD <- grep ("CalibrationDate", hD, value = TRUE)
  hD <- trimws (gsub ("CalibrationDate", "", hD, fixed = TRUE))
  hD <- gsub ("(<>|</>)", "", hD)
  hD <- ifelse (nchar (hD) < 11
                , paste0 (substr (hD, 1, 7), "20", substr (hD, 8, 11))
                , hD)
  hD <- toupper(hD)
  cDate <- as.POSIXct (hD, format = "%d-$b-%Y")
}
}



## flag and fix bad file dates!
### END OF EDITS
# data.frame (fDB, x = x)[is.na (fDB$fnDate), c (which (names (fDB) %in% c ("fnDate", "shortFN")), ncol (fDB)+1)]
## gotta fix x from above -- file name date extraction -- bad filenames, good dates




## copy hex files
## assign appropriate con file to each HEX file
fDB$procDir <- character (nrow (fDB))
fDB$calDist <- numeric (nrow (fDB)) # days since calibration
for (i in 1:nrow (fDB)){
  ## assume file name date to be more reliable than metadata throughout!
#  dT <- as.numeric (difftime(fDB$metDate [i], cFDB$date, units = "days"))
  dT <- as.numeric (difftime(fDB$fnDate [i], cFDB$date, units = "days"))
  dT <- ifelse (cFDB$inst == fDB$instN [i], dT, dT + 1e9) # penalize wrong inst
  dT <- ifelse (dT < 0, dT*-1+365*20, dT)  # penalize records in the future (SEABIRD fails) -- 20 records
  fDB$calDist [i] <- min (dT)
  fDB$procDir [i] <- cFDB$dir [which.min (dT)]
}

## exclude those files for now pre-dating the first calibration file
# fDBx <- subset (fDB, fDB$calDist < 365*10) # should never be greater than 10 years
fDBx <- fDB

## copy files to new/final directory
cpCk <- with (fDBx, file.copy (from = file, to = paste0 (procDir, shortFNx)))  ## could manipulate shortFN!
if (any (!cpCk)){
  print (summary (cpCk))
  stop ("copy of hex files failed")
}
rm (cpCk)
unlink (nD, recursive = TRUE, force = TRUE)

### missing feature: !!! XXX

## remove config-directories with no hex files
## -- this is currently not automated, but has to be done manually
## i.e. config files are staged in a separate folder until data has been collected.




## make small dataset for testing
x <- lapply (levels (as.factor (gsub ("/hex2process/", "/hex2test/", fDB$procDir)))
             , FUN = dir.create, recursive = TRUE, showWarnings = FALSE); rm (x)
xmlC <- list.files ("~/GISdata/LCI/CTD-processing/allCTD/hex2process", ".xmlcon"
                    , recursive = TRUE, ignore.case = TRUE, include.dirs = TRUE)
## remove 8138 and 2021 callibration until they've been used!
# xmlC <- grep ("SBE19plus", xmlC, value = TRUE)
xmlC <- xmlC [-grep ("2021", xmlC)]

file.copy (paste0 ("~/GISdata/LCI/CTD-processing/allCTD/hex2process/", xmlC)
           , paste0 ("~/GISdata/LCI/CTD-processing/allCTD/hex2test/", xmlC))
rm (xmlC)

fDB2 <- list.files ("~/GISdata/LCI/CTD-processing/allCTD/hex2process/", ".HEX"
                    , recursive = TRUE, ignore.case = TRUE, include.dirs = TRUE)
set.seed (8)
fDB2 <- sample (fDB2, 30) ## better to force distribution, but good for now -- happens to work
file.copy (paste0 ("~/GISdata/LCI/CTD-processing/allCTD/hex2process/", fDB2)
           , paste0 ("~/GISdata/LCI/CTD-processing/allCTD/hex2test/", fDB2))


## EOF
