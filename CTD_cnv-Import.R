#!/usr/bin/env Rscript

## (c) Martin Renner
## licence: GPL-3

## NOAA/KBRR LCI study
## based on dataSetup.R -- pulled-out CTD import section.
## new dataSetup.R to read-in output file produced here instead of doing the
## heavy lifiting of CTD import and plotting itself.


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

fN <- list.files ("~/GISdata/LCI/CTD/6\ DERIVE/", ".cnv", full.names = FALSE)
# fN <- list.files ("~/GISdata/LCI/CTD", recursive = TRUE)
fN <- list.files ("~/tmp/LCI_noaa/cache/hex-raw CTD files --reprocess/"  ## tmp to try file names!
                  , recursive = TRUE, full.names = FALSE)
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

save.image ("~/tmp/LCI_noaa/cache/CNVx.RData")  ## this to be read by dataSetup.R
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVx.RData")


## start-over this part -- read metadata/field notes from KBRR ACCESS database


if (0){# ideal: read-in data from ACCESS database via ODBC -- may be not worth the troubles
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

}else if (1){ # manually exported tables, read-in those data and link to existing tables
  # rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CNVx.RData")
  stationEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblStationEvent.csv")
  transectEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblTransectEvent.csv")
#  sampleEv <- read.csv ("~/GISdata/LCI/EVOS_LTM_tables/tblSampleEvent.txt")

  ## clean up dates/times
  stationEv$Date <- ifelse (stationEv$Date == "", "1900-1-1", stationEv$Date)
  stationEv$Time <- ifelse (stationEv$Time == "", "1900-01-01 00:00", stationEv$Time)
  stationEv$timeStamp <- paste (format (as.POSIXct (stationEv$Date), "%Y-%m-%d")
                               , format (as.POSIXct (stationEv$Time), "%H:%M"))
  stationEv$timeStamp <- ifelse (stationEv$timeStamp < as.POSIXct("1901-01-01 00:00")
                                 , NA, stationEv$timeStamp)

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
    print (x [order (x$timeStamp)[1:nrow (x)], c(21,23,24,25,26,27,29,30, 31, 28, 7)])
  }
  rm (x)

  # pdf ("~/tmp/LCI_noaa/media/badPositions_CTD.pdf")
  # plot (LonNotes~LonMast, stationEv, asp = 1)
  # plot (LatNotes~LatMast, stationEv, asp = 1)
  #
  # plot (LatNotes~LonNotes, stationEv, asp = 1)
  # plot (LatMast~LonMast, stationEv, asp = 1)
  # dev.off()
  ## eoedits
  rm (stationErr)


  ## fill-in missing coordinates in Notebook with generic station positions
  stationEv$LonNotes <- with (stationEv, ifelse (is.na (LonNotes), LonMast, LonNotes))
  stationEv$LatNotes <- with (stationEv, ifelse (is.na (LatNotes), LatMast, LatNotes))
  ## remove surplus fields
  stationEv <- stationEv [,-which (names (stationEv) %in%
                                     c("LatitudeDeg", "LatitudeMins"
                                       , "LongitudeDeg", "LongitudeMins"
                                       , "LatMast", "LonMast"))]

  ## just was below with metadata, link cnv names to relevant notebook cast records
## BUT, as long as positions are not great, use master positions


  ## end of edits !!

  ## MATCH file names to database --- WHAT to do about doubles??? (same station sampled 2x+ per day)
  fNDB <- with (stationEv, paste0 (format (timeStamp, "%Y-%m-%d")
                                   , ifelse (Transect %in% as.character (3:9), "T_", "")
                                   , Transect, "_S", Station)
  )

}else {



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

}


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





save.image ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## this to be read by dataSetup.R

cat ("\n\n#\n#\n#", format (Sys.time(), format = "%Y-%m-%d %H:%M"
                          , usetz = FALSE)
, " \n# \n# End of dataSetup.R\n#\n#\n")
## EOF
