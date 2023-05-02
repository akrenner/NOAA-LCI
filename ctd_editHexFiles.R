#!/usr/bin/env Rscript
##
## Run this interactively to add notes, transect/station identifiers to hex-file headers and file names
##
## insert headers from notebook database table
## Rename files and save to edited hex files
##

rm (list=ls())

## interactively find folder of new survey
uneditedD <- "~/GISdata/LCI/CTD-processing/Workspace/ctd-data_2017-ongoing/1_Unedited .hex files/2023/2023-02"


newSurvey <- list.files(uneditedD, pattern=".hex", full.names=TRUE, recursive=TRUE)
notes <- list.files(uneditedD, pattern=".csv", full.names=TRUE,recursive=TRUE)
dir.create(gsub ("1_Unedited .hex files", "tmp_2_edited .hex files", uneditedD)
           , showWarnings=FALSE, recursive=FALSE)
noteT <- read.csv (notes)

require ("tidyr")
for (i in 1:length (newSurvey)){
  hex <- scan (newSurvey[i], what="character", sep="\n")

  ## consistency checks:
  ## hex-header and filename report the same cast #
  headCast <- hex [grep ("^\\* cast", hex)] %>%
    substr (start=7, stop=10) %>%
    trimws()
  fnCast <- newSurvey [i] %>%
    substr (start=nchar (newSurvey [i])-6, stop=nchar (newSurvey [i])-4)
  notR <- noteT [match (headCast, noteT$Cast.),]
  notR$Transect <- ifelse (notR$Transect=="AlongBay", "AB", paste0 ("T", notR$Transect))

  ## consistency checks
  ## match by cast number
  ## check by time and date -- stop if off by 5 min or more
  cat (i, headCast, "\n")
  if (headCast != fnCast){stop (hex [i], "filename cast# does not match header\n")}else{
    rm (fnCast)
  }
  ctdTime <- hex [grep ("^\\* cast", hex)] %>%
    substr(start=12, stop=31) %>%
    as.POSIXct(format="%d %b %Y %H:%M:%S")
  deltaT <- difftime (as.POSIXct(paste (notR$Date_isotxt, notR$Time))
                      , ctdTime, units="min") %>%
    abs()
  if (deltaT > 5){stop ("Notes and cast times differ by > 5 min in ", newSurvey [i])}
  ## end of checks

  ## add geographic coordinates, station match_name, etc. to hex-header
  eHex <- c (hex [1:6]
             , paste0 ("** Cook Inlet LTM ", notR$Transect)
             , paste0 ("** Date: ", notR$Date_isotxt)
             , paste0 ("** Station:S", sprintf ("%02d", as.numeric (notR$Station)))
             , paste0 ("** Start Time: ", notR$Time)
             , paste0 ("** Depth (m): ", notR$Depth)
             , paste0 ("** Latitude:", notR$Lat_dDeg)
             , paste0 ("** Longitude:", notR$Lon_dDeg)
             # , paste0 ("** Comment: ", notR$C)
             , hex [7:length (hex)])

  ## define new filename, following convention
  nFN <- paste0 (format (as.POSIXct (notR$Date_isotxt), "%Y_%m-%d_")  ## new filename
                 , notR$Transect, "_S", notR$Station, "_cast", headCast, ".hex")
  nD <- gsub ("1_Unedited .hex files/.+"
              , paste0 ("tmp_2_edited .hex files/"
                        , format (ctdTime, "%Y"), "/"
                        , notR$SurveyName, "/")
              , newSurvey [i])
  dir.create(nD, showWarnings=FALSE, recursive=TRUE)
  write.table (eHex, file=paste0 (nD, nFN), append=FALSE, quote=FALSE, row.names=FALSE
               , col.names=FALSE)
}


cat ("\n############################################################################")
cat ("\n##\n##\n##\n## After successful run and inspection: zip-up unedited .hex files! ## \n##\n##\n##\n")
cat ("\n############################################################################")

## EOF
