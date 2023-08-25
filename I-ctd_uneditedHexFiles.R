#!/usr/bin/env Rscript
##
## Run this interactively to add notes, transect/station identifiers to hex-file headers and file names
##
## insert headers from notebook database table
## Rename files and save to edited hex files
##

## needs work -- not the most recent version?

rm (list=ls())

## interactively find folder of new survey
unedDL <- list.dirs("~/GISdata/LCI/CTD-processing/Workspace/ctd-data_2017-ongoing/1_Unedited .hex files/")
uneditedD <- unedDL [length (unedDL)-1] ## skip "Troubleshooting"
# uneditedD <- "~/GISdata/LCI/CTD-processing/Workspace/ctd-data_2017-ongoing/1_Unedited .hex files/2023/2023-07"

## automatically fine the most recent survey based on modification time of notebookTable.csv file
notes <- list.files ("~/GISdata/LCI/CTD-processing/Workspace/ctd-data_2017-ongoing/1_Unedited .hex files/"
                     , pattern=".csv", full.names=TRUE, recursive=TRUE)
uneditedD <- dirname(notes [which.max (file.mtime(notes))])

hexF <- list.files(uneditedD, pattern=".hex", full.names=TRUE, recursive=TRUE)
notes <- list.files(uneditedD, pattern=".csv", full.names=TRUE,recursive=TRUE)
noteT <- read.csv (notes)
noteT <- subset (noteT, Type=="CTD")
noteT$Station <- as.character(noteT$Station) # in case all are numeric

require ("tidyr")
for (i in seq_along(hexF)){
  hex <- scan (hexF[i], what="character", sep="\n")

  ## consistency checks:
  ## hex-header and filename report the same cast #
  headCast <- hex [grep ("\\* cast", hex)] %>%
    substr (start=7, stop=10) %>%
    trimws() %>%
    as.numeric()
  fnCast <- hexF [i] %>%
    substr (start=nchar (hexF [i])-6, stop=nchar (hexF [i])-4) %>%
    as.numeric()
  notR <- noteT [match (headCast, noteT$Cast.),]
  notR$Transect <- ifelse (notR$Transect=="AlongBay", "AB", paste0 ("T", notR$Transect))

  ## consistency checks
  ## match by cast number
  ## check by time and date -- stop if off by 5 min or more
  cat (i, "cast:", headCast, "\n")
  if (headCast != fnCast){stop (hex [2], "filename cast# does not match header\n")}else{
    rm (fnCast)
  }
  ctdTime <- hex [grep ("^\\* cast", hex)] %>%
    substr(start=12, stop=31) %>%
    as.POSIXct(format="%d %b %Y %H:%M:%S")
  deltaT <- difftime (as.POSIXct(paste (notR$Date_isotxt, notR$Time))
                      , ctdTime, units="min") %>%
    abs()
  if (deltaT > 5){stop ("Notes and cast times differ by > 5 min in ", hexF [i])}
  ## end of checks

  ## add geographic coordinates, station match_name, etc. to hex-header
  cL <- c (grep ("Cruise:", hex)  ## in case that standard headers had been added
           , grep ("Station:", hex)
           , grep ("Latitude:", hex)
           ,grep ("Longitude:", hex)
  )
  if (length (cL)>0){hex <- hex [-cL]}
  rm (cL)
  # notR$Station <- ifelse (nchar (notR$Station) < 4, paste0 ("S", notR$Station)
  #                         , notR$Station)
  eHex <- c (hex [1:6]
             , paste0 ("** Ship:", notR$Vessel)
             , paste0 ("** Cook Inlet LTM ", notR$Transect)
             , paste0 ("** Station:", notR$Station)  #sprintf ("%02d", as.numeric (notR$Station)))
             , paste0 ("** Date: ", notR$Date_isotxt)
             , paste0 ("** Start Time: ", notR$Time)
             , paste0 ("** Depth (m): ", notR$Depth)
             , paste0 ("** Latitude:", notR$Lat_dDeg)
             , paste0 ("** Longitude:", notR$Lon_dDeg)
             # , paste0 ("** Comment: ", notR$C)
             , hex [7:length (hex)])

  ## define new filename, following convention

  headCast <-   sprintf("%03d", headCast)
  if (nchar (notR$Station) < 4){
    notR$Station <- as.numeric (notR$Station)
    stn <-  paste0 ("S", sprintf ("%02d", notR$Station))
  }else{
    stn <- notR$Station
  }
  nFN <- paste0 (format (as.POSIXct (notR$Date_isotxt), "%Y_%m-%d_")  ## new filename
                 , notR$Transect, "_", stn
                 , "_cast", headCast, ".hex")
  nD <- gsub ("1_Unedited .hex files/.+"
              #, paste0 ("tmp_2_edited .hex files/"
              , paste0 ("2_edited .hex files/"
                        , format (ctdTime, "%Y"), "/"
                        , notR$SurveyName, "/")
              , hexF [i])
  if (length (grep ("aircast", hexF [i])) < 1){  # skip aircasts
    dir.create(nD, showWarnings=FALSE, recursive=TRUE)
    write.table (eHex, file=paste0 (nD, nFN), append=FALSE, quote=FALSE, row.names=FALSE
                 , col.names=FALSE)
  }
}


cat ("\n############################################################################")
cat ("\n##\n##\n##\n## After successful run, inspect hex files in edited folder ## \n##\n##\n##\n")
cat ("\n############################################################################")

## EOF
