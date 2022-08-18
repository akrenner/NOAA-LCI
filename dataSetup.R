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
print (Sys.time())


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
  if (1){
    pdf (paste0 ("~/tmp/LCI_noaa/media/", fN, ".pdf"), ...)
  }else{
    if (width > 50){
      width <- 100*width
      height <- 100*height
    }else{
      width <- 480; height <- 480
    }
    png (paste0 ("~/tmp/LCI_noaa/media/", fN, ".png"), ...) # automatically adjust scale?
  }
}

if (!require("pacman")) install.packages("pacman"
  , repos = "http://cran.fhcrc.org/", dependencies = TRUE)
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


## importing CNV files processed earlier is hopeless:
## need to process HEX files using Seabird BATCH files! See
## http://www.comm-tec.com/Training/Training_Courses/SBE/Module%203%20Basic%20Data%20Processing.pdf
## this is done by scripts called in ctd_workflow.R

# load (paste0 (dirL[4], "/CNV1.RData")) # get physOc and stn from CTD_cleanup.R
Require ("tidyverse")
aD <- "~/GISdata/LCI/CTD-processing/aggregatedFiles"  ## annual data
aD <- "~/tmp/LCI_noaa/data-products/CTD/"             ## latest cutting-edge data
physOcT <- list.files(aD, pattern=".csv", full.name=TRUE) %>%
  lapply (read.csv, skip=1, header=TRUE) %>%
  bind_rows
rm (aD)
physOc <- with (physOcT, data.frame (Match_Name=Station
                                     , isoTime=as.POSIXct (paste (Date, Time))
                                     , latitude_DD=Latitude_DD
                                     , longitude_DD=Longitude_DD
                                     , Transect
                                     , File.Name=factor (File.Name), CTD.serial
                                     , Bottom.Depth
                                     , Pressure..Strain.Gauge..db.=pressure_db
                                     , Depth.saltwater..m.=Depth
                                     , Temperature_ITS90_DegC, Salinity_PSU
                                     , Density_sigma.theta.kg.m.3
                                     , Oxygen_umol_kg=Oxygen_umol.kg
                                     , Oxygen_sat.perc.=Oxygen.Saturation_perc
                                     # need SBE O2 concentration umol.kg in here
                                     , Nitrogen.saturation..mg.l.  ## make it umol.kg
                                     , PAR.Irradiance
                                     , Fluorescence_mg_m3
                                     , turbidity=Turbidity
                                     , beamAttenuation=Beam_attenuation
                                     , beamTransmission=Beam_transmission
))
rm (physOcT)

stn <- read.csv ("~/GISdata/LCI/MasterStationLocations.csv")
stn <- subset (stn, !is.na (Lon_decDegree))
stn <- subset (stn, !is.na (Lat_decDegree))
stn$Plankton <- stn$Plankton == "Y"



# save.image ("~/tmp/LCI_noaa/cache/cachePO1.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/cachePO1.RData")
## Require ("XLConnect")
## stn <- readWorksheetFromFile ("~/GISdata/LCI/MasterStationLocations.xlsx", sheet = 1)


# save.image ("~/tmp/LCI_noaa/cache/CTD.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData"); require (parallel)



## plotting and analyis of CTD data outsourced from here to anaCTD.R



## Kris:
## - presistance of mixing across seasons and tides
## fluorescence in total water column?


############################################
## univariate summaries for each CTD cast ##
############################################


poSS <- with (physOc, data.frame (File.Name = levels (File.Name)))

poM <- match (poSS$File.Name, physOc$File.Name)

poSS$Match_Name <- physOc$Match_Name [poM]
poSS$latitude_DD <- physOc$latitude_DD [poM]
poSS$longitude_DD <- physOc$longitude_DD [poM]
poSS$timeStamp <- physOc$isoTime [poM]
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
## XXX
## should not need this -- fix in CTD processing!!
poSS <- subset (poSS, !is.na (poM))
rm (poM)


## multiple samples of same station on one day!
## x <- summary (factor (poSS$SampleID_H), maxsum = 10000)
## sort (poSS$File.Name [poSS$SampleID_H %in% names (which (x>1))])
## x <- summary (factor (poSS$SampleID), maxsum = 10000)
## sort (poSS$File.Name [poSS$SampleID %in% names (which (x>1))])

# poSS$maxDepth <- aggregate (Depth.saltwater..m.~File.Name ## fails -- needed??
#                  , data = physOc, FUN = max)$Depth.saltwater..m.
## not sure where NAs are coming from, but this fixes it.
## Better to take maxDepth from station master list
poSS$maxDepth <- rep (NA, nrow (poSS))
mD <- aggregate (Depth.saltwater..m.~File.Name
                          , data = physOc, FUN = max)
poSS$maxDepth <- mD$Depth.saltwater..m. [match (poSS$File.Name, mD$File.Name)]
rm (mD)

dMean <- function (fn, fldn){
    ## calculate mean of field for the last 10 m above the bottom
    cast <- subset (physOc, File.Name == fn)
    lLay <- c (-10, 0) + max (cast$Depth.saltwater..m.)
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
    timetable <- data.frame (Station = kasbay, DateTime =
                               seq (tstmp -12*3600, tstmp + 12*3600, by = 600)
                             , stringsAsFactors = FALSE)
    ## return (diff (range (timetable$DateTime)))
    tid <- tide_height_data (timetable)
    return (diff (range (tid$TideHeight)))
}
## this step takes a while! [approx 5 min, depending on computer]
poSS$tideRange <- unlist (mclapply (poSS$timeStamp, FUN = tRange, mc.cores = nCPUs))
## rerun tideRange
if (1){ # keep in case mclapply fails
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
    rm (i)
  }
}
rm (tRange)
## tidal phase
tPhase <- function (tstmp, lat, lon){
  ## return radians degree of tidal phase during cast
  Require ("suncalc")
  poSS$sunAlt <- with (poSS, getSunlightPosition (data = data.frame (date = timeStamp, lat = latitude_DD, lon = longitude_DD)))$altitude # , keep = "altitude")) -- in radians
  ## Require (oce)
  ## poSS$sunAlt <- with (poSS, sunAngle(timeStamp, longitude = longitude_DD, latitude = latitude_DD, useRefraction = FALSE)
}
rm (tPhase)


daylight <- function (dt){
  require ("suncalc")
  sunAlt <- getSunlightPosition (date = dt
                                 , lat = 59.643, lon = -151.526)$altitude # in radians
  sunDeg <- sunAlt / pi * 180
  dayNight <- ifelse (sunDeg > -6, "day", "night")  # civil twighlight
  dayNight <- ifelse (sunDeg > 0, "day", "night")  # direct solar radiation
  dayNight
}
poSS$dayLight <- daylight (poSS$timeStamp)
rm (daylight)

agg <- function (var, ..., refDF){ # more robust in case of NA
  agDF <- aggregate (var~File.Name, data = physOc, ...)
  agDF [match (refDF$File.Name, agDF [,1]),2]
}

poSS$aveTemp <- aggregate (Temperature_ITS90_DegC~File.Name, data = physOc
                           , FUN = mean)$Temperature_ITS90_DegC

poSS$SST <- agg (physOc$Temperature_ITS90_DegC, subset = physOc$Depth.saltwater..m. <= 3
                       , FUN = mean, na.rm=TRUE, refDF=poSS)
poSS$minTemp <- aggregate (Temperature_ITS90_DegC~File.Name, data = physOc
                           , FUN = min)$Temperature_ITS90_DegC
poSS$deepTemp <- unlist (mclapply (poSS$File.Name, FUN = dMean, fldn = "Temperature_ITS90_DegC"
                                   , mc.cores = nCPUs))
poSS$SSS <- agg (physOc$Salinity_PSU, FUN=mean, na.rm = TRUE, refDF=poSS)
poSS$aveSalinity <- aggregate (Salinity_PSU~File.Name, data = physOc
                               , FUN = mean)$Salinity_PSU
poSS$deepSal <- unlist (mclapply (poSS$File.Name, FUN = dMean, fldn = "Salinity_PSU"
                                  , mc.cores = nCPUs))
poSS$aveDens <- unlist (mclapply (poSS$File.Name, FUN = dMean, fldn = "Density_sigma.theta.kg.m.3"
                                  , mc.cores = nCPUs))

rm (dMean, agg)
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
# poSS$minO2 <- sAgg ("Oxygen_SBE.43..mg.l.")
poSS$minO2 <- sAgg ("Oxygen_umol_kg")
# poSS$O2perc <- sAgg ("Oxygen.Saturation.Garcia.Gordon.umol_kg")
poSS$O2perc <- sAgg ("Oxygen_sat.perc.")
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
# is.na (poSS$PARdepth1p) <- poSS$sunAlt < 0
is.na (poSS$PARdepth5p) <- poSS$sunAlt < 0

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

save.image ("~/tmp/LCI_noaa/cache/sampleTable.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/sampleTable.RData")



###################
## phytoplankton ##
###################

phyp <- read.csv ("~/GISdata/LCI/KBL-Phytoplankton.csv")
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

zoop <- read.csv ("~/GISdata/LCI/Kachemak\ Bay\ Zooplankton.csv", as.is = TRUE)

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
zoop$Species <- ifelse (zoop$Species == "M\xfcller's larvae", "Mullers larvae", zoop$Species)
zoop$Species <- tolower (as.character (zoop$Species))
if (length (grep ("plastic", zoop$Species)) > 0){
  zoop <- zoop [-grep ("plastic", zoop$Species),]
}
# substring (zoop$Species, 1,1) <- toupper (substring (zoop$Species, 1,1)) # not working ?
zoop$Species <- paste (toupper (substring (zoop$Species, 1,1)) , substring (zoop$Species, 2, 100), sep = "")
print (sort (levels (factor (zoop$Species))))


save.image ("~/tmp/LCI_noaa/cache/fileDump.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/fileDump.RData")

## export zooplankton data to standardized file (matching first columns as in CTD aggregates)
zoopOut <- with (zoop, data.frame (Station = Match_Name), Date = isoDate, Time
                 , SampleID = SampleID_H
                 , Latitude_DD = Lat_decDegree
                 , Longitude_DD = Lon_decDegree, Mesh, Flow, Water.Sampled_m3 = Water.Sampled_..m3.
                 , Split
                 , Species
                 , Count = RTotal)
write.csv(zoopOut, row.names = FALSE
          , file = "~/tmp/LCI_noaa/data-products/CookInletKachemakBay_Zooplankton.csv")
rm (zoopOut)

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


# Require ("sp"); Require ("rgdal"); Require ("rgeos") # for gBuffer
Require ("sp") ; Require ("sf")


spTran <- function (x, p4){
    # proj4string (x) <- CRS ("+proj=longlat +datum=WGS84 +ellps=WGS84")
#    Require ("rgdal")
  Require ("sf")
  suppressWarnings (y <- spTransform (x, CRS (p4)))
    return (y)
}


## find all seabird observations within XX km of station at the same date as zoop station
## dependent on a R-dump file -- load straight from NPPSD?
base::load ("~/tmp/NPPSDv2countW_-1.RData")
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



cat ("\n\n## phytoplankton stations with missing coordinates:\n
     ## removed for now -- XXX but need to fix in MasterStationLocations.csv")
print (summary (factor (phyCenv$Match_Name [is.na (phyCenv$lon)])))
phyC <- subset (phyC, !is.na (phyCenv$lon))
phyCenv <- subset (phyCenv, !is.na (phyCenv$lon))

## geographically overlay seabirds and physical oceanography stations
print (poSS [which (is.na (poSS$longitude_DD) | is.na (poSS$latitude_DD)),])

stnP <- stn
coordinates (stnP) <- ~Lon_decDegree+Lat_decDegree
coordinates (poSS) <- ~longitude_DD+latitude_DD
coordinates (phyCenv) <- ~lon+lat
coordinates (zooCenv) <- ~Lon_decDegree+Lat_decDegree
coordinates (NPPSD2) <- ~lon+lat
proj4string (stnP) <- LLprj
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
# Require ("sf")
# Require ("terra") # replacement for raster
# coast <- terra::intersect(coastSP, bP)
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
plot (stnP, col = "red", pch =2, add = TRUE)
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

## need to supply absolute path because raster object is just a pointer.
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

stnP <- spTran (stnP, pr)
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

## match stnP and birds at different levels of buffer
# for (i in length (stnB)){

Require ("rgeos"); Require ("parallel")
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

stnT <- subset (stnP, grepl ("[1-9]|AlongBay", stn$Line)) # excl one-off stations
stnT <- subset (stnP, stnP$Plankton) ## better subset here from stnT? XX

lBuff <- gBuffer (stnT, width = bDist (stnT), byid = TRUE)
## lBuff <- st_buffer (stnT, dist=bDist(stnT))  ## sf version, substituting retiring rgeos--not working like this
rm (stnT)

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

rm (findBirds, lBuff, birds)
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


## save CTD data for oceanographic processing. poSS = summary data -> signatureData
save (stn, physOc, file="~/tmp/LCI_noaa/cache/CTDcasts.RData") ## for the wall, etc. -- add coastline and bathymetry
rm (physOc)                             # no needed any more, poSS takes it place

stn <- stnP  # for zoop and other scripts that use sp
save.image ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")


cat ("\n\n#\n#\n#", format (Sys.time(), format = "%Y-%m-%d %H:%M"
                          , usetz = FALSE)
, " \n# \n# End of dataSetup.R\n#\n#\n")

## EOF
