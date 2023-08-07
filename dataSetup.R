#!/usr/bin/env Rscript

## (c) Martin Renner
## licence: GPL-3

## NOAA/KBRR LCI study
## read in plankton and oceanographic data
## geographically link with seabird data
## interannual comparison in relation to SST
##
## re-arrange data columns, add a metadata-line and produce CSV files for
## data publication (zooplankton, phytoplankton. CTD done elsewhere)


rm (list = ls())
## CAREFUL with this!!
# system ("rm -r ~/tmp/LCI_noaa/")
# unlink ("~/tmp/LCI_noaa/", recursive = TRUE)
print (Sys.time())


deepThd <- 15 ## bottom threshold -- everything above considered surface,
              ## everything below bottom water
              ## use 30 m as cut-off for deep-water, sure to be below pycnocline

## for GWA report
printSampleDates <- TRUE
printSampleDates <- FALSE


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


require ("sp")  ## move to sf

Seasonal <- function (month){           # now using breaks from zoop analysis -- sorry for the circularity
    month <- as.numeric (month)
    cut (month, breaks = c(-13,0,2,4,8,10, 25)
            , labels = c ("fall", "winter", "spring", "summer", "fall", "winter"))
}


require ("parallel")
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

# base::load (paste0 (dirL[4], "/CNV1.RData")) # get physOc and stn from CTD_cleanup.R
require ("tidyverse")
aD <- "~/GISdata/LCI/CTD-processing/aggregatedFiles"  ## annual data
aD <- "~/tmp/LCI_noaa/data-products/CTD/"             ## latest cutting-edge data
                      # forcing physOct$Station to "character" would be convenient
                      # (trouble when some CSV files have only numeric, others also characters)
                      # only pick-up GulfWatch casts here (?)
physOcT <- list.files(aD, pattern="Cook[a-zA-Z0-9_]*.csv$", full.name=TRUE) %>%
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



## add new derived variable: slope of density gradient
## best to do this here = ??
## plan A: calculate slope for each step
## plan B: fit smoothing spline and produce derivative
# cast <- factor (paste0 (physOc$Match_Name, physOc$isoTime))
# physOc$densityGradient <- sapply (1:length (levels (cast))
#                                   , function (i){
#                                     cst <- subset (physOc, cast == levels (cast)[i])
#                                     slp <- (lag (cst$Density_sigma.theta.kg.m.3) - cst$Density_sigma.theta.kg.m.3) /
#                                       (lag (cst$Depth.saltwater..m.)- cst$Depth.saltwater..m.)
#                                     #slp <- data.frame (gradient=slp)
#                                     slp
#                                   }) %>%
#   unlist
physOc$bvf <- sapply (1:length (levels (physOc$File.Name))  ## this is nearly identical to d-dens/d-sigma
                                  , function (i){
                                    require ("oce")
                                    cast <- subset (physOc, File.Name == levels (physOc$File.Name)[i])
                                    bvf <- oce::swN2 (pressure=cast$Pressure..Strain.Gauge..db.
                                                       , sigmaTheta=cast$Density_sigma.theta.kg.m.3
                                                       , derivs="smoothing" # "simple" ## or "smoothing"
                                                       # , df="simple"
                                                       )
                                    bvf
                                  }) %>%
  unlist
physOc$bvf <- ifelse (is.na (physOc$bvf), 0, physOc$bvf)

stn <- read.csv ("~/GISdata/LCI/MasterStationLocations.csv")
stn <- subset (stn, !is.na (Lon_decDegree))
stn <- subset (stn, !is.na (Lat_decDegree))
stn$Plankton <- stn$Plankton == "Y"



## Kris:
## - persistence of mixing across seasons and tides
## fluorescence in total water column?


############################################
## univariate summaries for each CTD cast ##
############################################


## set-up headers of poSS
poSS <- with (physOc, data.frame (File.Name = levels (File.Name)))
poM <- match (poSS$File.Name, physOc$File.Name)

poSS$Match_Name <- physOc$Match_Name [poM]
poSS$Transect <- physOc$Transect [poM]

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
                                     )) ## in case there are 1 samples per day -- include hour
## XXX
## should not need this -- fix in CTD processing!!
poSS <- subset (poSS, !is.na (poM))
rm (poM)


## multiple samples of same station on one day!
## x <- summary (factor (poSS$SampleID_H), maxsum = 10000)
## sort (poSS$File.Name [poSS$SampleID_H %in% names (which (x>1))])
## x <- summary (factor (poSS$SampleID), maxsum = 10000)
## sort (poSS$File.Name [poSS$SampleID %in% names (which (x>1))])

dMean <- function (fn, fldn){  ## bottom Mean
    ## calculate mean of field for the last 10 m above the bottom
    cast <- subset (physOc, File.Name == fn)
    lLay <- c (-10, 0) + max (cast$Depth.saltwater..m.)
    outM <- mean (subset (cast [,which (names (cast) == fldn)]
                        , (lLay [1] < cast$Depth.saltwater..m.)
                          ))
    return (outM)
}

agg <- function (var, ..., refDF){ # more robust in case of NA
  agDF <- aggregate (var~File.Name, data = physOc, ...)
  agDF [match (refDF$File.Name, agDF [,1]),2]
}
poSS$maxDepth <- agg (physOc$Depth.saltwater..m., FUN=max, na.rm=TRUE,refDF=poSS)



## date/time-based conditions: daylight and tidal cycle
require ("rtide")
tRange <- function (tstmp){
    ## uses NOAA tide table data
    ## only available for US, but 8x faster than webtide (with oce)
    require ("rtide")
    kasbay <- tide_stations ("Kasitsna.*")
    timetable <- data.frame (Station = kasbay, DateTime =
                               seq (tstmp -12*3600, tstmp + 12*3600, by = 600)
                             , stringsAsFactors = FALSE)
    ## return (diff (range (timetable$DateTime)))
    tid <- tide_height_data (timetable)
    return (diff (range (tid$TideHeight)))
}
## this step takes a while! [approx 5 min, depending on computer]
poSS$tideRange <- unlist (mclapply (poSS$timeStamp, FUN = tRange, mc.cores=nCPUs))
## rerun tideRange
if (0){ # keep in case mclapply fails
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
tPhase <- function (tstmp, lat, lon){  ## REVIEW THIS!
  ## return radians degree of tidal phase during cast
  require ("suncalc")
  poSS$sunAlt <- with (poSS, getSunlightPosition (data = data.frame (date = timeStamp, lat = latitude_DD, lon = longitude_DD)))$altitude # , keep = "altitude")) -- in radians
  ## require (oce)
  ## poSS$sunAlt <- with (poSS, sunAngle(timeStamp, longitude = longitude_DD, latitude = latitude_DD, useRefraction = FALSE)
}
# POss$tidePhase <- unlist (mclapply (poSS$timeStamp, mc.cores=nCPUs))
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




## temperature derivaties

poSS$TempMean <- aggregate (Temperature_ITS90_DegC~File.Name, data = physOc
                           , FUN = mean)$Temperature_ITS90_DegC

poSS$SST <- agg (physOc$Temperature_ITS90_DegC, subset=physOc$Depth.saltwater..m. <= 3
                       , FUN = median, na.rm=TRUE, refDF=poSS)
poSS$TempSurface <- agg (physOc$Temperature_ITS90_DegC, subset=physOc$Depth.saltwater..m. <= deepThd
                 , FUN = mean, na.rm=TRUE, refDF=poSS)
poSS$TempDeep <- agg (physOc$Temperature_ITS90_DegC, subset=physOc$Depth.saltwater..m. > deepThd
                      , FUN = mean, na.rm=TRUE, refDF=poSS)
poSS$TempMin <- agg (physOc$Temperature_ITS90_DegC, FUN=min, refDF=poSS)
poSS$TempBottom <- unlist (mclapply (poSS$File.Name, FUN=dMean, fldn="Temperature_ITS90_DegC"
                                   , mc.cores=nCPUs))
poSS$TempMax <- agg (physOc$Temperature_ITS90_DegC, FUN=max, refDF=poSS)

## salinity derivatives
poSS$SSS <- agg (physOc$Salinity_PSU, FUN=mean, na.rm=TRUE, refDF=poSS)
poSS$SalMean <- agg (physOc$Salinity_PSU, FUN = mean, refDF=poSS)
poSS$SalSurface <-agg (physOc$Salinity_PSU, subset=physOc$Depth.saltwater..m. <= deepThd
                       , FUN=mean, na.rm=TRUE, refDF=poSS)
poSS$SalDeep <- agg (physOc$Salinity_PSU, subset=physOc$Depth.saltwater..m. > 30 # deepThd
                      , FUN=mean, na.rm=TRUE, refDF=poSS)
poSS$SalBottom <- unlist (mclapply (poSS$File.Name, FUN=dMean, fldn="Salinity_PSU"
                                  , mc.cores=nCPUs))

## density and other seawater properties
poSS$DensMean <- agg (physOc$Density_sigma.theta.kg.m.3, FUN=mean, na.rm=TRUE, refDF=poSS)
poSS$DensBottom <- unlist (mclapply (poSS$File.Name, FUN = dMean, fldn = "Density_sigma.theta.kg.m.3"
                                  , mc.cores = nCPUs))
rm (dMean, agg)
## poSS$aveSpice <- aggregate (Spice~File.Name, data = physOc, FUN = mean)$Spice
## almost the same als salinity. skip it

## derived seawater summary statistics
require ("parallel")
wcStab <- function (fn){
    ## calculate water column stability for a given File.Name as difference density between upper and lower water layer
    ## any reference to this??
    uLay <- c (0,3)
    lLay <- c (30,35)
    cast <- subset (physOc, File.Name == fn)
    lLay <- c (-5, 0) + max (cast$Depth.saltwater..m.) # lowest 5 m instead of fixed depth
                                        # some casts only 2 or 5 m deep -- then what?
    uDens <- mean (subset (cast, (uLay [1] < Depth.saltwater..m.) &
                                 (Depth.saltwater..m. < uLay [2]), na.rm = TRUE)$Density_sigma.theta.kg.m.3)
    lDens <- mean (subset (cast, (lLay [1] < Depth.saltwater..m.) &
                                 (Depth.saltwater..m. < lLay [2]), na.rm = TRUE)$Density_sigma.theta.kg.m.3)
    stabIdx <- (lDens - uDens) - (mean (lLay) - mean (uLay))
    stabIdx <- ifelse (is.infinite (stabIdx), NA, stabIdx)
    return (stabIdx)
#    return ((lDens - uDens)/(mean (lLay) - mean (uLay)))
#     return (lDens - uDens)
}
poSS$stability <- unlist (mclapply (poSS$File.Name, FUN = wcStab, mc.cores = nCPUs))
rm (wcStab)

## physOc$N2 <- median, max, mean N2 in deep-water section -- this should be a
## good indicator of presence of deep-water salinity gradient
# XXX replace mclapply with agg, wherever possible! XXX
poSS$bvfMean <- unlist (mclapply (poSS$File.Name, mc.cores = nCPUs, FUN=function (fn){
  cast <- subset (physOc, File.Name==fn)
  mean (cast$bvf, na.rm=TRUE)
}))

poSS$bvfMax <- unlist (mclapply (poSS$File.Name, mc.cores = nCPUs, FUN=function (fn){
  cast <- subset (physOc, File.Name==fn)
  max (cast$bvf, na.rm=TRUE)
}))

poSS$pclDepth <- unlist (mclapply (poSS$File.Name, mc.cores=nCPUs, FUN=function (fn){
  cast <- subset (physOc, File.Name==fn)
  cast$Depth.saltwater..m. [which.max (cast$bvf)]
}))
## freshwater content
poSS$FreshWaterCont <- unlist (mclapply (poSS$File.Name, mc.cores=nCPUs, FUN=function (fn){
  require ("readr")
  fW <- subset (physOc, (File.Name==fn) & (Depth.saltwater..m. <= deepThd))  ## surface layer only  use deepThd XXX
  sum (33 - fW$Salinity_PSU, na.rm=TRUE) ## max recorded = 32.75
}))
poSS$FreshWaterContDeep <- unlist (mclapply (poSS$File.Name, mc.cores=nCPUs, FUN=function (fn){
  require ("readr")
  fW <- subset (physOc, (File.Name==fn) & (Depth.saltwater..m. > deepThd))
  sum (33 - fW$Salinity_PSU, na.rm=TRUE) ## max recorded = 32.75
}))
poSS$FreshWaterContDeep2 <- unlist (mclapply (poSS$File.Name, mc.cores=nCPUs, FUN=function (fn){
  require ("readr")
  fW <- subset (physOc, File.Name==fn) %>%
    subset (Depth.saltwater..m. > 40)
  sum (33 - fW$Salinity_PSU, na.rm=TRUE) ## max recorded = 32.75
}))



## which depth-cutoff?
if (1){
  png ("~/tmp/LCI_noaa/media/FreshWate20-40.png", height=11*200, width=8*200, res=200)
  par (mfrow=c(3,1))
  plot (FreshWaterContDeep~timeStamp, poSS, type="l")
  plot (FreshWaterContDeep2~timeStamp, poSS, type="l")
  # plot (FreshWaterCont~timeStamp, poSS)
  # lines (FreshWaterCont30~timeStamp, poSS, col="red")
  plot (FreshWaterContDeep~FreshWaterContDeep2, poSS)
  dev.off()
}

## plant stuff
sAgg <- function (varN, data = physOc, FUN = sum, ...){
  aDF <- aggregate (formula (paste (varN, "File.Name", sep = "~"))
                    , data, FUN, ...)
  return (aDF [match (poSS$File.Name, aDF$File.Name),2])
}

poSS$Fluorescence <- sAgg ("Fluorescence_mg_m3")
poSS$minO2 <- sAgg ("Oxygen_umol_kg", FUN=min)
poSS$O2perc <- sAgg ("Oxygen_sat.perc.", FUN=mean)
if ("turbidity" %in% names (physOc)){
    ## poSS$turbidity <- sAgg ("turbidity", FUN = mean)
    ## print (summary (poSS$turbidity))
    poSS$logTurb <- sAgg ("turbidity", FUN = function (x){mean(log10(x))}
                        , subset = physOc$Depth.saltwater..m. < 20)
}else{
    cat ("\nno turbidity here\n")
}
rm (sAgg)
## PAR going nowhere?
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
is.na (poSS$PARdepth5p) <- poSS$sunAlt < 0

save.image ("~/tmp/LCI_noaa/cache/cachePO1.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/cachePO1.RData")


pT <- poSS
pT$Year <- as.numeric (format (pT$timeStamp, "%Y"))

## quick overview of sampling ##
if (printSampleDates){
  cat ("\n\nCTD sampling dates\n")
  pT <- subset (pT, Year > 2016)
  length (levels (factor (pT$Match_Name)))

  pT$Transect <- factor (pT$Transect)
  for (i in 1:length (levels (pT$Transect))){
    cat ("\n\n", levels (pT$Transect)[i], "\n")
    print (sort (levels (factor (
      subset (pT, Transect==levels (pT$Transect)[i])$timeStamp
    ))))
  }
  rm (pT)
}


## QAQC --- should go elsewhere!!!

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


## bad geographic positions: on land, too far south, .. -- move this into CTD processing scripts
## calc distance between actual and planned station position
SCo <- stn[match (poSS$Match_Name, stn$Match_Name), names (stn) %in% c("Lon_decDegree"
                                                                     , "Lat_decDegree")]
SCo <- as.matrix (cbind (SCo, cbind (poSS$longitude_DD, poSS$latitude_DD)))
require ("fields")
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

phyp <- read.csv ("~/GISdata/LCI/phytoplankton/phytoplankton.csv"
                  , skip=1)
# phyp <- read.csv ("~/GISdata/LCI/KBL-Phytoplankton-2017-02.csv")
# phyp <- read.csv ("~/GISdata/LCI/KBL-Phytoplankton-2018-10-sorted.csv")
## current 2018 version does not have date
phyp <- cbind (Match_Name = trimws (phyp$Station), phyp)
phyp$Match_Name <- gsub ("Transect\\s([1-9]),\\sStation\\s([0-9]+)", "\\1_\\2", phyp$Match_Name)
# phyp$Match_Name <- gsub ("\\s([A-D])$", "_\\1", phyp$Match_Name) # space to underscore
phyp$Match_Name <- gsub ("\\sBay", "", phyp$Match_Name) # del Cove
phyp$Match_Name <- gsub ("\\sCove", "", phyp$Match_Name) # del Bay
phyp$Match_Name <- gsub ("\\sLab", "", phyp$Match_Name) # del Lab
phyp$Match_Name <- gsub ("^Kachemak\\s", "AlongBay_", phyp$Match_Name)
phyp$Match_Name <- gsub ("^China\\sPoot", "ChinaPoot", phyp$Match_Name)
phyp$Match_Name <- gsub ("\\s", "_", phyp$Match_Name) # space to underscore


## any way to recover 'unknown 2021'??
levels (factor (phyp$Date))
phyp <- subset (phyp, Date != "unknown 2021")
phyp <- subset (phyp, Date != "")


levels (factor (phyp$Match_Name [which (is.na (match (phyp$Match_Name, stn$Match_Name)))]))
phyp <- cbind (stn [match (phyp$Match_Name
                           , stn$Match_Name), which (names (stn) %in% c("Lon_decDegree", "Lat_decDegree"))]
               , timeStamp = as.POSIXlt (phyp$Date) ## XXX should have time as well!!! XXX temp!!
               , phyp)
names (phyp)[1:2] <- c("lon", "lat")

## add: month, year, SampleID
# require ("tidyverse")
require (magrittr) # for pipe!
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
phyCenv <- phyp [,c(1:which (names (phyCenv) == "Station") # or use Match_Name? difference?
                    , which (names (phyp) == "Total.cell.count"))]
phyC <- phyp [,which (names (phyp) == "Alexandrium.spp.") :
                     which (names (phyp) == "unknown.pennate.diatom")]
## check that phyC are integers?!
sort (apply (phyC, 2, function (x){
  sum (x %% 1)
}))
rm (phyp)

## quick overview of sampling ##
if (printSampleDates){
  cat ("\n\nPhytoplankton sampling dates\n")
  pT <- subset (phyCenv, Year > 2020)
  pT$Transect <- factor (pT$Transect)
  for (i in 1:length (levels (pT$Transect))){
    cat ("\n\n", levels (pT$Transect)[i], "\n")
    print (sort (levels (factor (
      subset (pT, Transect==levels (pT$Transect)[i])$timeStamp
    ))))
  }
  rm (pT, i)
}

#################
## zooplankton ##
#################

zoop <- read.csv ("~/GISdata/LCI/Kachemak\ Bay\ Zooplankton.csv", as.is = TRUE)

## require ("XLConnect")
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
zoop$Match_Name <- gsub ("^_", "", zoop$Match_Name)
refN <- match (zoop$Match_Name, stn$Match_Name)
## zoop$Match_Name [is.na (refN)]    # bad zoop stations that don't match master list
# add geographic coordinates from stn
if (any (is.na (refN))){stop ("zooplankton station does not match reference list")}
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
zoop <- subset (zoop, !is.na (zoop$isoDate))
#### zoop <- zoop [,match (zoop$SampleID, poSS$SampleID)]


### fix species names
## fix Mueller's larvae in source!
zoop$Species <- ifelse (zoop$Species == "M\xfcller's larvae", "Mullers larvae", zoop$Species)
zoop$Species <- ifelse (zoop$Species == "Müller's larvae", "Mullers larvae", zoop$Species)
zoop$Species <- tolower (as.character (zoop$Species))
if (length (grep ("plastic", zoop$Species)) > 0){
  zoop <- zoop [-grep ("plastic", zoop$Species),]
}
# substring (zoop$Species, 1,1) <- toupper (substring (zoop$Species, 1,1)) # not working ?
zoop$Species <- paste (toupper (substring (zoop$Species, 1,1)) , substring (zoop$Species, 2, 100), sep = "")
print (sort (levels (factor (zoop$Species))))

## lookup depth from field notes
base::load ("~/tmp/LCI_noaa/cache/FieldNotes.RData") ## sam
zoop$Depth <- sam$Depth [match (zoop$SampleID, sam$SampleID)]
zoop$Depth <- ifelse (zoop$Depth > 60, 50, zoop$Depth)

save.image ("~/tmp/LCI_noaa/cache/fileDump.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/fileDump.RData")

## export zooplankton data to standardized file (matching first columns as in CTD aggregates)
zoopOut <- with (zoop, data.frame (Station = Match_Name, Date = isoDate, Time
                 , Latitude_DD = Lat_decDegree
                 , Longitude_DD = Lon_decDegree
                 , Transect
                 , StationN=Station
                 , Depth
                 , Mesh, Flow, Water.Sampled_m3=Water.Sampled..m3.
                 , SampleID = SampleID_H
                 , Split
                 , Species
                 , Count = RTotal))

zoopOut$StationN <- ifelse (zoopOut$StationN %in% 1:100
                            , paste0 ("S_", zoopOut$Station), zoopOut$Station)
tF <- "~/tmp/LCI_noaa/data-products/zooplankton_KachemakBay.csv"
write (paste0 ("## Collected as part of GulfWatch on predefined stations in Kachemak Bay/lower Cook Inlet. CTD sampled on every station. Concurrent CTD and phytoplankton on select stations. 2012-2021.")
        , file=tF, append=FALSE, ncolumns=1)
suppressWarnings(write.table(zoopOut, file=tF, append=TRUE, quote=FALSE, sep=","
                              , na="", row.names=FALSE, col.names=TRUE))
rm (zoopOut, tF)


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
write.csv (zooC, "~/tmp/LCI_noaa/media/ZoopCommunity.csv")
## print (summary (zooC))


## corrected water volume to use as off-set
## density = count * split / volume
zoop$volSample <- with (zoop, Water.Sampled..m3. / Split) ## remove when safe to do so
zoop$volSampleM3 <- with (zoop, Flow *0.283 / Split )  ## 0.283 -- used throughout in Excel file
## merge zoop with poSS
## zooCenv <- zoop [!duplicated (zoop$SampleID_H), c(1:12, which (names (zoop) == "volSample"))]
zooCenv <- zoop [match (row.names (zooC), zoop$SampleID_H)
                ,c (1:12, which (names (zoop) == "volSampleM3"))]
# zooCenv$zoopDensity <-
zooCenv$Year <- as.numeric (format (zooCenv$timeStamp, "%Y"))
rm (zoop)
## check that poSS data is available for every zooCenv sample
summary (zooC$SampleID %in% poSS$SampleID)
summary (zooC$SampleID_H %in% poSS$SampleID_H)

if (printSampleDates){
  ## quick overview of sampling ##
  cat ("\n\nZooplankton sampling dates\n")
  pT <- subset (zooCenv, Year > 2016)
  pT$Transect <- factor (pT$Transect)
  for (i in 1:length (levels (pT$Transect))){
    cat ("\n\n", levels (pT$Transect)[i], "\n")
    print (sort (levels (factor (
      subset (pT, Transect==levels (pT$Transect)[i])$timeStamp
    ))))
  }
  z <- subset (zooC, zooCenv$Year > 2016)

  rm (pT, i, z)
}
##  spplot (zoop, "RTotal")
## cluster analysis and/or DCA of along-bay vs transect 9




save.image ("~/tmp/LCI_noaa/cache/zoopEnd.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopEnd.RData")


#############
# Nutrients #
#############

## pelagic stations (so far, 2021 only)
nut <- read.csv ("~/GISdata/LCI/Nutrients/CookInletKachemakBay_Nutrients_2021.csv")
nutC <- nut [,1:3]


## match to stn reference
if (!all (levels (factor (nut$Station)) %in% stn$Match_Name)){ ## error trap
  stop ("A station in nutrient table does not match reference station name\n\n")
}
sx <- match (nut$Station, stn$Match_Name)
nutO <- with (nut, data.frame (Station, Date #, Time=NA
                               , Latitude_DD=stn$Lon_decDegree [sx]
                               , Longitude_DD=stn$Lat_decDegree [sx]
                               , Transect=stn$Line [sx]
                               , StationN=stn$Station [sx]
))
nutO$StationN <- ifelse (nutO$StationN %in% 1:100, paste0 ("S_", nutO$Station), nutO$Station)
nutO <- cbind (nutO, nut [,3:ncol (nut)])
tF <- "~/tmp/LCI_noaa/data-products/nutrients_KachemakBay_pelagic2021.csv"
write (paste0 ("## Collected as part of GulfWatch on predefined stations in Kachemak Bay/lower Cook Inlet. CTD sampled on every station. Concurrent CTD zoo- and phytoplankton on select stations. 2021 only.")
       , file=tF, append=FALSE, ncolumns=1)
suppressWarnings(write.table(nutO, file=tF, append=TRUE, quote=FALSE, sep=","
                             , na="", row.names=FALSE, col.names=TRUE))
# write.csv (ctdA, file = tF, row.names = FALSE, quote = FALSE)
rm (tF, nutO, sx)


#######################
# Ocean Acidification #
#######################

## exploratory analysis did not yield much, other than noise -- abandone.
oa <- read.csv ("~/GISdata/LCI/Ocean_Acidification_OA/OA2015-2018.csv")
oa <- read.csv ("~/GISdata/LCI/Ocean_Acidification_OA/OA2017-2021.csv")
oa$timeStamp <- as.POSIXct(oa$sample.date)
oa$Year <- format (oa$timeStamp, "%Y")
oa$MatchName <- substr (oa$sample.ID, 1, 5)
oa$Transect <- substr (oa$sample.ID, 1,3)
oa$Transect <- gsub ("^KB", "AB", oa$Transect)
oa$Transect <- gsub ("_$", "", oa$Transect)

## quick overview of sampling ##
if (printSampleDates){
  cat ("\n\nOA sampling dates\n")
  pT <- subset (oa, Year > 2020)
  pT$Transect <- factor (pT$Transect)
  for (i in 1:length (levels (pT$Transect))){
    cat ("\n\n", levels (pT$Transect)[i], "\n")
    print (sort (levels (factor (
      subset (pT, Transect==levels (pT$Transect)[i])$timeStamp
    ))))
  }
  rm (pT)
}


##############
## seabirds ##
##############

stnB <- c (1,5,10,20,50)*1e3           # buffer -- at different scales
stnB <- 10e3                           # buffer -- 10 km

require ("sp")
# pj4str <- "+proj=lcc +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +datum=WGS84 +units=m +no_defs +ellps=WGS84"
LLprj <- CRS ("+proj=longlat +datum=WGS84 +ellps=WGS84")


## bounding-box for LCI
lonL <- c(-154.2,-151.2)
latL <- c(58.8,60.6)


# require ("sp"); require ("rgdal"); require ("rgeos") # for gBuffer
require ("sp")
require ("sf")


spTran <- function (x, p4){
    # proj4string (x) <- CRS ("+proj=longlat +datum=WGS84 +ellps=WGS84")
#    require ("rgdal")
  require ("sf")
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
if (any (is.na (poSS$longitude_DD) | any (is.na (poSS$latitude_DD)))){
  print (poSS [which (is.na (poSS$longitude_DD) | is.na (poSS$latitude_DD)),])
}

stnP <- stn
coordinates (stnP) <- ~Lon_decDegree+Lat_decDegree
coordinates (poSS) <- ~longitude_DD+latitude_DD
coordinates (phyCenv) <- ~lon+lat
coordinates (zooCenv) <- ~Lon_decDegree+Lat_decDegree
coordinates (NPPSD2) <- ~lon+lat
slot (stnP, "proj4string") <- LLprj
slot (poSS, "proj4string") <- LLprj
slot (phyCenv, "proj4string") <- LLprj
slot (zooCenv, "proj4string") <- LLprj
slot (NPPSD2, "proj4string") <- LLprj   ## Error from missing dependent file?



## coastline from gshhs
## migrate from Rghhg to shape file for windows compatibility
require ("maptools")
require ("zip")
tD <- tempdir()
unzip ("~/GISdata/data/coastline/gshhg-shp-2.3.7.zip"
  , junkpaths = TRUE, exdir = tD)

require ("sf")
coastSF <- read_sf (dsn = tD, layer = "GSHHS_f_L1") ## select f, h, i, l, c
## clip to bounding box: larger Cook Inlet area
b <- st_bbox (coastSF)
b[c(1,3)] <- lonL+c(-8,11)
b[c(2,4)] <- latL+c(-5,2)
bP <- as (st_as_sfc (b), "Spatial") # get spatial polygon for intersect

coastSP <- as (coastSF, "Spatial")
# require ("stars")
# coast <- st_intersection (coastSP, bP)
# coast <- st_intersects (coastSP, bP)

if (1){
require ("raster")
# require ("rgeos")
coast <- raster::intersect(coastSP, bP)
}else{
 #  require ("terra") # replacement for raster
 # coast <- terra::intersect(coastSP, bP)
 require ("stars")
 coast <- st_intersects (coastSP, bP)
}
# plot (coastC)
unlink (tD, TRUE); rm (tD)
rm (b, bP, coastSP, coastSF)

save.image ("~/tmp/LCI_noaa/cache/mapPlot.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/mapPlot.RData")



slot (coast, "proj4string") <- slot (poSS, "proj4string")   ## migrate to sf
badPO <- !is.na (over (poSS, coast)$id)                     ## migrate to sf
PDF ("testSamplesites")
plot (coast, col = "beige", axes = TRUE, xaxs = "i", yaxs = "i", xlim = lonL, ylim = latL)
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
require ("raster")
require ("rgdal")
if (.Platform$OS.type == "windows"){
  bath <- raster ("~/GISdata/LCI/bathymetry/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
}else{
  bath <- raster ("/Users/martin/GISdata/LCI/bathymetry/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
}

require ("stars")
## NC4 version -- gives trouble with projection?
## st_mosaic not working for this. Try nc4 files again?
# bathyZ <- st_mosaic (read_stars ("~/GISdata/LCI/bathymetry/Zimmermann/CI_BATHY.nc4") # Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
#                      , read_stars ("~/GISdata/LCI/bathymetry/Zimmermann/CGOA_BATHY.nc4"))
bathyZ <- read_stars ("~/GISdata/LCI/bathymetry/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")
bathyZ2 <- read_stars ("~/GISdata/LCI/bathymetry/CGOA_bathymetry_grid/cgoa_bathy/w001001.adf")

bathCont <- rasterToContour (bath, levels = c(50, 100, 200, 500))
# bathCont <- st_contour (bathyZ, contour_lines=TRUE, na.rm=TRUE, breaks=c(-50, -100, -200, -500))


ch <- chull(coordinates (NPPSD2))
chCoor <- coordinates (NPPSD2)[c(ch, ch[1]), ]  # closed polygon
sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(chCoor)), ID=1))
                         , proj4string = slot (NPPSD2, "proj4string"))
grd <- spsample (sp_poly, n = 100^2, "regular") # ok, but grid size somewhat mysterious -- move to sf?
                     # Warning: CRS object has comment...

## grd <- makegrid (sp_poly, cellsize = 1e3)
## coordinates (grd) <- ~x1+x2
## proj4string (grd) <- CRS (proj4string (NPPSD2))
## gridded (grd) <- TRUE
rm (ch, chCoor, sp_poly)

pr <- proj4string(bath)  ## fails when using slot (bath, "crs")
# pr <- sf::st_crs (bathyZ)

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
if (0){ ## not working?
  require (ncdf4)
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

require ("rgeos")
require ("parallel")
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

require ("dplyr")
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


#######################################
### save data for future processing ###
#######################################

write.csv (poSS@data, file="~/tmp/LCI_noaa/data-products/CTDcastSummaries.csv")
## save CTD data for oceanographic processing. poSS = summary data -> signatureData
save (stn, physOc, poSS, coast, bath  ## bath = Zimmerman bathymetry
      , file="~/tmp/LCI_noaa/cache/CTDcasts.RData") ## for the wall, etc. -- add coastline and bathymetry
rm (physOc)                             # no needed any more, poSS takes it place

stn <- stnP  # for zoop and other scripts that use sp
save.image ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData")


cat ("\n\n#\n#\n#", format (Sys.time(), format = "%Y-%m-%d %H:%M"
                          , usetz = FALSE)
, " \n# \n# End of dataSetup.R\n#\n#\n")

## EOF
