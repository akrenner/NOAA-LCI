#!/usr/bin/env RScript

###########################
## CTD anomaly over time ##
###########################


## load data
## start with file from dataSetup.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTDcasts.RData")  # contains physOc -- raw CTD profiles
require ("oce")
# source("CTDsectionFcts.R")

## set-up plot and paper size


###########################
## bugs/missing features ##
# - salinity at 50 m (ACC signature)
# - freshwater contenst of first 30 m (local runoff, freshwater lens)
# - sum of fluorescence over time -- see separate work on SWMP



# x-axis is on shaking grounds. 7000 = eye-balled. Make that programatic
# x add vertical lines to mark years?
# x move years to be between tick-marks
# x salinity-anomaly: all variability in surface-layer
# x plot 'normal' seasonal profile
# x anomalies: smooth vertically and/or seasonally
# x set water depth for section
# x center color scale on 0


## x salinity: 0-10 m  and 10 to 100 m
## x fresh-water content: psu = X, integrate over 0:10
## overlay all years
## => GAK1-comparison has been done,  water from below GAK1 coming into kachemak bay


deepThd <- 15   ## deep vs surface layer

plotRAW <- FALSE
plotRAW <- TRUE
pngR <- 300

quantR <- 0.99  ## curtail data at this percentile



## nauseating rainbow
temperatCol <- oceColorsTurbo(50) # oceColorsTemperature (11)
## ODV colors
salCol <- colorRampPalette (col=rev (c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa"))
                            , bias=0.3)(50) #oceColorsSalinity(50)

## modern colors -- overwrite
if (1){
  tCol <- oceColorsTemperature (11)
  salCol <- oceColorsSalinity (11)
}





### data prep
## define sections
physOc$DateISO <- format (physOc$isoTime, "%Y-%m-%d")
# physOc$transDate <- factor (with (physOc, paste (DateISO, Transect, sep = " T-")))
physOc$transDate <- factor (with (physOc, paste0 ("T-", Transect, " ", DateISO)))
physOc$Transect <- factor (physOc$Transect)
physOc$year <- factor (format (physOc$isoTime, "%Y"))
## combine CTD and station meta-data
physOc <- subset (physOc, !is.na (physOc$Transect)) ## who's slipping through the cracks??
## stn should be no longer needed -- see dataSetup.R
# physOc <- cbind (physOc, stn [match (physOc$Match_Name, stn$Match_Name)
#                               , which (names (stn) %in% c(# "Line",
#                                                           "Lon_decDegree", "Lat_decDegree", "Depth_m"))])
physOc$Match_Name <- as.factor (physOc$Match_Name)
# print (summary (physOc))

mediaD <- "~/tmp/LCI_noaa/media/CTDsections/time-sections/"
dir.create(mediaD, recursive=TRUE, showWarnings=FALSE)


save.image ("~/tmp/LCI_noaa/cache/ctdAnomalies.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdAnomalies.RData")


## long term mean / anomaly functions
longM <- function (var, date, maO=31){  ## cyclical long-term mean -- move this to annualPlotFct.R ?
  ## calculate long term mean of var for use in anomaly calculation
  ## smooth using zoo moving average smoother
  if (length (var) != length (date)){stop ("var and date have to be of equal length")}
  # df <- data.frame (var, date)
  # df <- df [order (df$date),]
  jday=as.numeric (format (date, "%j"))-1  # 1 jan needs to be day 0, not 1!
  df <- rbind (data.frame (var, date, jday=jday-365)
               , data.frame (var, date, jday)
               , data.frame (var, date, jday=jday+365)
  )
  aD <- aggregate (var~jday, df, FUN=mean, na.rm=TRUE)
  aD2 <- data.frame (jday=-366:(366*2))
  aD2$var <- aD$var [match (aD2$jday, aD$jday)]
  suppressPackageStartupMessages(require ("zoo"))
  aD2$MA <- zoo::rollapply(aD2$var, width=maO, FUN=mean, na.rm=TRUE ## critical for monthly data!
                           , fill=c(NA, "extend", NA)
                           , partial=FALSE, align = "center")
  #aD2$MA <- na.approx (aD2$MA, na.rm=FALSE, x=aD2$jday)   # order is important -- this last (or biased)
  aD2$MA <- na.spline (aD2$MA, na.rm=FALSE, x=aD2$jday)   # order is important -- this last (or biased)
  aD2$loss <- predict (loess (MA~jday, aD2))
  oD <- subset (aD2, (0 < jday)  & (jday < 367))
  oD
}
anomF <- function (var, date, longM){
  df <- data.frame (var, date, jday=format (date, "%j"))
  ## longM needs to contain MA and jday!
  df$anom <- df$var - longM$MA [match (df$jday, longM$jday)]
  df$anom
}



pickStn <- which (levels (physOc$Match_Name) %in%
                    c("9_6", "AlongBay_3", "3_14", "3_13", "3_12", "3_11", "3_10", "3_1", "AlongBay_10"))
pickStn <- 1:length (levels (physOc$Match_Name))
pickStn <- 87 # 9-6
for (k in pickStn){
  try ({
  # k <- 87  ## 9-6
  stnK <- levels (physOc$Match_Name)[k]
  cat (stnK, "\n")
  xC <- subset (physOc, Match_Name == stnK)
  xC <- xC [order (xC$isoTime),]
  ## cut-off at 85m as several seasons missing beyond that depth
  xC <- subset (xC, Depth.saltwater..m. < 85)

  ## turn into section
  xC$Date <- as.factor (xC$Date)
  require ("oce")

  ##########################
  ### calculate anomalies ##
  ##########################

  ## bin by depth (rather than just pressure)
  xC$depthR <- factor (round (xC$Depth.saltwater..m.))
  xC$month <- factor (format (xC$isoTime, "%m"))
  ## aggregate useing oce function -- skip aggregation in pre-processing by SBprocessing
  ## calculate normals

  ctdAgg <- aggregate (Temperature_ITS90_DegC ~ depthR+month, xC, FUN = mean, na.rm = TRUE)
  ctdAgg$Salinity_PSU <- aggregate (Salinity_PSU ~ depthR+month, xC, FUN = mean, na.rm = TRUE)$Salinity_PSU
  ctdAgg$Pressure..Strain.Gauge..db. <- aggregate (Pressure..Strain.Gauge..db. ~ depthR+month, xC, FUN = mean, na.rm = TRUE)$Pressure..Strain.Gauge..db.
  ctdAgg$Fluorescence_mg_m3 <- aggregate (Fluorescence_mg_m3 ~ depthR+month, xC, FUN = mean, na.rm = TRUE)$Fluorescence_mg_m3
  ctdAgg$bvf <- aggregate (bvf~depthR+month, xC, FUN=mean, na.rm = TRUE)$bvf

  ## smooth normals
  ctdAgg$monthI <- as.numeric (levels (ctdAgg$month))[ctdAgg$month]
  preDF <- ctdAgg; preDF$monthI <- preDF$monthI - 12
  postDF <- ctdAgg; postDF$monthI <- postDF$monthI + 12
  sDF <- rbind (preDF, ctdAgg, postDF)
  rm (preDF, postDF)

  ## plot and obs and smooths/predictions
  # plot (Temperature_ITS90_DegC~monthI, ctdAgg, subset = depthR == i)
  # lS <- loess (Temperature_ITS90_DegC~monthI, data = sDF, subset = depthR == i, span = 0.2)
  # gS <- gam (Temperature_ITS90_DegC~s(monthI), data = sDF, subset = depthR == i)
  # lines (lS$x, lS$fitted, col = "green")
  # lines (lS$x, gS$fitted.values, col = "blue")

  # require ("mgcv")  ## patterns in residuals -- stick with loess
  # gam (Temperature_ITS90_DegC~s(monthI), data = sDF, subset = depthR == i)
  anoF <- function (varN, df=sDF){
    vN <- which (names (df) == varN)
    sOut <- sapply (levels (df$depthR), FUN=function (i){
      loess (as.formula(paste0 (varN, "~monthI"))
             , df, subset=depthR==i
             , span=0.25)$fitted[(1:12)+12]
    })
    sapply (1:nrow (ctdAgg), FUN=function (i){
      sOut [ctdAgg$monthI [i], ctdAgg$depthR [i]]
    })
  }
  ctdAgg$tloess <- anoF ("Temperature_ITS90_DegC")
  ctdAgg$sloess <- anoF ("Salinity_PSU")
  ctdAgg$floess <- anoF ("Fluorescence_mg_m3")
  ctdAgg$bvfloess <- anoF ("bvf")
  rm (anoF)

  ## model normals instead
  # mDF <- rbind (xC, xC, xC)
  # mDF$yDate <- c (as.POSIXct (paste0 ("2000-", format (xC$isoTime, "%m-%d")))
  #                 , as.POSIXct (paste0 ("2001-", format (xC$isoTime, "%m-%d")))
  #                 , as.POSIXct (paste0 ("2002-", format (xC$isoTime, "%m-%d")))
  # )
  # require (mgcv)
  # sOut <- sapply (levels (mDF$depthR), FUN = function (i){
  #   gam (Temperature_ITS90_DegC~s(yDate), data = mDF, subset = mDF$depthR == i)$fitted
  # })

  ## anomaly = observ-smoothed normal
  matchN <- match (paste0 (xC$depthR, "-", xC$month), paste0 (ctdAgg$depthR,"-", ctdAgg$month))
  xC$anTem <- xC$Temperature_ITS90_DegC - ctdAgg$tloess [matchN]
  xC$anSal <- xC$Salinity_PSU - ctdAgg$sloess [matchN]
  xC$anBvf <- xC$bvf - ctdAgg$bvfloess [matchN]

  ##############################################
  ## seasonal climatologies of poSS summaries ##
  ##############################################
  poSSclim <- aggregate (TempDeep~month, poSS, subset=poSS$Match_Name==stnK
                         , FUN=mean, na.rm=TRUE)
  poSSclim$pclDepth <- aggregate (pclDepth~month, poSS, subset=poSS$Match_Name==stnK ## XXX settle names!
                         , FUN=mean, na.rm=TRUE)$pcDepth
  poSSclim$bvfMax <- aggregate (bvfMax~month, poSS, subset=poSS$Match_Name==stnK ## XXX settle names!
                                 , FUN=mean, na.rm=TRUE)$pcDepth
  poSSclim$stability <- aggregate (stability~month, poSS, subset=poSS$Match_Name==stnK
                                 , FUN=mean, na.rm=TRUE)$stability


  mkSection <- function (xC){  ## use CTDfunctions insted?!
    require (oce)
    xC <- xC [order (xC$isoTime),]
    xC$Date <- factor (xC$DateISO)
    cL <- lapply (1:length (levels (xC$Date))
                  , FUN = function (i){
                    sCTD <- subset (xC, xC$Date == levels (xC$Date)[i])
                    ocOb <- with (sCTD,
                                  as.ctd (salinity = Salinity_PSU
                                          , temperature = Temperature_ITS90_DegC
                                          , pressure = Pressure..Strain.Gauge..db.
                                          , longitude = as.numeric (longitude_DD)
                                          , latitude = as.numeric (latitude_DD)
                                          #, sectionId = transDate
                                          # , startTime = isoTime
                                  ))
                    ocOb@metadata$station <- xC$Match_Name [1]
                    ocOb@metadata$startTime <- sCTD$isoTime [1]
                    ocOb@metadata$waterDepth <- 103

                    ocOb <- oceSetData (ocOb, "fluorescence", sCTD$Fluorescence_mg_m3)
                    ocOb <- oceSetData (ocOb, "turbidity", sCTD$turbidity)
                    ocOb <- oceSetData (ocOb, "O2perc", sCTD$O2perc)
                    ocOb <- oceSetData (ocOb, "PAR", sCTD$PAR.Irradiance)
                    ocOb <- oceSetData (ocOb, "lFluorescence", log (sCTD$Fluorescence_mg_m3))
                    ocOb <- oceSetData (ocOb, "N2", sCTD$Nitrogen.saturation..mg.l.)
                    ocOb <- oceSetData (ocOb, "Spice", sCTD$Spice)
                    ocOb <- oceSetData (ocOb, "bvf", sCTD$bvf)
                    ocOb <- oceSetData (ocOb, "anTem", sCTD$anTem)
                    ocOb <- oceSetData (ocOb, "anSal", sCTD$anSal)
                    ocOb <- oceSetData (ocOb, "anBvf", sCTD$anBvf)

                    ocOb
                  }
    )
    cL <- as.section (cL)
    cL <- sectionSort (cL, by="time")
    cL
  }


  TSaxis <- function (isoTime, axes=TRUE, verticals=TRUE){
    tAx <- as.POSIXct (as.Date (paste0 (2012:max (as.numeric (format (isoTime, "%Y"))), "-01-01")))
    lAx <- as.POSIXct (as.Date (paste0 (2012:max (as.numeric (format (isoTime, "%Y"))), "-07-01")))
    if (verticals){
      abline (v = tAx, lty="dashed")
    }
    if (axes == TRUE){
      axis (1, at = tAx, label = FALSE)
      axis (1, at = lAx, label = format (lAx, "%Y"), tick = FALSE)
    }
  }

  plot.station <- function (section, axes = TRUE, ...){
    plot (section, showBottom = FALSE, xtype = "time", ztype = "image"
          #, at = FALSE
          # , stationTicks = TRUE
          , grid = FALSE
          , axes = FALSE, ...
          , xlab="", ylab="")
    axis (2, at = c(0, 20, 40, 60, 80, 100))
  }


  require ("RColorBrewer")
#  pdf (paste0 (mediaD, stnK, "-profile.pdf"), height = 11, width = 8.5)
  png (paste0 (mediaD, stnK, "-profile.png"), height = 11*pngR, width = 8.5*pngR, res=pngR)
    if (plotRAW){
    par (mfrow=c(5,1))
  }else{
    par (mfrow=c(3,1))
  }
  par (oma=c(0,3,2,0))

  ## current anomaly range: -7.5 to 6.7 -- needs to be symmetrical
  sF <- function (v, n = 12, qR=quantR){
    # aR <- max (abs (range (v, na.rm=TRUE)))
    aR <- max (abs (stats::quantile(v, probs=c(1-qR, qR), na.rm=TRUE)))
    aR <- signif (aR, 1)
    # aR <- ceiling (aR)
    seq (-aR, aR, length.out = n)
  }


  xCS <- mkSection (xC)
  if (plotRAW){
    ## time series of raw data
    plot.station (xCS, which="temperature"
                  , zcol=tCol
                  # , zbreaks=sF (xC$Temperature_ITS90_DecC)
                  , legend.loc="" #legend.text="temperature anomaly [°C]"
                   , mar=c(2.5,4,2.3,1.2)  ## default:  3.0 3.5 1.7 1.2
    )
    TSaxis (xC$isoTime)
    title (main="temperature [°C]", line=1.2)
    ## station-ticks
    axis (3, at=xC$isoTime, labels=FALSE)

    zB <- seq (26, ceiling(max (xC$Salinity_PSU, na.rm=TRUE)),length.out=length (salCol)+1)
    plot.station (xCS, which="salinity"
                  , zcol = salCol
                  , zbreaks=zB
                  , legend.loc="" #legend.text="temperature anomaly [°C]"
    )
    TSaxis (xC$isoTime)
    title (main="salinity [PSU]")
  }

  ## time series of anomalies
  zB <- sF (xC$anTem)
  plot.station (xCS, which = "anTem"
                , zcol = rev (brewer.pal (length (zB)-1, "RdBu"))
                , zbreaks = zB
                , legend.loc="" #legend.text="temperature anomaly [°C]"
  )
  TSaxis (xC$isoTime)
  # legend ("bottomright", legend="temperature anomaly [°C]", fill="white") #, bty="n")
  title (main="temperature anomaly [°C]")

  zB <- sF (subset (xC$anSal, xC$Depth.saltwater..m. <= deepThd))
  plot.station (mkSection (subset (xC, Depth.saltwater..m. <= deepThd))
                , which="anSal"
                , zcol=oceColors9B(length (zB)-1)  # brewer.pal (11, "PiYG")
                , zbreaks=zB
                , axes=FALSE
                , xlab=""
                #, ylim=c(-1,-10)
                 , mar=c(1.0,3.5,3.7,1.2)  #default:  3.0 3.5 1.7 1.2
                , legend.loc="" #legend.text="salinity anomaly [PSU]"
                )
  axis (2, at = pretty (0:deepThd))
  title (main="salinity anomaly [PSU]")


  zB <- sF (subset (xC$anSal, xC$Depth.saltwater..m. > deepThd))
  plot.station (mkSection (subset (xC, Depth.saltwater..m. > deepThd))
                , which="anSal"
                , zcol=oceColors9B(length (zB)-1)  # brewer.pal (11, "PiYG")
                , zbreaks=zB
               # , ylim=c(-10,-1*max (xC$Depth.saltwater..m.))
               # , ylim=c(deepThd, max (xC$Depth.saltwater..m.))  ## still places a zero -- report bug
               , mar=c(4.7,3.5,0,1.2) # default:  3.0 3.5 1.7 1.2
               , legend.loc=""
                )
  TSaxis (xC$isoTime)
  rm (zB)  ## keep xCS for buoyancy

  mtext ("Depth [m]", side=2, outer=TRUE)
  dev.off()

  save.image ("~/tmp/LCI_noaa/cache/t9ctd1.RData")
#  # rm (list = ls()); load ("~/tmp/LCI_noaa/cache/t9ctd1.RData")


  ## buoyancy
## raw (climatoloty below)
  ## anomaly
  ## water column stability
  ## depth of pycnocline




  ######################
  ## plot climatology ##
  ######################

  prC <- ctdAgg; prC$monthI <- prC$monthI - 12
  poC <- ctdAgg; poC$monthI <- poC$monthI + 12
  ctdAggD <- rbind (prC, ctdAgg, poC); rm (prC, poC) ## to plot margins as well
  ## clean up defining section -- or go back to section functions?
  cT9 <- lapply (0:13, function (i){ # from prev month to month after current year -- why not longer?
    sCTD <- subset (ctdAggD, monthI == i)
    ocOb <- with (sCTD, as.ctd (# salinity = Salinity_PSU, temperature = Temperature_ITS90_DegC
      salinity = sloess, temperature = tloess
      , pressure = Pressure..Strain.Gauge..db.
      , longitude = rep (i, nrow (sCTD))
      , latitude = rep (0, nrow (sCTD))
    ))
    ocOb@metadata$waterDepth <- 103  ## needed? vary by station
    ocOb <- oceSetData (ocOb, "sSal",  sCTD$sloess)
    ocOb <- oceSetData (ocOb, "sTemp", sCTD$tloess)
    ocOb <- oceSetData (ocOb, "sFluo", sCTD$floess)
    ocOb <- oceSetData (ocOb, "sBvf",  sCTD$bvfloess)
    return (ocOb)
  })
  cT9 <- as.section (cT9)
  dTimes <- as.POSIXct(c("1999-12-15"
                                , paste0 ("2000-", 1:12, "-15")
                                , "2001-01-15"))
  cT9 [['station']] <- lapply (1:length (cT9 [['station']])
                               , function (i){
                                 oceSetMetadata(cT9 [['station']][[i]], 'startTime'
                                                , dTimes [i]
                                 )
                               }); rm (dTimes)
  cT9 <- sectionSort (cT9, by="time")
  rm (ctdAggD)


  anAx <- function (dAx = c(0, 50, 100)){  ## annotations for x-axis -- for one year
    ## XXX make option for time series and for climatology
    axis (1, at = as.POSIXct (as.Date (paste0 ("2000-", 1:12, "-01"))), label = FALSE)
    axis (1, at = as.POSIXct (as.Date (paste0 ("2000-", 1:12, "-15"))), label = month.abb, tick = FALSE)
    axis (2, at = dAx)
  }
  clPlot <- function (cT, which = "temperature", zcol = oceColorsTemperature(11), ...){
    plot (cT, which = which, xtype = "time", ztype = "image", zcol = zcol
          , xlim = c(as.POSIXct (as.Date (c("2000-01-01", "2000-12-31"))))
          , axes = FALSE
          , ...)
  }

  # pdf (paste0 (mediaD, stnK, "-climatology.pdf"), height = 11.5, width = 8)
  png (paste0 (mediaD, stnK, "-climatology.png"), height=11.5*pngR, width=8*pngR, res=pngR)
  par (mfrow=c(2,1))
  clPlot (cT9, which = "temperature"
          , zcol=tCol
          , zbreaks = seq (min (ctdAgg$Temperature_ITS90_DegC)
                           , max (ctdAgg$Temperature_ITS90_DegC), length.out = length (tCol)+1)
          # , zlim = c(4,12)
          )
  anAx(pretty (range (as.numeric (levels (ctdAgg$depthR))))) ## XXX pretty (max-depth)

  clPlot (cT9, which = "salinity"
          , zcol = salCol
          , zbreaks = seq (28, 31.5, length.out=length(salCol)+1)
          # , zlim = c(28,31.5)
  )
  anAx(pretty (range (as.numeric (levels (ctdAgg$depthR))))) ## XXX pretty (max-depth)
  dev.off()


  ## fluorescence
  # pdf (paste0 (mediaD, stnK, "-fluorescence-climatology.pdf"))
  png (paste0 (mediaD, stnK, "-fluorescence-climatology.png"), res=pngR, height=11*pngR, width=8.5*pngR)
  par (las = 1, mfrow=c(3,1))
  clPlot (cT9, which = "sFluo", zcol = oceColorsChlorophyll (4))
#  anAx(dAx = seq (0, 100, by = 20))
  anAx(pretty (range (as.numeric (levels (ctdAgg$depthR))))) ## XXX pretty (max-depth)

## add time series and anomaly
  plot.station (xCS, which="fluorescence"
                , zcol=oceColorsChlorophyll(32)
                # , zbreaks=zB
                , legend.loc="" #legend.text="temperature anomaly [°C]"
  )
  title (main=expression (Brunt~Väisälä~Buoyancy~frequency~"["*s^-2*"]"))
  TSaxis (xCS@metadata$time)
## add totals?
# aggregate (Fluorescence_mg_m3~date, xC)
  dev.off()


  ## buoyancy
#  pdf (paste0 (mediaD, stnK, "-buoyancy-climatology.pdf"), height=11.5, width=8)
  png (paste0 (mediaD, stnK, "-buoyancy-climatology.png"), height=11.5*pngR, width=8*pngR, res=pngR)
  par (mfrow=c(3,1))
  ## raw buoyancy profile
  ## bvf climatology
  ## max-bvf time-series -- with seasonal signal
  ## ?? depth of pycnocline time-series in summer -- with seasonal signal (or anomaly?)

  ## raw time-series profile
  #zB <- seq (0, ceiling(max (xC$bvf, na.rm=TRUE)), by=0.5)
  require ("cmocean")  ## for color ramp cmocean -- just use viridis
  options ('cmocean-version' = "2.0") # fix colors to cmocean 2.0

  ## seasonal climatology
  clPlot (cT9, which="sBvf"
          , zcol = colorRampPalette (c ("white", rev (cmocean ("haline")(32))))
  )
  anAx (pretty (range (as.numeric (levels (ctdAgg$depthR)))))
  title (main=expression (Brunt~Väisälä~Buoyancy~frequency-seasonal~climatology~"["*s^-2*"]"))

  ## raw time series of buoyancy
    plot.station (xCS, which="bvf"
                , zcol=colorRampPalette (c ("white", rev (cmocean ("haline")(32))))
                # , zbreaks=zB
                , legend.loc="" #legend.text="temperature anomaly [°C]"
                , ylim=c(0,20)
  )
  title (main=expression (Brunt~Väisälä~Buoyancy~frequency~"["*s^-2*"]"))
  TSaxis (xCS@metadata$time)

### XXX anAx (pre)
  if(0){
  ## anomaly  (too noisy?)
  zB <- sF (v=xC$anBvf, qR=0.90)
  plot.station (xCS, which="anBvf"
                , zcol=brewer.pal (11, "RdBu")
                , zbreaks=zB
                , legend.loc="" #legend.text="temperature anomaly [°C]"
  )
  title (main=expression (Buoyancy~frequency~Anomaly~"["*s^-2*"]"))
  }

## summaries: timeseries of strength of stratification and pycnocline depth
  plot (bvfMax~timeStamp, data=poSS, subset=poSS$Match_Name==stnK, type="l")
#  plot (bvfMean~timeStamp, data=poSS, subset=poSS$Match_Name==stnK, type="l")  ## almost the same as max
  plot (pclDepth~timeStamp, data=poSS, subset=poSS$Match_Name==stnK, type="l")
#  plot (stability~timeStamp, data=poSS, subset=poSS$Match_Name==stnK, type="l")  ## noisy -- errors?

  ## add: anomaly and climatology of bvfMax XXX
    dev.off()

  })
}
## alternative display of this data:


save.image ("~/tmp/LCI_noaa/cache/ctdT9S6_fw.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdT9S6_fw.RData")





#########################
## freshwater contents ##
#########################

## move this elsewhere! -- signature data?
## aggregate Freshwater contents over surface layer of T9
xC <- subset (poSS, Match_Name %in% paste0 ("9_", 1:10))
fw <- aggregate (FreshWaterCont~Date, data=xC, FUN=sum, na.rm=FALSE)
fw$freshDeep <- aggregate (FreshWaterContDeep~Date, data=xC, FUN=sum, na.rm=FALSE)$FreshWaterContDeep

names (fw) <- c ("Date", "freshCont", 'freshDeep')
fw$Date <- as.POSIXct(as.Date (fw$Date))
fw$month <- as.numeric (format (fw$Date, "%m"))
fw$year <- as.numeric (format (fw$Date, "%Y"))

## QAQC
# is.na (fw$freshCont [fw$freshCont > 200]) <- TRUE

## calc seasonal anomaly -- for starters based on month -- better to use full record in ARIMA as with SWAMP
fwS <- aggregate (freshCont~month, fw, mean)
fwS$sd <- aggregate (freshCont~month, fw, sd)$freshCont
lfw <- fwS; lfw$month <- lfw$month - 12
ufw <- fwS; ufw$month <- ufw$month + 12
fwS <- rbind (lfw, fwS, ufw); rm (lfw, ufw)
require (mgcv)
md <- gam (freshCont~s(month), data = fwS)
lS <- loess (freshCont~month, data = fwS, span = 0.2)
## stick with monthly means for now, rather than smooth
fw$fwA <- fw$freshCont - fwS$freshCont [match (fw$month, fwS$month)]



## add freshwater -- deep

# pdf (paste0 (mediaD, "T9S6_freshwatercontent.pdf"), height = 9, width = 6)
png (paste0 (mediaD, "T9_freshwatercontent.png"), height=9*pngR, width=6*pngR, res=pngR)
par (mfrow = c(3,1), mar = c (5,4, 0.1, 0.1))
## time series
plot (freshCont~Date, fw, type = "l", ylab = "freshwater content")
## seasonal climatology
plot (freshCont~month, fwS
      , xlim = c(1,12), type = "l", lwd = 2
      , ylab = "freshwater content"
)
lines (fwS$month, md$fitted.values, col = "red", lwd = 2, lty = "dashed")
lines (lS$x, lS$fitted, col = "blue", lwd = 2, lty = "dashed")
legend ("topleft", legend = c("monthly mean", "seasonal spline", "seasonal loess")
        , lty = c("solid", "dashed", "dashed")
        , col = c("black", "red", "blue")
        , lwd = 2
)
## freshwater anomaly
plot (fwA~Date, fw, type = "l", ylab = "freshwater contents anomaly", axes=FALSE)
axis (2)
abline (h = 0, col = "gray")
#  plot (f)
TSaxis (poSS$timeStamp)
# TSaxis (xC$isoTime)
dev.off()




### timing of freshwater -- panel for each year ###
if (0){
  require ("lattice")
  # pdf (paste0 (mediaD, "T9S6_freshSeason.pdf"), width = 7, height = 9)
  png (paste0 (mediaD, "T9S6_freshSeason.png"), width=7*100, height=9*100,res=100)
  print (xyplot (freshCont~month| factor (year), data= fw, as.table = TRUE, type = "l"))
  print (xyplot (fwA~month| factor (year), data= fw, as.table = TRUE, type = "l"
                 , xlab = "freshwater anomaly"))
  dev.off()
}



if (0){
  ## quick test on stratification
  t96S <- subset (poSS, Match_Name=="AlongBay_9") %>%
    subset (month %in% c(6,7,8))
  t96S$sSSS <- scale (t96S$SSS, center=TRUE, scale=TRUE)
  t96S$sTRange <- scale (t96S$tideRange, center=TRUE, scale=TRUE)
  plot (bvfMax~tideRange, t96S)
  lmStrat <- lm (bvfMax~sTRange + sSSS, t96S)
  summary (lmStrat)

  poSummer <- subset (poSS, month %in% 6:8)
  poSummer$Match_Name <- factor (poSummer$Match_Name)
  stCor <- sapply (levels (poSummer$Match_Name), function (x){
    tSum <- subset (poSummer, Match_Name == x)
    # lmStrat <- lm (bvfMax~sTRange+sSSS)
    lmStrat <- cor(tSum$bvfMax, tSum$tideRange, use="pairwise", method="pearson")
    lmStrat
  })
  ## expectation: the bigger the tidal range, the lower the bvfMax -> negative correlation
  ## in winter: expect no correlation
  SuWi <- subset (poSS, month %in% c(1,2,3,6,7,8))
  SuWi$sSSS <- scale (SuWi$SSS)
  SuWi$stideRange <- scale (SuWi$tideRange)
  SuWi$season <- ifelse (SuWi$month %in% c(1,2,3), "winter", "summer")
  tM <- lm (bvfMax~stideRange+sSSS+season+Match_Name, SuWi)
  summary (tM)
  tM <- lm (bvfMax~sSSS+season+Match_Name, SuWi)
  summary (tM)
  tM <- lm (bvfMax~stideRange+season+Match_Name, SuWi)
  summary (tM)
}





########################################
## deep water temperature time series ##
########################################

## - raw time series
## - superimposed anomalies/seasonal mean
## - marking first day >= threshold temperature in spring
## - timing of x degree C in spring

T96 <- subset (poSS, Match_Name=="9_6")
T96 <- T96@data [order (T96$timeStamp),]
require ("tidyr")

tbnorm <- longM (T96$TempBottom, T96$timeStamp)
T96$TempBot_anom <- anomF(T96$TempBottom, T96$timeStamp, tbnorm)
## see annualPlotFct.R::fixGap
T96f <- data.frame (timeStamp=seq (min (T96$timeStamp), max (T96$timeStamp), by=3600*24))
T96f$Date <- as.character (as.Date(T96f$timeStamp))
T96f$jday <- as.numeric (format (T96f$timeStamp, "%j"))-1
T96f$TempBottom <- T96$TempBottom [match (T96f$Date, T96$Date)]
T96f$TempBottomN <- na.approx (T96f$TempBottom, x=T96f$timeStamp, na.rm=FALSE)
T96f$TempBot_norm <- tbnorm$MA [match (T96f$jday, tbnorm$jday)]


# T96$jday <- as.numeric (format (T96$timeStamp, "%j"))-1
# T96$TempBot_norm <- tbnorm$MA [match (T96$jday, tbnorm$jday)]

if (0){
  plot (TempDeep~TempBottom, T96)
  abline(a=0,b=1)
}



png (paste0 (mediaD, "tempDeepTS.png"), res=pngR, height=8*pngR, width=8*pngR)
par (mfrow=c(3,1))
plot (TempBottomN~timeStamp, T96f, type="n", main="Bottom temperature at T9-6"
      , ylab=expression('Temperature'~'['*degree~'C'*']')
      , xlab="", axes=FALSE)
axis (2)
TSaxis(T96f$timeStamp, verticals=FALSE)
abline (h=4, lty="dashed") # mark 4 degrees C
## add lines, marking the anomaly in blue/red
for (i in 1:nrow (T96f)){
  with (T96f, lines (x=rep (timeStamp [i], 2)
                    ,y=c(TempBot_norm [i], TempBottomN[i])
                    , col=ifelse (TempBottomN [i] > TempBot_norm [i], "red", "blue")
                    , lwd=2
                    ))
}
# lines (TempBottomN~timeStamp, T96f, col="black", lwd=2)
lines (TempBot_norm~timeStamp, T96f, col="gray", lwd=3)
box()

## plot timing of 4 degrees C over year
T96f$Year <- as.numeric (format (T96f$timeStamp, "%Y"))


thTempL <- seq (4, 6, by=0.5)

springM <- sapply (thTempL, function (y){
  aggregate (TempBottomN~Year, data=T96f, function (x, thTemp=y){
    x <- c (-1, x)
    first4 <- min ((1:length (x))[x <=4], na.rm=TRUE)
    last4 <- max ((1:length (x[1:(366/2)]))[x<=thTemp], na.rm=TRUE)
    lD <- ifelse (last4==1, NA, last4)
    as.Date("2000-01-01")+lD
    # last4
  })$TempBottomN
})

rownames(springM) <- levels (factor (T96f$Year))
springM <- as.Date (springM)

plot  (range (T96f$Year), range (springM, na.rm=TRUE), type="n", xlab="", ylab="")
for (i in 1:length (thTempL)){
  points (springM [,i]~as.numeric (rownames (springM)), col=i, pch=19)
}
legend ("bottomright", pch=19, col=1:nrow (springM), legend=thTempL)

dev.off()

## earliest day per year to reach 4 degrees C -- still useful?






## TS-diagram
# plot (Salinity_PSU~Temperature_ITS90_DegC, data=physOc, col=format (physOc$isoTime, "%m"))
# use oce template for pretty plot


# graphics.off()

## EOF
