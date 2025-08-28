#!/usr/bin/env RScript

###########################
## CTD anomaly over time ##
###########################


## load data
## start with file from dataSetup.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTDcasts.RData")  # from dataSetup.R -- contains physOc -- raw CTD profiles
require ("oce")
require ("RColorBrewer")
require ("tidyverse")
# source("CTDsectionFcts.R")

## set-up plot and paper size


###########################
## bugs/missing features ##


# - salinity at 50 m (ACC signature)
# - freshwater contenst of first 30 m (local runoff, freshwater lens)
# - sum of Chlorophyll over time -- see separate work on SWMP



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


### temperature: 4 C mean for Herring,   8, 12 C max for HABs


## select which stations to plot -- all or only named stations
physOc$Match_Name <- as.factor (physOc$Match_Name)
pickStn <- which (levels (physOc$Match_Name) %in%
  c("9_6", "9_8", "9_2", "AlongBay_3", "AlongBay_10", "4_6", "4_8", "4_3"))
#                     c("9_6", "AlongBay_3", "3_14", "3_13", "3_12", "3_11"))
# pickStn <- seq_along(levels (physOc$Match_Name)) ## some fail as-is: simpleLoess span too small
# pickStn <- 87 # 9-6


deepThd <- 20   ## deep vs surface layer

plotRAW <- FALSE
plotRAW <- TRUE
pngR <- 300

quantR <- 0.99  ## curtail data at this percentile


fDim <- c(8.5, 11)
# fDim <- c(16,9)   ## figure dimensions



## gray scale -- honest
tCol <- gray.colors(101)
salCol <- gray.colors(101)

## modern colors -- overwrite
tCol <- oceColorsTemperature (11)
salCol <- oceColorsSalinity (11)

# ## from https://www.esri.com/arcgis-blog/products/arcgis-pro/mapping/a-meaningful-temperature-palette/
# tCol <- rgb (c (230, 157, 49, 62, 123, 177, 170, 155, 145, 64)
#              , c (238, 176, 77, 115, 146, 146, 123, 81, 45, 20)
#              , c (253, 211, 123, 143, 136, 102, 89, 79, 75, 37)
#              , maxColorValue=255)
# tCol <- rev (c ("#de5842", "#fcd059", "#ededea"
#                 , "#bfe1bf", "#a2d7d8"))
tCol <- oceColorsTurbo(1000)
tCol <- rev (brewer.pal (11, "Spectral"))
tColAn <- rev (brewer.pal (length (salCol), "RdBu"))
# tCol <- colorRampPalette (tCol, alpha=FALSE)(1000)  ## interpolate colors, or make them continuous


## nauseating rainbow
kr <- TRUE
kr <- FALSE
if (kr) {
  tCol <- oceColorsTurbo(1000)
  salCol <- colorRampPalette (col = rev (c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa"))
    , bias = 0.3)(1000) ## ODV colors
  didntlikeit <- colorRampPalette(rev (c ("#de5842", "#fcd059", "#ededea",
    "#bfe1bf", "#a2d7d8")))(9)
}



### data prep
## define sections
physOc$DateISO <- format (physOc$isoTime, "%Y-%m-%d")
# physOc$transDate <- factor (with (physOc, paste (DateISO, Transect, sep = " T-")))
physOc$transDate <- factor (with (physOc, paste0 ("T-", Transect, " ", DateISO)))
physOc$Transect <- factor (physOc$Transect)
physOc$year <- as.numeric (format (physOc$isoTime, "%Y"))
## combine CTD and station meta-data
physOc <- subset (physOc, !is.na (physOc$Transect)) ## who's slipping through the cracks??
## stn should be no longer needed -- see dataSetup.R
# physOc <- cbind (physOc, stn [match (physOc$Match_Name, stn$Match_Name)
#                               , which (names (stn) %in% c(# "Line",
#                                                           "Lon_decDegree", "Lat_decDegree", "Depth_m"))])
physOc$Match_Name <- as.factor (physOc$Match_Name)
# print (summary (physOc))

mediaD <- "~/tmp/LCI_noaa/media/CTDsections/time-sections"
dir.create(mediaD, recursive = TRUE, showWarnings = FALSE)


save.image ("~/tmp/LCI_noaa/cache-t/ctdAnomalies.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache-t/ctdAnomalies.RData")



########################################
## long term mean / anomaly functions ##
########################################

longM <- function(var, date, maO = 31) {  ## cyclical long-term mean -- move this to annualPlotFct.R ?
  ## calculate long term mean of var for use in anomaly calculation
  ## smooth using zoo moving average smoother
  if (length (var) != length (date)) {stop ("var and date have to be of equal length")}
  # df <- data.frame (var, date)
  # df <- df [order (df$date),]
  jday = as.numeric (format (date, "%j")) - 1  # 1 jan needs to be day 0, not 1!
  df <- rbind (data.frame (var, date, jday = jday - 365)
    , data.frame (var, date, jday)
    , data.frame (var, date, jday = jday + 365)
  )
  aD <- aggregate (var ~ jday, df, FUN = mean, na.rm = TRUE)
  aD2 <- data.frame (jday = -366:(366 * 2))
  aD2$var <- aD$var [match (aD2$jday, aD$jday)]
  suppressPackageStartupMessages(require ("zoo"))
  aD2$MA <- zoo::rollapply(aD2$var, width = maO, FUN = mean, na.rm = TRUE ## critical for monthly data!
    , fill = c(NA, "extend", NA)
    , partial = FALSE, align = "center")
  # aD2$MA <- na.approx (aD2$MA, na.rm=FALSE, x=aD2$jday)   # order is important -- this last (or biased)
  aD2$MA <- na.spline (aD2$MA, na.rm = FALSE, x = aD2$jday)   # order is important -- this last (or biased)
  aD2$loss <- predict (loess (MA ~ jday, aD2))
  oD <- subset (aD2, (0 < jday)  & (jday < 367))
  oD
}
anomF <- function(var, date, longM) {
  df <- data.frame (var, date, jday = format (date, "%j"))
  ## longM needs to contain MA and jday!
  df$anom <- df$var - longM$MA [match (df$jday, longM$jday)]
  df$anom
}
# require ("mgcv")  ## patterns in residuals -- stick with loess
# gam (Temperature_ITS90_DegC~s(monthI), data = sDF, subset = depthR == i)
anoF <- function(varN, df = sDF) {
  vN <- which (names (df) == varN)
  sOut <- sapply (levels (df$depthR), FUN = function(i) {
    loess (as.formula(paste0 (varN, "~monthI"))
      , df, subset = depthR == i
      , span = 0.25)$fitted[(1:12) + 12]
  })
  sapply (seq_len(nrow(ctdAgg)), FUN = function(i) {
    sOut [ctdAgg$monthI [i], ctdAgg$depthR [i]]
  })
}

## daily anomaly function: supply measurements of vairable X
## return: daily interpolated measurements over time series, and normals for those days
dailyTS <- function(df, varN) {
  var <- df [, which (names (df) == varN)]
  ## ensure that df has variable "timeStamp" (or isoDate?)
  if ("isoDate" %in% names (df)) {df$timeStamp <- df$isoDate}
  if (!"Date" %in% names (df)) {df$Date <- as.Date (df$timeStamp)}
  varNorm <- longM (var, df$timeStamp)
  if (class (df$timeStamp)[1] != 'POSIXct') {stop ("timeStamp needs to be POSIXct")}
  dfD <- data.frame (timeStamp = seq (min (df$timeStamp), max (df$timeStamp), by = 3600 * 24)) # daily values
  dfD$Date <- as.character (as.Date (dfD$timeStamp))
  dfD$jday <- as.numeric (format (dfD$timeStamp, "%j%"))  ## ok to have 2000-1-1 to be day 1, not 0
  dfD$varS <- var [match (dfD$Date, df$Date)]  ## keep those to indicate dates of measurements
  dfD$varSN <- na.approx (dfD$varS, x = dfD$timeStamp, na.rm = FALSE) ## interpolated measurements
  dfD$var_norm < varNorm$MA [match (dfD$jday, varNorm$jday)]
  names (dfD) <- gsub ("^var", varN, names (dfD))
  dfD
}


## this is a section over time, CTDsectionFcts.R::mkSection is a space section
mkSection <- function(xC) {
  require (oce)
  xC <- xC [order (xC$isoTime), ]
  xC$Date <- factor (xC$DateISO)
  cL <- lapply (seq_along(levels (xC$Date))
    , FUN = function(i) {
      sCTD <- subset (xC, xC$Date == levels (xC$Date)[i])
      ocOb <- with (sCTD,
        as.ctd (salinity = Salinity_PSU
          , temperature = Temperature_ITS90_DegC
          , pressure = Pressure..Strain.Gauge..db.
          , longitude = as.numeric (longitude_DD)
          , latitude = as.numeric (latitude_DD)
          # , sectionId = transDate
          # , startTime = isoTime
      ))
      ocOb@metadata$station <- xC$Match_Name [1]
      ocOb@metadata$startTime <- sCTD$isoTime [1]
      ocOb@metadata$waterDepth <- 103

      ocOb <- oceSetData (ocOb, "chlorophyll", sCTD$Chlorophyll_mg_m3)
      ocOb <- oceSetData (ocOb, "turbidity", sCTD$turbidity)
      ocOb <- oceSetData (ocOb, "O2perc", sCTD$O2perc)
      ocOb <- oceSetData (ocOb, "PAR", sCTD$PAR.Irradiance)
      ocOb <- oceSetData (ocOb, "lChlorophyll", log (sCTD$Chlorophyll_mg_m3))
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
  cL <- sectionSort (cL, by = "time")
  cL
}


TSaxis <- function(isoTime, axes = TRUE, verticals = TRUE) {
  tAx <- as.POSIXct (as.Date (paste0 (2012:max (as.numeric (format (isoTime, "%Y"))), "-01-01")))
  lAx <- as.POSIXct (as.Date (paste0 (2012:max (as.numeric (format (isoTime, "%Y"))), "-07-01")))
  if (verticals) {
    abline (v = tAx, lty = "dashed")
  }
  if (axes == TRUE) {
    axis (1, at = tAx, label = FALSE)
    axis (1, at = lAx, label = format (lAx, "%Y"), tick = FALSE)
  }
}

plot.station <- function(section, axes = TRUE, ...) {
  plot (section, showBottom = FALSE, xtype = "time", ztype = "image"
    # , at = FALSE
    # , stationTicks = TRUE
    , grid = FALSE
    , axes = FALSE, ...
    , xlab = "", ylab = "")
  axis (2, at = c(0, 20, 40, 60, 80, 100))
}



#############################################
##                                         ##
## Big Loop, making plots for each station ##
##                                         ##
#############################################


for (k in pickStn) {
  try ({
    #   k <- 87  ## 9-6
    stnK <- levels (physOc$Match_Name)[k]
    cat (stnK, "\n")
    xC <- subset (physOc, Match_Name == stnK)
    xC <- xC [order (xC$isoTime), ]
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

    ctdAgg <- aggregate (Temperature_ITS90_DegC ~ depthR + month, xC, FUN = mean, na.rm = TRUE)
    ctdAgg$Salinity_PSU <- aggregate (Salinity_PSU ~ depthR + month, xC, FUN = mean, na.rm = TRUE)$Salinity_PSU
    ctdAgg$Pressure..Strain.Gauge..db. <- aggregate (Pressure..Strain.Gauge..db. ~ depthR + month, xC, FUN = mean, na.rm = TRUE)$Pressure..Strain.Gauge..db.
    ctdAgg$Chlorophyll_mg_m3 <- aggregate (Chlorophyll_mg_m3 ~ depthR + month, xC, FUN = mean, na.rm = TRUE)$Chlorophyll_mg_m3
    ctdAgg$bvf <- aggregate (bvf ~ depthR + month, xC, FUN = mean, na.rm = TRUE)$bvf

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

    ctdAgg$tloess <- anoF ("Temperature_ITS90_DegC")
    ctdAgg$sloess <- anoF ("Salinity_PSU")
    ctdAgg$floess <- anoF ("Chlorophyll_mg_m3")
    ctdAgg$bvfloess <- anoF ("bvf")

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
    matchN <- match (paste0 (xC$depthR, "-", xC$month), paste0 (ctdAgg$depthR, "-", ctdAgg$month))
    xC$anTem <- xC$Temperature_ITS90_DegC - ctdAgg$tloess [matchN]
    xC$anSal <- xC$Salinity_PSU - ctdAgg$sloess [matchN]
    xC$anBvf <- xC$bvf - ctdAgg$bvfloess [matchN]

    ##############################################
    ## seasonal climatologies of poSS summaries ##
    ##############################################
    poSSclim <- aggregate (TempDeep ~ month, poSS, subset = poSS$Match_Name == stnK
      , FUN = mean, na.rm = TRUE)
    poSSclim$pclDepth <- aggregate (pclDepth ~ month, poSS, subset = poSS$Match_Name == stnK ## XXX settle names!
      , FUN = mean, na.rm = TRUE)$pcDepth
    poSSclim$bvfMax <- aggregate (bvfMax ~ month, poSS, subset = poSS$Match_Name == stnK ## XXX settle names!
      , FUN = mean, na.rm = TRUE)$pcDepth
    poSSclim$stability <- aggregate (stability ~ month, poSS, subset = poSS$Match_Name == stnK
      , FUN = mean, na.rm = TRUE)$stability
    poSSclim$SalDeep <- aggregate (SalDeep ~ month, poSS, subset = poSS$Match_Name == stnK
      , FUN = mean, na.rm = TRUE)$SalDeep


    png (paste0 (mediaD, "/2-", stnK, "-TSprofile.png"), height = fDim [2] * pngR, width = fDim [1] * pngR, res = pngR)
    if (plotRAW) {
      par (mfrow = c(5, 1))
    } else {
      par (mfrow = c(3, 1))
    }
    par (oma = c(0, 3, 2, 0))

    ## current anomaly range: -7.5 to 6.7 -- needs to be symmetrical
    sF <- function(v, n = 12, qR = quantR) {
      # aR <- max (abs (range (v, na.rm=TRUE)))
      aR <- max (abs (stats::quantile(v, probs = c(1 - qR, qR), na.rm = TRUE)))
      aR <- signif (aR, 1)
      # aR <- ceiling (aR)
      seq (-aR, aR, length.out = n)
    }


    xCS <- mkSection (xC)
    if (plotRAW) {
      ## time series of raw data
      plot.station (xCS, which = "temperature"
        , zcol = tCol
        # , zbreaks=sF (xC$Temperature_ITS90_DecC)
        , legend.loc = "" # legend.text="temperature anomaly [°C]"
        , mar = c(2.5, 4, 2.3, 1.2)  ## default:  3.0 3.5 1.7 1.2
      )
      TSaxis (xC$isoTime)
      title (main = "temperature [°C]", line = 1.2)
      ## station-ticks
      axis (3, at = xC$isoTime, labels = FALSE)

      zB <- seq (26, ceiling(max (xC$Salinity_PSU, na.rm = TRUE)), length.out = length (salCol) + 1)
      plot.station (xCS, which = "salinity"
        , zcol = salCol
        , zbreaks = zB
        , legend.loc = "" # legend.text="temperature anomaly [°C]"
      )
      TSaxis (xC$isoTime)
      title (main = "salinity [PSU]")
    }

    ## time series of anomalies
    zB <- sF (xC$anTem, n = 1 + length (tColAn), qR = 0.997)
    zB <- c (seq (-3, -0.5, by = 0.5), seq(0.5, 3, by = 0.5))
    if (length (zB) != length (tColAn) + 1) {stop ("Fix zB for temp anomaly")}
    plot.station (xCS, which = "anTem"
      , zcol = tColAn
      # , zcol = rev (brewer.pal (length (zB)-1, "RdBu"))
      , zbreaks = zB
      , legend.loc = "" # legend.text="temperature anomaly [°C]"
    )
    TSaxis (xC$isoTime)
    # legend ("bottomright", legend="temperature anomaly [°C]", fill="white") #, bty="n")
    title (main = "temperature anomaly [°C]")

    zB <- sF (subset (xC$anSal, xC$Depth.saltwater..m. <= deepThd))
    plot.station (mkSection (subset (xC, Depth.saltwater..m. <= deepThd))
      , which = "anSal"
      , zcol = oceColors9B(length (zB) - 1)  # brewer.pal (11, "PiYG")
      , zbreaks = zB
      , axes = FALSE
      , xlab = ""
      # , ylim=c(-1,-10)
      , mar = c(1.0, 3.5, 3.7, 1.2)  # default:  3.0 3.5 1.7 1.2
      , legend.loc = "" # legend.text="salinity anomaly [PSU]"
    )
    axis (2, at = pretty (0:deepThd))
    title (main = "salinity anomaly [PSU]")


    zB <- sF (subset (xC$anSal, xC$Depth.saltwater..m. > deepThd))
    plot.station (mkSection (subset (xC, Depth.saltwater..m. > deepThd))
      , which = "anSal"
      , zcol = oceColors9B(length (zB) - 1)  # brewer.pal (11, "PiYG")
      , zbreaks = zB
      # , ylim=c(-10,-1*max (xC$Depth.saltwater..m.))
      # , ylim=c(deepThd, max (xC$Depth.saltwater..m.))  ## still places a zero -- report bug
      , mar = c(4.7, 3.5, 0, 1.2) # default:  3.0 3.5 1.7 1.2
      , legend.loc = ""
    )
    TSaxis (xC$isoTime)
    rm (zB)  ## keep xCS for buoyancy

    mtext ("Depth [m]", side = 2, outer = TRUE)
    dev.off()

    # save.image ("~/tmp/LCI_noaa/cache-t/t9ctd1.RData")
    # rm (list = ls()); load ("~/tmp/LCI_noaa/cache-t/t9ctd1.RData")


    ## buoyancy
    ## raw (climatoloty below)
    ## anomaly
    ## water column stability
    ## depth of pycnocline




    #########################
    ## plot TS climatology ##
    #########################

    prC <- ctdAgg; prC$monthI <- prC$monthI - 12
    poC <- ctdAgg; poC$monthI <- poC$monthI + 12
    ctdAggD <- rbind (prC, ctdAgg, poC); rm (prC, poC) ## to plot margins as well
    ## clean up defining section -- or go back to section functions?
    cT9 <- lapply (0:13, function(i) { # from prev month to month after current year -- why not longer?
      sCTD <- subset (ctdAggD, monthI == i)
      ocOb <- with (sCTD, as.ctd ( # salinity = Salinity_PSU, temperature = Temperature_ITS90_DegC
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
    cT9 [['station']] <- lapply (seq_along(cT9 [['station']])
      , function(i) {
        oceSetMetadata(cT9 [['station']][[i]], 'startTime'
          , dTimes [i]
        )
      }); rm (dTimes)
    cT9 <- sectionSort (cT9, by = "time")
    rm (ctdAggD)


    anAx <- function(dAx = c(0, 50, 100)) {  ## annotations for x-axis -- for one year
      ## XXX make option for time series and for climatology
      axis (1, at = as.POSIXct (as.Date (paste0 ("2000-", 1:12, "-01"))), label = FALSE)
      axis (1, at = as.POSIXct (as.Date (paste0 ("2000-", 1:12, "-15"))), label = month.abb, tick = FALSE)
      axis (2, at = dAx)
    }
    clPlot <- function(cT, which = "temperature", zcol = oceColorsTemperature(11), ...) {
      plot (cT, which = which, xtype = "time", ztype = "image", zcol = zcol
        , xlim = c(as.POSIXct (as.Date (c("2000-01-01", "2000-12-31"))))
        , axes = FALSE, xlab = ""
        , ...)
    }

    png (paste0 (mediaD, "/1-", stnK, "-TS_climatology.png"), height = fDim[2] * pngR, width = fDim[2] * pngR, res = pngR)
    par (mfrow = c(2, 1))
    clPlot (cT9, which = "temperature"
      , zcol = tCol
      , zbreaks = seq (min (ctdAgg$Temperature_ITS90_DegC)
        , max (ctdAgg$Temperature_ITS90_DegC), length.out = length (tCol) + 1)
      # , zlim = c(4,12)
    )
    title (main = expression (TEMPERATURE ~ CLIMATOLOGY ~ '['^o * C * ']'))
    anAx(pretty (range (as.numeric (levels (ctdAgg$depthR))))) ## XXX pretty (max-depth)

    clPlot (cT9, which = "salinity"
      , zcol = salCol
      , zbreaks = seq (28, 31.5, length.out = length(salCol) + 1)
      # , zlim = c(28,31.5)
    )
    anAx(pretty (range (as.numeric (levels (ctdAgg$depthR))))) ## XXX pretty (max-depth)
    title (main = expression (SALINITY ~ CLIMATOLOGY ~ '[' * PSU * ']'))
    dev.off()


    ## Chlorophyll
    png (paste0 (mediaD, "/3-", stnK, "-chlorophyll-climatology.png"), res = pngR
      , height = fDim[2] * pngR, width = fDim[1] * pngR)
    par (las = 1, mfrow = c(3, 1))
    clPlot (cT9, which = "sFluo", zcol = oceColorsChlorophyll (4))
    #  anAx(dAx = seq (0, 100, by = 20))
    anAx(pretty (range (as.numeric (levels (ctdAgg$depthR))))) ## XXX pretty (max-depth)

    ## add time series and anomaly
    plot.station (xCS, which = "chlorophyll"
      , zcol = oceColorsChlorophyll(32)
      # , zbreaks=zB
      , legend.loc = "" # legend.text="temperature anomaly [°C]"
    )
    title (main = expression (Chlorophyll ~ a ~ "[" * mg ~ m^-3 * "]"))
    TSaxis (xCS@metadata$time)

    ## total chlorophyll time series plot
    clf <- aggregate (Chlorophyll_mg_m3 ~ Date, xC, FUN = mean, na.rm = TRUE)  # sum of concentration makes no sense, needs to be mean
    names (clf) <- c ("Date", "Chlorophyll")
    clf$Date <- as.Date (clf$Date)

    clfnorm <- longM (clf$Chlorophyll, clf$Date)
    T96f <- data.frame (timeStamp = seq (min (xC$isoTime), max (xC$isoTime), by = 3600 * 24)) ## standardize timeStamp vs isoTime
    T96f$Date <- as.Date(T96f$timeStamp)
    T96f$jday <- as.numeric (format (T96f$timeStamp, "%j")) - 1
    T96f$chl <- clf$Chlorophyll [match (T96f$Date, clf$Date)]
    T96f$chlN <- na.approx (T96f$chl, x = T96f$timeStamp, na.rm = FALSE)
    T96f$chl_norm <- clfnorm$MA [match (T96f$jday, clfnorm$jday)]
    rm (clfnorm)

    # plot (chlN~Date, T96f, type="l", ylab=expression (Chlorophyl~a~"["*mg~m^-3*"]"))
    # lines (chl_norm~Date, T96f, col = "lightgreen")
    # for (i in 1:nrow (T96f)){
    #   with (T96f, lines (x=rep (Date [i], 2)
    #                      ,y=c(chl_norm [i], chlN[i])
    #                      , col=ifelse (chlN [i] > chl_norm [i], "darkgreen", "lightgreen")
    #                      , lwd=2
    #   ))
    # }
    par (mar = c(5, 4, 4, 4.9) + 0.1)  ## to align last plot with plots above in same panel
    plot (chlN ~ Date, T96f, pch = 19
      , col = ifelse (T96f$chlN > T96f$chl_norm, "darkgreen", "lightgreen")
      , ylab = expression (Chlorophyl ~ a ~ "[" * mg ~ m^-3 * "]"))
    lines (chl_norm ~ Date, T96f, col = "gray")
    legend ("topright", lwd = c(5, 5, 1), col = c("darkgreen", "lightgreen", "gray")
      , legend = c("more", "less", "mean"), bty = "n")
    abline (v = min (physOc$year):max (physOc$year), col = "gray", lty = "dashed")
    dev.off()


    ## single panel of chlorophyll for monthly report -- model this after temp anomaly
    png (paste0 (mediaD, "/3-", stnK, "-chlorophyll-anomaly.png"), res = pngR
      , height = fDim[2] * pngR / 3, width = fDim[1] * pngR)
    plot (chlN ~ Date, T96f, pch = 19
      , col = ifelse (T96f$chlN > T96f$chl_norm, "darkgreen", "lightgreen")
      , ylab = expression (Chlorophyl ~ a ~ "[" * mg ~ m^-3 * "]"))
    lines (chl_norm ~ Date, T96f, col = "gray")
    legend ("topright", lwd = c(5, 5, 1), col = c("darkgreen", "lightgreen", "gray")
      , legend = c("more", "less", "mean"), bty = "n")
    abline (v = min (physOc$year):max (physOc$year), col = "gray", lty = "dashed")
    dev.off()
    ## single panel of chlorophyll section for monthly report
    png (paste0 (mediaD, "/3-", stnK, "-chlorophyll-timesection.png"), res = pngR
         , height = fDim[2] * pngR / 3, width = fDim[1] * pngR)
    plot.station (xCS, which = "chlorophyll"
                  , zcol = oceColorsChlorophyll(32)
                  # , zbreaks=zB
                  , legend.loc = "" # legend.text="temperature anomaly [°C]"
    )
    title (main = expression (Chlorophyll ~ a ~ "[" * mg ~ m^-3 * "]"))
    TSaxis (xCS@metadata$time)
    dev.off()


    rm (T96f, clf)


    ##############
    ## buoyancy ##
    ##############
    dir.create(paste0 (mediaD, "_experimental/"), showWarnings = FALSE, recursive = TRUE)
    png (paste0 (mediaD, "_experimental", "/4-", stnK, "-buoyancy-climatology.png"), height = fDim[2] * pngR, width = fDim[1] * pngR, res = pngR)
    par (mfrow = c(3, 1))
    ## raw buoyancy profile
    ## bvf climatology
    ## max-bvf time-series -- with seasonal signal
    ## ?? depth of pycnocline time-series in summer -- with seasonal signal (or anomaly?)

    ## raw time-series profile
    # zB <- seq (0, ceiling(max (xC$bvf, na.rm=TRUE)), by=0.5)
    require ("cmocean")  ## for color ramp cmocean -- just use viridis
    options ('cmocean-version' = "2.0") # fix colors to cmocean 2.0

    ## seasonal climatology
    clPlot (cT9, which = "sBvf"
      , zcol = colorRampPalette (c ("white", rev (cmocean ("haline")(32))))
    )
    anAx (pretty (range (as.numeric (levels (ctdAgg$depthR)))))
    title (main = expression (Brunt ~ Väisälä ~ Buoyancy ~ frequency - seasonal ~ climatology ~ "[" * s^-2 * "]"))

    ## raw time series of buoyancy
    plot.station (xCS, which = "bvf"
      , zcol = colorRampPalette (c ("white", rev (cmocean ("haline")(32))))
      # , zbreaks=zB
      , legend.loc = "" # legend.text="temperature anomaly [°C]"
      , ylim = c(deepThd, 0)  ## new requirement of new oce version?
    )
    title (main = expression (Brunt - Väisälä ~ buoyancy ~ frequency ~ "[" * s^-2 * "]"))
    TSaxis (xCS@metadata$time)

    ### XXX anAx (pre)
    if (1) {
      ## anomaly  (too noisy?)
      zB <- sF (v = xC$anBvf, qR = 0.90)
      plot.station (xCS, which = "anBvf"
        , zcol = brewer.pal (11, "RdBu")
        , zbreaks = zB
        , legend.loc = "" # legend.text="temperature anomaly [°C]"
      )
      title (main = expression (Buoyancy ~ frequency ~ Anomaly ~ "[" * s^-2 * "]"))
    }

    ## summaries: timeseries of strength of stratification and pycnocline depth
    ### plot (bvfMax~timeStamp, data=poSS, subset=poSS$Match_Name==stnK, type="l")
    #  plot (bvfMean~timeStamp, data=poSS, subset=poSS$Match_Name==stnK, type="l")  ## almost the same as max
    #  plot (pclDepth~timeStamp, data=poSS, subset=poSS$Match_Name==stnK, type="l")  ## maybe useful for summer-only
    #  plot (stability~timeStamp, data=poSS, subset=poSS$Match_Name==stnK, type="l")  ## noisy -- errors?

    ## boyancy long-term mean
    # bVar <- poSS$bvfMax
    bVar <- poSS$bvfMean
    tbnorm <- longM (bVar, poSS$timeStamp)
    T96f <- data.frame (timeStamp = seq (min (poSS$timeStamp), max (poSS$timeStamp), by = 3600 * 24))
    T96f$Date <- as.character (as.Date(T96f$timeStamp))
    T96f$jday <- as.numeric (format (T96f$timeStamp, "%j")) - 1
    T96f$TempS <- bVar [match (T96f$Date, poSS$Date)]
    T96f$TempSN <- na.approx (T96f$TempS, x = T96f$timeStamp, na.rm = FALSE)
    T96f$TempS_norm <- tbnorm$MA [match (T96f$jday, tbnorm$jday)]

    par (mar = c(5, 4, 4, 4.9) + 0.1)  ## to align last plot with plots above in same panel
    plot (TempSN ~ timeStamp, T96f, type = "n", main = "Mean buoyancy frequency"
      , ylab = "" # expression('Temperature'~'['*degree~'C'*']')
      , xlab = "", axes = FALSE)
    axis (2)
    TSaxis(T96f$timeStamp, verticals = FALSE)
    lines (TempSN ~ timeStamp, T96f, col = "black", lwd = 2)
    lines (TempS_norm ~ timeStamp, T96f, col = "blue", lwd = 2)
    legend ("topright", lwd = 2, col = c("black", "blue"), legend = c("max buoyancy", "seasonal mean"), bty = "n")
    box()
    dev.off()




    #########################################
    ## chlorophyll and pycnocline patterns ##
    #########################################
    png (paste0 (mediaD, "/6-", stnK, "-chlorophyll-buoyancy-climatology.png"), res = pngR
      , height = 6 * pngR, width = 8 * pngR)
    par (las = 1, mfrow = c(2, 1))
    ## chlorophyll
    clPlot (cT9, which = "sFluo", zcol = oceColorsChlorophyll(12)
      , ylim = c(0, 25)) ## add contour
    anAx(pretty (range (as.numeric (levels (ctdAgg$depthR)))))
    title (main = expression (Chlorophyll ~ concentration ~ "[" * mg ~ m^-3 * "]"))

    ## buoyancy-frequency
    clPlot (cT9, which = "sBvf"
      , zcol = colorRampPalette (c ("white", rev (cmocean ("haline")(8))))(12)
      , ylim = c(0, 25)
    )
    anAx (pretty (range (as.numeric (levels (ctdAgg$depthR)))))
    title (main = expression (Brunt - Väisälä ~ buoyancy ~ frequency ~ "[" * s^-2 * "]"))

    ## PAR
    ## turbidity
    dev.off()



    ## --------- emulate Figure 4 from GWA report -- in ODV colors, because Kris want's it
    ## three panels, 1 page: Temperature, Salinity, Chlorophyll

    ODV_colours <- colorRampPalette(c("#feb483", "#d31f2a", "#ffc000", "#27ab19"
      , "#0db5e6", "#7139fe", "#d16cfa"))(50) %>%
      rev()
    dir.create("~/tmp/LCI_noaa/media/EVOS/", showWarnings = FALSE, recursive = TRUE)
    png ("~/tmp/LCI_noaa/media/EVOS/Figure4-TSC-panel.png", width = 6.5 * 150, height = 8.0 * 150, res = 150)
    par (mfrow = c(3, 1))
    Mr <- c(3, 3.5, 2.5, 1.2)

    ## time series of raw data
    plot.station (xCS, which = "temperature"
      #                  , zcol=ODV_colours
      , zcol = tCol
      # , zbreaks=sF (xC$Temperature_ITS90_DecC)
      , legend.loc = "" # legend.text="temperature anomaly [°C]"
      , mar = Mr
      #              , mar=c(2.5,4,2.3,1.2)  ## default:  3.0 3.5 1.7 1.2
    )
    TSaxis (xC$isoTime)
    title (main = "temperature [°C]", line = 1.2)
    axis (3, at = xC$isoTime, labels = FALSE)


    # zB <- seq (26, ceiling(max (xC$Salinity_PSU, na.rm=TRUE)),length.out=length (salCol)+1)
    plot.station (xCS, which = "salinity"
      , zcol = ODV_colours
      #              , zbreaks=zB
      , legend.loc = "" # legend.text="temperature anomaly [°C]"
      , mar = Mr
    )
    TSaxis (xC$isoTime)
    title (main = "salinity [PSU]")


    plot.station (xCS, which = "chlorophyll"
      , zcol = ODV_colours
      #              , zbreaks=zB
      , legend.loc = "" # legend.text="temperature anomaly [°C]"
      , mar = Mr
    )
    TSaxis (xC$isoTime)

    require (latex2exp)
    eX <- TeX ("Chlorophyll [$mg~m^{-3}$]")
    #    eX <- expression ("Chlorophyll ["~mg~m^-3~"]")
    #    plot (1:2, main=eX)
    title (main = eX)
    dev.off()
    rm (eX, ODV_colours)
    ## --------------- end of visual assault --------
  })
}
## alternative display of this data:
rm (anoF)


save.image ("~/tmp/LCI_noaa/cache-t/ctdT9S6_fw.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache-t/ctdT9S6_fw.RData")






#########################
## freshwater contents ##
#########################

## move this elsewhere! -- signature data?
## aggregate Freshwater contents over surface layer of T9
xC <- subset (poSS, Match_Name %in% paste0 ("9_", 1:10))
fw <- aggregate (SalSurface ~ Date, data = xC, FUN = mean, na.rm = FALSE)
fw$SalDeep <- aggregate (SalDeep ~ Date, data = xC, FUN = mean, na.rm = FALSE)$SalDeep

TH_sal <- ceiling(max (physOc$Salinity_PSU, na.rm = TRUE))
fw$freshCont <- 33 - fw$SalSurface
fw$freshDeep <- 33 - fw$SalDeep

# names (fw) <- c ("Date", "freshCont", 'freshDeep')
fw$Date <- as.POSIXct(as.Date (fw$Date))
fw$month <- as.numeric (format (fw$Date, "%m"))
fw$year <- as.numeric (format (fw$Date, "%Y"))

## QAQC
# is.na (fw$freshCont [fw$freshCont > 200]) <- TRUE
# fw$freshDeep <- ifelse (fw$freshDeep > 2000, NA, fw$freshDeep)  ## bad CTD battery?
# fw$FreshWaterCont <- ifelse (fw$FreshWaterCont > 2000, NA, fw$FreshWaterCont)  ## bad CTD battery?

## calc seasonal anomaly -- for starters based on month -- better to use full record in ARIMA as with SWAMP
fwS <- aggregate (freshCont ~ month, fw, mean)
fwS$sd <- aggregate (freshCont ~ month, fw, stats::sd)$freshCont
lfw <- fwS; lfw$month <- lfw$month - 12
ufw <- fwS; ufw$month <- ufw$month + 12
fwS <- rbind (lfw, fwS, ufw); rm (lfw, ufw)
require (mgcv)
md <- gam (freshCont ~ s(month), data = fwS)
lS <- loess (freshCont ~ month, data = fwS, span = 0.2)
## stick with monthly means for now, rather than smooth
fw$fwA <- fw$freshCont - fwS$freshCont [match (fw$month, fwS$month)]
# fw$fwA <-


## add freshwater -- deep

png (paste0 (mediaD, "/T9_freshwatercontent.png"), height = fDim[2] * pngR, width = fDim[1] * pngR, res = pngR)
par (mfrow = c(3, 1), mar = c (5, 4, 0.1, 0.1))
## time series
plot (freshCont ~ Date, fw, type = "l", ylab = "freshwater content")
## seasonal climatology
plot (freshCont ~ month, fwS
  , xlim = c(1, 12), type = "l", lwd = 2
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
plot (fwA ~ Date, fw, type = "l", ylab = "freshwater contents anomaly", axes = FALSE)
axis (2)
abline (h = 0, col = "gray")
#  plot (f)
TSaxis (poSS$timeStamp)
# TSaxis (xC$isoTime)
box()
dev.off()




### timing of freshwater -- panel for each year ###
if (0) {
  require ("lattice")
  png (paste0 (mediaD, "/T9S6_freshSeason.png"), width = 7 * 100, height = 9 * 100, res = 100)
  print (xyplot (freshCont ~ month | factor (year), data = fw, as.table = TRUE, type = "l"))
  print (xyplot (fwA ~ month | factor (year), data = fw, as.table = TRUE, type = "l"
    , xlab = "freshwater anomaly"))
  dev.off()
}


## variation of freshwater -- all in one panel == for GWA report

fwX <- fw
lY <- rep ("dashed", 2)
fw <- subset (fw, year < 2022)

fw$year <- factor (fw$year)
fw$jday <- as.numeric (format (fw$Date, "%j"))
fwag <- aggregate (freshCont ~ month + year, data = fw, FUN = mean)
fwag$freshDeep <- aggregate (freshDeep ~ month + year, data = fw, FUN = mean)$freshDeep

png (paste0 (mediaD, "/T9_freshwaterSpagettiYears.png"), width = 7 * 100, height = 12 * 100, res = 100)
par (mfrow = c(2, 1))
par (mar = c(4 - 3, 4, 4, 1))
plot (freshCont ~ month, data = fwag, type = "n", ylab = "", xlab = "", axes = FALSE)
axis (2)
for (i in seq_along(levels (fwag$year))) {
  lines (freshCont ~ month, data = fwag, subset = fwag$year == levels (fwag$year)[i]
    , col = i, lwd = 2
    , lty = c(rep ('solid', length (levels (fwag$year)) - length (lY)), lY)[i]
  )
  # points (freshCont~month, data=fwag, subset=fwag$year==levels (fwag$year)[i], col=i, pch=19)
}
legend ("bottomright", legend = levels (fwag$year), col = seq_along(levels (fwag$year))
  , lwd = 2, bty = "n", ncol = 4
  , lty = c(rep ('solid', length (levels (fwag$year)) - length (lY)), lY)
)
legend ("topright", legend = "surface water", bty = "n")
box()

par (mar = c(4, 4, 4 - 3, 1))
plot (freshDeep ~ month, data = fwag, type = "n", ylab = "", xlab = "", axes = FALSE)
axis (1, labels = month.abb, at = 1:12); axis (2)
for (i in seq_along(levels (fwag$year))) {
  lines (freshDeep ~ month, data = fwag, subset = fwag$year == levels (fwag$year)[i]
    , col = i, lwd = 2
    , lty = c(rep ('solid', length (levels (fwag$year)) - length (lY)), lY)[i]
  )
}
legend ("topleft", legend = levels (fwag$year), col = seq_along(levels (fwag$year))
  , lwd = 2, bty = "n", ncol = 4
  , lty = c(rep ('solid', length (levels (fwag$year)) - length (lY)), lY)
)
legend ("topright", legend = "deep water", bty = "n")
box()

mtext ("Freshwater content index", side = 2, line = -1.5, outer = TRUE)
dev.off()
fw <- fwX; rm (fwX, fwag)




if (0) {
  ## quick test on stratification
  t96S <- subset (poSS, Match_Name == "AlongBay_9") %>%
    subset (month %in% c(6, 7, 8))
  t96S$sSSS <- scale (t96S$SSS, center = TRUE, scale = TRUE)
  t96S$sTRange <- scale (t96S$tideRange, center = TRUE, scale = TRUE)
  plot (bvfMax ~ tideRange, t96S)
  lmStrat <- lm (bvfMax ~ sTRange + sSSS, t96S)
  summary (lmStrat)

  poSummer <- subset (poSS, month %in% 6:8)
  poSummer$Match_Name <- factor (poSummer$Match_Name)
  stCor <- sapply (levels (poSummer$Match_Name), function(x) {
    tSum <- subset (poSummer, Match_Name == x)
    # lmStrat <- lm (bvfMax~sTRange+sSSS)
    lmStrat <- cor(tSum$bvfMax, tSum$tideRange, use = "pairwise", method = "pearson")
    lmStrat
  })
  ## expectation: the bigger the tidal range, the lower the bvfMax -> negative correlation
  ## in winter: expect no correlation
  SuWi <- subset (poSS, month %in% c(1, 2, 3, 6, 7, 8))
  SuWi$sSSS <- scale (SuWi$SSS)
  SuWi$stideRange <- scale (SuWi$tideRange)
  SuWi$season <- ifelse (SuWi$month %in% c(1, 2, 3), "winter", "summer")
  tM <- lm (bvfMax ~ stideRange + sSSS + season + Match_Name, SuWi)
  summary (tM)
  tM <- lm (bvfMax ~ sSSS + season + Match_Name, SuWi)
  summary (tM)
  tM <- lm (bvfMax ~ stideRange + season + Match_Name, SuWi)
  summary (tM)
}




save.image ("~/tmp/LCI_noaa/cache-t/ctdT96-dwt.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache-t/ctdT96-dwt.RData")

########################################
## deep water temperature time series ##
########################################

## - raw time series
## - superimposed anomalies/seasonal mean
## - marking first day >= threshold temperature in spring
## - timing of x degree C in spring
## move up to plot for each station?
## could also do this for Seldovia Air and water temperatures.

T96 <- subset (poSS, Match_Name == "9_6")  ## migrate to sf
T96 <- T96 [order (T96$timeStamp), ]
require ("tidyr")

tL <- c("Deep", "Max", "SalDeep", "TempSurface", "SalSurface")
titleL <- c ("Deep-Water Temperature", "Maximum Temperature", "Deep-Water Salinity"
  , "Surface Temperature", "Surface Salinity")
## deep-water: mean at depth > 15 m
for (iS in seq_along(tL)) {
  T96$TempS <- with (T96, list (TempDeep, TempMax, SalDeep, TempSurface, SalSurface))[[iS]]
  tempName <- tL [iS]

  if (tempName == "Max") {
    thTempL <- c(8, 12) ## list of threshold temperatures
  } else if (tempName == "Deep") {
    thTempL <- c (4, 4) ## seq (4, 8, by=0.5)
  }
  tbnorm <- longM (T96$TempS, T96$timeStamp)
  # T96$TempS_anom <- anomF(T96$TempS, T96$timeStamp, tbnorm)
  ## see annualPlotFct.R::fixGap -- all gaps, then interpolate NAs
  T96f <- data.frame (timeStamp = seq (min (T96$timeStamp), max (T96$timeStamp), by = 3600 * 24))
  T96f$Date <- as.character (as.Date(T96f$timeStamp))
  T96f$jday <- as.numeric (format (T96f$timeStamp, "%j")) - 1
  T96f$TempS <- T96$TempS [match (T96f$Date, T96$Date)]
  T96f$TempSN <- na.approx (T96f$TempS, x = T96f$timeStamp, na.rm = FALSE)
  T96f$TempS_norm <- tbnorm$MA [match (T96f$jday, tbnorm$jday)]
  rm (tbnorm)

  if (tempName == ("Deep")) {
    png (paste0 (mediaD, "/5-T9-6_Temp", tempName, "TS.png"), res = pngR
      , height = fDim [2] * pngR / 2, width = fDim [1] * pngR * 1.5)
    anomCol <- c("red", "blue"); anomL <- c ("warmer", "colder")
    # par (mfrow=c(2,1)) ## do not plot thresholds for salinity
    yLabt <- "Temperature [°C]"
  } else {
    png (paste0 (mediaD, "/5-", tempName, "TS.png"), res = pngR
      , height = fDim [2] * pngR / 2, width = fDim [1] * pngR * 1.5)
    if (tempName == "TempSurface" | tempName == "Max") {
      anomCol <- c("red", "blue"); anomL <- c ("warmer", "colder")
      # par (mfrow=c(2,1)) ## do not plot thresholds for salinity
      yLabt <- "Temperature [°C]"
    } else {
      anomCol <- c("yellow", "green"); anomL <- c("saltier", "fresher")
      yLabt <- "Salinity [PSU]"
    }
  }
  plot (TempSN ~ timeStamp, T96f, type = "n", main = paste (titleL [iS], "at T9-6")
    , ylab = yLabt, xlab = "", axes = FALSE)
  axis (2)
  abline (v = as.POSIXct (paste0 (2000:2040, "-1-1")), col = "gray", lwd = 1.0)
  TSaxis(T96f$timeStamp, verticals = FALSE)
  if (tempName == "Deep") {
    #    abline (h=thTempL, lty="dashed") # mark 4 degrees C
  }
  mLW <- 3
  nLW <- mLW
  legend("bottomright", lwd = c (nLW, mLW, 3, 3), col = c ("gray", "black", anomCol)
    , legend = c(paste0("mean [", min(T96$Year), "-", max(T96$Year), "]")
      , "31 d moving-average", anomL)
    , bty = "o", ncol = 2
    , bg = "white", box.col = "white")
  ## add lines, marking the anomaly in blue/red
  for (j in seq_len(nrow(T96f))) {
    with (T96f, lines (x = rep (timeStamp [j], 2)
      , y = c(TempS_norm [j], TempSN[j])
      , col = ifelse (TempSN [j] > TempS_norm [j], anomCol [1], anomCol [2])
      , lwd = 2
    ))
  }
  lines (TempS_norm ~ timeStamp, T96f, col = "gray", lwd = nLW)
  lines (TempSN ~ timeStamp, T96f, col = "black", lwd = mLW)
  box()
  rm (anomCol, anomL, yLabt)

  ## plot timing of 4 degrees C over year
  if (0 & tempName == "Deep") {
    T96f$Year <- as.numeric (format (T96f$timeStamp, "%Y"))

    springM <- sapply (thTempL, function(y) {
      aggregate (TempSN ~ Year, data = T96f, function(x, thTemp = y) {
        lD <- min ((seq_along(x[1:(366 / 2)]))[x >= thTemp], na.rm = TRUE)
        x <- c (-1, x)
        #        if (tempName=="Max"){
        #          x4 <- min ((seq_along(x[1:(300)]))[x>=thTemp], na.rm=TRUE)  ## give it to fall, not next winter
        #        }else{
        x4 <- max ((seq_along(x[1:(366 / 2)]))[x <= thTemp], na.rm = TRUE)
        #        }
        lD <- ifelse (x4 == 1, NA, x4)   ## review this further!!  2024 isn't right XXX
        # lD <- ifelse (x4>=364/2, NA, lD)
        as.Date("2000-01-01") + lD
        # last4
      })$TempSN
    })

    springM [is.infinite((springM))] <- NA
    rownames(springM) <- levels (factor (T96f$Year))
    springM <- as.Date (springM) # this would turn matrix into vector if ncol=1
    if (length (thTempL) > 1) {
      suppressWarnings(colr <- brewer.pal(length (thTempL), "Accent"))
    } else {colr <- "black"}

    ## version 1 -- year on x-axis
    if (!kr) {
      yL <- as.numeric (rownames((springM)))
      plot  (seq (min (yL), max(yL) + 1, length.out = nrow(springM)), springM [, 1], type = "n", xlab = "", ylab = ""
        , main = paste ("Earliest threshold", tempName, "temperature")
        , ylim = range (springM, na.rm = TRUE)
        , axes = FALSE)
      yD <- pretty (springM)
      axis (2, tick = TRUE, labels = format (yD, "%e %b"), at = yD) # "%m-%d")
      rm (yD)
      axis (1, tick = TRUE, labels = FALSE, at = yL)
      axis (1, tick = FALSE, labels = yL, at = yL + 0.5)
      box()
      abline (h = as.Date(paste0 ("2000-0", 1:8, "-01")), lty = "dashed", col = "gray")
      if (tempName == "Max") {xi <- seq_along(thTempL)} else {xi <- 1}
      #      if (as.numeric (format (Sys.Date(), "%m")) < 6){ ## cut out current, incomplete year
      #       springM <- springM [1:(nrow (springM)-1),]
      #      }
      for (i in xi) {
        points (springM [, i] ~ I(yL + 0.5), col = colr [i], pch = 19, cex = 2)
        for (j in seq_len(nrow(springM))) {
          lines (c(0.1, 0.9) + yL [j], springM [c(j, j), i], col = colr [i], lwd = 3)
        }
        # lines (springM [,i]~yL, col=colr [i], lwd=3, type="s")
        # lines (springM [,i]~I(yL+1), col=colr [i], lwd=3, type="S") ## to connect last dot in middle of year
      }
      legend ("bottomright" ## needs to move below plot and needs to be smaller
        , lwd = 3
        , pch = 19, cex = 1
        , col = colr, legend = thTempL [xi]
        , title = "temperature [°C]" # expression(temperature~"["*degree~C*"]")
        , bty = "o", ncol = 3, pt.cex = 2, pt.lwd = 3)
      rm (yL, xi)
    } else {  ## version 2 -- year on y-axis
      plot (springM [, 1], levels (factor (T96f$Year)), ylab = ""
        , xlab = expression (First ~ day ~ with ~ temperature ~ at ~ 4^o ~ C)
        , pch = 19, col = "black", cex = 3, lwd = 3
      )
      # paste0 ("First day with temperature at ", ))
      abline (h = levels (factor (T96f$Year)), lty = "dashed")
    }
  }
  dev.off()

  # ## add plain deep temperature anomaly
  # if (tempName=="Deep"){
  #   png (paste0 (mediaD, "/5-",tempName ,"TS-plain.png"), res=pngR
  #        , height=fDim [2]*pngR/2, width=fDim [1]*pngR*1.5)
  #
  #   dev.off()
  # }

}




# rm (springM)
rm (thTempL, tempName, tL)
rm (T96, T96f)






## GAK1 -- and compare deep-water salinity to T8-6
## data on workspace: 1998-2017
## depth: 20, 30, 60, 100, 150, 200, 250 m
##
## climatology of depth profiles (as above)
## anomaly-like plot for each depth
##

if (0) {  ## need to look at gak-line, not gak1=mooring!  mooring is too far inshore, not reaching into ACC

  zL <- list.files("~/GISdata/LCI/GAK1mooring-data-improved/", "*.zip", full.names = TRUE)
  list.files (zL [1], "*.csv")
  fl <- unzip (zL [1], list = TRUE)

  x <- read.csv (paste (zL[1], fl$Name [1], sep = "/"))



  fl <- unzip (zL [1], list = TRUE)
  require ("tidyverse")
  require ("oce")
  # tD <- tempdir()
  gak <- readr::read_csv (unzip (zL[1])) ## put unzip into temp dir!!
  names (gak) <- c("Station", "Type", "longitude"
    , "latitude", "bottomDepth", "DateTime"
    , "depth", "pressure", "temperature"
    , "conductivity", "salinity", "densSigma"
    , "instrument")
  unlink ("UAK_GAK1_*.csv")  ## better to use tempdir()
  gak <- subset (gak, !is.na (depth))
  gak$depthF <- ifelse (gak$depth < 40, 30, gak$depth)
  gak$depthF <- factor (gak$depthF)

  gakS <- lapply (seq_along(levels (gak$depthF))
    , function(i) {
      gax <- subset (gak, depthF == levels (gak$depthF)[i])
      with (gax, as.ctd (salinity, temperature
        , pressure
        , time = DateTime
        , longitude = -149.5
        , latitude = 59.84
        , deploymentType = "moored"))
    }) %>%
    as.section   # (waterDepth=250)
}

# u <- "http://erddap.aoos.org/erddap/tabledap/org_gulfwatchalaska_gak1"
# require(ncdf4)
# dataset <- nc_open (u)
# names (dataset)
# rm (u)


## Cross-correlation of seldovia-deep and GAK1 20 m
## Cross-correlation of T9-6 100 m and GAK1 100 m
## Correlation-map of T9-6 100m and GAK-line 100m/surface


## TS-diagram
# plot (Salinity_PSU~Temperature_ITS90_DegC, data=physOc, col=format (physOc$isoTime, "%m"))
# use oce template for pretty plot





## move this to a new chlorophyll script?
## test correlation between surface and DCM chlorophyll concentration
surface <- 2
deep <- c (15, 20)
save.image ("~/tmp/LCI_noaa/cache-t/ctdTimechl.RData")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache-t/ctdTimechl.RData")

t96 <- subset (physOc, Match_Name == "9_6")

chlA <- aggregate (Chlorophyll_mg_m3 ~ DateISO, data = t96, FUN = mean, na.rm = TRUE
  , subset = Depth.saltwater..m. <= surface)
chlAd <- aggregate (Chlorophyll_mg_m3 ~ DateISO, data = t96, FUN = mean, na.rm = TRUE
  , subset = (Depth.saltwater..m. >= deep [1]) & (Depth.saltwater..m. <= deep [2]))
chlA$deep <- chlAd$Chlorophyll_mg_m3 [match (chlA$DateISO, chlAd$DateISO)]


png (paste0 (mediaD, "/x-chlorophy_TS.png"), res = pngR
  , height = fDim [2] * pngR / 2, width = fDim [1] * pngR * 1.5)

par (xlog = TRUE, ylog = TRUE)
plot (Chlorophyll_mg_m3 ~ deep, chlA, col = "green", pch = 19, log = "xy")
summary (lm (Chlorophyll_mg_m3 ~ deep, chlA))
cor (chlA$Chlorophyll_mg_m3, chlA$deep, method = "spearman", use = "pairwise.complete.obs")
cor (chlA$Chlorophyll_mg_m3, chlA$deep, method = "pearson", use = "pairwise.complete.obs")^2

chlA$DateISO <- as.POSIXct(chlA$DateISO)
par (ylog = "TRUE")
plot (Chlorophyll_mg_m3 ~ DateISO, data = chlA, type = "l", log = "y", lwd = 2, col = "lightgreen"
  , main = "Chlorophyll at T9-6")
lines (chlA$DateISO, chlA$deep, col = "darkgreen", lwd = 2)
legend ("topright", lwd = 2, col = c("lightgreen", "darkgreen")
  , legend = c("surface", paste0 (deep [1], "-", deep [2], " m"))
  , bty = "n")
dev.off()
rm (t96, chlA, chlAd)

## geographic correlation
## 2017-04
## 2018-07 and 09
## 2019-07
## AlongBay:  2023-03  2023-05  2023-09
geoC <- subset (physOc, substr (physOc$DateISO, 1, 7) == "2017-04")
geoC <- subset (physOc, substr (physOc$DateISO, 1, 7) == "2023-05")

geoC <- subset (physOc, (physOc$Transect == "AlongBay") & (format (physOc$isoTime, "%m") == "05"))


chlA <- aggregate (Chlorophyll_mg_m3 ~ Match_Name, data = geoC, FUN = mean, na.rm = TRUE
  , subset = Depth.saltwater..m. <= surface)
chlAd <- aggregate (Chlorophyll_mg_m3 ~ Match_Name, data = geoC, FUN = mean, na.rm = TRUE
  , subset = (Depth.saltwater..m. >= deep [1]) & (Depth.saltwater..m. <= deep [2]))
chlA$deep <- chlAd$Chlorophyll_mg_m3 [match (chlA$Match_Name, chlAd$Match_Name)]
chlA$lat <- geoC$latitude_DD [match (chlA$Match_Name, geoC$Match_Name)]
chlA <- chlA [order (chlA$lat), ]

par (ylog = TRUE)
plot (Chlorophyll_mg_m3 ~ lat, chlA, type = "l", log = "y", lwd = 2, col = "lightgreen"
  , main = "Chlorophyll in May 2023 on AlongBay transect")
lines (chlA$lat, chlA$deep, col = "darkgreen", lwd = 2)
legend ("topright", lwd = 2, col = c("lightgreen", "darkgreen")
  , legend = c("surface", paste0 (deep [1], "-", deep [2], " m"))
)

graphics.off()
cat ("\nEnd of CTD_timeseries.R\n\n")

## EOF
