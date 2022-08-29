## CTD anomaly over time


## load data
## start with file from dataSetup.R
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/CTD.RData")  # contains physOc -- raw CTD profiles
load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## from CTD_cleanup.R

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") ## this contains poSS -- CTD summaries
## link physOc and stn
## should be poSS and stn -- check!

## set-up plot and paper size


###########################
## bugs/missing features ##
###########################



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




### data prep
require ("oce")
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

dir.create("~/tmp/LCI_noaa/media/ctdClimatologies", recursive = TRUE, showWarnings = FALSE)

save.image ("~/tmp/LCI_noaa/cache/ctdAnomalies.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdAnomalies.RData")


pickStn <- which (levels (physOc$Match_Name) %in% c("9_6", "AlongBay_3", "3_14", "3_13", "3_12", "3_11", "3_10", "3_1"))# , "AlongBay_10"))
for (k in 1:length (levels (physOc$Match_Name))){
  try ({
# for (k in pickStn){
  stnK <- levels (physOc$Match_Name)[k]
  cat (stnK, "\n")
  xC <- subset (physOc, Match_Name == stnK)
  ## use only T9-6
  # xC <- subset (physOc, (Transect == "9" & Station == "6"))
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

  ## smooth normals
# ctdAgg$monthI <- as.numeric (ctdAgg$month)  ## this messes with things -- don't XXX
#  ctdAgg$monthI <- as.numeric (as.character (ctdAgg$month))
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
  ## temperature
  sOut <- sapply (levels (sDF$depthR), FUN = function (i){
    loess(Temperature_ITS90_DegC~monthI, sDF, subset = depthR == i, span = 0.25)$fitted[(1:12)+12]
  })  # 12x103 matrix
  ctdAgg$tloess <- sapply (1:nrow (ctdAgg), FUN = function (i){sOut [ctdAgg$monthI [i], ctdAgg$depthR [i]]})
  ## salinity
  sOut <- sapply (levels (sDF$depthR), FUN = function (i){
    loess(Salinity_PSU~monthI, sDF, subset = depthR == i, span = 0.25)$fitted[(1:12)+12]
  })
  ctdAgg$sloess <- sapply (1:nrow (ctdAgg), FUN = function (i){sOut [ctdAgg$monthI [i], ctdAgg$depthR [i]]})
  ## fluorescence

  sOut <- sapply (levels (sDF$depthR), FUN = function (i){
    loess(Fluorescence_mg_m3~monthI, sDF, subset = depthR == i, span = 0.25)$fitted[(1:12)+12]
  })
  ctdAgg$floess <- sapply (1:nrow (ctdAgg), FUN = function (i){sOut [ctdAgg$monthI [i], ctdAgg$depthR [i]]})



  rm (sOut)


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


  ## normal to anomaly
  matchN <- match (paste0 (xC$depthR, "-", xC$month), paste0 (ctdAgg$depthR,"-", ctdAgg$month))
  # xC$nTem <- ctdAgg$Temperature_ITS90_DegC [matchN]
  # xC$nSal <- ctdAgg$Salinity_PSU [matchN]
  xC$nTem <- ctdAgg$tloess [matchN]
  xC$nSal <- ctdAgg$sloess [matchN]
  xC$anTem <- xC$Temperature_ITS90_DegC - xC$nTem
  xC$anSal <- xC$Salinity_PSU - xC$nSal


  mkSection <- function (xC){
    require (oce)
    cL <- lapply (1:length (levels (xC$Date))
                  , FUN = function (i){
                    sCTD <- subset (xC, xC$Date == levels (xC$Date)[i])
                    ocOb <- with (sCTD,
                                  as.ctd (salinity = Salinity_PSU
                                          , temperature = Temperature_ITS90_DegC
                                          , pressure = Pressure..Strain.Gauge..db.
                                          , longitude = as.numeric (longitude_DD)
                                          , latitude = as.numeric (latitude_DD)
                                          , station = Date
                                          #, sectionId = transDate
                                          , time = isoTime
                                  ))
                    ocOb@metadata$waterDepth <- 103
                    ocOb <- oceSetData (ocOb, "fluorescence", sCTD$Fluorescence_mg_m3)
                    ocOb <- oceSetData (ocOb, "turbidity", sCTD$turbidity)
                    ocOb <- oceSetData (ocOb, "O2perc", sCTD$O2perc)
                    ocOb <- oceSetData (ocOb, "PAR", sCTD$PAR.Irradiance)
                    ocOb <- oceSetData (ocOb, "N2", sCTD$Nitrogen.saturation..mg.l.)
                    ocOb <- oceSetData (ocOb, "Spice", sCTD$Spice)
                    ocOb <- oceSetData (ocOb, "anTem", sCTD$anTem)
                    ocOb <- oceSetData (ocOb, "anSal", sCTD$anSal)

                    sCTD$logFluorescence <- log (sCTD$Fluorescence_mg_m3)
                    ocOb <- oceSetData (ocOb, "lFluorescence", sCTD$logFluorescence)

                    ocOb
                  }
    )
    as.section(cL)
  }


  plot.station <- function (section, axes = TRUE, ...){
    plot (section, showBottom = FALSE, xtype = "time", ztype = "image"
          #, at = FALSE
          # , stationTicks = TRUE
          , grid = FALSE
          , axes = FALSE, ...)
    axis (2, at = c(0, 20, 40, 60, 80, 100))
    if (axes == TRUE){
      tAx <- as.POSIXct (as.Date (paste0 (2012:max (as.numeric (format (xC$isoTime, "%Y"))), "-01-01")))
      lAx <- as.POSIXct (as.Date (paste0 (2012:max (as.numeric (format (xC$isoTime, "%Y"))), "-07-01")))
      axis (1, at = tAx, label = FALSE)
      axis (1, at = lAx, label = format (lAx, "%Y"), tick = FALSE)
      abline (v = tAx)
    }
  }


  require ("RColorBrewer")
  pdf (paste0 ("~/tmp/LCI_noaa/media/ctdClimatologies/"
               , stnK, "-profile.pdf")
       , height = 11, width = 8.5)
  par (mfrow = c(3,1))
  ## current anomaly range: -7.5 to 6.7
  sF <- function (v, n = 12){
    aR <- max (abs (range (v, na.rm = TRUE)))
    aR <- ceiling (aR)
    seq (-aR, aR, length.out = n)
  }


  zB <- sF (xC$anTem)
  # zB <- seq (-8.25, 9, by = 1.5)
  # zB <- seq (-8.0, 8.0, length.out = 12)
  plot.station (mkSection (xC)
                , which = "anTem"
                , zcol = rev (brewer.pal (length (zB)-1, "RdBu"))
                , zbreaks = zB
  )
  rm (zB)
  #axis (2, at = c(0, 50, 100))

  sF <- function (v, n = 12){
    aR <- max (abs (range (v, na.rm = TRUE)))
    aR <- ceiling (aR)
    seq (-aR, aR, length.out = n)
  }
  zB <- sF (subset (xC$anSal, xC$Depth.saltwater..m. <= 10))
  plot.station (mkSection (subset (xC, Depth.saltwater..m. <= 10))
                , which = "anSal", zcol = brewer.pal (11, "PiYG"), zbreaks = zB
                , axes = FALSE)
  axis (2, at = seq (0, 10, by = 2)) #c(0, 5, 10))
  zB <- sF (subset (xC$anSal, xC$Depth.saltwater..m. > 10))
  plot.station (mkSection (subset (xC, Depth.saltwater..m. > 10))
                , which = "anSal", zcol = brewer.pal (11, "PiYG"), zbreaks = zB)
  dev.off()

#  save.image ("~/tmp/LCI_noaa/cache/t9ctd1.RData")
#  # rm (list = ls()); load ("~/tmp/LCI_noaa/cache/t9ctd1.RData")



  ## plot climatology
  prC <- ctdAgg; prC$monthI <- prC$monthI - 12
  poC <- ctdAgg; poC$monthI <- poC$monthI + 12
  ctdAgg <- rbind (prC, ctdAgg, poC); rm (prC, poC)
  cT9 <- lapply (0:13, function (i){ # from prev month to month after current year -- why not longer?
# for (i in 0:13){ cat (i, "\n")
    sCTD <- subset (ctdAgg, monthI == i)
    ocOb <- with (sCTD, as.ctd (# salinity = Salinity_PSU, temperature = Temperature_ITS90_DegC
      salinity = sloess, temperature = tloess
      , pressure = Pressure..Strain.Gauge..db.
      , longitude = rep (i, nrow (sCTD))
      , latitude = rep (0, nrow (sCTD))
      , time = as.POSIXct (as.Date (ifelse (monthI < 1, paste0 ("1999-", monthI+12, "-15") # falls into DST gap without as.Date?
                                   , ifelse (monthI <= 12, paste0 ("2000-", monthI, "-15")
                                             , paste0 ("2001-", monthI-12, "-15")))))
    ))
    ocOb@metadata$waterDepth <- 103
    ocOb <- oceSetData (ocOb, "sSal", sCTD$sloess)
    ocOb <- oceSetData (ocOb, "sTemp", sCTD$tloess)

    ocOb <- oceSetData (ocOb, "sFluo", sCTD$floess)

  })
  cT9 <- as.section (cT9)
  # rm (ctdAgg)


  anAx <- function (dAx = c(0, 50, 100)){
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

  pdf (paste0 ("~/tmp/LCI_noaa/media/ctdClimatologies/"
               , stnK, "-climatology.pdf")
       , height = 11.5, width = 8)
  par (mfrow = c (2,1))
  clPlot (cT9, which = "temperature"
          , zcol = oceColorsTemperature (11)
          , zbreaks = seq (3,13, length.out = 12)
          # , zlim = c(4,12)
          )
  # clPlot (cT9, which = "sTemp", zcol = oceColorsTemperature (11))
  anAx(pretty (range (as.numeric (levels (ctdAgg$depthR))))) ## XXX pretty (max-depth)
  clPlot (cT9, which = "salinity"
           , zcol = oceColorsSalinity(11)
          , zbreaks = seq (28, 31.5, length.out = 12)
          # , zlim = c(28,31.5)
          )
  # clPlot (cT9, which = "sSal", zcol = oceColorsSalinity(11))
  anAx(pretty (range (as.numeric (levels (ctdAgg$depthR))))) ## XXX pretty (max-depth)
  dev.off()



  ## fluorescence
  pdf (paste ("~/tmp/LCI_noaa/media/ctdClimatologies/"
              , stnK, "-fluorescence-climatology.pdf"))
  par (las = 1)
  clPlot (cT9, which = "sFluo", zcol = oceColorsChlorophyll (4))
#  anAx(dAx = seq (0, 100, by = 20))
  anAx(pretty (range (as.numeric (levels (ctdAgg$depthR))))) ## XXX pretty (max-depth)
  dev.off()
})
}
## alternative display of this data:


save.image ("~/tmp/LCI_noaa/cache/ctdT9S6_fw.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdT9S6_fw.RData")





rm (clPlot, anAx)





#########################
## freshwater contents ##
#########################

fw <- subset (xC, Depth.saltwater..m. <= 10)
fw <- aggregate (Salinity_PSU~Date, data = fw, FUN = function (x){
  sum (max (xC$Salinity_PSU, na.rm = TRUE)-x, na.rm = FALSE)
})
names (fw) <- c ("Date", "freshCont")
fw$Date <- as.POSIXct(as.Date (fw$Date))
fw$month <- as.numeric (format (fw$Date, "%m"))
fw$year <- as.numeric (format (fw$Date, "%Y"))

## QAQC
 is.na (fw$freshCont [fw$freshCont > 200]) <- TRUE

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



if (1){
  pdf ("~/tmp/LCI_noaa/media/T9S6_freshwatercontent.pdf", height = 9, width = 6)
  par (mfrow = c(3,1)
       , mar = c (5,4, 0.1, 0.1)
  )
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
  plot (fwA ~ Date, fw, type = "l", ylab = "freshwater contents anomaly")
  abline (h = 0, col = "gray")

  dev.off()
}



### timing of freshwater -- panel for each year ###
require ("lattice")
pdf ("~/tmp/LCI_noaa/media/T9S6_freshSeason.pdf", width = 7, height = 9)
print (xyplot (freshCont~month| factor (year), data= fw, as.table = TRUE, type = "l"))
print (xyplot (fwA~month| factor (year), data= fw, as.table = TRUE, type = "l"
               , xlab = "freshwater anomaly"))
dev.off()


graphics.off()

## EOF
