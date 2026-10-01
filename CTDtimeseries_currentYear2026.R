#!/usr/bin/env RScript

###########################
## CTD anomaly over time ##
###########################


## load data
## start with file from dataSetup.R
rm(list = ls()); load("~/tmp/LCI_noaa/cache/CTDcasts.RData")  # from dataSetup.R -- contains physOc -- raw CTD profiles
# load("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")  ## ??


# source("CTDsectionFcts.R")

## set-up plot and paper size




## select which stations to plot -- all or only named stations
physOc$Match_Name <- as.factor(physOc$Match_Name)
# pickStn <- which(levels(physOc$Match_Name) %in%
#   c("9_6", "9_8", "9_2", "AlongBay_4", "4_6", "4_3"))  # span is too small for many AlongBay transects
# #                     c("9_6", "AlongBay_3", "3_14", "3_13", "3_12", "3_11"))
# # pickStn <- seq_along(levels(physOc$Match_Name)) ## some fail as-is: simpleLoess span too small
pickStn <- which(levels(physOc$Match_Name) %in% c("9_6", "AlongBay_5", "AlongBay_3", "AlongBay_7"))
pickStn <- which(levels(physOc$Match_Name) %in% c("AlongBay_7"))  # for news
# pickStn <- which(levels(physOc$Match_Name) %in% c("AlongBay_13", "AlongBay_2", "AlongBay_3"))


quantR <- 0.99  ## curtail data at this percentile


fDim <- c(8.5, 11)
# fDim <- c(16,9)   ## figure dimensions



## gray scale -- honest
tCol <- gray.colors(101)
salCol <- gray.colors(101)

## modern colors -- overwrite
tCol <- oce::oceColorsTemperature(11)
salCol <- oce::oceColorsSalinity(11)

# ## from https://www.esri.com/arcgis-blog/products/arcgis-pro/mapping/a-meaningful-temperature-palette/
# tCol <- rgb(c(230, 157, 49, 62, 123, 177, 170, 155, 145, 64)
#              , c(238, 176, 77, 115, 146, 146, 123, 81, 45, 20)
#              , c(253, 211, 123, 143, 136, 102, 89, 79, 75, 37)
#              , maxColorValue=255)
# tCol <- rev(c("#de5842", "#fcd059", "#ededea"
#                 , "#bfe1bf", "#a2d7d8"))
tCol <- oce::oceColorsTurbo(1000)
tCol <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
tColAn <- rev(RColorBrewer::brewer.pal(length(salCol), "RdBu"))
# tCol <- colorRampPalette(tCol, alpha=FALSE)(1000)  ## interpolate colors, or make them continuous


## antiquated rainbow
jet <- TRUE
jet <- FALSE
if(jet) {
  tCol <- oce::oceColorsTurbo(1000)
  salCol <- colorRampPalette(col = rev(c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa"))
    , bias = 0.3)(1000) ## ODV colors
  didntlikeit <- colorRampPalette(rev(c("#de5842", "#fcd059", "#ededea",
    "#bfe1bf", "#a2d7d8")))(9)
}



### data prep
## define sections
physOc$DateISO <- format(physOc$isoTime, "%Y-%m-%d")
# physOc$transDate <- factor(with(physOc, paste(DateISO, Transect, sep = " T-")))
physOc$transDate <- factor(with(physOc, paste0("T-", Transect, " ", DateISO)))
physOc$Transect <- factor(physOc$Transect)
physOc$year <- as.numeric(format(physOc$isoTime, "%Y"))
## combine CTD and station meta-data
physOc <- subset(physOc, !is.na(physOc$Transect)) ## who's slipping through the cracks??
## stn should be no longer needed -- see dataSetup.R
# physOc <- cbind(physOc, stn [match(physOc$Match_Name, stn$Match_Name)
#                               , which(names(stn) %in% c(# "Line",
#                                                           "Lon_decDegree", "Lat_decDegree", "Depth_m"))])
# print(summary(physOc))

mediaD <- "~/tmp/LCI_noaa/media/CTDsections/time-sections"
dir.create(mediaD, recursive = TRUE, showWarnings = FALSE)


save.image("~/tmp/LCI_noaa/cache-t/ctdAnomalies.RData")
# rm(list = ls()); load("~/tmp/LCI_noaa/cache-t/ctdAnomalies.RData")



########################################
## long term mean / anomaly functions ##
########################################

longM <- function(var, date, maO = 31) {  ## cyclical long-term mean -- move this to annualPlotFct.R ?
  ## calculate long term mean of var for use in anomaly calculation
  ## smooth using zoo moving average smoother

  if(class(date)[1] != "Date") {
    date <- as.Date(date)
  }

  # ## restrict years to a set baseline
  # timeR <- as.Date(c("2012-01-01", "2025-31-12"))
  # var <- subset (var, (timeR[1] < date) & (date < timeR[2]))
  # date <- subset (date, (timeR[1] < date) & (date < timeR[2]))


  if(length(var) != length(date)) {stop("var and date have to be of equal length")}
  # df <- data.frame(var, date)
  # df <- df [order(df$date),]
  jday = as.numeric(format(date, "%j")) - 1  # 1 jan needs to be day 0, not 1!
  df <- rbind(data.frame(var, date, jday = jday - 365)
    , data.frame(var, date, jday)
    , data.frame(var, date, jday = jday + 365)
  )
  aD <- aggregate(var ~ jday, df, FUN = mean, na.rm = TRUE)
  aD2 <- data.frame(jday = -366:(366 * 2))
  aD2$var <- aD$var [match(aD2$jday, aD$jday)]
  aD2$MA <- zoo::rollapply(aD2$var, width = maO, FUN = mean, na.rm = TRUE ## critical for monthly data!
    , fill = c(NA, "extend", NA)
    , partial = FALSE, align = "center")
  # aD2$MA <- zoo::na.approx(aD2$MA, na.rm=FALSE, x=aD2$jday)   # order is important -- this last(or biased)
  aD2$MA <- zoo::na.spline(aD2$MA, na.rm = FALSE, x = aD2$jday)   # order is important -- this last(or biased)
  aD2$loss <- predict(loess(MA ~ jday, aD2))
  oD <- subset(aD2,(0 < jday)  &(jday < 367))
  oD
}
anomF <- function(var, date, longM) {
  df <- data.frame(var, date, jday = format(date, "%j"))
  ## longM needs to contain MA and jday!
  df$anom <- df$var - longM$MA [match(df$jday, longM$jday)]
  df$anom
}
## patterns in residuals -- stick with loess
# mgcv::gam(Temperature_ITS90_DegC~s(monthI), data = sDF, subset = depthR == i)
anoF <- function(varN, df = sDF) {
  vN <- which(names(df) == varN)
  sOut <- sapply(levels(df$depthR), FUN = function(i) {
    loess(as.formula(paste0(varN, "~monthI"))
      , df, subset = depthR == i
      , span = 0.25)$fitted[(1:12) + 12]
  })
  sapply(seq_len(nrow(ctdAgg)), FUN = function(i) {
    sOut [ctdAgg$monthI [i], ctdAgg$depthR [i]]
  })
}

## daily anomaly function: supply measurements of vairable X
## return: daily interpolated measurements over time series, and normals for those days
dailyTS <- function(df, varN) {
  var <- df [, which(names(df) == varN)]
  ## ensure that df has variable "timeStamp"(or isoDate?)
  if("isoDate" %in% names(df)) {df$timeStamp <- df$isoDate}
  if(!"Date" %in% names(df)) {df$Date <- as.Date(df$timeStamp)}
  varNorm <- longM(var, df$timeStamp)
  if(class(df$timeStamp)[1] != 'POSIXct') {stop("timeStamp needs to be POSIXct")}
  dfD <- data.frame(timeStamp = seq(min(df$timeStamp), max(df$timeStamp), by = 3600 * 24)) # daily values
  dfD$Date <- as.character(as.Date(dfD$timeStamp))
  dfD$jday <- as.numeric(format(dfD$timeStamp, "%j%"))  ## ok to have 2000-1-1 to be day 1, not 0
  dfD$varS <- var [match(dfD$Date, df$Date)]  ## keep those to indicate dates of measurements
  dfD$varSN <- zoo::na.approx(dfD$varS, x = dfD$timeStamp, na.rm = FALSE) ## interpolated measurements
  dfD$var_norm < varNorm$MA [match(dfD$jday, varNorm$jday)]
  names(dfD) <- gsub("^var", varN, names(dfD))
  dfD
}






## this is a section over time, CTDsectionFcts.R::mkSection is a space section
mkSection <- function(xC) {
  require(oce)
  xC <- xC [order(xC$isoTime), ]
  xC$Date <- factor(xC$DateISO)
  cL <- lapply(seq_along(levels(xC$Date))
    , FUN = function(i) {
      sCTD <- subset(xC, xC$Date == levels(xC$Date)[i])
      ocOb <- with(sCTD,
        oce::as.ctd(salinity = Salinity_PSU
          , temperature = Temperature_ITS90_DegC
          , pressure = Pressure..Strain.Gauge..db.
          , longitude = as.numeric(longitude_DD)
          , latitude = as.numeric(latitude_DD)
          # , sectionId = transDate
          # , startTime = isoTime
      ))
      ocOb@metadata$station <- xC$Match_Name [1]
      ocOb@metadata$startTime <- sCTD$isoTime [1]
      ocOb@metadata$waterDepth <- 103

      ocOb <- oce::oceSetData(ocOb, "chlorophyll", sCTD$Chlorophyll_mg_m3)
      ocOb <- oce::oceSetData(ocOb, "turbidity", sCTD$turbidity)
      ocOb <- oce::oceSetData(ocOb, "O2perc", sCTD$O2perc)
      ocOb <- oce::oceSetData(ocOb, "PAR", sCTD$PAR.Irradiance)
      # ocOb <- oce::oceSetData(ocOb, "lChlorophyll", log(sCTD$Chlorophyll_mg_m3))
      # ocOb <- oce::oceSetData(ocOb, "N2", sCTD$Nitrogen.saturation..mg.l.)
      # ocOb <- oce::oceSetData(ocOb, "Spice", sCTD$Spice)
      ocOb <- oce::oceSetData(ocOb, "bvf", sCTD$bvf)
      ocOb <- oce::oceSetData(ocOb, "anTem", sCTD$anTem)
      ocOb <- oce::oceSetData(ocOb, "anSal", sCTD$anSal)
      ocOb <- oce::oceSetData(ocOb, "anBvf", sCTD$anBvf)

      ocOb
    }
  )
  cL <- oce::as.section(cL)
  cL <- oce::sectionSort(cL, by = "time")
  cL
}


TSaxis <- function(isoTime, axes = TRUE, verticals = TRUE) {
  tAx <- as.POSIXct(as.Date(paste0(2012:max(as.numeric(format(isoTime, "%Y"))), "-01-01")))
  lAx <- as.POSIXct(as.Date(paste0(2012:max(as.numeric(format(isoTime, "%Y"))), "-07-01")))
  if(verticals) {
    abline(v = tAx, lty = "dashed")
  }
  if(axes == TRUE) {
    axis(1, at = tAx, label = FALSE)
    axis(1, at = lAx, label = format(lAx, "%Y"), tick = FALSE)
  }
}

plot.station <- function(section, axes = TRUE, ...) {
  plot(section, showBottom = FALSE, xtype = "time", ztype = "image"
    # , at = FALSE
    # , stationTicks = TRUE
    , grid = FALSE, axes = FALSE
    , xlab = "", ylab = "", ... )
  axis(2, at = c(0, 20, 40, 60, 80, 100))
}

sF <- function(v, n = 12, qR = quantR) {
## current anomaly range: -7.5 to 6.7 -- needs to be symmetrical
  # aR <- max(abs(range(v, na.rm=TRUE)))
  aR <- max(abs(stats::quantile(v, probs = c(1 - qR, qR), na.rm = TRUE)))
  aR <- signif(aR, 1)
  # aR <- ceiling(aR)
  seq(-aR, aR, length.out = n)
}

anAx <- function(dAx = c(0, 50, 100)) {  ## annotations for x-axis -- for one year
  ## XXX make option for time series and for climatology
  axis(1, at = as.POSIXct(as.Date(paste0("2000-", 1:12, "-01"))), label = FALSE)
  axis(1, at = as.POSIXct(as.Date(paste0("2000-", 1:12, "-15"))), label = month.abb, tick = FALSE)
  axis(2, at = dAx)
}

clPlot <- function(cT, which = "temperature", zcol = oce::oceColorsTemperature(11), ...) {
  plot(cT, which = which, xtype = "time", ztype = "image", zcol = zcol
       , xlim = c(as.POSIXct(as.Date(c("2000-01-01", "2000-12-31"))))
       , axes = FALSE, xlab = ""
       , ...)
}



#############################################
##                                         ##
## Big Loop, making plots for each station ##
##                                         ##
#############################################


for(k in pickStn) {
    # k <- pickStn[1]
    stnK <- levels(physOc$Match_Name)[k]
    cat(stnK, "\n")
    xC <- subset(physOc, as.character(Match_Name) == stnK)
    xC <- xC [order(xC$isoTime), ]
    ## cut-off at 85m as several seasons missing beyond that depth
    xC <- subset(xC, Depth.saltwater..m. < 85)

    ## turn into section
    xC$Date <- as.factor(xC$Date)
    require("oce")

    ##########################
    ### calculate anomalies ##
    ##########################

    ## bin by depth(rather than just pressure)
    xC$depthR <- factor(round(xC$Depth.saltwater..m.))
    xC$month <- factor(format(xC$isoTime, "%m"))
    ## aggregate useing oce function -- skip aggregation in pre-processing by SBprocessing
    ## calculate normals

    ctdAgg <- aggregate(Temperature_ITS90_DegC ~ depthR + month, xC, FUN = mean, na.rm = TRUE)
    ctdAgg$Salinity_PSU <- aggregate(Salinity_PSU ~ depthR + month, xC, FUN = mean, na.rm = TRUE)$Salinity_PSU
    ctdAgg$Pressure..Strain.Gauge..db. <- aggregate(Pressure..Strain.Gauge..db. ~ depthR + month, xC, FUN = mean, na.rm = TRUE)$Pressure..Strain.Gauge..db.
    ctdAgg$Chlorophyll_mg_m3 <- aggregate(Chlorophyll_mg_m3 ~ depthR + month, xC, FUN = mean, na.rm = TRUE)$Chlorophyll_mg_m3
    ctdAgg$bvf <- aggregate(bvf ~ depthR + month, xC, FUN = mean, na.rm = TRUE)$bvf

    ## smooth normals
    ctdAgg$monthI <- as.numeric(levels(ctdAgg$month))[ctdAgg$month]
    preDF <- ctdAgg; preDF$monthI <- preDF$monthI - 12
    postDF <- ctdAgg; postDF$monthI <- postDF$monthI + 12
    sDF <- rbind(preDF, ctdAgg, postDF)
    rm(preDF, postDF)

    ctdAgg$tloess <- anoF("Temperature_ITS90_DegC")
    ctdAgg$sloess <- anoF("Salinity_PSU")
    ctdAgg$floess <- anoF("Chlorophyll_mg_m3")
    ctdAgg$bvfloess <- anoF("bvf")

    ## anomaly = observ-smoothed normal
    matchN <- match(paste0(xC$depthR, "-", xC$month), paste0(ctdAgg$depthR, "-", ctdAgg$month))
    xC$anTem <- xC$Temperature_ITS90_DegC - ctdAgg$tloess [matchN]
    xC$anSal <- xC$Salinity_PSU - ctdAgg$sloess [matchN]
    xC$anBvf <- xC$bvf - ctdAgg$bvfloess [matchN]

    pngR <- 150
    png(paste0(mediaD, "/News2026-", stnK, "-TSprofile.png")
        , height = fDim [2] * pngR, width = fDim [1] * pngR, res = pngR)
    par(mfrow = c(3, 1))
    par(oma = c(0, 3, 2, 0))



    xCS <- mkSection(xC)

      ## time series of raw data
      ## plotting time section with function defined above

      tBreak <- c(seq(1, 13, length.out=length(tCol)+1))  ## climatology struggels
      tBreak <- c(seq(3, 13, length.out=length(tCol)+1))  ## to fit climatology

      plot.station(xCS, which = "temperature"
        , xlim=as.POSIXct(c("2026-01-01", "2026-12-31"))
        , zcol = tCol
        , zbreaks = tBreak
        , legend.loc = "" # legend.text="temperature anomaly [°C]"
        , mar = c(2.5, 4, 2.3, 1.2)  ## default:  3.0 3.5 1.7 1.2
        , axes = FALSE
      )
      axis(2)
#     TSaxis(xC$isoTime)  ## avoid plotting a year when plotting only one
      title(main = "2026 temperature [°C]", line = 1.2)
      ## station-ticks
      axis(3, at = xC$isoTime, labels = FALSE)



    #########################
    ## plot TS climatology ##
    #########################

    prC <- ctdAgg; prC$monthI <- prC$monthI - 12
    poC <- ctdAgg; poC$monthI <- poC$monthI + 12
    ctdAggD <- rbind(prC, ctdAgg, poC); rm(prC, poC) ## to plot margins as well
    ## clean up defining section -- or go back to section functions?
    cT9 <- lapply(0:13, function(i) { # from prev month to month after current year -- why not longer?
      sCTD <- subset(ctdAggD, monthI == i)
      ocOb <- with(sCTD, oce::as.ctd( # salinity = Salinity_PSU, temperature = Temperature_ITS90_DegC
        salinity = sloess, temperature = tloess
        , pressure = Pressure..Strain.Gauge..db.
        , longitude = rep(i, nrow(sCTD))
        , latitude = rep(0, nrow(sCTD))
      ))
      ocOb@metadata$waterDepth <- 103  ## needed? vary by station
      ocOb <- oce::oceSetData(ocOb, "sSal",  sCTD$sloess)
      ocOb <- oce::oceSetData(ocOb, "sTemp", sCTD$tloess)
      ocOb <- oce::oceSetData(ocOb, "sFluo", sCTD$floess)
      ocOb <- oce::oceSetData(ocOb, "sBvf",  sCTD$bvfloess)
      return(ocOb)
    })
    cT9 <- as.section(cT9)
    dTimes <- as.POSIXct(c("1999-12-15"
      , paste0("2000-", 1:12, "-15")
      , "2001-01-15"))
    cT9 [['station']] <- lapply(seq_along(cT9 [['station']])
      , function(i) {
        oce::oceSetMetadata(cT9 [['station']][[i]], 'startTime'
          , dTimes [i]
        )
      }); rm(dTimes)
    cT9 <- sectionSort(cT9, by = "time")
    rm(ctdAggD)



    clPlot(cT9, which = "temperature"
      , legend.loc = "" # legend.text="temperature anomaly [°C]"
      , zcol = tCol
#     , zbreaks = seq(min(ctdAgg$Temperature_ITS90_DegC), max(ctdAgg$Temperature_ITS90_DegC), length.out = length(tCol) + 1)
      , zbreaks = tBreak
    )
    title(main = expression(Temperature ~ Climatology ~ '['^o * C * ']'))
    anAx(pretty(range(as.numeric(levels(ctdAgg$depthR))))) ## XXX pretty(max-depth)




    #############################
    ## time series of anomalies
    #############################
    zB <- sF(xC$anTem, n = 1 + length(tColAn), qR = 0.997)
    zB <- c(seq(-3, -0.5, by = 0.5), seq(0.5, 3, by = 0.5))
    if(length(zB) != length(tColAn) + 1) {stop("Fix zB for temp anomaly")}
    plot.station(xCS, which = "anTem"
                 , zcol = tColAn
                 , xlim=as.POSIXct(c("2026-01-01", "2026-12-31"))
                 # , zcol = rev(RColorBrewer::brewer.pal(length(zB)-1, "RdBu"))
                 , zbreaks = zB
                 , legend.loc = "" # legend.text="temperature anomaly [°C]"
                 , axes=FALSE
    )
    axis(2)
    #    TSaxis(xC$isoTime)
    # legend("bottomright", legend="temperature anomaly [°C]", fill="white") #, bty="n")
    title(main = "2026 temperature anomaly [°C]")

    rm(zB)  ## keep xCS for buoyancy

    mtext("Depth [m]", side = 2, outer = TRUE)



    dev.off()
}


graphics.off()

## EOF
