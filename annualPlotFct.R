#!/usr/bin/env Rscript

## annualPlotFct.R
## functions for annual plots of long-term mean vs. current (previous) year

## submit to/use more SWMPr functions
## - qaqc
## - smoother


# pco <- as.numeric (format (Sys.Date(), "%Y"))


## standardize colors
rangCol <- c("lightyellow", "yellow")
rangCol <- c("gray", "darkgray")
rangCol <- c("gray", "darkgray")
# qantCol1 <- c("lightgray", NA)
qantCol <- c (gray (0.9), "gray")
meanCol <- "darkgray"

# year2 <- TRUE   ## switch 1 vs. 2 years on/off -- where best to put this switch?


if (0){
## using RColorBrewer:
require ("RColorBrewer")
bCol <- brewer.pal(12, "Paired")
rangCol <- bCol [5:6]
qantCol <- bCol [9]
currentCol <- bCol [1:2]
meanCol <- bCol [4]
}


# plot (1:12, pch = 19, cex = 3, col = brewer.pal (12, "Paired"))


plotSetup <- function(longMean, current, ylab = NULL, ...){
  plot (1:366
        , seq (min (c(longMean, current), na.rm = TRUE)
               , max (c(longMean, current), na.rm = TRUE), length.out = 366)
        , type = "n", axes = FALSE
        , xlim = c(5,355)
        , xlab = ""
        , ylab = ylab
  )
  axis (2)
  axis (1, at = 15+as.numeric (format (as.POSIXct (paste0 ("2019-", 1:12, "-1")), "%j"))
        , labels = month.abb, tick = FALSE) # center month-labels
  axis (1, at = c (as.numeric (format (as.POSIXct (paste0 ("2019-", 1:12, "-1"))
                                       , "%j")), 366), labels = FALSE) # add 366 to mark end of Dec
}


annualPlot <- function (longMean, percL, percU, current  ## current may be a N x 2 matrix
                        , jday , perc2L = NA, perc2U = NA, maxV = NA, minV = NA
                        , ylab = ""
                        , currentCol # = currentCol # "red"
                        , pastYear = TRUE, newYear = FALSE
                        , ...){
  plotSetup (longMean, current, ylab = ylab, ...)

  addGraphs (longMean, percL, percU, current, jday, perc2L = NA, perc2U = NA, maxV = NA, minV = NA
             , currentCol, pastYear = pastYear, newYear = newYear)
  # axis (1, at = c(1, 366), labels = NA) # redraw in case polygon went over axis
  return()
}

addGraphs <- function (longMean, percL, percU, current  ## current may be a N x 2 matrix
                       , jday , perc2L = NA, perc2U = NA, maxV = NA, minV = NA
                       , currentCol # = currentCol # "red"
                       , pastYear = TRUE, newYear = FALSE, plotRange = TRUE
){
  if (0){  # abandone range
    # if (!all (is.na (maxV)) && !all (is.na (minV))){
    pCo <- data.frame (x = c(jday, rev (jday))
                       , y = c (maxV, rev (minV)))
    # polygon (pCo, col = "lightyellow", border = "yellow")
    polygon (pCo, col = rangCol [1], border = rangCol [2])
  }

  pCo <- data.frame (x = c (jday, rev (jday))
                     , y = c(percL, rev (percU)))
  pCo <- subset (pCo, !is.na (pCo$y))
  if (0){  # abandone 2nd quantile
    polygon (pCo, col = qantCol [1], border = NA)
    pCo <- data.frame (x = c (jday, rev (jday))
                       , y = c(perc2L, rev (perc2U)))
    pCo <- subset (pCo, !is.na (y))
    polygon (pCo, col = qantCol [2], border = NA)
  }else{
    if (plotRange){
      polygon (pCo, col = qantCol [1], border = NA)
    }
  }
  lines (longMean~jday, col = meanCol, lwd = 3)

  # if (length (currentCol) < 1){
  #   if (class (current) == "matrix"){current <- as.numeric (current [,1])}
  #   lines (current~jday, col = currentCol, lwd = 4)
  # }else{
  if (pastYear){
    lines (current [,3]~jday, col = currentCol [3], lwd = 2) #, lty = "dashed") ## new: previous year
  }
  lines (current [,1]~jday, col = currentCol [1], lwd = 4)
  if (newYear){
    lines (current [,2]~jday, col = currentCol [2], lwd = 4)
  }
  axis (1, at = c(1, 366), labels = NA) # redraw in case polygon went over axis
  return()
}


cLegendO <- function (..., mRange = NULL, currentYear = NULL
                     , cYcol # = currentCol
                     , qntl = NULL
                     , sYears = NULL, sLwd = NULL, sLty = NULL, sLcol = NULL
                     , pastYear = FALSE, newYear = TRUE
){ # sYears = ??
  ## combined legend for both box and line elements.
  ## see: http://tolstoy.newcastle.edu.au/R/e2/help/07/05/16777.html
  if (!all (length (sYears) == length (sLwd), length (sYears) == length (sLty), length (sYears) == length (sLcol))){
    stop ("extra line length not consistent")
  }
  if (length (cYcol) > 1){
    currentYear <- c (currentYear - 1, currentYear, currentYear + 1)
  }
  legend (..., bty = "n"
          , legend = c(paste0 ("mean [", mRange [1], "-", mRange [2], "]")
                       , currentYear
                       , sYears
                       , "10-90 percentile"
                       # , paste0 (qntl * 100, "%-ile of mean") ## skip range or 2nd quantile
                       # , paste0 (qntl * 100, "% of variability") ## skip range or 2nd quantile
          )
           , lty = c(1, 1, rep (1, length (currentYear)-1), sLty, 0)
          , lwd = c (3, 2, rep (4, length (currentYear)-1), sLwd, 0)
          , pch = c(NA, rep (NA, length (currentYear)), rep (NA, length (sYears)), 22)
          , pt.cex = 2
          , pt.bg = c (NA, rep (NA, length (currentYear)), rep (NA, length (sYears)), qantCol [1])
          #        , col = c(meanCol, cYcol, meanCol) # l, "black") #qantCol [1])
          , col = c(meanCol, cYcol [c(3,1,2)], meanCol, sLcol, "black") #qantCol [1])
  )
}


cLegend <- function (..., mRange = NULL, currentYear = NULL
                     , cYcol # = currentCol
                     , qntl = NULL
                     , sYears = NULL, sLwd = NULL, sLty = NULL, sLcol = NULL
                     , pastYear = TRUE, newYear = FALSE
){ # sYears = ??
  ## combined legend for both box and line elements.
  ## see: http://tolstoy.newcastle.edu.au/R/e2/help/07/05/16777.html
  if (!all (length (sYears) == length (sLwd), length (sYears) == length (sLty), length (sYears) == length (sLcol))){
    stop ("extra line length not consistent")
  }
  if (pastYear && newYear){
    yT <- c (currentYear-1, currentYear, currentYear + 1)
    bg <- c (NA, NA, NA, NA, qantCol [1])
    lC <- c (meanCol, cYcol [c(3,1,2)], meanCol, sLcol, "black") #qantCol [1])
    lW <- c (3, 2, 4, 4)
  }else if (pastYear && !newYear){
    yT <- c (currentYear -1, currentYear)
    bg <- c (NA, NA, NA, qantCol [1])
    lC <- c (meanCol, cYcol [c (3, 1)], meanCol, sLcol, "black")
    lW <- c (3, 2, 4)
  }else if (!pastYear && newYear){ # current and new year
    yT <- c (currentYear, currentYear + 1)
    bg <- c (NA, NA, NA, qantCol [1])
    lC <- c (meanCol, cYcol [c (1,2)], meanCol, sLcol, "black")
    lW <- c (3, 4, 4)
  }else{ # only current year
    yT <- currentYear
    bg <- c (NA, NA, qantCol [1])
    lC <- c (meanCol, cYcol [c (1)], meanCol, sLcol, "black")
    lW <- c (3, 4, 4)
    lW <- c (4,4)
  }
  legend (..., bty = "n"
          , legend = c(paste0 ("mean [", mRange [1], "-", mRange [2], "]")
                       , yT
                       , "10th-90th %ile" #percentile"
          )
          , lty = c (rep (1, length (yT)+1), 0)
          , lwd = c (lW, 0)
          , pch = c (NA, rep (NA, length (yT)), 22)
          , pt.cex = 2
          , pt.bg = bg
          , col = lC
  )
}



aPlot <- function (df, vName, MA = TRUE, ...){
  # wrapper for annualPlot
  # assumes df was created using prepDF, resulting in standardized field names
  # -- using MA or raw??
  if (MA){
    longMean <- df [,which (names (df) == paste0 ("MA_", vName))]
    percL <- df [,which (names (df) == paste0 ("maL1_", vName))]
    percU <- df [,which (names (df) == paste0 ("maU1_", vName))]
    current <-df [,which (names (df) == paste0 ("pYMA_", vName))]
    cpo <- df [, which (names (df) == paste0 ("cYMA_", vName))]
    pcpo <- df [, which (names (df) == paste0 ("pcYMA_", vName))]
    maxV <- df [,which (names (df) == paste0 ("maxMA_", vName))]
    minV <- df [,which (names (df) == paste0 ("minMA_", vName))]
    #allY <- df [,which (names (df) == paste0 ("y_")]
    allY <- df [,grep ("^y_", names (df))]  ## XXX include paste0 ("^y_\d+4_", vName)
  }else{
    longMean <- df [,which (names (df) == vName)]
    percL <- df [,which (names (df) == paste0 ("perL1_", vName))]
    percU <- df [,which (names (df) == paste0 ("perU1_", vName))]
    current <-df [,which (names (df) == paste0 ("pY_", vName))]
    cpo <- df [, which (names (df) == paste0 ("cY_", vName))]
    pcpo <- df [, which (names (df) == paste0 ("pcY_", vName))]
    maxV <- df [,which (names (df) == paste0 ("max_", vName))]
    minV <- df [,which (names (df) == paste0 ("min_", vName))]

  }
  annualPlot (longMean, percL, percU
              , cbind (current, cpo, pcpo) #, current, cpo
              , df$jday, maxV = maxV, minV = minV, ...)
}



prepDF <- function (dat, varName, sumFct = function (x){mean (x, na.rm = TRUE)}
                     , maO = 31
                     , currentYear = as.numeric (format (Sys.Date(), "%Y"))-1
                     , qntl = c(0.8, 0.9)
){
  if (! all (c("jday", "year", varName) %in% names (dat))){
    stop (paste ("Data frame must contain the variables jday, year, and", varName))
  }
  if (length (varName) > 1){stop ("so far can only process one variable at a time")}

  ## flexible for varName to be a vector!!  -- XXX extra feature

  ## current/past year
  xVar <- dat [,which (names (dat) == varName)]
  dMeans <- aggregate (xVar~jday+year, dat, FUN = sumFct) # daily means

    ## moving average in here or supply as varName?
  ## ma to be replaced by backwards ma
if (1){
  suppressMessages (require ("zoo"))
#  dMeans$MA <- rollmean (dMeans$xVar, k = maO, fill = FALSE, align = "center")
  dMeans$MA <- rollmean (dMeans$xVar, k = maO, fill = FALSE, align = "right")
  dMeans$MA <- rollapply (dMeans$xVar, width = maO, FUN = mean, na.rm = TRUE
                          , fill = NA, partial = FALSE, align = "center")
  # or align = "left" ?
}else{
    ## bug in SWMPr smoothing function: last day = up-tick?
    require ("SWMPr")
    dMeans$MA <- unlist (smoother(dMeans$xVar, window = maO, sides = 2)) # sides = 2 for centered
    # dMeans$MA <- maT (dMeans$xVar, maO)

    ## alternative to smoother?!  this uses moving-AVERAGE, rather than moving sumFct XXX
    ## sides = 2 or sides = 1?

    # dMeans$XXX <- XX$varName [match (...)
    ## match ().... when length (varName) > 1
}
  dLTmean <- subset (dMeans, year < currentYear)
    tDay <- aggregate (xVar~jday, dLTmean, FUN = mean, na.rm = TRUE)  # not sumFct here! it's a mean!
    tDay$sd <- aggregate (xVar~jday, dLTmean, FUN = sd, na.rm = TRUE)$xVar
    tDay$MA <- aggregate (MA~jday, dLTmean, FUN = mean, na.rm = TRUE)$MA

  #  for (j in c (varName, MA)){
  for (i in 1:length (qntl)){
    tDay$perL <- aggregate (xVar~jday, dLTmean, FUN = quantile, probs = 0.5-0.5*qntl [i])$xVar
    tDay$perU <- aggregate (xVar~jday, dLTmean, FUN = quantile, probs = 0.5+0.5*qntl [i])$xVar
    names (tDay)[which (names (tDay) == "perL")] <- paste0 ("perL", i)
    names (tDay)[which (names (tDay) == "perU")] <- paste0 ("perU", i)
    tDay$maL <- aggregate (MA~jday, dLTmean, FUN = quantile, probs = 0.5-0.5*qntl [i])$MA
    tDay$maU <- aggregate (MA~jday, dLTmean, FUN = quantile, probs = 0.5+0.5*qntl [i])$MA
    names (tDay)[which (names (tDay) == "maL")] <- paste0 ("maL", i)
    names (tDay)[which (names (tDay) == "maU")] <- paste0 ("maU", i)
  }
  tDay$max <- aggregate (xVar~jday, dLTmean, FUN = max, na.rm = TRUE)$xVar # dLTmean?? XX
  tDay$min <- aggregate (xVar~jday, dLTmean, FUN = min, na.rm = TRUE)$xVar
  tDay$maxMA <- aggregate (MA~jday, dLTmean, FUN = max, na.rm = TRUE)$MA
  tDay$minMA <- aggregate (MA~jday, dLTmean, FUN = min, na.rm = TRUE)$MA


  # is this critically needed?? -- N years of data per date
  tDay$yearN <- aggregate ((!is.na (xVar))~jday, dMeans, FUN = sum
                           , na.rm = TRUE)[1:nrow (tDay),2] # some scripts fail at 366
  ## present/pretend/current year-1
  pY <- subset (dMeans, year == currentYear)
  tDay$pY <- pY$xVar [match (tDay$jday, pY$jday)]
  tDay$pYMA <- pY$MA [match (tDay$jday, pY$jday)]
  rm (pY)

  ## all years
  yr <- sapply (levels (factor (dMeans$year)), function (x){
    pY <- subset (dMeans, year == x)
    pY$MA [match (tDay$jday, pY$jday)]}
  )
  colnames(yr) <- paste0 ("y_", colnames(yr))
  tDay <- cbind (tDay, yr); rm (yr)


  ## current year (incomplete)
  cY <- subset (dMeans, year == currentYear+1)
  tDay$cY <- cY$xVar [match (tDay$jday, cY$jday)]
  tDay$cYMA <- cY$MA [match (tDay$jday, cY$jday)]
  rm (cY)
  ## add circular spline?

  ## previous year
  pcY <- subset (dMeans, year == currentYear - 1)
  tDay$pcY <- pcY$xVar [match (tDay$jday, pcY$jday)]
  tDay$pcYMA <- pcY$MA [match (tDay$jday, pcY$jday)]
  rm (pcY)

    ## fix names
#  names (tDay) <- gsub ("xVar", paste0 (varName, "_"), names (tDay))
  names (tDay) <- gsub ("xVar", varName, names (tDay))
  names (tDay)[3:ncol (tDay)] <- paste0 (names (tDay)[3:ncol (tDay)], "_", varName)

  return (tDay)
}




addTimehelpers <- function (df){
  ## assumes "datetimestamp" is present
  df$jday <- as.numeric (strftime (df$datetimestamp, "%j"))
  df$year <- as.numeric (strftime (df$datetimestamp, "%Y"))
  suppressMessages (require (lubridate))
  df$month <- month (df$datetimestamp)
  df$week <- week (df$datetimestamp)
  return (df)
}

fixGap <- function (df, intvl = 15*60){ # interval of TS in seconds
  ## fix time gaps in standard SWMP data file with NAs
  ## fix min sampling interval (60 s * 15) = standard throughout SWMP?
  # print (min (df$datetimestamp)); print (max (df$datetimestamp))
  # print (summary (df))
  tR <- seq (min (df$datetimestamp, na.rm = TRUE)
             , max (df$datetimestamp, na.rm = TRUE)
             , by = 15*60)
  nD <- data.frame (datetimestamp = tR
                    , df [match (tR, df$datetimestamp), 2:ncol (df)])
  return (addTimehelpers (nD))
  }


## add Fahrenheit scale to temperature plot
fAxis <- function (cGrad, side = 4, line = 3, mT = "Fahrenheit", ...){
  slope <- 9/5
  offset <- 32
  alt.ax <- pretty (slope * cGrad + offset)
  alt.at <- (alt.ax - offset) / slope
  axis (side = side, at = alt.at, labels = alt.ax, srt = 90)
  mtext (mT, side = side, line = line, ...)
}

## add scale in inches
iAxis <- function (mmL, side = 4, line = 3, lab = "", ...){
  mmperinch <- 25.4
  alt.ax <- pretty (mmL / mmperinch)
  alt.at <- alt.ax * mmperinch
  axis (side = side, at = alt.at, labels = alt.ax, srt = 90)
  mtext (lab, side = side, line = line, ...)
}



## load cached SWMP data and update it with the latest from CDMO
getSWMP <- function (station, QAQC = TRUE){
  ## load SWMP data from local zip file. Update to the most current data
  ## from CDMO and cache those updates for the next run.
  ## need to specify location of zip file from SWMP and cache folder below
  ## an initial zip file from CDMO is required.
  ## It is recommended to update this zip file on occasion.


  require ("SWMPr")

  cacheFolder <- "~/tmp/LCI_noaa/cache/SWMP/"
  zipFile <- "~/GISdata/LCI/SWMP/current"

  dir.create(cacheFolder, showWarnings = FALSE)
  #    if (.Platform$OS.type == "windows"){
  require ("R.utils")
  SMPfile <- filePath (zipFile, expandLinks = "local") # works on all platforms?
  #    }else{ # MacOS or Linux
  #      SMPfile <- zipFile
  #    }



  ## need to delete cacheFolder if zip file is newer to avoid data gaps

  ## cacheFolder should be deleted, when zip file was updated  -- automate that?
  if (file.exists(paste0 (cacheFolder, "/kachomet.RData"))){
    if (file.info (paste0 (cacheFolder, "/kachomet.RData"))$ctime <
        file.info (SMPfile)$ctime){
      unlink (cacheFolder, recursive = "TRUE")
    }
  }

  suppressWarnings (lT <- try (load (paste0 (cacheFolder, "/", station, ".RData"))
                               , silent=TRUE)) # yields smp
  if (class (lT)[1] == "try-error"){
    smp <- import_local(SMPfile, station) ## this is initially required!
    if (QAQC){
      smp <- qaqc (smp)  ## scrutinize further? Is this wise here? keep level 1?
    }
  }
  if (any (is.na (smp$datetimestamp))){stop ("NAs in timestamp")}
  #  ## not sure whyere the bad line is coming from, but it has to go
  #smp <- smp [!is.na (smp$datetimestamp),]
  fN <- difftime(Sys.time(), max (smp$datetimestamp), units = "days")
  ## catch for stations that are inactive?
  if ((2 < fN) & (fN < 5*365.25)){ # skip downloads for less than 2 day and legacy stations
    # ## skip downloads for legacy stations
    smp2 <- try (all_params (station
                             , Max = ceiling (as.numeric(fN)*4*24))
                 , silent = FALSE)  # XXX needs registered (static?) IP address. NCCOS VPN ok
    if (QAQC){
      smp2 <- qaqc (smp2)
    }

    if (class (smp2)[1] == "swmpr"){
      ## remove bad lines
      if (any (is.na (smp2$datetimestamp))){
        smp2 <- smp2 [!is.na (smp2$datetimestamp),]
      }
      ## order of field names does not match between hmr2 and hmr
      ## re-assemble and remove duplicates
      smp3 <- smp2 [,sapply (1:ncol (smp), FUN = function (i){
        which (names (smp)[i] == names (smp2))
      })]
      smp <- rbind (smp, smp3)
      if (any (is.na (smp$datetimestamp))){stop ("NAs in timestamp")}

      rm (smp2, smp3, fN)
      smp <- smp [which (!duplicated(smp$datetimestamp)),]
    }
  }
  ## fixGap() here??
  ## smp <- qaqc (smp, qaqc_keep = "0") ## here??
  save (smp, file = paste0 (cacheFolder, "/", station, ".RData"))
  return (smp)
}



nEvents <- function (dat, varName, thrht){
  ## number of days where varName exeeds threshold thrht
  ## compare current N days to long term mean and quantiles
  ## use for wind (storms and gales) and surf and others
  vN <- which (names (dat) == varName)
  names (dat)[vN] <- "xvar"  # for convenience
  eventL <- sapply (1:length (thrht)
                    , function (i){
                      agY <- aggregate (xvar~year
                                        , data = aggregate (xvar~jday+year
                                                            , data = dat
                                                            , FUN = function (x){
                                                              any (x > thrht [i])}
                                        )
                                        , FUN = sum)

                      c (mean = mean (agY$xvar, na.rm = TRUE)
                         , median = median(agY$xvar, na.rm = TRUE)
                         , lowerQ = quantile (agY$xvar, 0.1, na.rm = TRUE)
                         , upperQ = quantile (agY$xvar, 0.9, na.rm = TRUE)
                         , agY$xvar [(nrow (agY)-2):(nrow (agY)-1)]) #the current and previous year
                    })
  colnames(eventL) <- paste0 ("T", thrht)
  rownames(eventL)[5:6] <- paste0 ("Y", max (dat$year)-c(2,1))
  as.data.frame (t (eventL))
}


cDir <- function (wd, nDir = 8){
  ## classify direction into cardinal categories
  ## following https://community.rstudio.com/t/convert-wind-direction-degrees-into-factors-in-a-data-frame/14636/4
  if (!(nDir %in% c(4,8,16))){
    error ("nDir has to be 4, 8, or 16")
  }
  if (nDir == 4){
    rose_breaks <- c (0, 360/8, (1/8 + (1:3 / 4)) * 360, 360)
    rose_labs <- c("N", "E", "S", "W", "N")
  }else if (nDir == 8){
    rose_breaks <- c (0, 360/16, (1/16 + (1:7 / 8)) * 360, 360)
    rose_labs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
  }else{
    rose_breaks <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)
    rose_labs <- c("N", "NNE", "NE", "ENE","E", "ESE", "SE", "SSE",
                   "S", "SSW", "SW", "WSW","W", "WNW", "NW", "NNW","N")
  }
  wd <- ifelse (wd < 0, 360 + wd, wd)
  cut (wd, breaks = rose_breaks, labels = rose_labs
       , right = FALSE, include.lowest = TRUE)
}


#EOF