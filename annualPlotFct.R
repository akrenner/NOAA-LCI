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


plotSetup <- function(longMean, current, ...){
  plot (1:365
        , seq (min (c(longMean, current), na.rm = TRUE)
               , max (c(longMean, current), na.rm = TRUE), length.out = 365)
        , type = "n", axes = FALSE
        , xlim = c(5,355)
        , xlab = "", ...
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
                        , ...){
  plotSetup (longMean, current, ylab = ylab, ...)

  addGraphs (longMean, percL, percU, current, jday, perc2L = NA, perc2U = NA, maxV = NA, minV = NA
             , currentCol)
  # axis (1, at = c(1, 366), labels = NA) # redraw in case polygon went over axis
  return()
}

addGraphs <- function (longMean, percL, percU, current  ## current may be a N x 2 matrix
                        , jday , perc2L = NA, perc2U = NA, maxV = NA, minV = NA
                        , currentCol # = currentCol # "red"
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
        polygon (pCo, col = qantCol [1], border = NA)
      }
  lines (longMean~jday, col = meanCol, lwd = 3)
  if (length (currentCol) < 1){
    if (class (current) == "matrix"){current <- as.numeric (current [,1])}
    lines (current~jday, col = currentCol, lwd = 4)
  }else{
    lines (current [,1]~jday, col = currentCol [1], lwd = 4)
    lines (current [,2]~jday, col = currentCol [2], lwd = 4)
  }
  axis (1, at = c(1, 366), labels = NA) # redraw in case polygon went over axis
  return()
}


cLegend <- function (..., mRange = NULL, currentYear = NULL
                     , cYcol # = currentCol
                     , qntl = NULL
                     , sYears = NULL, sLwd = NULL, sLty = NULL, sLcol = NULL){
  ## combined legend for both box and line elements.
  ## see: http://tolstoy.newcastle.edu.au/R/e2/help/07/05/16777.html
  if (!all (length (sYears) == length (sLwd), length (sYears) == length (sLty), length (sYears) == length (sLcol))){
    stop ("extra line length not consistent")
  }
  # if (length (sYears) == 0){
  #   sYears <- NULL
  # }
#  print (currentYear)
  if (length (cYcol) > 1){currentYear <- c (currentYear, currentYear + 1)}

#  currentYear <- c(2015, 2016)
# currentYear <- c (currenctYear, currentYear+1)

  legend (..., bty = "n"
          , legend = c(paste0 ("mean [", mRange [1], "-", mRange [2], "]")
                       , currentYear
                       , sYears
                      , paste0 (qntl * 100, "%-ile") ## skip range or 2nd quantile
#                       , paste0 (qntl * 100, "% of variability") ## skip range or 2nd quantile
          )
          , lty = c(1, rep (1, length (currentYear)), sLty, 0)
          , lwd = c (3, rep (4, length (currentYear)), sLwd, 0)
          , pch = c(NA, rep (NA, length (currentYear)), rep (NA, length (sYears)), 22)
          , pt.cex = 2
          , pt.bg = c (NA, rep (NA, length (currentYear)), rep (NA, length (sYears)), qantCol [1]
                       )
          , col = c(meanCol, cYcol, sLcol, "black") #qantCol [1])
  )

  #   if (length (cYcol) > 1){
  # legend (..., bty = "n"
  #         , legend = c(paste0 ("mean [", mRange [1], "-", mRange [2], "]")
  #                      , currentYear
  #                      , currentYear + 1  ## addition of present year -- automate this part! XXX
  #                      , sYears
  #                      , paste0 (qntl * 100, "%-ile") ## skip range or 2nd quantile
  #                      )
  #         , lty = c(1, 1, 1, sLty, 0), lwd = c (3, 4, 4, sLwd, 0)
  #         , pch = c(NA, NA, NA, rep (NA, length (sYears)), 22), pt.cex = 2
  #         , pt.bg = c (NA, NA, NA, rep (NA, length (sYears)), qantCol [1])
  #         , col = c(meanCol, cYcol, sLcol, qantCol [1])
  # )
  # }else{
  #   legend (..., bty = "n"
  #           , legend = c(paste0 ("mean [", mRange [1], "-", mRange [2], "]")
  #                        , currentYear
  #                        , sYears
  #                        , paste0 (qntl * 100, "%-ile") ## skip range or 2nd quantile
  #           )
  #           , lty = c(1, 1, sLty, 0), lwd = c (3, 4, 4, sLwd, 0)
  #           , pch = c(NA, NA, rep (NA, length (sYears)), 22), pt.cex = 2
  #           , pt.bg = c (NA, NA, rep (NA, length (sYears)), qantCol [1])
  #           , col = c(meanCol, cYcol, sLcol, qantCol [1])
  #   )
  #
  # }
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
    maxV <- df [,which (names (df) == paste0 ("maxMA_", vName))]
    minV <- df [,which (names (df) == paste0 ("minMA_", vName))]
  }else{
    longMean <- df [,which (names (df) == vName)]
    percL <- df [,which (names (df) == paste0 ("perL1_", vName))]
    percU <- df [,which (names (df) == paste0 ("perU1_", vName))]
    current <-df [,which (names (df) == paste0 ("pY_", vName))]
    cpo <- df [, which (names (df) == paste0 ("cY_", vName))]
    maxV <- df [,which (names (df) == paste0 ("max_", vName))]
    minV <- df [,which (names (df) == paste0 ("min_", vName))]
  }
  annualPlot (longMean, percL, percU
              , cbind (current, cpo) #, current, cpo
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

  require ("SWMPr")
  dMeans$MA <- unlist (smoother(dMeans$xVar, window = maO, sides = 1)) # sides = 2 for centered
  # dMeans$MA <- maT (dMeans$xVar, maO)


  # dMeans$XXX <- XX$varName [match (...)
  ## match ().... when length (varName) > 1

  tDay <- aggregate (xVar~jday, dMeans, FUN = sumFct, subset = year < currentYear)  # add ... for sumFct?
  tDay$sd <- aggregate (xVar~jday, dMeans, FUN = sd, na.rm = TRUE
                        , subset = year < currentYear)$xVar
  tDay$MA <- aggregate (MA~jday, dMeans, FUN = sumFct, subset = year < currentYear)$MA

  qN <- function (x, q, lQ = FALSE){
    quantile (x, probs = 0.5 + (lQ * -1)+ 0.5 * qntl, na.rm = TRUE)
  }

#  for (j in c (varName, MA)){
  for (i in 1:length (qntl)){
#    tDay$perL <- aggregate (xVar~jday, dMeans, FUN = qN, q = qntl [i])$xVar
    tDay$perL <- aggregate (xVar~jday, dMeans, FUN = quantile, probs = 0.5-0.5*qntl [i])$xVar
    tDay$perU <- aggregate (xVar~jday, dMeans, FUN = quantile, probs = 0.5+0.5*qntl [i])$xVar
    names (tDay)[which (names (tDay) == "perL")] <- paste0 ("perL", i)
    names (tDay)[which (names (tDay) == "perU")] <- paste0 ("perU", i)
    tDay$maL <- aggregate (MA~jday, dMeans, FUN = quantile, probs = 0.5-0.5*qntl [i])$MA
    tDay$maU <- aggregate (MA~jday, dMeans, FUN = quantile, probs = 0.5+0.5*qntl [i])$MA
    names (tDay)[which (names (tDay) == "maL")] <- paste0 ("maL", i)
    names (tDay)[which (names (tDay) == "maU")] <- paste0 ("maU", i)
  }
  tDay$max <- aggregate (xVar~jday, dMeans, FUN = max, na.rm = TRUE)$xVar
  tDay$min <- aggregate (xVar~jday, dMeans, FUN = min, na.rm = TRUE)$xVar
  tDay$maxMA <- aggregate (MA~jday, dMeans, FUN = max, na.rm = TRUE)$MA
  tDay$minMA <- aggregate (MA~jday, dMeans, FUN = min, na.rm = TRUE)$MA


  tDay$yearN <- aggregate ((!is.na (xVar))~jday, dMeans, FUN = sum, na.rm = TRUE)[,2]

  ## current year-1
  pY <- subset (dMeans, year == currentYear)
  tDay$pY <- pY$xVar [match (tDay$jday, pY$jday)]
  tDay$pYMA <- pY$MA [match (tDay$jday, pY$jday)]
  rm (pY)

  ## current year (incomplete)
  cY <- subset (dMeans, year == currentYear+1)
  tDay$cY <- cY$xVar [match (tDay$jday, cY$jday)]
  tDay$cYMA <- cY$MA [match (tDay$jday, cY$jday)]
  rm (cY)
  ## add circular spline?

  ## fix names
  names (tDay) <- gsub ("xVar", varName, names (tDay))
  names (tDay)[3:ncol (tDay)] <- paste0 (names (tDay)[3:ncol (tDay)], "_", varName)

  return (tDay)
}


addTimehelpers <- function (df){
  ## assumes "datetimestamp" is present
  df$jday <- as.numeric (strftime (df$datetimestamp, "%j"))
  df$year <- as.numeric (strftime (df$datetimestamp, "%Y"))
  require (lubridate)
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

#EOF