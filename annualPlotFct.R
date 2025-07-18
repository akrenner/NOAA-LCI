#!/usr/bin/env Rscript

## annualPlotFct.R
## functions for annual plots of long-term mean vs. current (previous) year

## submit to/use more SWMPr functions
## - qaqc
## - smoother


# pco <- as.integer (format (Sys.Date(), "%Y"))


## standardize colors
## is there a better way than doing it here?
rangCol <- c("lightyellow", "yellow")
rangCol <- c("gray", "darkgray")
rangCol <- c("gray", "darkgray")
# qantCol1 <- c("lightgray", NA)
qantCol <- c (gray (0.9), "gray")
meanCol <- "darkgray"

# year2 <- TRUE   ## switch 1 vs. 2 years on/off -- where best to put this switch?

# 365 <- 365 ## standardize a year to have 365 days for convenience

# if (!require("pacman")) install.packages("pacman"
#                                          , repos = "http://cran.fhcrc.org/", dependencies = TRUE)
# Require <- pacman::p_load




if (0) {
  ## using RColorBrewer:
  require ("RColorBrewer")
  bCol <- brewer.pal(12, "Paired")
  rangCol <- bCol [5:6]
  qantCol <- bCol [9]
  currentCol <- bCol [1:2]
  meanCol <- bCol [4]
}


# plot (1:12, pch=19, cex=3, col=brewer.pal (12, "Paired"))


plotSetup <- function(longMean, current, ylab = NULL # , xlim=c(5,355)
                      , ...) {
  par (xaxs = "i")
  plot (1:365
    , seq (min (c(longMean, current), na.rm = TRUE)
      , max (c(longMean, current), na.rm = TRUE), length.out = 365)
    , type = "n", axes = FALSE
    , xlim = c(5, 355)
    , xlab = ""
    , ylab = ylab
    , ...
  )
  axis (2)
  # axis (1, at=15+as.numeric (format (as.POSIXct (paste0 ("2019-", 1:12, "-1")), "%j"))
  #       , labels=month.abb, tick=FALSE) # center month-labels
  ## label only every second month
  axis (1, at = as.numeric (format (as.POSIXct (paste0 ("2019-", 1:6 * 2 - 1, "-15")), "%j"))
    , labels = month.abb[1:6 * 2 - 1], tick = FALSE) # center month-labels
  axis (1, at = c (as.numeric (format (as.POSIXct (paste0 ("2019-", 1:12, "-1"))
    , "%j")), 365), labels = FALSE) # add 365 to mark end of Dec
}


aPlot <- function(df, vName, MA = TRUE
                  , pastYear = TRUE
                  , ongoingYear = FALSE  ## good convention = ? 1,2=c-1,c, not og
                  , ...) {
  # wrapper for annualPlot
  # assumes df was created using prepDF, resulting in standardized field names
  # -- using MA or raw??
  if (MA) {
    longMean <- df [, which (names (df) == paste0 ("MA_", vName))]
    percL <- df [, which (names (df) == paste0 ("maL1_", vName))]
    percU <- df [, which (names (df) == paste0 ("maU1_", vName))]
    pcpo <- df [, which (names (df) == paste0 ("pcYMA_", vName))] # pre-current
    current <- df [, which (names (df) == paste0 ("pYMA_", vName))]
    ong <- df [, which (names (df) == paste0 ("ogYMA_", vName))]  # ongoing
    maxV <- df [, which (names (df) == paste0 ("maxMA_", vName))]
    minV <- df [, which (names (df) == paste0 ("minMA_", vName))]
    # allY <- df [,which (names (df) == paste0 ("y_")]
    # allY <- df [,grep ("^y_", names (df))]  ## XXX include paste0 ("^y_\d+4_", vName)
  } else { ## these need updates if ever used again!!
    longMean <- df [, which (names (df) == vName)]
    percL <- df [, which (names (df) == paste0 ("perL1_", vName))]
    percU <- df [, which (names (df) == paste0 ("perU1_", vName))]
    pcpo <- df [, which (names (df) == paste0 ("pcY_", vName))]
    current <- df [, which (names (df) == paste0 ("pY_", vName))]
    ong <- df [, which (names (df) == paste0 ("ogY_", vName))]
    maxV <- df [, which (names (df) == paste0 ("max_", vName))]
    minV <- df [, which (names (df) == paste0 ("min_", vName))]
  }
  annualPlot (longMean, percL, percU
    , current = cbind (pcpo, current, ong) # , current, cpo
    # , current=cbind (pcpo, current, cpo, pcpo) #, current, cpo
    , df$jday, maxV = maxV, minV = minV
    , pastYear = pastYear, ongoingYear = ongoingYear
    # add yearPick instead of pastYear, ongoingYear ?
    , ...)
}


annualPlot <- function(longMean, percL, percU, current  ## current may be a N x 3 matrix: past, current, ongoing
                       , jday, perc2L = NA, perc2U = NA, maxV = NA, minV = NA
                       , ylab = ""
                       , currentCol # = currentCol # "red"
                       , pastYear = TRUE, ongoingYear = FALSE
                       , ...) {
  plotSetup (longMean, current, ylab = ylab, ...)
  addGraphs (longMean, percL, percU, current, jday, perc2L = NA, perc2U = NA, maxV = NA, minV = NA
    , currentCol, pastYear = pastYear, ongoingYear = ongoingYear)
  # axis (1, at=c(1, 365), labels=NA) # redraw in case polygon went over axis
  return()
}


addGraphs <- function(longMean, percL, percU # means and upper/lower percentiles
                      , current  ## current may be a N x 2 matrix
                      , jday
                      , perc2L = NA, perc2U = NA # 2ndd envelope, e.g. max/min
                      , maxV = NA, minV = NA
                      , currentCol # = currentCol # "red"
                      , pastYear = TRUE, ongoingYear = FALSE, plotRange = TRUE
) {
  if (length (currentCol) != 3) {warning("currentCol should have three colors!\n\n")} # error or warning?

  if (0) {  # abandone range
    # if (!all (is.na (maxV)) && !all (is.na (minV))){
    pCo <- data.frame (x = c(jday, rev (jday))
      , y = c (maxV, rev (minV)))
    # polygon (pCo, col = "lightyellow", border = "yellow")
    polygon (pCo, col = rangCol [1], border = rangCol [2])
  }

  pCo <- data.frame (x = c (jday, rev (jday))
    , y = c(percL, rev (percU)))
  pCo <- subset (pCo, !is.na (pCo$y))
  if (0) {  # abandone 2nd quantile
    polygon (pCo, col = qantCol [1], border = NA)
    pCo <- data.frame (x = c (jday, rev (jday))
      , y = c(perc2L, rev (perc2U)))
    pCo <- subset (pCo, !is.na (pCo$y))
    polygon (pCo, col = qantCol [2], border = NA)
  } else {
    if (plotRange) {
      polygon (pCo, col = qantCol [1], border = NA)
    }
  }
  lines (longMean ~ jday, col = meanCol, lwd = 3)

  # if (length (currentCol) < 1){
  #   if (class (current) == "matrix"){current <- as.numeric (current [,1])}
  #   lines (current~jday, col=currentCol, lwd=4)
  # }else{
  if (pastYear) {
    lines (current [, 1] ~ jday, col = currentCol [1], lwd = 2) # , lty = "dashed") ## new: previous year
  }
  lines (current [, 2] ~ jday, col = currentCol [2], lwd = 4)
  if (ongoingYear) {
    lines (current [, 3] ~ jday, col = currentCol [3], lwd = 4)
  }
  axis (1, at = c(1, 365), labels = NA) # redraw in case polygon went over axis
  return()
}


cLegend <- function(..., mRange = NULL, currentYear = NULL
                    , cYcol # = currentCol
                    , qntl = NULL
                    , sYears = NULL, sLwd = NULL, sLty = NULL, sLcol = NULL # extra lines and text to include. Used in annual-snowpack.R
                    , pastYear = TRUE, ongoingYear = FALSE
) {
  ## combined legend for both box and line elements.
  ## see: http://tolstoy.newcastle.edu.au/R/e2/help/07/05/16777.html
  if (!all (length (sYears) == length (sLwd), length (sYears) == length (sLty), length (sYears) == length (sLcol))) {
    stop ("extra line length not consistent")
  }
  ySel <- which (c(pastYear, TRUE, ongoingYear))
  yT <- c (c(currentYear - 1, currentYear, currentYear + 1)[ySel], sYears)
  lT <- c (1, 0, c (1, 1, 1)[ySel], sLty)
  lW <- c (3, 0, c(2, 4, 4)[ySel], sLwd, 1) # last 4 or NA?
  pC <- c (NA, 22, rep (NA, length (yT)))
  bg <- c(NA, qantCol [1], rep (NA, length (yT)))
  lC <- c (meanCol, "black", cYcol [ySel], sLcol) # black=border of range
  rm (ySel)
  legend (..., bty = "n"
    , legend = c(paste0 ("mean [", mRange [1], "-", mRange [2], "]")
      , "10-90th percentile" # "10th-90th %ile"
      , yT
    )
    , lty = lT
    , lwd = lW
    , pch = pC
    , pt.cex = 2
    , pt.bg = bg
    , col = lC
  )
}





saggregate <- function(x, data, FUN, ..., refDF) { ## account for missing factors in df compared to tdf
  ## safer than aggregate
  # nA <- eval (substitute(aggregate (x, data, FUN, ..., simplify=TRUE, drop=FALSE))) # see https://stackoverflow.com/questions/68084640/how-to-properly-write-a-wrapper-for-lm-with-dots-only-error-3-used-in-an-i
  nA <- aggregate (x, data, FUN, ..., simplify = TRUE, drop = FALSE)  # above breaks prepDF below -- not sure why
  nA <- nA [match (refDF$jday, nA$jday), ]
  nA
}

seasonalMA <- function(var, jday, width = maO) {
  ## work in progress -- not yet functioning
  df <- cbind (var = rep (var, 3), jds = c(jday - 365, jday, jday + 365))
  suppressPackageStartupMessages(require ("zoo"))
  if (length (var) > 365) {
    df <- aggregate (var ~ jds + year, df, mean, na.rm = FALSE) # ## NAs are lost -- gap-fill?
  }
  dfAng <- data.frame (jds = seq (-365, 2 * 365))
  dfAng$var <- dfA$var [dfA$jds, dfAng$jds]
  sMA <- zoo::rollapply (dfAng$var, width = width, FUN = mean
    , fill = c(NA, NA, NA)
    , align = "center"
    , na.rm = FALSE # better to set false?? effect?
  )
  stop ("need to change rollapply to roll_meanr")
  sMAy <- subset (sMA, dfAng$jds %in% 1:365)
  sMAy
}



prepDF <- function(dat, varName, sumFct = function(x) {mean (x, na.rm = TRUE)}
                   , maO = 31
                   , currentYear # =as.integer (format (Sys.Date(), "%Y"))-1  ## force this to be stated explicitly (for troubleshooting)
                   , qntl = c(0.8, 0.9)
) {
  if (length (varName) > 1) {stop ("so far can only process one variable at a time")}

  if (!all (c ("datetimestamp", varName) %in% names (dat))) {
    stop (paste ("Date frame must contain the variable 'datetimestamp' and", varName))
  } else  if (!all (c("jday", "year") %in% names (dat))) {
    dat$jday <- as.numeric (format (as.POSIXct(dat$datetimestamp), "%j"))
    dat$year <- as.numeric (format (as.POSIXct(dat$datetimestamp), "%Y"))
  }
  # if (all (c("jday", "year", varName) %in% names (dat))){
  #   stop (paste ("Data frame must contain the variables jday, year, and", varName))
  # }

  ## flexible for varName to be a vector!!  -- XXX extra feature
  dat <- as.data.frame (dat) # error when using tibble/table
  dat$xVar <- dat [, which (names (dat) == varName)]

  suppressPackageStartupMessages(require ("zoo"))
  dat <- dat [order (dat$datetimestamp), ] # just to be sure

  ## necessary to use dMeans to pad missiing values as NA!
  dMeans <- aggregate (xVar ~ jday + year, dat, FUN = sumFct)
  dRef <- with (dMeans, data.frame(year = rep (min (year):max(year), each = 365)))
  dRef$jday <- rep (1:365, nrow (dRef) / 365)
  dRef$xVar <- dMeans$xVar [match (paste (dRef$year, dRef$jday, sep = "-")
    , paste (dMeans$year, dMeans$jday, sep = "-"))]  ## XXXX things break here!! XXX
  dMeans <- dRef; rm (dRef)
  dMeans$MA <- zoo::rollapply (dMeans$xVar, width = maO, partial = TRUE
    , align = "center"
    # , fill=c(NA,"extend",NA)
    , FUN = function(x) {
      if (maO - sum (is.na (x)) > maO / 4) {  ## allow up to 1/2 of window NA
        out <- sumFct(x)
      } else {
        out <- NA
      }
      return (out)
    }
  )  # tweak THIS one!
  ###########
  # dMeans$xVar <- na.approx(dMeans$xVar) #### XXXXX temporary fix X!!! XXXXXXXXXXXXXXXXXXXXX
  ############

  ## construct long-term climatology, using data excluding the present year
  dLTmean <- subset (dMeans, year < currentYear)  ## climatology excluding current year
  tDay <- aggregate (xVar ~ jday, dLTmean, FUN = mean, na.rm = TRUE)  # not sumFct here! it's a mean!
  tDay$sd <- saggregate (xVar ~ jday, data = dLTmean, FUN = stats::sd, na.rm = TRUE, refDF = tDay)$xVar
  tDay$MA <- saggregate (MA ~ jday, data = dLTmean, FUN = mean, na.rm = TRUE, refDF = tDay)$MA

  ## testing
  if (0) {
    pY <- subset (dMeans, year == currentYear)
    tDay$pY <- pY$xVar [match (tDay$jday, pY$jday)]
    tDay$pYMA <- pY$MA [match (tDay$jday, pY$jday)]
    plot (pY ~ jday, tDay)
    lines (pYMA ~ jday, tDay, lwd = 2, col = "blue")
  }



  #  for (j in c (varName, MA)){
  for (i in seq_along (qntl)) {
    tDay$perL <- saggregate (xVar ~ jday, dLTmean, FUN = quantile, probs = 0.5 - 0.5 * qntl [i], refDF = tDay)$xVar
    tDay$perU <- saggregate (xVar ~ jday, dLTmean, FUN = quantile, probs = 0.5 + 0.5 * qntl [i], refDF = tDay)$xVar
    names (tDay)[which (names (tDay) == "perL")] <- paste0 ("perL", i)
    names (tDay)[which (names (tDay) == "perU")] <- paste0 ("perU", i)
    tDay$maL <- saggregate (MA ~ jday, dLTmean, FUN = quantile, probs = 0.5 - 0.5 * qntl [i], refDF = tDay)$MA
    tDay$maU <- saggregate (MA ~ jday, dLTmean, FUN = quantile, probs = 0.5 + 0.5 * qntl [i], refDF = tDay)$MA
    names (tDay)[which (names (tDay) == "maL")] <- paste0 ("maL", i)
    names (tDay)[which (names (tDay) == "maU")] <- paste0 ("maU", i)
  }
  tDay$max <- saggregate (xVar ~ jday, dLTmean, FUN = max, na.rm = TRUE, refDF = tDay)$xVar
  tDay$min <- saggregate (xVar ~ jday, dLTmean, FUN = min, na.rm = TRUE, refDF = tDay)$xVar
  tDay$maxMA <- saggregate (MA ~ jday, dLTmean, FUN = max, na.rm = TRUE, refDF = tDay)$MA
  tDay$minMA <- saggregate (MA ~ jday, dLTmean, FUN = min, na.rm = TRUE, refDF = tDay)$MA

  # is this critically needed?? -- N years of data per date
  tDay$yearN <- saggregate ((!is.na (xVar)) ~ jday, data = dMeans, FUN = sum
    , na.rm = TRUE, refDF = tDay)[seq_len (nrow (tDay)), 2] # some scripts fail at 366

  ## previous year
  pcY <- subset (dMeans, year == (currentYear - 1))
  tDay$pcY <- pcY$xVar [match (tDay$jday, pcY$jday)]
  tDay$pcYMA <- pcY$MA [match (tDay$jday, pcY$jday)]
  rm (pcY)

  ## present/pretend/current year-1
  pY <- subset (dMeans, year == currentYear)
  tDay$pY <- pY$xVar [match (tDay$jday, pY$jday)]
  tDay$pYMA <- pY$MA [match (tDay$jday, pY$jday)]
  rm (pY)

  ## ongoing year (incomplete)
  ogY <- subset (dMeans, year == (currentYear + 1))
  tDay$ogY <- ogY$xVar [match (tDay$jday, ogY$jday)]
  tDay$ogYMA <- ogY$MA [match (tDay$jday, ogY$jday)]
  rm (ogY)

  ## all years
  dMeans$year <- factor (dMeans$year)
  yr <- sapply (seq_along(levels (dMeans$year)), function(x) {
    pY <- subset (dMeans, dMeans$year == levels (dMeans$year)[x])
    pY$MA [match (tDay$jday, pY$jday)]
  })
  colnames(yr) <- paste0 ("y_", levels (dMeans$year))
  tDay <- cbind (tDay, yr); rm (yr)


  ## fix names
  #  names (tDay) <- gsub ("xVar", paste0 (varName, "_"), names (tDay))
  names (tDay) <- gsub ("xVar", varName, names (tDay))
  names (tDay)[3:ncol (tDay)] <- paste0 (names (tDay)[3:ncol (tDay)], "_", varName)

  return (tDay)
}


# if (1){## instead of first MA and then aggregate that, first aggregate/CI, then MA. Better handle on NAs?
#   rm (prepDF)
#   source ("prepDF.R") # see whether this fixes artifacts
# }

addTimehelpers <- function(df) {
  ## assumes "datetimestamp" is present
  df$jday <- as.integer (strftime (df$datetimestamp, "%j"))
  df$year <- as.integer (strftime (df$datetimestamp, "%Y"))
  suppressPackageStartupMessages (require (lubridate))
  df$month <- month (df$datetimestamp)
  df$week <- week (df$datetimestamp)
  return (df)
}

fixGap <- function(df, intvl = 15 * 60) { # interval of TS in seconds
  ## fix time gaps in standard SWMP data file with NAs
  ## fix min sampling interval (60 s * 15) = standard throughout SWMP?
  # print (min (df$datetimestamp)); print (max (df$datetimestamp))
  # print (summary (df))
  tR <- seq (min (df$datetimestamp, na.rm = TRUE)
    , max (df$datetimestamp, na.rm = TRUE)
    , by = 15 * 60)
  nD <- data.frame (datetimestamp = tR
    , df [match (tR, df$datetimestamp), 2:ncol (df)])
  return (addTimehelpers (nD))
}


## add Fahrenheit scale to temperature plot
fAxis <- function(cGrad, side = 4, line = 2.5
                  , mT = "Temperature [°F]"  # expression('Temperature '~'['*degree~'F'*']')
                  , ...) {
  slope <- 9 / 5
  offset <- 32
  alt.ax <- pretty (slope * cGrad + offset)
  alt.at <- (alt.ax - offset) / slope
  axis (side = side, at = alt.at, labels = alt.ax, srt = 90)
  mtext (mT, side = side, line = line, ...)
}

## add scale in inches
iAxis <- function(mmL, side = 4, line = 3, lab = "", ...) {
  mmperinch <- 25.4
  alt.ax <- pretty (mmL / mmperinch)
  alt.at <- alt.ax * mmperinch
  axis (side = side, at = alt.at, labels = alt.ax, srt = 90)
  mtext (lab, side = side, line = line, ...)
}



## load cached SWMP data and update it with the latest from CDMO
getSWMP <- function(station = "kachdwq", QAQC = TRUE) {
  ## load SWMP data from local zip file. Update to the most current data
  ## from CDMO and cache those updates for the next run.
  ## need to specify location of zip file from SWMP and cache folder below
  ## an initial zip file from CDMO is required.
  ## It is recommended to update this zip file on occasion.
  require ("SWMPr")
  require ("R.utils")

  cacheFolder <- "~/tmp/LCI_noaa/cache/SWMP/"
  dir.create(cacheFolder, showWarnings = FALSE)
  cacheStation <- paste0 (cacheFolder, station, ".RData")

  zF <- list.files ("~/GISdata/LCI/SWMP", ".zip", full.names = TRUE)
  if (length (zF) < 1) {stop ("Need to download SWMP data from https://cdmo.baruch.sc.edu/get/landing.cfm")}
  SMPfile <- zF [which.max (file.info (zF)$ctime)] ## find most recent file
  rm (zF)

  ## delete cacheFolder if zip file is newer
  if (file.exists(cacheStation)) {
    if (file.info (cacheStation)$ctime < file.info (SMPfile)$ctime) {
      unlink (cacheStation)
    }
  }

  if (file.exists(cacheStation)) {
    base::load (cacheStation)
  } else {
    smp <- import_local(SMPfile, station)
    if (QAQC) {
      smp <- qaqc (smp)  ## scrutinize further? Wise to do here? Keep level 1?
    }
  }
  if (any (is.na (smp$datetimestamp))) {stop ("NAs in timestamp")}
  #  ## not sure where the bad line is coming from, but it has to go
  # smp <- smp [!is.na (smp$datetimestamp),]
  fN <- difftime(Sys.time(), max (smp$datetimestamp), units = "days")
  ## catch for stations that are inactive?
  if ((2 < fN) && (fN < 5 * 365.25)) { # skip downloads for less than 2 day and legacy stations
    # ## skip downloads for legacy stations
    smp2 <- try (all_params (station
      , Max = ceiling (as.numeric(fN) * 4 * 24))
    , silent = FALSE)  # XXX needs registered (static?) IP address. NCCOS VPN ok
    if (class (smp2)[1] != "try-error") {
      if (QAQC) {
        smp2 <- qaqc (smp2)
      }

      if (class (smp2)[1] == "swmpr") {
        ## remove bad lines
        if (any (is.na (smp2$datetimestamp))) {
          smp2 <- smp2 [!is.na (smp2$datetimestamp), ]
        }
        ## order of field names does not match between hmr2 and hmr
        ## re-assemble and remove duplicates
        smp3 <- smp2 [, sapply (seq_len (ncol (smp)), FUN = function(i) {
          which (names (smp)[i] == names (smp2))
        })]
        smp <- rbind (smp, smp3)
        if (any (is.na (smp$datetimestamp))) {stop ("NAs in timestamp")}

        rm (smp2, smp3, fN)
      }
      smp <- smp [which (!duplicated(smp$datetimestamp)), ]
    }
  }
  ## fixGap() here??
  ## smp <- qaqc (smp, qaqc_keep = "0") ## here??
  save (smp, file = cacheStation)
  return (smp)
}


# ## get ghcnd noaa weather using GSODR package
# ## not real-time -- most current > 6 month old -- deal breaker
# ## replacing noaaWeather.R
#   ## get weather data from Global Surface Summary of the Day (GSOD)
#   ## see https://github.com/ropensci/GSODR
#   ## see noaaWeather.R for unit conversions
# require ("GSODR")
# nearest_stations(LAT=59.6, LON=-151.5, distance=100)
# hmr <- get_GSOD(years=2012:(as.numeric(format (Sys.Date(), "%Y"))-1)
#                 , station="997176-99999" # Homer Spit
#                 )



getNOAAweather <- function(station = "HOMER AIRPORT", clearcache = FALSE, cacheF = FALSE, showsites = FALSE) {
  ## utilize worldmet::importNOAA adding caching function

  require ("worldmet")

  ## catch errors
  if (length (station) != 1L) stop ("Can only process one station at a time")

  ## cache data
  if (class (cacheF) == "character") {
    cacheFolder <- cacheF
    dir.create(cacheFolder, showWarnings = FALSE, recursive = TRUE)
  } else {
    cacheFolder <- tempdir()
  }

  if (clearcache) {
    unlink (cacheFolder, recursive = TRUE)
  }


  fetchMeta <- function() {
    wrldSites <- worldmet::getMeta (plot = FALSE, returnMap = FALSE) ## download everything? country="US", state="AK",
    saveRDS (wrldSites, paste0 (cacheFolder, "meta.rds"))
    wrldSites
  }


  ## load caches
  if (file.exists(paste0 (cacheFolder, "meta.rds"))) {
    wrldSites <- readRDS (paste0 (cacheFolder, "meta.rds"))
  } else {
    wrldSites <- fetchMeta()
  }
  station <- toupper(station)
  if (!station %in% wrldSites$station) {
    stop (paste ("Station", station, "not found in worldmet meta data."))
  }
  if (difftime (Sys.Date(), as.Date (wrldSites$end [match (station, wrldSites$station)])
    , units = "days") > 28) {
    wrldSites <- fetchMeta()
  }
  if (showsites) {
    AKpick <- wrldSites |>
      dplyr::filter (55 < latitude & latitude < 61) |>
      dplyr::filter (-154 < longitude & longitude < -148)
    # print (AKpick)
    cat ("Nearby stations:\n\n", paste (AKpick$station, collapse = "\n "))
  }
  stn <- wrldSites [match (station, wrldSites$station), ]


  ## cache inventory
  cYears <- list.files(cacheFolder, pattern = stn$code) |>
    substr (start = 14, 17) |>
    as.numeric()

  ## set-up years to fetch
  yR <- as.numeric (format (stn$begin, "%Y")):
  as.numeric (format (stn$end, "%Y"))                 # all available years
  # yR <- 2022:as.numeric (format (Sys.Date(), "%Y"))   # minimal for testing
  ## revise yR, use as many cached files, as possible
  yR <- yR [which (!yR %in% cYears)]
  yR <- unique (c (yR, as.numeric (format (Sys.Date(), "%Y")))) # always fetch last year again

  weather <- importNOAA (code = stn$code
    , year = yR
    , hourly = TRUE
    , path = cacheFolder
  )


  cWeather <- lapply (list.files (cacheFolder, pattern = stn$code), function(i) { # relist, in case of missing data
    readRDS (paste0 (cacheFolder, i))
  }) |>   ## all years are cached and read again -- no need to combine them
    dplyr::bind_rows() |>
    dplyr::select (!year)

  cWeather

  # cWeather <- lapply (list.files (cacheFolder, pattern=stn$code), function (i){ # relist, in case of missing data
  #   readRDS (paste0 (cacheFolder, i))
  # })
  # # cWeather <- do.call ("rbind", cWeather)
  # # weather <- unique (rbind (cWeather, weather))
  # weatherO <- do.call ("rbind", cWeather) |>
  #   dplyr::select (!(year))
  # #   rbind (weather) |>
  # #   dplyr::distinct()
  # weatherO

  # hmr <- importNOAA (code="703410-25507" # PAHO, Homer airport
  #                    , year=2000:cYear  ## starts in 1973
  #                    , hourly=FALSE
  #                    , path=cacheFolder)
  # hsp <- importNOAA (code="997176-99999"
  #                    , hourly=FALSE
  #                    , year=2012:cYear
  #                    , cacheFolder)
}


gNOAAS <- function(station, clearcache, cacheF = FALSE, showsites = FALSE) {
  NWeather2SWMP <- function(dat) {  ## convert to SWMP format
    fixF <- function(field) {
      if (field %in% names (dat)) {
        out <- dat [[field]]
      } else {
        out <- rep (NA, nrow (dat))
      }
    }
    with (dat, data.frame(
      datetimestamp = date
      , jday = as.numeric (format (date, "%j"))
      , year = as.numeric (format (date, "%Y"))
      , atemp = air_temp
      , rh = RH
      , bp = rep (NA, nrow (dat))
      , wspd = ws  # XXXXX conversion to wind speed to m/s??
      , maxwspd = fixF ("peak_wind_gust") # rep (NA, nrow (dat))
      , wdir = wd
      , sdwdir = rep (NA, nrow (dat))
      , totpar = fixF ("precip_6") # total precipitation in 6 hours
      , toprcp = rep (NA, nrow (dat))  ## probability of precipitation
      , totsorad = rep (NA, nrow (dat))
    ))
  }

  getNOAAweather(station = station, clearcache = clearcache, cacheF = cacheF, showsites = showsites) |>
    NWeather2SWMP()
}



getNOAAweather_airports <- function(stationID = "PAHO", clearcache = FALSE) {
  ## get data from mesonet.argon.iastate.edu as recommended by Brian Brettschneider
  ## using package riem
  ## data from University of Iowa. Downside: not covering every NOAA station (has Homer airport, but not spit or Flat Island)
  ##  riem_measures (station="VOHY", date_start="2014-01-01", date_end=as.character (Sys.Date()))
  # rN <- riem_networks()  # AK_ASOS  Alaska ASOS
  # rS <- riem_stations(network="AK_ASOS")
  # rS [which (rS$county %in% "Kenai Peninsula"),]

  ## require (pmetar): another package to access data from Iowa State University

  ## better?  fetch data directly from NOAA buoy network using wget
  ## see https://medium.com/@holtan.chase/retrieving-data-from-national-data-buoy-center-api-f94d262c7ea7
  ## https://www.ndbc.noaa.gov/faq/rt_data_access.shtml  -- at least some archived data available



  require ("riem")
  if (clearcache) {
    unlink (paste0 ("~/tmp/LCI_noaa/cache/noaaWeather/", stationID, ".RData"))
  }
  dir.create ("~/tmp/LCI_noaa/cache/noaaWeather", showWarnings = FALSE, recursive = TRUE)
  if (file.exists(paste0 ("~/tmp/LCI_noaa/cache/noaaWeather/", stationID, ".RData"))) {
    load (paste0 ("~/tmp/LCI_noaa/cache/noaaWeather/", stationID, ".RData"))
    lastD <- max (rW$valid) # valid = date-time
    ## fudge to get 1 year overlap
    lastD <- as.Date (paste0 (as.integer (substr(as.character(lastD), start = 1, stop = 4)) - 1, "-01-01"))
  } else {
    lastD <- "2000-01-01"
  }
  rWn <- try (riem_measures (station = stationID, date_start = lastD, date_end = as.character(Sys.Date()))
    , silent = TRUE)

  if (class (rWn)[1] == "try-error") {
    if (!exists ("rW")) {
      stop ("Have to be online for initial run fetching NOAA weather data.")
    }
  } else {
    if (!exists ("rW")) {
      rW <- rWn
    } else {
      ## remove overlapping data
      rW <- subset (rW, valid < lastD)
      rW <- rbind (rW, rWn)
    }
  }
  rm (rWn)
  save (rW, file = paste0 ("~/tmp/LCI_noaa/cache/noaaWeather/", stationID, ".RData"))

  ## clean-up data/convert units, as appropriate, for compatibility with getNOAA


  rW
}



## if missing, install buoydata
if (!require ("buoydata")) {
  renv::install ("NOAA-EDAB/buoydata", prompt = FALSE)
}


getNOAA <- function(buoyID = "46108", set = "stdmet", clearcache = FALSE) {  # default=kachemak bay wavebuoy
  ## rnoaa alternatives:
  ## buoydata: download from NDBC -- realtime?
  ## THREDDS server. Or wget from http site
  ## GSODR: global surface summary of day stations, similar to rnoaa

  ## NOAA seems to recommend THREDDS -- attempts:
  if (0) {
    library(ncdf4)
    goes.nc = nc_open("http://basin.ceoe.udel.edu/thredds/dodsC/GOESJPLSST.nc")
    goes.nc

    goes.nc = nc_open("https://www.ncei.noaa.gov/thredds-ocean/dodsC/ndbc/cmanwx/2025/03/NDBC_WPOW1_202503_D8_v00.nc.html")
    goes.nc = nc_open("https://www.ncei.noaa.gov/thredds-ocean/fileServer/ndbc/cmanwx/2025/03/NDBC_WPOW1_202503_D8_v00.nc")

    goes.nc = nc_open ("~/NDBC_WPOW1_202503_D8_v00.nc")

    goes.nc
    nc_close(goes.nc)
  }

  buoyID <- tolower(buoyID)
  cacheF <- paste0 ("~/tmp/LCI_noaa/cache/noaaBuoy/", buoyID, ".RData")

  if (clearcache) {
    unlink (paste0 ("~/tmp/LCI_noaa/cache/noaaBuoy/", buoyID), recursive = TRUE)
    unlink (cacheF)
    # unlink ("~/tmp/LCI_noaa/cache/noaaBuoy/", recursive=TRUE)
    # dir.create("~/tmp/LCI_noaa/cache/noaaBuoy/", showWarnings=FALSE, recursive=TRUE)
  }


  require ("buoydata")  # install with remotes::install_github("NOAA-EDAB/buoydata")
  if (file.exists(cacheF)) {
    load (cacheF)
    startY <- max (wDB$datetimestamp) |>
      format ("%Y") |>
      as.numeric() - 1
  } else {
    startY <- buoydata::buoyDataWorld |>
      dplyr::filter(ID == buoyID) |>
      dplyr::select(Y1) |>
      as.numeric()
  }
  buoydata::get_buoy_data(buoyid = buoyID,
    year = startY:as.integer(format (Sys.Date(), "%Y"))
    , outDir = "~/tmp/LCI_noaa/cache/noaaWeather")
  # wB <- combine_buoy_data(buoyID, variable="WVHT", inDir="~/tmp/LCI_noaa/cache/noaaWeather/")
  wB <- list.files(path = paste0 ("~/tmp/LCI_noaa/cache/noaaWeather/",  ## should only read new files XXX
    buoyID, "/"), patter = "\\.csv$",
  full.names = TRUE) |>
    readr::read_csv(id = "file_name", col_names = TRUE, comment = "#", na = "999")

  wB$datetimestamp <- with (wB, as.POSIXct(paste0 (X.YY, "-", MM, "-", DD, " "
    , hh, ":", mm), tz = "UTC"))


  if (exists ("wDB")) {
    wDB <- rbind (wDB, wB)
  } else {
    wDB <- wB
  }
  rm (wB)
  save (wDB, file = cacheF)


  ## add real-time data -- check in buoydata; already fixed?
  ## using erddap -- haven't figured this out
  if (0) {
    require ("rerddap") ## another rnoaa alternative?? only for gridded data?
    url <- 'https://coastwatch.pfeg.noaa.gov/erddap/'
    # find all gridded datasets
    griddedDatasets <- rerddap::ed_datasets(url = url, which = "tabledap")
    # select chl daily 2km
    buoy <- griddedDatasets |>
      dplyr::filter(Dataset.ID == "cwwcNDBCMet")

    # get info about dataset
    info <- rerddap::info(buoy$Dataset.ID, url = url)
    info


    if (clearcache) {
      rerddap::cache_delete_all()
    }
    cD <- rerddap::griddap(info, latitude = c(54, 60)
      , longitude = c(-159, -150)
      , fiels = c("wtmp", "wvht"))


    ## straight from source -- AOOS erddap server for lower cook inlet wave buoy
    test <- rerddap::tabledap(info, fields = c("time", "atmp"), url = url)
    #                                "https://erddap.aoos.org/erddap/tabledap/aoos_204.html", fmt="csv")

    # info (url="https://erddap.aoos.org/erddap/tabledap/aoos_204.html") ## missing datasetid





    ## MR search
    noaaS <- servers() |>
      filter(grepl ("NOAA", name)) |>
      as.data.frame()

    for (i in seq_len (nrow (noaaS))) {
      cat ("\n\n", i, noaaS$short_name [i], "\n")
      print (try (ed_search (query = "buoy", url = noaaS$url [i])))
    }


    out <- ed_search (query = c("aoos"), which = 'table')


    ## try IOOS Sensors ERDDAP
    url <- "https://erddap.sensors.ioos.us/erddap/"
    datasets <- rerddap::ed_datasets(url = url, which = "tabledap")
    datasets <- rerddap::ed_search(query = "buoy", url = url)

    datasets$info$title
  }


  ## add real-time data -- manual from http
  ## from https://www.ndbc.noaa.gov/download_data.php?filename=4610812025.txt.gz&dir=data/adcp/Jan/

  # tdy <- as.POSIXct("2025-05-18")
  tdy <- Sys.Date()
  ## set-up file structure
  cMon <- month.abb [1:(as.numeric (format (tdy, "%m")) - 1)]
  ## copy output of fwf_empty(noaaexamplefile.txt), as   clns <- fwf_empty("~/Desktop/4610812025.txt", skip=2)
  clns <- list (begin = c(0L, 5L, 8L, 11L, 14L, 17L, 21L, 26L, 32L, 38L, 44L, 49L, 53L,
    60L, 68L, 72L, 78L, 83L),
  end = c(4L, 7L, 10L, 13L, 16L, 20L, 25L, 30L, 36L, 42L, 48L, 52L, 59L,
    65L, 71L, 77L, 82L, NA)
  , colNames = colnames(wDB)[2:ncol (wDB)]
  # colNnames=c("YY", "DD", "hh", "mm", "WDIR", "WSPD", "GST", "WVHT", "DPD",
  #             "APD", "MWD", "PRES", "ATMP", "WTMP", "DEWP", "VIS", "TIDE")
  )
  rtB <- lapply (seq_along(cMon), function(i) {
    ## form of https://www.ndbc.noaa.gov/data/adcp/Jan/4610812025.txt.gz
    ## https://www.ndbc.noaa.gov/data/stdmet/Jan/4610812025.txt.gz
    nD <- suppressWarnings (try (readr::read_fwf(file = paste0 ("https://www.ndbc.noaa.gov/data/stdmet/",
      cMon[i], "/", buoyID, i
      , format (tdy, "%Y"), ".txt.gz")
    , col_positions = clns, skip = 2 # , na=999.0
    , id = "file_name"), silent = TRUE))
    if (class (nD)[1] == "try-error") { # try again for last available month
      nD <- try (readr::read_fwf(file = paste0 ("https://www.ndbc.noaa.gov/data/stdmet/",
        cMon[i], "/", buoyID, ".txt")
      , col_positions = clns, skip = 2 # , na=999.0
      , id = "file_name"), silent = TRUE)
    }
    if (class (nD)[1] == "try-error") {nD <- wDB [0, ]}
    nD
  })
  # https://erddap.aoos.org/erddap/tabledap/aoos_204.csv?time%2Csea_surface_wave_significant_height%2Csea_surface_wave_from_direction%2Csea_surface_wave_significant_height_qc_agg%2Csea_surface_wave_from_direction_qc_agg%2Cz&time%3E%3D2025-05-31T08%3A00%3A00Z&time%3C%3D2025-06-10T08%3A00%3A00Z
  # rta <- read.csv ("https://erddap.aoos.org/erddap/tabledap/aoos_204.csv?time%2Csea_surface_wave_significant_height%2Csea_surface_wave_from_direction%2Csea_surface_wave_significant_height_qc_agg%2Csea_surface_wave_from_direction_qc_agg%2Cz&time%3E%3D2025-05-31T07%3A30%3A00Z&time%3C%3D2025-06-10T07%3A30%3A00Z")
  rtB <- do.call("rbind", rtB)

  ## add the last 45 days of "real time" data
  ## example:  https://www.ndbc.noaa.gov/data/realtime2/46108.txt
  nD <- try (readr::read_fwf(file = paste0 ("https://www.ndbc.noaa.gov/data/realtime2/"
    , topupper (buoyID), ".txt")
  , col_positions = clns, skip = 2 # , na=999.0
  , id = "file_name"), silent = TRUE)
  if (class (nD)[1] != "try-error") {
    rtB <- rbind (rtB, nD)
  }

  colnames(rtB) <- colnames (wDB)
  rtB$datetimestamp <- with (rtB, as.POSIXct(paste0 (X.YY, "-", MM, "-", DD, " "
    , hh, ":", mm), tz = "UTC"))
  wDB <- rbind (wDB, rtB); rm (rtB)


  # ## QAQC
  wDB <- wDB [!duplicated(wDB$datetimestamp), ]
  # tm <- gsub ("T", " ", wDB$datetimestamp)
  # tm <- gsub ("Z", "", tm)
  # wDB$datetimestamp <- as.POSIXct (tm, format = "%F %T", tz = "UTC") # move this up?
  # rm (tm)

  # for (i in 1:length (meta)){  ## meta is a tibble...
  #   mN <- which (names (wDB) == names (meta [i]))
  #   is.na (wDB [,mN])[which (wDB [,mN] == meta [[i]]$missval)] <- TRUE  # set missing values to NA
  # }
  # ## ensure windspeed is m/s
  # if (meta$wind_spd$units != "meters/second"){cat (meta$wind_spd$units); stop ("Fix wspd units")}
  #

  return (wDB)
}

gNOAAbuoy <- function(buoyID, clearcache = FALSE) {
  ## wrapper for getNOAA to fix units and field names to fit SWAMP data
  dat <- getNOAA (buoyID = buoyID, clearcache = clearcache)
  out <- with (dat, data.frame(
    datetimestamp = datetimestamp
    , jday = as.numeric (format (datetimestamp, "%j"))
    , year = as.numeric (format (datetimestamp, "%Y"))
    , atemp = air_temp
    , rh = rep (NA, nrow(dat))  # no relative humidity in buoy data
    , bp = rep (NA, nrow(dat))  # no barometric pressure in buoy data
    , wspd = wspd * 0.5144444444  # convert knots to m/s
    , maxwspd = rep (NA, nrow(dat))  # no peak wind gust in buoy data
    , wdir = wd
    , sdwdir = rep (NA, nrow(dat))
    , totpar = rep (NA, nrow(dat))  # no total PAR in buoy data
    , totprcp = rep (NA, nrow(dat))  # no total precipitation in buoy data
    , totsorad = rep (NA, nrow(dat))  # no total solar radiation in buoy data
  ))
  return (out)
}



if (0) {
  ## wrap with around/append to above functions!
  hmr <- with (nAir, data.frame (datetimestamp = valid
    , jday = as.numeric (format (valid, "%j"))
    , year = as.numeric (format (valid, "%Y"))
    , atemp = (tmpf - 32) * 5 / 9
    , rh = relh
    , bp = rep (is.na (nrow (nAir)))
    , wspd = sknt * 0.5144444444 # convert knots to m/s
    , maxwspd = peak_wind_gust * 0.5144444444
    , wdir = drct # peak_wind_drct
    # , wdir=drct
    , sdwdir = rep (is.na (nrow (nAir)))
    , totpar = rep (is.na (nrow (nAir)))
    , totprcp = p01i * 25.4 ## need to check on units -- inches -> mm XXX
    , totsorad = rep (is.na (nrow (nAir)))
  ))
  rm (nAir)
}






nEvents <- function(dat, varName, thrht) {
  ## number of days where varName exeeds threshold thrht
  ## compare current N days to long term mean and quantiles
  ## use for wind (storms and gales) and surf and others
  vN <- which (names (dat) == varName)
  names (dat)[vN] <- "xvar"  # for convenience
  eventL <- sapply (seq_along(thrht)
    , function(i) {
      agY <- aggregate (xvar ~ year
        , data = aggregate (xvar ~ jday + year
          , data = dat
          , FUN = function(x) {
            any (x > thrht [i])}
        )
        , FUN = sum)

      c (mean = mean (agY$xvar, na.rm = TRUE)
        , median = median(agY$xvar, na.rm = TRUE)
        , lowerQ = quantile (agY$xvar, 0.1, na.rm = TRUE)
        , upperQ = quantile (agY$xvar, 0.9, na.rm = TRUE)
        , agY$xvar [(nrow (agY) - 2):(nrow (agY) - 0)]) # the current and previous year
    })
  colnames(eventL) <- paste0 ("T", thrht)
  rownames(eventL)[5:7] <- paste0 ("Y", max (dat$year) - c(2, 1, 0))
  as.data.frame (t (eventL))
}


cDir <- function(wd, nDir = 8) {
  ## classify direction into cardinal categories
  ## following https://community.rstudio.com/t/convert-wind-direction-degrees-into-factors-in-a-data-frame/14636/4
  if (!(nDir %in% c(4, 8, 16))) {
    stop ("nDir has to be 4, 8, or 16")
  }
  if (nDir == 4) {
    rose_breaks <- c (0, 360 / 8, (1 / 8 + (1:3 / 4)) * 360, 360)
    rose_labs <- c("N", "E", "S", "W", "N")
  } else if (nDir == 8) {
    rose_breaks <- c (0, 360 / 16, (1 / 16 + (1:7 / 8)) * 360, 360)
    rose_labs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
  } else {
    rose_breaks <- c(0, 360 / 32, (1 / 32 + (1:15 / 16)) * 360, 360)
    rose_labs <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
      "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
  }
  wd <- ifelse (wd < 0, 360 + wd, wd)
  cut (wd, breaks = rose_breaks, labels = rose_labs
    , right = FALSE, include.lowest = TRUE)
}




## combine several PNGs into one PDF
merge.png.pdf <- function(pdfFile, pngFiles, deletePngFiles = FALSE) {
  ## taken from https://jonkimanalyze.wordpress.com/2014/07/24/r-compile-png-files-into-pdf/
  #### Package Install ####
  gridExists <- require ("grid")
  if (!gridExists) {
    install.packages ("grid")
    library ("grid")
  }
  pngPackageExists <- require ("png")
  if (!pngPackageExists) {
    install.packages ("png")
    library ("png")
  }
  #########################

  pdf(pdfFile) ## could/should load aspect ratio of PNGs?
  par (mar = rep (0.2, 4))
  n <- length(pngFiles)
  for (i in 1:n) {
    pngFile <- pngFiles[i]
    pngRaster <- readPNG(pngFile)
    grid.raster(pngRaster, width = unit(0.8, "npc"), height = unit(0.8, "npc"))
    #    grid.raster(pngRaster, width=unit(5.5, "inches"), height= unit(5.5, "inches"))
    if (i < n) plot.new()
  }
  dev.off()
  if (deletePngFiles) {
    unlink(pngFiles)
  }
}



# EOF
