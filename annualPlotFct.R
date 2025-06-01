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




if (0){
  ## using RColorBrewer:
  require ("RColorBrewer")
  bCol <- brewer.pal(12, "Paired")
  rangCol <- bCol [5:6]
  qantCol <- bCol [9]
  currentCol <- bCol [1:2]
  meanCol <- bCol [4]
}


# plot (1:12, pch=19, cex=3, col=brewer.pal (12, "Paired"))


plotSetup <- function(longMean, current, ylab=NULL# , xlim=c(5,355)
                      , ...){
  par (xaxs="i")
  plot (1:365
        , seq (min (c(longMean, current), na.rm=TRUE)
               , max (c(longMean, current), na.rm=TRUE), length.out=365)
        , type = "n", axes=FALSE
        , xlim=c(5,355)
        , xlab = ""
        , ylab=ylab
        , ...
  )
  axis (2)
  # axis (1, at=15+as.numeric (format (as.POSIXct (paste0 ("2019-", 1:12, "-1")), "%j"))
  #       , labels=month.abb, tick=FALSE) # center month-labels
  ## label only every second month
  axis (1, at=as.numeric (format (as.POSIXct (paste0 ("2019-", 1:6*2-1, "-15")), "%j"))
        , labels=month.abb[1:6*2-1], tick=FALSE) # center month-labels
  axis (1, at=c (as.numeric (format (as.POSIXct (paste0 ("2019-", 1:12, "-1"))
                                     , "%j")), 365), labels=FALSE) # add 365 to mark end of Dec
}


aPlot <- function (df, vName, MA=TRUE
                   , pastYear=TRUE
                   , ongoingYear=FALSE  ## good convention = ? 1,2=c-1,c, not og
                   , ...){
  # wrapper for annualPlot
  # assumes df was created using prepDF, resulting in standardized field names
  # -- using MA or raw??
  if (MA){
    longMean <- df [,which (names (df) == paste0 ("MA_", vName))]
    percL <- df [,which (names (df) == paste0 ("maL1_", vName))]
    percU <- df [,which (names (df) == paste0 ("maU1_", vName))]
    pcpo <- df [, which (names (df) == paste0 ("pcYMA_", vName))] # pre-current
    current <-df [,which (names (df) == paste0 ("pYMA_", vName))]
    ong <- df [, which (names (df) == paste0 ("ogYMA_", vName))]  # ongoing
    maxV <- df [,which (names (df) == paste0 ("maxMA_", vName))]
    minV <- df [,which (names (df) == paste0 ("minMA_", vName))]
    #allY <- df [,which (names (df) == paste0 ("y_")]
    allY <- df [,grep ("^y_", names (df))]  ## XXX include paste0 ("^y_\d+4_", vName)
  }else{ ## these need updates if ever used again!!
    longMean <- df [,which (names (df) == vName)]
    percL <- df [,which (names (df) == paste0 ("perL1_", vName))]
    percU <- df [,which (names (df) == paste0 ("perU1_", vName))]
    pcpo <- df [, which (names (df) == paste0 ("pcY_", vName))]
    current <-df [,which (names (df) == paste0 ("pY_", vName))]
    ong <- df [, which (names (df) == paste0 ("ogY_", vName))]
    maxV <- df [,which (names (df) == paste0 ("max_", vName))]
    minV <- df [,which (names (df) == paste0 ("min_", vName))]
  }
  annualPlot (longMean, percL, percU
              , current=cbind (pcpo, current, ong) #, current, cpo
              # , current=cbind (pcpo, current, cpo, pcpo) #, current, cpo
              , df$jday, maxV=maxV, minV=minV
              , pastYear=pastYear, ongoingYear=ongoingYear
              # add yearPick instead of pastYear, ongoingYear ?
              , ...)
}


annualPlot <- function (longMean, percL, percU, current  ## current may be a N x 3 matrix: past, current, ongoing
                        , jday , perc2L=NA, perc2U=NA, maxV=NA, minV=NA
                        , ylab = ""
                        , currentCol # = currentCol # "red"
                        , pastYear=TRUE, ongoingYear=FALSE
                        , ...){
  plotSetup (longMean, current, ylab=ylab, ...)
  addGraphs (longMean, percL, percU, current, jday, perc2L=NA, perc2U=NA, maxV=NA, minV=NA
             , currentCol, pastYear=pastYear, ongoingYear=ongoingYear)
  # axis (1, at=c(1, 365), labels=NA) # redraw in case polygon went over axis
  return()
}


addGraphs <- function (longMean, percL, percU # means and upper/lower percentiles
                       , current  ## current may be a N x 2 matrix
                       , jday
                       , perc2L=NA, perc2U=NA # 2ndd envelope, e.g. max/min
                       , maxV=NA, minV=NA
                       , currentCol # = currentCol # "red"
                       , pastYear=TRUE, ongoingYear=FALSE, plotRange=TRUE
){
  if (length (currentCol) != 3){warning("currentCol should have three colors!\n\n")} # error or warning?

  if (0){  # abandone range
    # if (!all (is.na (maxV)) && !all (is.na (minV))){
    pCo <- data.frame (x=c(jday, rev (jday))
                       , y=c (maxV, rev (minV)))
    # polygon (pCo, col = "lightyellow", border = "yellow")
    polygon (pCo, col=rangCol [1], border=rangCol [2])
  }

  pCo <- data.frame (x=c (jday, rev (jday))
                     , y=c(percL, rev (percU)))
  pCo <- subset (pCo, !is.na (pCo$y))
  if (0){  # abandone 2nd quantile
    polygon (pCo, col=qantCol [1], border=NA)
    pCo <- data.frame (x=c (jday, rev (jday))
                       , y=c(perc2L, rev (perc2U)))
    pCo <- subset (pCo, !is.na (y))
    polygon (pCo, col=qantCol [2], border=NA)
  }else{
    if (plotRange){
      polygon (pCo, col=qantCol [1], border=NA)
    }
  }
  lines (longMean~jday, col=meanCol, lwd=3)

  # if (length (currentCol) < 1){
  #   if (class (current) == "matrix"){current <- as.numeric (current [,1])}
  #   lines (current~jday, col=currentCol, lwd=4)
  # }else{
  if (pastYear){
    lines (current [,1]~jday, col=currentCol [1], lwd=2) #, lty = "dashed") ## new: previous year
  }
  lines (current [,2]~jday, col=currentCol [2], lwd=4)
  if (ongoingYear){
    lines (current [,3]~jday, col=currentCol [3], lwd=4)
  }
  axis (1, at=c(1, 365), labels=NA) # redraw in case polygon went over axis
  return()
}


cLegend <- function (..., mRange=NULL, currentYear=NULL
                     , cYcol # = currentCol
                     , qntl=NULL
                     , sYears=NULL, sLwd=NULL, sLty=NULL, sLcol=NULL # extra lines and text to include. Used in annual-snowpack.R
                     , pastYear=TRUE, ongoingYear=FALSE
){
  ## combined legend for both box and line elements.
  ## see: http://tolstoy.newcastle.edu.au/R/e2/help/07/05/16777.html
  if (!all (length (sYears) == length (sLwd), length (sYears) == length (sLty), length (sYears) == length (sLcol))){
    stop ("extra line length not consistent")
  }
  ySel <- which (c(pastYear, TRUE, ongoingYear))
  yT <- c (c(currentYear-1, currentYear, currentYear+1)[ySel], sYears)
  lT <- c (1, 0, c (1,1,1)[ySel], sLty)
  lW <- c (3, 0, c(2,4,4)[ySel], sLwd, 1) # last 4 or NA?
  pC <- c (NA, 22, rep (NA, length (yT)))
  bg <- c(NA, qantCol [1], rep (NA, length (yT)))
  lC <- c (meanCol, "black", cYcol [ySel], sLcol) # black=border of range
  rm (ySel)
  legend (..., bty = "n"
          , legend=c(paste0 ("mean [", mRange [1], "-", mRange [2], "]")
                     , "10-90th percentile" #"10th-90th %ile"
                     , yT
          )
          , lty=lT
          , lwd=lW
          , pch=pC
          , pt.cex=2
          , pt.bg=bg
          , col=lC
  )
}





saggregate <- function (x, data, FUN, ..., refDF){ ## account for missing factors in df compared to tdf
  ## safer than aggregate
  # nA <- eval (substitute(aggregate (x, data, FUN, ..., simplify=TRUE, drop=FALSE))) # see https://stackoverflow.com/questions/68084640/how-to-properly-write-a-wrapper-for-lm-with-dots-only-error-3-used-in-an-i
  nA <- aggregate (x, data, FUN, ..., simplify=TRUE, drop=FALSE)  # above breaks prepDF below -- not sure why
  nA <- nA [match (refDF$jday, nA$jday),]
  nA
}

seasonalMA <- function (var, jday, width=maO){
  ## work in progress -- not yet functioning
  df <- cbind (var=rep (var, 3), jds=c(jday-365, jday, jday+365))
  suppressPackageStartupMessages(require ("zoo"))
  if (length (var) > 365){
    df <- aggregate (var~jds+year, df, mean, na.rm=FALSE) # ## NAs are lost -- gap-fill?
  }
  dfAng <- data.frame (jds=seq (-365, 2*365))
  dfAng$var <- dfA$var [dfA$jds, dfAng$jds]
  sMA <- zoo::rollapply (dfAng$var, width=width, FUN=mean
                         , fill=c(NA, NA, NA)
                         , align="center"
                         , na.rm=FALSE # better to set false?? effect?
  )
  stop ("need to change rollapply to roll_meanr")
  sMAy <- subset (sMA, dfAng$jds%in%1:365)
  sMay
}



prepDF <- function (dat, varName, sumFct=function (x){mean (x, na.rm=TRUE)}
                    , maO=31
                    , currentYear # =as.integer (format (Sys.Date(), "%Y"))-1  ## force this to be stated explicitly (for troubleshooting)
                    , qntl=c(0.8, 0.9)
){
  if (length (varName) > 1){stop ("so far can only process one variable at a time")}

  if (! all (c ("datetimestamp", varName) %in% names (dat))){
    stop (paste ("Date frame must contain the variable 'datetimestamp' and", varName))
  }else  if (! all (c("jday", "year") %in% names (dat))){
    dat$jday <- as.numeric (format (as.POSIXct(dat$datetimestamp), "%j"))
    dat$year <- as.numeric (format (as.POSIXct(dat$datetimestamp), "%Y"))
  }
  # if (all (c("jday", "year", varName) %in% names (dat))){
  #   stop (paste ("Data frame must contain the variables jday, year, and", varName))
  # }

  ## flexible for varName to be a vector!!  -- XXX extra feature
  dat$xVar <- dat [,which (names (dat) == varName)]

  suppressPackageStartupMessages(require ("zoo"))
  dat <- dat [order (dat$datetimestamp),] # just to be sure

  ## necessary to use dMeans to pad missiing values as NA!
  dMeans <- aggregate (xVar~jday+year, dat, FUN=sumFct)
  dRef <- with (dMeans, data.frame(year=rep (min (year):max(year), each=365)))
  dRef$jday <- rep (1:365, nrow (dRef)/365)
  dRef$xVar <- dMeans$xVar [match (paste (dRef$year, dRef$jday, sep="-")
                                   , paste (dMeans$year, dMeans$jday, sep="-"))]  ## XXXX things break here!! XXX
  dMeans <- dRef; rm (dRef)
  dMeans$MA <- zoo::rollapply (dMeans$xVar, width=maO, partial=TRUE
                               , align="center"
                               # , fill=c(NA,"extend",NA)
                               , FUN=function (x){
                                 if (maO-sum (is.na (x)) > maO/4){  ## allow up to 1/2 of window NA
                                   out <- sumFct(x)
                                 }else{
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
tDay <- aggregate (xVar~jday, dLTmean, FUN=mean, na.rm=TRUE)  # not sumFct here! it's a mean!
tDay$sd <- saggregate (xVar~jday, data=dLTmean, FUN=stats::sd, na.rm=TRUE, refDF=tDay)$xVar
tDay$MA <- saggregate (MA~jday, data=dLTmean, FUN=mean, na.rm=TRUE, refDF=tDay)$MA

  ## testing
  if (0){
    pY <- subset (dMeans, year == currentYear)
    tDay$pY <- pY$xVar [match (tDay$jday, pY$jday)]
    tDay$pYMA <- pY$MA [match (tDay$jday, pY$jday)]
    plot (pY~jday, tDay)
    lines (pYMA~jday, tDay, lwd=2, col="blue")
  }



  #  for (j in c (varName, MA)){
  for (i in 1:length (qntl)){
    tDay$perL <- saggregate (xVar~jday, dLTmean, FUN=quantile, probs=0.5-0.5*qntl [i], refDF=tDay)$xVar
    tDay$perU <- saggregate (xVar~jday, dLTmean, FUN=quantile, probs=0.5+0.5*qntl [i], refDF=tDay)$xVar
    names (tDay)[which (names (tDay) == "perL")] <- paste0 ("perL", i)
    names (tDay)[which (names (tDay) == "perU")] <- paste0 ("perU", i)
    tDay$maL <- saggregate (MA~jday, dLTmean, FUN=quantile, probs=0.5-0.5*qntl [i], refDF=tDay)$MA
    tDay$maU <- saggregate (MA~jday, dLTmean, FUN=quantile, probs=0.5+0.5*qntl [i], refDF=tDay)$MA
    names (tDay)[which (names (tDay) == "maL")] <- paste0 ("maL", i)
    names (tDay)[which (names (tDay) == "maU")] <- paste0 ("maU", i)
  }
  tDay$max <- saggregate (xVar~jday, dLTmean, FUN=max, na.rm=TRUE, refDF=tDay)$xVar
  tDay$min <- saggregate (xVar~jday, dLTmean, FUN=min, na.rm=TRUE, refDF=tDay)$xVar
  tDay$maxMA <- saggregate (MA~jday, dLTmean, FUN=max, na.rm=TRUE, refDF=tDay)$MA
  tDay$minMA <- saggregate (MA~jday, dLTmean, FUN=min, na.rm=TRUE, refDF=tDay)$MA

  # is this critically needed?? -- N years of data per date
  tDay$yearN <- saggregate ((!is.na (xVar))~jday, data=dMeans, FUN=sum
                            , na.rm=TRUE, refDF=tDay)[1:nrow (tDay),2] # some scripts fail at 366

  ## previous year
  pcY <- subset (dMeans, year == (currentYear-1))
  tDay$pcY <- pcY$xVar [match (tDay$jday, pcY$jday)]
  tDay$pcYMA <- pcY$MA [match (tDay$jday, pcY$jday)]
  rm (pcY)

  ## present/pretend/current year-1
  pY <- subset (dMeans, year == currentYear)
  tDay$pY <- pY$xVar [match (tDay$jday, pY$jday)]
  tDay$pYMA <- pY$MA [match (tDay$jday, pY$jday)]
  rm (pY)

  ## ongoing year (incomplete)
  ogY <- subset (dMeans, year == (currentYear+1))
  tDay$ogY <- ogY$xVar [match (tDay$jday, ogY$jday)]
  tDay$ogYMA <- ogY$MA [match (tDay$jday, ogY$jday)]
  rm (ogY)

  ## all years
  dMeans$year <- factor (dMeans$year)
  yr <- sapply (1:length (levels (dMeans$year)), function (x){
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

addTimehelpers <- function (df){
  ## assumes "datetimestamp" is present
  df$jday <- as.integer (strftime (df$datetimestamp, "%j"))
  df$year <- as.integer (strftime (df$datetimestamp, "%Y"))
  suppressPackageStartupMessages (require (lubridate))
  df$month <- month (df$datetimestamp)
  df$week <- week (df$datetimestamp)
  return (df)
}

fixGap <- function (df, intvl=15*60){ # interval of TS in seconds
  ## fix time gaps in standard SWMP data file with NAs
  ## fix min sampling interval (60 s * 15) = standard throughout SWMP?
  # print (min (df$datetimestamp)); print (max (df$datetimestamp))
  # print (summary (df))
  tR <- seq (min (df$datetimestamp, na.rm=TRUE)
             , max (df$datetimestamp, na.rm=TRUE)
             , by=15*60)
  nD <- data.frame (datetimestamp=tR
                    , df [match (tR, df$datetimestamp), 2:ncol (df)])
  return (addTimehelpers (nD))
}


## add Fahrenheit scale to temperature plot
fAxis <- function (cGrad, side=4, line=2.5
                   , mT = "Temperature [°F]"  #expression('Temperature '~'['*degree~'F'*']')
                   , ...){
  slope <- 9/5
  offset <- 32
  alt.ax <- pretty (slope * cGrad + offset)
  alt.at <- (alt.ax - offset) / slope
  axis (side=side, at=alt.at, labels=alt.ax, srt=90)
  mtext (mT, side=side, line=line, ...)
}

## add scale in inches
iAxis <- function (mmL, side=4, line=3, lab = "", ...){
  mmperinch <- 25.4
  alt.ax <- pretty (mmL / mmperinch)
  alt.at <- alt.ax * mmperinch
  axis (side=side, at=alt.at, labels=alt.ax, srt=90)
  mtext (lab, side=side, line=line, ...)
}



## load cached SWMP data and update it with the latest from CDMO
getSWMP <- function (station="kachdwq", QAQC=TRUE){
  ## load SWMP data from local zip file. Update to the most current data
  ## from CDMO and cache those updates for the next run.
  ## need to specify location of zip file from SWMP and cache folder below
  ## an initial zip file from CDMO is required.
  ## It is recommended to update this zip file on occasion.
  require ("SWMPr")
  require ("R.utils")

  cacheFolder <- "~/tmp/LCI_noaa/cache/SWMP/"
  dir.create(cacheFolder, showWarnings=FALSE)
  cacheStation <- paste0 (cacheFolder, station, ".RData")

  zF <- list.files ("~/GISdata/LCI/SWMP", ".zip", full.names=TRUE)
  if (length (zF) < 1){stop ("Need to download SWMP data from https://cdmo.baruch.sc.edu/get/landing.cfm")}
  SMPfile <- zF [which.max (file.info (zF)$ctime)] ## find most recent file
  rm (zF)

  ## delete cacheFolder if zip file is newer
  if (file.exists(cacheStation)){
    if (file.info (cacheStation)$ctime < file.info (SMPfile)$ctime){
      unlink (cacheStation)
    }
  }

  if (file.exists(cacheStation)){
    base::load (cacheStation)
      }else{
    smp <- import_local(SMPfile, station)
    if (QAQC){
      smp <- qaqc (smp)  ## scrutinize further? Wise to do here? Keep level 1?
    }
  }
  if (any (is.na (smp$datetimestamp))){stop ("NAs in timestamp")}
  #  ## not sure where the bad line is coming from, but it has to go
  #smp <- smp [!is.na (smp$datetimestamp),]
  fN <- difftime(Sys.time(), max (smp$datetimestamp), units = "days")
  ## catch for stations that are inactive?
  if ((2 < fN) & (fN < 5*365.25)){ # skip downloads for less than 2 day and legacy stations
    # ## skip downloads for legacy stations
    smp2 <- try (all_params (station
                             , Max=ceiling (as.numeric(fN)*4*24))
                 , silent=FALSE)  # XXX needs registered (static?) IP address. NCCOS VPN ok
    if (class (smp2)[1] != "try-error"){
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
        smp3 <- smp2 [,sapply (1:ncol (smp), FUN=function (i){
          which (names (smp)[i] == names (smp2))
        })]
        smp <- rbind (smp, smp3)
        if (any (is.na (smp$datetimestamp))){stop ("NAs in timestamp")}

        rm (smp2, smp3, fN)
      }
      smp <- smp [which (!duplicated(smp$datetimestamp)),]
    }
  }
  ## fixGap() here??
  ## smp <- qaqc (smp, qaqc_keep = "0") ## here??
  save (smp, file=cacheStation)
  return (smp)
}


getNOAAweather <- function (stationID="PAHO", clearcache=FALSE){
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
  if (clearcache){
    unlink (paste0 ("~/tmp/LCI_noaa/cache/noaaWeather/", stationID, ".RData"))
  }
  dir.create ("~/tmp/LCI_noaa/cache/noaaWeather", showWarnings=FALSE, recursive=TRUE)
  if (file.exists(paste0 ("~/tmp/LCI_noaa/cache/noaaWeather/", stationID, ".RData"))){
    load (paste0 ("~/tmp/LCI_noaa/cache/noaaWeather/", stationID, ".RData"))
    lastD <- max (rW$valid) # valid = date-time
    ## fudge to get 1 year overlap
    lastD <- as.Date (paste0 (as.integer (substr(as.character(lastD), start=1, stop=4))-1, "-01-01"))
  }else{
    lastD <- "2000-01-01"
  }
  rWn <- try (riem_measures (station=stationID, date_start=lastD, date_end=as.character(Sys.Date()))
              , silent=TRUE)

  if (class (rWn)[1]=="try-error"){
    if (!exists ("rW")){
      stop ("Have to be online for initial run fetching NOAA weather data.")
    }
  }else{
    if (!exists ("rW")){
      rW <- rWn
    }else{
      ## remove overlapping data
      rW <- subset (rW, valid < lastD)
      rW <- rbind (rW, rWn)
    }
  }
  rm (rWn)
  save (rW, file=paste0 ("~/tmp/LCI_noaa/cache/noaaWeather/", stationID, ".RData"))

  ## clean-up data/convert units, as appropriate, for compatibility with getNOAA


  rW
}


getNOAA <- function (buoyID=46108, set = "stdmet", clearcache=FALSE){  # default=kachemak bay wavebuoy
  #  require ("riem")  ## get data from mesonet.argon.iastate.edu as recommended by Brian Brettschneider
  #  riem_measures (station="VOHY", date_start="2014-01-01", date_end=as.character (Sys.Date()))
  # 2023-12-08: buoy still working with rnoaa -> use it while it works

  require ("rnoaa")
  if (clearcache){
    unlink (paste0 ("~/tmp/LCI_noaa/cache/noaaBuoy/", buoyID, ".RData"))
    unlink ("~/tmp/LCI_noaa/cache/noaaBuoy/", recursive=TRUE)
    dir.create("~/tmp/LCI_noaa/cache/noaaBuoy/", showWarnings=FALSE, recursive=TRUE)
  }
  ## this is slow -- cache as .RData file
  nw <- try (base::load (paste0 ("~/tmp/LCI_noaa/cache/noaaBuoy/", buoyID, ".RData")), silent=TRUE)
  if (class (nw) == "try-error"){
    if (buoyID == 46108){endD <- 2011}else{endD <- 1970}
    dir.create("~/tmp/LCI_noaa/cache/noaaBuoy/", showWarnings=FALSE, recursive=TRUE)
  }else{
    endD <- max (as.integer (substr (wDB$time, 1, 4)))  # last cached year
  }
  if (as.integer (format (Sys.Date(), "%Y")) - endD > -1){ # if not updated in a year
    wB <- lapply ((endD-1):as.integer (format (Sys.Date(), "%Y"))
                  , function (i){
                    try (buoy (dataset=set, buoyid=buoyID
                               , year=i), silent=TRUE)
                  }
    )
    for (i in 1:length (wB)){
      if (class (wB [[i]]) != "try-error"){
        if (!exists ("wDB")){
          wDB <- as.data.frame (wB[[i]]$data)
          meta <- wB [[i]]$meta
        }else{
          ## not sure why this would be a problem -- bad cache? do it anyway
          if ("datetimestamp" %in% names (wDB)){
            wDB <- wDB [,-datetimestamp]
          }
          wDB <- rbind (wDB, as.data.frame (wB [[i]]$data))
        }
      }
    }
  }

  ## add most recent
  cD <- try (buoy (dataset=set, buoyid=buoyID, year=9999))  ## 9999=most up-to-date data
  if (class (cD) == "buoy"){
    wDB <- rbind (wDB, as.data.frame (cD$data))
  }
  save (wDB, meta, file=paste0 ("~/tmp/LCI_noaa/cache/noaaBuoy/", buoyID, ".RData")) ## cache of buoy data

  ## QAQC
  wDB <- wDB [!duplicated(wDB$time),]
  tm <- gsub ("T", " ", wDB$time)
  tm <- gsub ("Z", "", tm)
  wDB$datetimestamp <- as.POSIXct (tm, format = "%F %T", tz = "UTC") # move this up?
  rm (tm)
  for (i in 1:length (meta)){  ## meta is a tibble...
    mN <- which (names (wDB) == names (meta [i]))
    is.na (wDB [,mN])[which (wDB [,mN] == meta [[i]]$missval)] <- TRUE  # set missing values to NA
  }
  ## ensure windspeed is m/s
  if (meta$wind_spd$units != "meters/second"){cat (meta$wind_spd$units); stop ("Fix wspd units")}
  return (wDB)
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
                                        , data=aggregate (xvar~jday+year
                                                          , data=dat
                                                          , FUN=function (x){
                                                            any (x > thrht [i])}
                                        )
                                        , FUN=sum)

                      c (mean=mean (agY$xvar, na.rm=TRUE)
                         , median=median(agY$xvar, na.rm=TRUE)
                         , lowerQ=quantile (agY$xvar, 0.1, na.rm=TRUE)
                         , upperQ=quantile (agY$xvar, 0.9, na.rm=TRUE)
                         , agY$xvar [(nrow (agY)-2):(nrow (agY)-0)]) #the current and previous year
                    })
  colnames(eventL) <- paste0 ("T", thrht)
  rownames(eventL)[5:7] <- paste0 ("Y", max (dat$year)-c(2,1, 0))
  as.data.frame (t (eventL))
}


cDir <- function (wd, nDir=8){
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
  cut (wd, breaks=rose_breaks, labels=rose_labs
       , right=FALSE, include.lowest=TRUE)
}




## combine several PNGs into one PDF
merge.png.pdf <- function(pdfFile, pngFiles, deletePngFiles=FALSE) {
  ## taken from https://jonkimanalyze.wordpress.com/2014/07/24/r-compile-png-files-into-pdf/
  #### Package Install ####
  gridExists <- require ("grid")
  if ( !gridExists ) {
    install.packages ("grid")
    library ("grid")
  }
  pngPackageExists <- require ("png")
  if ( !pngPackageExists ) {
    install.packages ("png")
    library ("png")
  }
  #########################

  pdf(pdfFile) ## could/should load aspect ratio of PNGs?
  par (mar = rep (0.2, 4))
  n <- length(pngFiles)
  for(i in 1:n) {
    pngFile <- pngFiles[i]
    pngRaster <- readPNG(pngFile)
    grid.raster(pngRaster, width=unit(0.8, "npc"), height= unit(0.8, "npc"))
    #    grid.raster(pngRaster, width=unit(5.5, "inches"), height= unit(5.5, "inches"))
    if (i < n) plot.new()
  }
  dev.off()
  if (deletePngFiles) {
    unlink(pngFiles)
  }
}



#EOF
