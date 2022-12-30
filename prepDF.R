prepDF <- function (dat, varName, sumFct=function (x){mean (x, na.rm=FALSE)}
                    , maO=31
                    , currentYear=as.integer (format (Sys.Date(), "%Y"))-1
                    , qntl=c(0.8, 0.9)
){
  if (! all (c("jday", "year", varName) %in% names (dat))){
    stop (paste ("Data frame must contain the variables jday, year, and", varName))
  }
  if (length (varName) > 1){stop ("so far can only process one variable at a time")}


  ## current/past year
  xVar <- dat [,which (names (dat) == varName)]
  ## aggregate to dailys: - standardize to a common time interval across datasets
  dMeans <- saggregate (xVar~jday+year, dat, FUN=sumFct
                        , refDF = data.frame (year=rep (min (dat$year):max(dat$year), 366)
                                              , jday=rep (1:366, each=length (min (dat$year):max (dat$year)))
                        )) # daily means -- needed for MA and CI

  ## output:
  ## annual mean (if more than 5 years)
  ## annual quantiles
  ## prev, current, ongoing years
  ## apply MA to all of these

  minN <- 5
  tMean <- aggregate (xVar~jday, subset (dMeans, year < currentYear)#, refDF=dMeans
                      , FUN=function (x){
    mx <- mean (x, na.rm = TRUE)
    ifelse (length (x) < minN, NA, mx)
  })

  tDay <- aggregate (xVar~jday, dMeans, FUN=mean, na.rm=TRUE)
  for (i in 1:length (qntl)){
    tDay$perL <- aggregate (xVar~jday, dMeans #, refDF=dMeans
                             , FUN=function (x){
      qx <- quantile (x, probs=0.5-0.5*qntl [i], na.rm=TRUE)
      ifelse(length (x) < minN, NA, qx)
    })$xVar
    names (tDay)[which (names (tDay)=="perL")] <- paste0 ("perL_", qntl [i])
  }
  ## end of ediits



  # align at center
  suppressPackageStartupMessages (Require ("zoo"))
  dMeans$MA <- zoo::rollapply (dMeans$xVar, width=maO, FUN=mean
                               #           , na.rm=FALSE  ## no apparent affect
                               , fill= c(NA, NA, NA)
                               #, partial=FALSE # maO/2
                               , align = "center")

  dLTmean <- subset (dMeans, year < currentYear)  ## climatology excluding current year
  tDay <- aggregate (xVar~jday, dLTmean, FUN=mean, na.rm=TRUE)  # not sumFct here! it's a mean!
  tDay$sd <- saggregate (xVar~jday, dLTmean, FUN=sd, na.rm=TRUE, refDF=tDay)$xVar
  tDay$MA <- saggregate (MA~jday, dLTmean, FUN=mean, na.rm=TRUE, refDF=tDay)$MA


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
  tDay$yearN <- saggregate ((!is.na (xVar))~jday, dMeans, FUN=sum
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
  yr <- sapply (levels (factor (dMeans$year)), function (x){
    pY <- subset (dMeans, year == x)
    pY$MA [match (tDay$jday, pY$jday)]
  })
  colnames(yr) <- paste0 ("y_", colnames(yr))
  tDay <- cbind (tDay, yr); rm (yr)

  ## fix names
  #  names (tDay) <- gsub ("xVar", paste0 (varName, "_"), names (tDay))
  names (tDay) <- gsub ("xVar", varName, names (tDay))
  names (tDay)[3:ncol (tDay)] <- paste0 (names (tDay)[3:ncol (tDay)], "_", varName)

  return (tDay)
}