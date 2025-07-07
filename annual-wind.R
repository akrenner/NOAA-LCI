#!/usr/bin/env RScript
# Martin Renner, NOAA-affiliate, 2020
## make climatology plot vs 2019 of high-wind events and average daily wind speed.
## use Lands End wind data from SWMP station


if (!exists ("quarterly")){
  rm (list=ls())
  quarterly <- TRUE
}


# setwd("~/myDocs/amyfiles/NOAA-LCI/")
# setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")


##########################################################
## parameters to agree upon



currentYear <- as.numeric (format (Sys.Date(), "%Y")) -1 # year before present
maO <- 31   # moving average window
vUnit <- "knots" # or comment out to default to m/s
qntl <- 0.9 # % quantile
stormT <- 48 # threshold for max wind speed to count as storm
galeT <- 34  # max wind speed for gale
scAdvT <- 23 # max wind speed for small craft advisory (AK value) -- sustained or frequent gusts
# currentCol <- c ("blue", "lightblue", "black") # colors for past, current, ongoing year

## worldmet does not provide maxwspd -- use swmp data instead (back up): sAir



mediaD <- "~/tmp/LCI_noaa/media/StateOfTheBay/"

require ("RColorBrewer")
if (quarterly){
  pastYear <- FALSE  ## for winter/spring publication
  ongoingY <- TRUE
  currentCol <- c (brewer.pal (4, "Paired")[1:2], "black")[c(3, 1, 2)]
  mediaD <- paste0 (mediaD, "update/")
}else{
  pastYear <- FALSE  ## for fall publication  # plot currentYear-1 ?
  ongoingY <- TRUE
  currentCol <- c ("black", brewer.pal (4, "Paired")[1:2])
}
## leave code below as-is
##########################################################


windT <- c(SCA=scAdvT, gale=galeT, storm=stormT)
# rm (scAdvT, galeT, stormT)


## --------- get weather data ---------- ##
load ("~/tmp/LCI_noaa/cache/annual-AirWeather.RData") ## loads hmr
source ("annualPlotFct.R")



## get up-to-date SWMP data
require ("openair") # for windRose
dir.create(mediaD, showWarnings=FALSE, recursive=TRUE)


## cycle through all wStations

kmK <- which (names (weatherL)=="kachomet")

# if (!exists ("wStations")){wStations <- metstation}
# for (k in seq_along(wStations)){
# for (k in seq_along(weatherL)){
for (i in kmK){
  # k <- 1
  metstation <- names (weatherL) [k]
  cat ("\n\n######\n", metstation, "started\n######\n\n")
  suppressWarnings(rm (hmr))


  # stationL <- "Homer Airport"
  # stationL <- c ("Homer Spit", "Flat Island", "Augustine Island", "East Amatuli")[k]
  #
  #
  #
  #
  #
  # ## get data from NOAA server or local cache
  # if (metstation== "PAHO"){
  #   load ("~/tmp/LCI_noaa/cache/annual-noaaAirWeather.RData") # hmr
  #   # hmr3 <- getNOAAweather(metstation)
  #   # hmr <- with (hmr3, data.frame (datetimestamp = valid, atemp=(tmpf-32)*5/9
  #   #                                , rh=relh
  #   #                                , bp=rep (is.na (nrow (hmr3)))
  #   #                                , wspd=sknt * 0.5144444444 # convert knots to m/s
  #   #                                , maxwspd=peak_wind_gust * 0.5144444444
  #   #                                , wdir=peak_wind_drct
  #   #                                # , wdir=drct
  #   #                                , sdwdir=rep (is.na (nrow (hmr3)))
  #   #                                , totpar=rep (is.na (nrow (hmr3)))
  #   #                                , totprcp=rep (is.na (nrow (hmr3)))
  #   #                                , totsorad=rep (is.na (nrow (hmr3)))
  #   # ))
  #   # rm (hmr3)
  # }else if (metstation == "kachomet"){
  #   # hmr <- getSWMP (metstation)
  #   load ("tmp/LCI_noaa/cache/annual-noaaAirWeather.RData")"  # Homer Airport
  # }else{
  #   hmr2 <- getNOAA (metstation) # fetch from NOAA
  #   # hmr2 <- getNOAA ("HMSA2") # fetch from NOAA
  #   hmr <- with (hmr2, data.frame (datetimestamp, atemp=air_temperature
  #                                  , rh=rep (is.na (nrow (hmr2)))  # relative humidity
  #                                  , bp=rep (is.na (nrow (hmr2)))  # barometric pressure
  #                                  , wspd=wind_spd # m/s -- same as SWMP  # NOAA wspd in m/s
  #                                  , maxwspd=gust
  #                                  , wdir=wind_dir
  #                                  , sdwdir=rep (is.na (nrow (hmr2)))
  #                                  , totpar=rep (is.na (nrow (hmr2)))
  #                                  , totprcp=rep (is.na (nrow (hmr2)))
  #                                  , totsorad=rep (is.na (nrow (hmr2)))
  #   ))
  #   rm (hmr2)
  # }

  #    load ("~/tmp/LCI_noaa/cache/annual-noaaAirWeather.RData")  # hmr,  Homer Airport
  hmr <- weatherL [[k]]


  ## apply QAQC flaggs ##
  # is.na (hmr$atemp [which (hmr$f_atemp != "<0>")]) <- TRUE
  # is.na (hmr$)
  # hmr <- qaqc (hmr, qaqc_keep="0")  # scrutinize this further? -- do this in getSWMP()


  ## alternative: get wind (and other weather) from HMSA2, Homer Spit -- Mike says it's more
  ## representative of wind in the bay than HMRA2, Homer, Research Reserve
  if (0){
    ## useing worldmet package
    require ("worldmet")
    AKsites <- getMeta(plot=FALSE, returnMap=FALSE) |>
      dplyr::filter (55 < latitude & latitude < 61) |>
      dplyr::filter (-154 < longitude & longitude < -148)
    write.csv (AKsites, file="~/tmp/LCI_noaa/NoaaMetStations.csv", row.names=FALSE)

    pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/NOAAWeatherStations.pdf", width=11.5, height=8)
    plot (latitude~longitude, AKsites, type="n", asp=2)
    text (AKsites$longitude, AKsites$latitude, labels=AKsites$station, cex=0.5)
    dev.off()
  }
  #######################



  # also try max, sum of h over gale, N gale days...
  ## alternative approach: gales per month with gale=max_wspd > 30 knots
  ## also consider: sd on a log-scale

  if (exists (vUnit)){ #} == "knots"){
    hmr$wspd <- hmr$wspd * 1.94384   ## 1 m/s=1.94384 knots
    hmr$maxwspd <- hmr$maxwspd * 1.94384
    wCaption <- "wind speed [knots]"
  }else{
    wCaption <- "wind speed [m/s]"
  }


  ## utility functions
  agFct <- function (x, thd=stormT){ ## gale days per maO
    # x1 <- sum(x>=thd, na.rm=TRUE)
    # ifelse (x1 > 1*4, 1, 0) # at least 1 h above threshold  ##?? needs review/revision!!
    any (x > thd)
  }  # 2 h at 20 knots looks good
  meanNA <- function (x){mean (x, na.rm=TRUE)}
  ## still needs some fixes -- ensure gales/storms are handled consistently
  # sagFct <- fuction (df, refDF, thd=stormT){
  #   saggregate (maxwsspd~yearJd, data = df, FUN=agFct, refDF=refDF, matchV="yearJd")
  # }

  # Gust <- aggregate (maxwspd~jday+year, hmr, FUN=agFct, thd=galeT)
  # dMeans$gale <- Gust$maxwspd [match (paste (dMeans$jday, dMeans$year), paste (Gust$jday, Gust$year))]


  ## apply QCQA -- ask Steve, check Seldovia -- not working as-is!
  # summary (factor (hmr$f_wspd))
  if (0){
    qaqcM <- function (fVar, tVar){
      fVar <- gsub ("[<>]", "", fVar)
      fVar <- gsub ("[GIT]", "", fVar, fixed=TRUE)
      fVar <- gsub ("[SOC] (CSM)", "", fVar, fixed=TRUE)
      fVar <- trimws (fVar)
      # levels (factor (fVar))
      tVar <- ifelse (fVar %in% c("0", "1", "2", "3", "4", "5"), tVar, NA) ## check with Steve Baird!!
      tVar
    }
    hmr$wspd <- qaqcM (hmr$f_wspd, hmr$wspd)
    hmr$maxwspd <- qaqcM (hmr$f_maxwspd, hmr$maxwspd)
    hmr$wdir <- qaqcM (hmr$f_wdir, hmr$wdir)
    hmr$atemp <- qaqcM (hmr$f_atemp, hmr$atemp)
    hmr$totprcp <- qaqcM (hmr$f_totprcp, hmr$totprcp)
    # 2003 precip data has no zeros, 2004 is missing
    hmr$totprcp <- with (hmr, ifelse (datetimestamp < as.POSIXct("2004-01-01"), NA, totprcp)) # no zeros in 2003 data
    hmr$rh <- qaqcM (hmr$f_rh, hmr$rh)
    rm (qaqcM)
  }
  ## find missing values, gaps in TS
  # summary (as.numeric (diff (hmr$datetimestamp)))
  # which (as.numeric (diff (hmr$datetimestamp)) > 15)

  hmr <- fixGap (hmr)  # this will also add helper variables year, jday, etc.


  # save (hmr.... file=....)


  ###############################################################
  ## data processing -- calculate averages and moving averages ##
  ###############################################################


  ## XXXX ---  this needs fixing to make it consistent throughout ###
  ## table of number of gales/storms -- mean vs current year
  ## count number of days with max wind above threshold
#  windSum <- nEvents (hmr, "maxwspd", thrht=windT)
  windSum <- nEvents (hmr, "wspd", thrht=windT)
  windSum$mean <- round (windSum$mean, 1)
  row.names (windSum) <- c ("SCA", "gales", "storms")
  cat ("\n\n# number of gales and storms per year, ", names (weatherL) [k],"\n")
  print (windSum [,1:(ncol (windSum)-1)]) # cut-out ongoing year
  rm (windSum)

  ## tabulate only winter storms
  hw <- hmr
  # hw$year <- ifelse (hw$month == 1 | hw$month == 2, hw$year-1, hw$year) # move Jan-Feb into prev year to keep season together
  hw$year <- ifelse (hw$month %in% c (1,2), hw$year-1, hw$year) # move Jan-Feb into prev year to keep season together
  hw <- subset (hw, month %in% c(1,2,12))
  windSum <- nEvents (hw, "maxwspd", thrht=windT)
  windSum$mean <- round (windSum$mean, 1)
  row.names (windSum) <- c ("SCA", "gales", "storms")
  cat ("\n\n# number of WINTER gales and storms per year, ", names (weatherL) [k],"\n")
  print (windSum)
  rm (windSum, hw)


  ## storms -- barchart
  yGale <- sapply (windT, function (y){
    aggregate (maxwspd~year, data = aggregate (maxwspd~jday+year, data = hmr
                                               , subset = year <= currentYear # skip incomplete ongoing
                                               , FUN = function (x){any (x > y)})
               , FUN=sum)$maxwspd
  })
  row.names (yGale) <- aggregate (maxwspd~year, data = hmr
                                  , subset = year <= currentYear
                                  , FUN = mean)$year
  cGale <- as.data.frame (yGale)
  cGale$SCAS <- cGale$SCA - rowSums (cGale[,2:3]) # SCA only -- to allow stacking
  cGale$galeS <- cGale$gale - cGale$storm # gales without storms -- to allow stacking
  ## pull-out current year to give different col in stacked barchart
  sTab <- t (cGale)
  sTab <- rbind (sTab
                 , galeC = c (rep (0, ncol (sTab)-1), sTab [which (row.names (sTab)=="galeS"),ncol (sTab)])
                 , stormC = c (rep (0, ncol (sTab)-1), sTab [3,ncol (sTab)])
  )
  # is.na (sTab [1:(nrow (sTab)-2), ncol (sTab)]) <- TRUE
  sTab [1:(nrow (sTab)-2), ncol (sTab)] <- 0

  require ("RColorBrewer")
  gCols <- c("lightgray", "darkgray", brewer.pal (4, "Paired")[1:2])

  # png (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-WindStack_", metstation, ".png")
  #      , width=1200, height=1200, res=300)
  pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-WindStack_", metstation, ".pdf")
       , width=4, height=4)
  par (mar=c (4,5,1,1))
  # barplot (sTab [which (row.names(sTab)%in% c("galeS", "storm", "stormC", "galeC")),]  ## excluding SCA, storms to bottom

  if (metstation == "FILA2"){
    lP <- "topleft"
    yR <- c(0, 90)
  }else{
    lP <- "topright"
    yR <- NULL
  }
  barplot (sTab [c(3,5,7,6),]  ## excluding SCA, storms to bottom
           , col=gCols# [4:1]
           , ylab="High-wind days per year"
           , ylim=yR
  )
  if (metstation=="kachomet"){
    title (main="Homer Spit")
  }else if (metstation=="FILA2"){
    title (main="Flat Island")
  }
  is.na (sTab [1:(nrow (sTab)-2), ncol (sTab)]) <- TRUE  # to have means unbiased
  abline (h=mean (as.data.frame (t (sTab))$gale, na.rm=TRUE), lwd=3, lty="dashed") # gray would be invisible
  abline (h=mean (as.data.frame (t (sTab))$storm, na.rm=TRUE), lwd=3, lty="dotted", col="black")
  ## add 90%tile for gales
  # abline (h=quantile(sTab [which (row.names(sTab)=="gale"),]
  #          , probs=c(0.05,0.95), na.rm=TRUE), lty="dashed", lwd=1)
  legend (lP, legend=c ("gale", "storm") # row.names(sTab)[1:2]
          , fill=gCols[c(2,1)], bty="n", ncol=1)
  dev.off()
  rm (cGale, yGale, gCols, sTab, lP)
  ## end of wind summary



  if (0){ ## violin plot of frequency of storms/gales
    require("vioplot")
    vioplot (yGale$maxwspd, ylab="N gales")
    ## abline (h=yGale$maxwspd [yGale$year == currentYear])
    points (1, yGale$maxwspd [yGale$year == currentYear]
            , pch=16, col="red", )
  }

  ### XXXX fixes !!!
  ## current year: your of report. past year = year of previous report
  dMeans <- aggregate (wspd~jday+year, hmr, FUN=meanNA) # daily means full time series
  Gust <- aggregate (maxwspd~jday+year, hmr, FUN=agFct, thd=stormT)
  dMeans$storm <- Gust$maxwspd [match (paste (dMeans$jday, dMeans$year), paste (Gust$jday, Gust$year))]
  Gust <- aggregate (maxwspd~jday+year, hmr, FUN=agFct, thd=galeT)
  dMeans$gale <- Gust$maxwspd [match (paste (dMeans$jday, dMeans$year), paste (Gust$jday, Gust$year))]
  Gust$sca <- aggregate (maxwspd~jday+year, hmr, FUN=agFct, thd=scAdvT)$maxwspd
  dMeans$sca <- Gust$sca [match (paste (dMeans$jday, dMeans$year), paste (Gust$jday, Gust$year))]
  rm (Gust)
  rm (agFct)

  ## mean wind direction (incl. speed)
  ## http://weatherclasses.com/uploads/3/6/2/3/36231461/computing_wind_direction_and_speed_from_u_and_v.pdf
  uw <- with (hmr, -1 * wspd * sin (wdir*pi/180))
  vw <- with (hmr, -1 * wspd * cos (wdir*pi/180))
  ## MR versions. u=x, v=y
  dMuw <- aggregate (uw~jday+year, hmr, FUN=meanNA)
  dMvw <- aggregate (vw~jday+year, hmr, FUN=meanNA)
  dMeans$uw <- dMuw$uw [match (paste (dMeans$jday, dMeans$year), paste (dMuw$jday, dMuw$year))]
  dMeans$vw <- dMvw$vw [match (paste (dMeans$jday, dMeans$year), paste (dMuw$jday, dMuw$year))]
  rm (uw, vw, dMuw, dMvw)

  meanWind <- function (u,v){ # weatherclasses.com as above
    if (any (is.na (c (u,v)))){
      out <- rep (NA, length (u))
    }# else
    180+(180/pi*atan2 (u,v))
  }
  dMeans$wdir <- with (dMeans, meanWind (uw, vw))
  dMeans$windSpd <- with (dMeans, sqrt (uw^2+vw^2))
  dMeans$wdir <- with (dMeans, ifelse (wdir < 0, wdir + 360, wdir)) # needed??
  require ("circular")
  circWind <- circular (hmr$wdir, type="directions", units="degrees", template="geographics")
  wDir2 <- aggregate (circWind~jday+year, hmr, FUN=function (x){
    as.numeric (mean (x, na.rm=TRUE)) ## this is NOT right -- need to apply weight by wind speed! XXX
  })
  dMeans$wDirCM <- wDir2$circWind [match (paste (dMeans$jday, dMeans$year), paste (wDir2$jday, wDir2$year))]
  rm (circWind, wDir2)

  ## MA of current/past year -- Moving Average
  ##--- this (and others should be handled with prepPDF) XXX

  # require (forecast)
  # load ("~/tmp/LCI_noaa/cache/MAfunction.RData") ## gets maT -- function -- backwards MA -- is that what we want? XXX
  # ma <- maT
  # dMeans$maW <- as.numeric (maT (dMeans$wspd, maO))
  # dMeans$galeMA <- as.numeric (maT (dMeans$gale, maO))
  require ("SWMPr")
  dMeans$maW <- unlist (smoother (dMeans$wspd, maO, sides=1))
  dMeans$galeMA <- unlist (smoother (dMeans$gale, maO, sides=1)) ## all NAs XXX fix


  ## annual data                 ## use prepDF instead?? XXX
  sSet <- with (dMeans, (year < currentYear)&(jday < 366))
  sSet <- subset (dMeans, (year < currentYear) & (jday < 366))
  tDay <- aggregate (wspd~jday, sSet, FUN=meanNA) # exclude current year
  tDay$sdWind <- unlist (saggregate (wspd~jday, sSet, FUN=stats::sd, refDF=tDay)$wspd)
  tDay$smoothWindMA <- unlist (saggregate (maW~jday, sSet, FUN=meanNA, refDF=tDay)$maW)
  tDay$sdMA <- unlist (saggregate (maW~jday, sSet, FUN=stats::sd, na.rm=TRUE, refDF=tDay)$maW) ## it's circular(ish): av across years
  tDay$lowPerMA <- unlist (saggregate (maW~jday, sSet, FUN=quantile, probs=0.5-0.5*qntl, na.rm=TRUE, refDF=tDay)$maW)
  tDay$uppPerMA <- unlist (saggregate (maW~jday, sSet, FUN=quantile, probs=0.5+0.5*qntl, na.rm=TRUE, refDF=tDay)$maW)
  lowQ <- 0.8
  tDay$lowPerMAs <- unlist (saggregate (maW~jday, sSet, FUN=quantile, probs=0.5-0.5*lowQ, na.rm=TRUE, refDF=tDay)$maW)
  tDay$uppPerMAs <- unlist (saggregate (maW~jday, sSet, FUN=quantile, probs=0.5+0.5*lowQ, na.rm=TRUE, refDF=tDay)$maW)
  rm (lowQ)

  ## all these aggregates are valuable to break when there are missing data!
  tDay$uw <- unlist (saggregate (uw~jday, sSet, FUN=meanNA, refDF=tDay)$uw)
  tDay$vw <- unlist (saggregate (vw~jday, sSet, FUN=meanNA, refDF=tDay)$vw)
  tDay$wdir <- unlist (meanWind (tDay$uw, tDay$vw))

  tDay$gale <- saggregate (gale~jday, sSet, FUN=meanNA, refDF=tDay)$gale # * maO
  rm (sSet)

  ## galeMA is all NAs -- fix it!
  # if (all (is.na (dMeans$galeMA)))
  ## blows up on all NAs
  # tDay$galeMA <- saggregate (galeMA~jday, dMeans, FUN=meanNA, subset=year < currentYear, refDF=tDay)$maGale
  # # may need different averager for storms?? kernel-density?? XXX
  # tDay$sdGaleMA <- saggregate (galeMA~jday, dMeans, FUN=stats::sd, na.rm=TRUE, subset=year < currentYear
  #                              , refDF=tDay)$maGale
  ## XX tDay$sca <- aggregate (sca~jday, dMeans, FUN=meanNA, subset=year < currentYear)$sca

  ## current year under consideration
  past365 <- subset (dMeans, subset=year == currentYear)
  tDay$p365  <- past365$wspd [match (tDay$jday, past365$jday)]  ## has some missing days
  tDay$p365ma <- past365$maW [match (tDay$jday, past365$jday)]
  tDay$p365uw <- past365$uw [match (tDay$jday, past365$jday)]
  tDay$p365vw <- past365$vw [match (tDay$jday, past365$jday)]

 # tDay$p365galeMA <- past365$galeMA [match (tDay$jday, past365$jday)] ## what if there are no gales?
  tDay$p365galDay <- past365$gale [match (tDay$jday, past365$jday)]
  tDay$p365wdir <- past365$wdir [match (tDay$jday, past365$jday)]
  tDay$p365scaDay <- past365$sca [match (tDay$jday, past365$jday)]

  # ongoing year
  og365 <- subset (dMeans, subset=year == currentYear + 1)
  tDay$og365 <- og365$wspd [match (tDay$jday, og365$jday)]
  tDay$og365ma <-og365$maW [match (tDay$jday, og365$jday)]  ## moving average for partial year
  # past year: current-1
  pp365 <- subset (dMeans, subset=year == currentYear -1)
  tDay$pp365ma <- pp365$maW [match (tDay$jday, pp365$jday)] ## gales and storms for partial year
  rm (past365, og365, pp365)

  ## weekly summary of annual data -- for wind uv only
  # tWeek <- aggregate (wspd~week, hmr, meanNA)
  tWeek <- aggregate (cbind (jday, uw, vw #, p365uw, p365vw  ## p365uw and vw are all NAs -- still needed?
  )~I(factor (jday %/% 7)), data=tDay, meanNA)
  names (tWeek)[1] <- "week"





  ## cardinal wind direction
  ## following https://community.rstudio.com/t/convert-wind-direction-degrees-into-factors-in-a-data-frame/14636/4
  ## example
  # cE <- c (358, 2, 89, 177, 92, 265, 46, 20); data.frame (cDir (cE), cE)

  if (all (is.na (tDay$p365wdir))){
    tDay$p365wCar <- rep (NA, nrow (tDay))  # 2020 has only NAs
  }else{
    tDay$p365wCar <- cDir (tDay$p365wdir) # cardinal directions  ## Error in 2020: all NAs.
  }
  # rm (cDir)


  ### trouble-shooting
  # save.image ("~/tmp/LCI_noaa/cache/WindTrouble.RData")
  # rm (list=ls()); load ("~/tmp/LCI_noaa/cache/WindTrouble.RData")

  tDay <- subset (tDay, jday < 366) # 366 is not stable because not many samples
  ## cleaned-up plot
  if (metstation == "kachomet"){ # don't cache non-SWMP site because they don't have precipitation
    save (hmr, file="~/tmp/LCI_noaa/cache/metDat.RData")
  }
  # save.image("~/tmp/LCI_noaa/cache/wind2.RData")
  # rm (list=ls()); load ("~/tmp/LCI_noaa/cache/wind2.RData")






  ################
  ## make PLOTS ##
  ################

  x <- dir.create("~/tmp/LCI_noaa/media/StateOfTheBay/", showWarnings=FALSE); rm (x)
  # pdf (paste0 (mediaD, "sa-wind_", metstation,".pdf"), width=9, height=6)
  png (paste0 (mediaD, "sa-wind_", metstation,".png"), width=9*300, height=6*300, res=300)


  if (0){  ## farewell my good friend ##
    ## combine several panels in one plot
    layout (matrix(c(1,2,3), nrow=3)
            , widths=rep (3,3), heights=c(0.5,0.5,3))
    # par (fig=c(), new=TRUE) # see https://www.statmethods.net/advgraphs/layout.html
    ## wind vectors
    ## still would need some cleanup before use
    sF=0.7
    lW=1.0
    xGrenz <- c(5,360)
    par (mar=c(0,4,0,0.1))
    plot (1:nrow (tDay), seq (-10, 10, length.out=nrow (tDay)), type="n", asp=1
          , axes=FALSE, xlab="", ylab="long-term", xlim=xGrenz)
    with  (tDay, segments(x0=jday, y0=0, x1=jday - uw*sF, y1=0 - vw*sF, col="blue"
                          , lwd=lW))
    plot (1:nrow (tDay), seq (-10, 10, length.out=nrow (tDay)), type="n", asp=1
          , axes=FALSE, xlab="", ylab=currentYear, xlim=xGrenz)
    with (tDay, segments(x0=jday, y0=0, x1=jday - p365uw*sF, y1=0 - p365vw*sF
                         , col=ifelse (p365scaDay == 1, "black", "red")
                         , lwd=lW + p365galDay*2))
    rm (sF, lW)
  }

  ## better to move to standard var names above
  par (mar=c(3,4,1.5,0.1))
  plotSetup (tDay$lowPerMA, tDay$uppPerMA, ylab=wCaption
             # , ylim=c(0,25)
#             , ylim=c(0,15) # for spit only
, ylim=c(0,8) # for Homer Airport
             , main=metstation)

  oP <- par()
  ## windrose insert
  if (0){
    ## lattice plot will start a new page, no matter what -- use temp file
    ## lattice plot is the best-looking amongst the ggplot, base-plot and lattice options
    ## all attempts to place graphics at the appropriate position in base-plot
    ## or Hmisc::subplot or using par() failed -> tmp file
    hmrS <- hmr
    hmrS <- subset (hmr, year == currentYear)
    hmrS$date <- hmrS$datetimestamp

    windR <- function (df){  ## noisy. sink () does not suppress console output or warnings
      require ("openair", warn.conflicts=FALSE, quietly=TRUE) # for windRose
      wR <- windRose (df, ws="wspd", wd="wdir"
                      #, type="yClass"
                      , type=c("season") #, "yClass")
                      , auto.text=FALSE, paddle=FALSE, annotate=FALSE
                      , breaks=c (0, 10, 20, 30, max (ceiling (df$wspd), na.rm=TRUE))
                      , key.footer="knots"
                      , grid.line=10  # list (value=10, lty=5, col="purple")
                      #  , statistic="prop.mean"
                      #, max.freq=30
      )
      print (wR)
    }

    if (0){ #   pick a particular month vs compare all year
      iMonth <- 12 # picked as an interesting example for year X
      hmrS <- subset (hmr, (year < current) & month %in% c(iMonth))   ## optionall show only a iMonth
      hmrS$date <- as.POSIXct(hmrS$datetimestamp)
      hmrS$yClass <- factor (ifelse (hmrS$year < currentYear
                                     , paste ("average", month.abb [iMonth])
                                     , paste (month.abb [iMonth], currentYear)))
      rm (iMonth) # arbitrarily pick a month to tell the story
    }else{  ## all year vs climatology
      hmrS$date <- as.POSIXct(hmrS$datetimestamp)
      hmrS$yClass <- factor (ifelse (hmrS$year < currentYear
                                     , paste0 (min (hmrS$year), "-", currentYear - 1)
                                     , currentYear))
      hmrS <- subset (hmrS, year <= currentYear)   # exclude present year
      # hmrS$wspd <- ceiling(hmrS$wspd)
      # Unknown or uninitialised column: `subsets`.
    }
    #  par (fig=c(0.25,0.5,0.75,1))

    tF <- tempdir()
    # postscript (paste0 (tF, "ltc.ps"), width=9,height=6, paper="special")
    ## PostScriptTrace (file, outfile, ....)   ## XXX do this eventually XXX
    # readPicture or grImport
    # see https://cran.r-project.org/web/packages/grImport/vignettes/import.pdf
    png (paste0 (tF, "ltc.png"), width=11*200, height=6*200, res=400)
    #  par (mar=c())
    windR (hmrS)
    dev.off()
    require ("png")
    img2 <- readPNG (paste0 (tF, "ltc.png"))
    unlink(tF, recursive=TRUE); rm (tF)
    ## calculate coordinates for raster-image, to avoid readjusting it each year
    ## or keep fixed y-axis?
    rasterImage (img2, xleft=60, ybottom=10.3  ## fits well when y-axis is scaled 0-25
                 , xright=340, ytop=27, interpolate=FALSE)
    # rasterImage (img2, xleft=60, ybottom=8.3
    #              , xright=340, ytop=14.8, interpolate=FALSE)
    rm (img2)
    #rm (xGrez, pCo)
  }

  # par (oP)# reset to original plotting geometry
  par (crt=oP$crt # reset to original plotting geometry
       , fig=oP$fig, fin=oP$fin, lab=oP$lab, mai=oP$mai, mar=oP$mar #, mfg=oP$mfg
       , mgp=oP$mgp, omd=oP$omd, pin=oP$pin, plt=oP$plt, ps=oP$ps, pty=oP$pty
       , usr=oP$usr, xaxp=oP$xaxp, xaxs=oP$xaxs, xaxt=oP$xaxt, yaxp=oP$yaxp
       , yaxs=oP$yaxs, ylbias=oP$ylbias)
  ## plot lines AFTER windrose to be able to wrap tigher around white corners of inserted plot
  with (tDay, addGraphs (longMean=smoothWindMA, percL=lowPerMA, percU=uppPerMA
                         , current=cbind (pp365ma, p365ma, og365ma)
                         , jday=jday
                         , currentCol=currentCol
                         , pastYear=pastYear, ongoingYear=ongoingY,
  ))
  # lines (og365ma~jday, tDay, col="pink", lwd=3) # partial year -- temporary
  ## otherwise include in addGraphs with ongoingYear=TRUE


  ## add gale pictogram into this graph
  # with (subset (tDay, p365galDay > 0),
  #       text (jday, rep (c (5.9, 7.1), length.out=length (jday)), labels=p365wCar))
  if (0){ # plot SCA days?
    with (subset (tDay, (p365scaDay > 0)&(!p365galDay >0)),
          text (jday, p365ma + 0.5, labels=p365wCar, srt=0, cex=0.8))
  }
  # with (subset (tDay, p365galDay > 0), text (jday, 5.8, labels=p365wCar))
  require ("png")
  ## plot gales or storms?
  galeS <- subset (tDay, p365galDay > 0)  ## should be storms!
  hgt <- 1.1; wdh <- 15
  img <- readPNG ("pictograms/cloud.png")
  if (nrow (galeS) > 0){
    ## jitter to avoid overlapping clouds
    jit <- c(-0.5, -1.1, 0.3)
    with (galeS, rasterImage (img, xleft=jday-9, ybottom=p365ma + 1.5 + jit
                              , xright=jday-9+wdh, ytop=p365ma + 1.5 + hgt + jit
                              # , angle=p365wdir+ 90
                              ##  rotates around bottom-left point -- would need compensation
    ))
    with (galeS, text (jday, p365ma + 2.0 + jit, labels=p365wCar, cex=0.6))
  }
  rm (galeS)
  ## legend
  bP <- cLegend ("bottomleft", qntl=qntl [1], inset=0.02
                 , currentYear=currentYear
                 , mRange=c(min (hmr$year), currentYear - 1)
                 , cYcol=currentCol
                 , title=paste (maO, "day moving average")
                 , pastYear=pastYear, ongoingYear=ongoingY,
  )
  ## legend for gale clouds in other corner
  if (1){
    ## gales
    yL <- 0.7; xL <- 250
    rasterImage (img, xleft=xL, xright=xL+wdh, ybottom=yL-0.5, ytop=yL-0.5+hgt)
    text (xL+13, yL - 0.05, paste0 ("Gales with wind direction")  # "N,E,S,W  direction and timing\n of gales (", currentYear, ", >", galeT, " knots)")
          , pos=4, adj=c(0,1))
    # yL <- 2.7
    # text (365, yL + 0.1, paste0 ("N,E,S,W  gale (>", galeT, " knots)"), pos=2)
    ## no storms in 2021 -- drop this part this year
    # rasterImage (img, xleft=280, xright=280+wdh, ybottom=yL + 1.2, ytop=yL + 1.2+hgt)
    # text (365, yL + 1.8, paste0 ("storm (>", stormT, " knots)"), pos=2)
  }
  # par (oP)
  par (crt=oP$crt # reset to original plotting geometry
       , fig=oP$fig, fin=oP$fin, lab=oP$lab, mai=oP$mai, mar=oP$mar #, mfg=oP$mfg
       , mgp=oP$mgp, omd=oP$omd, pin=oP$pin, plt=oP$plt, ps=oP$ps, pty=oP$pty
       , usr=oP$usr, xaxp=oP$xaxp, xaxs=oP$xaxs, xaxt=oP$xaxt, yaxp=oP$yaxp
       , yaxs=oP$yaxs, ylbias=oP$ylbias)

  ## show 1:1 diagonal
  dev.off()
  rm(hgt, wdh, bP, img, yL, xL)


  ## start-over/add windrose
  # rm (list=ls()); load ("~/tmp/LCI_noaa/cache/wind2.RData")

  # if (!require ("openair")){
  # require("devtools") ## needs Rtools -- which needs VPN
  # install_github('davidcarslaw/openair')
  # }

  if (0){
    # openair:windRose -- lattice-plot; struggling with type for class other than times
    pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/dayBreeze_", metstation, ".pdf")
         , width=9, height=6)
    # hmrS <- subset (hmr, (month %in% c(1,2,3,12))) # Vincent!
    # hmrS$yClass <- factor (ifelse (hmrS$datetimestamp < as.POSIXct ("2019-03-01"), "mean", "2019/20"))
    for (i in 1:12){
      hmrS <- subset (hmr, year < 2020)
      hmrS <- subset (hmrS, month == i)
      hmrS$date <- as.POSIXct(hmrS$datetimestamp)
      hmrS$yClass <- factor (ifelse (hmrS$year < currentYear, paste0 ("mean-", i)
                                     , paste0 (currentYear, "-", i)))

      windRose(hmrS, ws="wspd", wd="wdir"
               , type="yClass"
               , auto.text=TRUE, paddle=FALSE, annotate=TRUE
               , breaks=c (0, 5, 10, 15,20,30,60)
               #         , breaks=c (0, 15,20,30, 40, 60)
               , key.footer="knots"
               #, max.freq=30
      )
    }
    dev.off()
  }


  pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/winterStorms.pdf", width=9, height=6)
  hmrS <- subset (hmr, (month %in% c(1,2,3,12))) # Vincent!
  hmrS <- subset (hmrS, year < 2020)
  # hmrS <- subset (hmrS, month == 12)
  hmrS$date <- as.POSIXct(hmrS$datetimestamp)
  hmrS$yClass <- factor (ifelse (hmrS$datetimestamp < as.POSIXct ("2019-03-01"), "mean", "2019/20"))
  windRose(hmrS, ws="wspd", wd="wdir"
           , type="yClass"
           , auto.text=TRUE, paddle=FALSE, annotate=TRUE
           , breaks=c (0, 5, 10, 15,20,30,60)
           #         , breaks=c (0, 15,20,30, 40, 60)
           , key.footer="knots"
           #, max.freq=30
  )
  dev.off()


  hmrS <- subset (hmr, year < 2020)
  hmrS$date <- as.POSIXct(hmrS$datetimestamp)
  hmrS$yClass <- factor (ifelse (hmrS$year < currentYear, "average", currentYear))

  pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/windRose_", metstation, ".pdf")
       , width=9, height=6)
  windRose(hmrS, ws="wspd", wd="wdir"
           # , type="yClass"
           , type=c("season", "yClass")
           #         , type=c("month", "yClass")
           , auto.text=TRUE, paddle=FALSE, annotate=FALSE
           #         , breaks=c (0, 10, 20, 25,30,50)
           , breaks=c(0, 10, scAdvT, 50)
           , key.footer="knots"
           #         , breaks=c(0,20, 30, 40)
           #, max.freq=30
  )
  dev.off()




  # ### base-graphics windrose
  # dM <- subset (dMeans, jday >= 335) # 1 Dec=335
  #
  # windR <- function (subsV, ...){
  #   require ("climatol")
  #   wndfr <- with (subset (dM, subsV)
  #                  , table (cut (wspd, breaks=c(0,3,6,9,60), include.lowest=TRUE)
  #                           , cDir (wdir, nDir=16)))
  #   # convert table to data.frame so rosavent will accept it
  #   wndfr <- reshape (as.data.frame(wndfr), timevar="Var2", idvar="Var1", direction="wide")
  #   row.names (wndfr) <- wndfr$Var1
  #   wndfr <- wndfr [,2:ncol (wndfr)]
  #   names (wndfr) <- gsub ("Freq.", "", names (wndfr))
  #   rosavent (as.data.frame (wndfr), uni=vUnit, key=TRUE, flab=1)
  # }
  # # require("Hmisc") # subplot incompatible with layout() :(
  # # spSi <- 1.7
  # # subplot (windR (dM$year < currentYear)
  # #          , x=160, y=12.8, vadj=1, size=c(spSi, spSi))
  # # text (160, 13, "mean Dec")
  # # subplot (windR (dM$year == currentYear)
  # #          , x=260, y=12.8, vadj=1, size=c (spSi, spSi)) #, main=currentYear)
  # # text (260, 13, paste ("Dec", currentYear))
  # pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/windRoseBase.pdf", width=9, height=6)
  # par (mfrow=c(1,2))
  # windR (dM$year < currentYear)
  # windR (dM$year == currentYear)
  # dev.off()
  # rm (dM)


  # library(devtools)
  # install_github("tomhopper/windrose")   ## not that pretty -- looks like ggplot2
  # require ("windrose")
  # data(wind_data)
  # wind_rose <- windrose(wind_data, spd=Wind_Speed_meter_per_second, dir=Wind_Direction_deg)
  # plot(wind_rose)

  # require ("clifro")  # builds on ggplot2
  # example (windrose)




  ##################################
  ## Walter Leith climate diagram ##
  ##################################

  ## monthly temp and precip
  climD <- function (cDF){
    ## let it fail or interpolate NAs?
    ymC <- aggregate (atemp~year+month, cDF, FUN=max, na.rm=TRUE)
    ymC$minT <- aggregate (atemp~year+month, cDF, FUN=min, na.rm=TRUE)$atemp
    rain <- aggregate (totprcp~year+month, cDF, FUN=sum, na.rm=TRUE)
    ymC$precip <- rain$totprcp [match (paste (ymC$year, ymC$month), paste (rain$year, rain$month))]
    monthC <- aggregate (precip~month, ymC, mean, na.rm=TRUE)
    monthC$maxT <- aggregate (atemp~month, ymC, mean, na.rm=TRUE)$atemp
    monthC$minT <- aggregate (minT~month, ymC, FUN=mean, na.rm=TRUE)$minT
    monthC$absMin <- aggregate (atemp~month, cDF, FUN=min, na.rm=TRUE)$atemp
    monthT <- t (monthC [,2:ncol (monthC)])
    if (nrow (monthC) == 12){ ## interpolate NAs?
      colnames (monthT) <- month.abb
    }else{
      colnames (monthT) <- month.abb [monthC$month] # match (monthC$month, 1:12)]
    }
    monthT
  }

  # png ("~/tmp/LCI_noaa/media/climateDiag.png", width=480, height=960)
  # par (mfrow=c(2,1))
  if (0){
  # if (metstation == "kachomet"){
    cat ("\nTry to make climate diagram [requires full year of precip data]\n")
    pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/climateDiag.pdf")
    ## alternative: wldiag in dgolicher/giscourse on github
    ## for same but with more customization, see library (iki.dataclim)
    require ("climatol")
    ## Error in sprintf("%d-%d", yeari, yearf) :
    ## invalid format '%d'; use format %f, %e, %g or %a for numeric objects
    diagwl (dat=climD (subset (hmr, year < currentYear))  ## diagwl currently producing error
            # , cols=1:12
            , cols=NULL   ## undocumented bug in clitools.R of climatol::diagwl
            , stname="Homer Spit" #, alt=10
            , per=paste (range (subset (hmr, year < currentYear)$year, na.rm=TRUE), collapse="-")
    )
    tD <- try ({  ## fails if, e.g. hmr$atemp contains NAs, e.g. September 2021
      diagwl (climD (subset (hmr, year == currentYear))
              , stname="Homer Spit"
              , per=currentYear)
    }, silent=TRUE)
    if (class (tD) == "try-error"){
      cat ("Climate diagram is incomplete (due to missing data?)\n\n")
    }
    dev.off()
  }



## Compare wind roses of Homer Spit and Flat Island
if (metstation == "kachomet"){

  hmr2 <- getNOAA ("FILA2") # fetch Flat Island data from NOAA
  fi <- with (hmr2, data.frame (datetimestamp, atemp=air_temperature
                                , rh=rep (is.na (nrow (hmr2)))  # barometric pressure
                                , bp=rep (is.na (nrow (hmr2)))  # relative humidity
                                , wspd=wind_spd # m/s -- same as SWMP  # NOAA wspd in m/s
                                , maxwspd=gust
                                , wdir=wind_dir
                                , sdwdir=rep (is.na (nrow (hmr2)))
                                , totpar=rep (is.na (nrow (hmr2)))
                                , totprcp=rep (is.na (nrow (hmr2)))
                                , totsorad=rep (is.na (nrow (hmr2)))
  ))
  if (exists (vUnit)){ #} == "knots"){
    fi$wspd <- fi$wspd * 1.94384   ## 1 knot=1.94384 m/s
    fi$maxwspd <- fi$maxwspd * 1.94384
    wCaption <- "wind speed [knots]"
  }else{
    wCaption <- "wind speed [m/s]"
  }
  fi <- addTimehelpers (fi)
  hmrS <- rbind (cbind (station=paste0 ("Flat Island - ", currentYear), fi)
                 , cbind (station=paste0 ("Homer Spit - ", currentYear), hmr))
  rm (hmr2)
  hmrS <- subset (hmrS, year == currentYear)  ## make sure this stays (see label)
  hmrS$date <- as.POSIXct(hmrS$datetimestamp)
  hmrS$station <- as.factor (hmrS$station)



  ## compare wind Flat Island with Homer Spit -- windroses
  pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-windRose_Locations2.pdf")
       , width=6, height=4)
  windRose(hmrS, ws="wspd", wd="wdir"
           # , type="yClass"
           , type="station"
           #, type=c("season", "station")
           , auto.text=TRUE, paddle=FALSE, annotate=FALSE
           #         , breaks=c (0, 10, 20, 25,30,50)
           , breaks=c(0, 10, scAdvT, ceiling (max (hmrS$wspd, na.rm = TRUE)))
           , key.footer="knots"
           #         , breaks=c(0,20, 30, 40)
           #, max.freq=30
  )
  dev.off()
}
}

cat ("Finished annual-wind.R\n")
# EOF
