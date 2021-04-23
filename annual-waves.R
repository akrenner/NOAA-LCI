
################################
## wave height from bouy data ##
################################


rm (list = ls())

## also wave direction?
if (0){
  bs <- buoy_stations ()
  leaflet (data = bs) %>%
    addTiles() %>%
    addCircles()

  bs [grep ("46108", bs$station),]  ## lower cook inlet

  dset <- c ('adcp','adcp2','cwind','dart','mmbcur','ocean'
             ,'pwind', 'stdmet', 'swden', 'wlevel')
  stn <- c (46105, 46108, "HMRA2")  # near Barrens, outer KBay, spit

  for (i in 1:length (dset)){
    cat (dset [i])
    try (print (head (buoy (dset [i], buoyid = stn [1]))))
  }
  rm (i)

  lci <- buoy (dataset = 'swden', buoyid = 46108)  # spectral wave height
  lci <- buoy (dataset = 'dart', buoyid = 46108, year = 2020)

  lci <- buoy (dataset = 'adcp', buoyid = 46108, year = 2020)  # doppler
  # lci <- buoy (dataset = 'adcp2', buoyid = 46108, year = 2020)
  # lci <- buoy (dataset = 'cwind', buoyid = 46108, year = 2020)
  # lci <- buoy (dataset = 'dart', buoyid = 46108, year = 2020)
  # lci <- buoy (dataset = 'mmbcur', buoyid = 46108, year = 2020)
  # lci <- buoy (dataset = 'ocean', buoyid = 46108, year = 2020)
  # lci <- buoy (dataset = 'pwind', buoyid = 46108, year = 2020)
  lci <- buoy (dataset = 'stdmet', buoyid = 46108, year = 2020)
  lci <- buoy (dataset = 'swden', buoyid = 46108, year = 2020)
  # lci <- buoy (dataset = 'wlevel', buoyid = 46108, year = 2020)


  lci <- buoy (dataset = "stdmet", buoyid = 46108, year = 2021) # includes waves
  bT <- as.POSIXct(lci$data$time)
  summary (bT)
}


## fetch buoy data from NOAA server
## only load new data, fetch rest from local cache
# to reset: unlink ("~/tmp/LCI_noaa/cache/noaawaves.RData")
require ("rnoaa")
nw <- try (load ("~/tmp/LCI_noaa/cache/noaawaves.RData"))
# unlink ("~/tmp/LCI_noaa/cache/noaawaves.RData")
if (class (nw) == "try-error"){
  endD <- 2011
}else{
  endD <- max (as.numeric (substr (wDB$time, 1, 4)))
}
if (endD < as.numeric (format (Sys.Date(), "%Y"))){
  wB <- lapply (endD:as.numeric (format (Sys.Date(), "%Y"))
                , function (i){
                  try (buoy (dataset = "stdmet", buoyid = 46108
                             , year = i))
                }
  )

  for (i in 1:length (wB)){
    if (!exists ("wDB")){
      wDB <- as.data.frame (wB [[i]]$data)
    }else{
      if (class (wB [[i]]) != "try-error"){
        wDB <- rbind (wDB, as.data.frame (wB [[i]]$data))
      }
    }
  }
  rm (wB, i)
}
rm (nw, endD)

## add most recent
cD <- try (buoy (dataset="stdmet", buoyid = 46108, year = 9999))
if (class (cD) == "buoy"){
  wDB <- rbind (wDB, as.data.frame (cD$data))
}
rm (cD)

save.image ("~/tmp/LCI_noaa/cache/noaawaves.RData") ## cache of buoy data
# unlink ("~/tmp/LCI_noaa/cache/noaawaves.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/noaawaves.RData")

## QAQC
wDB <- wDB [!duplicated(wDB$time),]
tm <- gsub ("T", "", wDB$time)
tm <- gsub ("Z", "", tm)
wDB$datetimestamp <- as.POSIXct (tm, format = "%F %T", tz = "UTC")
rm (tm)
is.na (wDB$wave_height)[which (wDB$wave_height == 99)] <- TRUE
is.na (wDB$dominant_wpd)[which (wDB$dominant_wpd == 99)] <- TRUE  # dominant wave period [s]
is.na (wDB$average_wpd)[which (wDB$average_wpd == 99)] <- TRUE  # dominant wave period [s]
# wDB <- subset (wDB, !duplicated(wDB$datetimestamp))

# wDB$datetimestamp <- strptime (wDB$time
#                                 #, format = "%Y-%m-%dT%H:%M%SZ"
#                                 , tz = "UTC")
# plot (wave_height~datetimestamp, wDB, type = "l")


## Augustine Island wind -- as covariate to KBay wind?
# Aug <- isd ("994700", wban = 99999, year = 2020)
tl <- try (load ("~/tmp/LCI_noaa/cache/noaa-Augustine.RData"))
if (class (tl) == "try-error"){
  aB <- lapply (as.numeric (levels (factor (format (aDB$datetimestamp, "%Y"))))
                , function (x){try (isd ("994700", wban = 99999, year = x))}
  )
  pF <- function (df){
    with (as.data.frame (df), data.frame (date, time, date_flag, quality, wind_direction, wind_direction_quality
                                          , wind_code, wind_speed, wind_speed_quality))
  }
  for (i in 1:length (aB)){
    if (!exists ("aDB")){
      aDB <- pF (aB [[i]])
    }else{
      if (class (aB [[i]])[1] != "try-error"){
        aDB <- rbind (aDB, pF (aB [[i]]))
      }
    }
  }
  rm (aB, pF, i)
  save (aDB, file = "~/tmp/LCI_noaa/cache/noaa-Augustine.RData")
}
rm (tl)
source ("annualPlotFct.R")
aDB$datetimestamp <- as.POSIXct (with (aDB, paste (date, time)), format = "%Y%m%d %H%M")
aDB <- addTimehelpers(aDB)


require ("dplyr")
hmr <-  meteo_pull_monitors ("USW00025507"
                              , date_min = "1970-01-01"  # goes back to 1932-09-01
                              , date_max = as.character (Sys.Date())) %>%
  dplyr::rename (datetimestamp = date, location = id
                 #, totprcp = prcp # wdfg not ideal equivalent
                 #, atemp = tavg
                 , wspd = awnd, maxwspd = wsfg, wdir = wdfg) %>%
  addTimehelpers ()

## cross-correlation wind and waves
## linear model waves~windH + windA








if (1){ ## mean daily wave height -- for when do we have data

  dailyW <- aggregate (wave_height~format (datetimestamp
                                           , "%Y-%m-%d")
                       , wDB, FUN = mean)
  names (dailyW)[1] <- "date"
  ## max daily wave height
  dailyW$maxW <- aggregate (wave_height~format (datetimestamp
                                                , "%Y-%m-%d")
                            , wDB, FUN = max)$wave_height
  dailyW$date <- as.POSIXct (dailyW$date, format = "%Y-%m-%d")

  pdf ("~/tmp/LCI_noaa/media/waveData.pdf")
  plot (wave_height~datetimestamp, wDB, type = "l")
  plot (wave_height~date, dailyW, type = "l")
  plot (wave_height~average_wpd, wDB)
  hist (wDB$mean_wave_dir)
  abline (v = c(50, 150, 290))
  wDB$wave_dir_cat <- cut (wDB$mean_wave_dir, breaks = c(0, 50, 150, 290, 365)
                           , labels = c("NW", "E", "SW", "NW")
                           , include.lowest = TRUE, right = TRUE) # right = closed to right, open on left
  for (i in 1:length (levels (wDB$wave_dir_cat))){
    plot (wave_height~average_wpd, wDB
          , subset = wDB$wave_dir_cat == levels (wDB$wave_dir_cat)[i]
          , main = levels (wDB$wave_dir_cat)[i]
          , ylim = range (wDB$wave_height, na.rm = TRUE)
          , xlim = range (wDB$average_wpd, na.rm = TRUE)
          )
  }
  wDBx <- wDB [order (wDB$wave_dir_cat),]
  plot (wave_height~average_wpd, wDBx, col = wDB$wave_dir_cat)
  rm (wDBx)
  dev.off()
  rm (dailyW, i)
}

# pdf ("~/tmp/LCI_noaa/media/wavePeriod.pdf")
# plot (wave_height~dominant_wpd, tDay)
# plot (wave_height~average_wpd, tDay)
# dev.off()



save.image("~/tmp/LCI_noaa/cache/annual_waves2.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/annual_waves2.RData")


## order: current, present, previous
# currentCol <- c("darkblue", "blue", "lightblue")
currentCol <- c("darkblue", "blue", "lightblue")

currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
# maO <- 3 # 30
maO <- 30
qntl = 0.9

source ("annualPlotFct.R")

# tDay <- fixGap (tDay)
tDay <- addTimehelpers (wDB)
tDayW <- prepDF (dat = tDay, varName = "wave_height"
                #, maO = maO, qntl = qntl
               , sumFct = mean
   #             , sumFct = max
                , maO = maO
                , qntl = qntl
)


pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-waves.pdf"), width = 9, height = 6)
par (mar = c(3,4,1,4)) # space for 2nd y-axis (feet)

aPlot (tDayW, "wave_height", ylab = "wave height [m]"
       , currentCol = currentCol
       , MA = TRUE
#       , ylim = c (0,1.2)
)
box()
# lines (tDay$jday, tDay [,which (names (tDay) == paste0 ("y_", currentYear - 1, "_wave_height"))]
#        , lty = "dashed", lwd = 2, col = currentCol [1])
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)

wFt <- tDayW$wave_height / 0.3048
# fAxis (c (-15, 15), mT = expression ('wave height [feet]'))
alt.ax <- pretty (wFt)
alt.at <- (alt.ax) * 0.3048
axis (side = 4, at = alt.at, labels = alt.ax, srt = 90)
mtext ("wave height [ft]", side = 4, line = 2.5)
rm (wFt, alt.ax, alt.at)
# for (i in 2011:2018){
#   x <- try (lines (tDayW$jday
#         , tDayW [,which (names (tDayW) == paste0 ("y_", i, "_wave_height"))]
#         , lwd = 0.5, col = i))
# }
# rm (x)
# legend ("bottomleft", bty = "n", legend = 2011:2018, col = 2011:2018, lwd = 0.5)
dev.off()
rm (tDayW)




pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-wavesPD.pdf", width = 9, height = 6)
# maO <- 30
tDayP <- prepDF (dat = tDay, varName = "dominant_wpd", maO = maO)
aPlot (tDayP, "dominant_wpd", ylab = "dominant wave period [s]"
       , currentCol = currentCol
       , MA = TRUE
       #       , ylim = c (0,1.2)
)
box()
# lines (tDay$jday, tDay [,which (names (tDay) == paste0 ("y_", currentYear - 1, "_wave_height"))]
#        , lty = "dashed", lwd = 2, col = currentCol [1])
cLegend ("bottomleft"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)
tDayP <- prepDF (dat = tDay, varName = "average_wpd") #, maO = 1)
aPlot (tDayP, "average_wpd", ylab = "average wave period [s]"
       , currentCol = currentCol
       , MA = TRUE
       #       , ylim = c (0,1.2)
)
box()
cLegend ("bottomleft"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)
dev.off()
rm (tDayP)



## when were surf conditions good?
## long-term = logistic model?


## good surf conditions:
## wave period: > 5 s
## wave height: > 1 m
## tide: at least xx feet (ask Vince)
## wind offshore
## daylight
## => epic!


## find tide
## find daylight
require ("rtide")
# tStn <- tide_stations("Kasitsna.*")
tStn <- tide_stations("Seldovia*")
timetable <- data.frame (Station = tStn, DateTime = wDB$datetimestamp)
wDB$tideHght <- tide_height_data (timetable)$TideHeight  # slow -- cache it?
rm (tStn, timetable)
require ("suncalc")
wDB$sunAlt <- getSunlightPosition (date = wDB$datetimestamp
                                   , lat = 59.643, lon = -151.526)$altitude # in radians
## interpolate home wind direction -- fail (why = ?)
## need hourly or better data => SWMP.  Use buoy data for now
# wDB$windDir_hom x <- approx (hmr$datetimestamp, hmr$wdf5, xout = wDB$datetimestamp)

## wind from SWMP station on Homer Spit
load ("~/tmp/LCI_noaa/cache/metDat.RData")  # yields "hmr"
wDB$windDir <- approx (hmr$datetimestamp, hmr$wdir, xout = wDB$datetimestamp)$y
wDB$windspd <- approx (hmr$datetimestamp, hmr$wspd, xout = wDB$datetimestamp)$y

wDB <- addTimehelpers(wDB)

save.image ("~/tmp/LCI_noaa/cache/wavesSurf.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/wavesSurf.RData")

################
## surf score ##
################

wDB$onshoreWind <- ifelse ((120 < wDB$windDir) | (wDB$windDir < 270), TRUE, FALSE)


wDB$surf <- 0
wDB$surf <- ifelse (wDB$wave_height > 1, wDB$surf + (wDB$wave_height-1)*3, wDB$surf)
wDB$surf <- ifelse (wDB$dominant_wpd > 10, wDB$surf + (wDB$dominant_wpd - 10)/2, wDB$surf) # make this gradual?
wDB$surf <- ifelse (wDB$wave_dir_cat == "SW", wDB$surf + 1, wDB$surf)
wDB$surf <- ifelse (wDB$tideHght > 5, wDB$surf + 1, wDB$surf) # according to Vince, 16'
# wDB$surf <- ifelse (wDB$windDir)
## wind: onshore / offshore
wDB$surf <- ifelse ((120 < wDB$windDir) & (wDB$windDir < 270) & (wDB$windspd > 5)
                    , wDB$surf - 5, wDB$surf)
wDB$surf <- ifelse ((wDB$windDir > 270) | (wDB$windDir < 120), wDB$surf + 1, wDB$surf)

## deal-breakers
wDB$surf <- ifelse (wDB$wave_height > 1, wDB$surf, 0)
wDB$surf <- ifelse (wDB$sunAlt > 0, wDB$surf, 0)  # NA?
wDB$surf <- ifelse (wDB$average_wpd > 5, wDB$surf, 0) # make this gradual?

wDB$surf <- ifelse (wDB$surf < 0, 0, wDB$surf)
summary (wDB$surf) # 4,8,16




pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-surf.pdf")

plot (surf~datetimestamp, wDB, ylab = "surf-score")
plot (dominant_wpd~datetimestamp, wDB)
plot (average_wpd~datetimestamp, wDB)
hist (wDB$dominant_wpd)
hist (wDB$average_wpd)
hist (wDB$tideHght)
hist (wDB$surf)
hist (subset (wDB$surf, wDB$surf > 0))

surfC <- aggregate (surf ~ jday + year, wDB # calendar
                       , FUN = function (x){mean (x > 1)}
                       )
summary (surfC$surf)
plot (surf~jday, surfC
      , pch = 19, col = ifelse (year == 2021, "red"
                                , ifelse(year == 2020, "blue", "black"))
      , ylab = "daily surf score")
legend ("top", bty = "n", pch = 19, col = c("red", "blue", "black"),
        legend = c("2021", "2020", "2011-2019"))


wDB$surfs <- ifelse (wDB$surf > 1, 1, 0)
sTday <- prepDF(wDB, "surfs"
#                , sumFct = function (x){mean (x, na.rm = TRUE)}
                , sumFct = function (x){any (x > 0)}
)
aPlot (sTday, "surfs", ylab = "chance of good surf"
       , currentCol = currentCol, MA = TRUE, main = "Days with surf")
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)

sTday <- prepDF(wDB, "surfs")
aPlot (sTday, "surfs", ylab = "chance of good surf"
       , currentCol = currentCol, MA = TRUE, main = "Proportion of time with surf")
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)

wDB$surfs <- ifelse (wDB$surf > 4, maO, 0)
sTday <- prepDF(wDB, "surfs", sumFct = function (x){any (x > 0)})
aPlot (sTday, "surfs", ylab = "days with GREAT surf"
       , currentCol = currentCol, MA = TRUE, main = paste ("Days per", maO, "days"))
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)


sTday <- prepDF(wDB, "surf")
aPlot (sTday, "surf", ylab = "chance of good surf"
       , currentCol = currentCol, MA = TRUE, main = "Mean surf score")
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)


dev.off()


## https://geologycafe.com/oceans/chapter10.html
## shallow water wave:
## approaching shore, wave period stays constant,
## but wave length and velocity decrease
## => wave height increases
## wave breaks when depth (d) is about the same as wave height (h).
## wave starts to break when wave height to wavelength exceeds a ratio of 1:7
## gentle slope: spilling breakers
## moderate slopes: plunging breakers
## sttep slope: surging breakers


# ## smoothing spline, cyclical
# require ("pbs")
# mdl <- glm (surf~pbs:pbs (jday, df = 4, Boundary.knots = c(1,366)), data = surfC)


## EOF
