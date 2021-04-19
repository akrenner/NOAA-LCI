
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
  rm (nw, endD)

  for (i in 1:length (wB)){
    if (!exists ("wDB")){
      wDB <- as.data.frame (wB [[i]]$data)
    }else{
      if (class (wB [[i]]) != "try-error"){
        wDB <- rbind (wDB, as.data.frame (wB [[i]]$data))
      }
    }
  }
  rm (wB)
}
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
  rm (aB, tl, pF)
  save (aDB, file = "~/tmp/LCI_noaa/cache/noaa-Augustine.RData")
}
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
  rm (dailyW)
}

# pdf ("~/tmp/LCI_noaa/media/wavePeriod.pdf")
# plot (wave_height~dominant_wpd, tDay)
# plot (wave_height~average_wpd, tDay)
# dev.off()



save.image("~/tmp/LCI_noaa/cache/annual_waves2.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/annual_waves2.RData")

currentCol <- c("blue", "lightblue")
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

for (i in 2011:2018){
  try (lines (tDayW$jday
        , tDayW [,which (names (tDayW) == paste0 ("y_", i, "_wave_height"))]
        , lwd = 0.5, col = i))
}
legend ("bottomleft", bty = "n", legend = 2011:2018, col = 2011:2018, lwd = 0.5)
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

## EOF
