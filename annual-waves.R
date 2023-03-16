
################################
## wave height from bouy data ##
################################

# setwd("~/myDocs/amyfiles/NOAA-LCI/")


if (!exists ("quarterly")){
  rm (list=ls())
  quarterly <- TRUE
}


## order: current, present, previous
require ("RColorBrewer")
currentCol <- c ("black", brewer.pal (4, "Paired"))[c(1,3,2)]
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
# maO <- 3 # 30
maO <- 30
qntl = 0.9



mediaD <- "~/tmp/LCI_noaa/media/StateOfTheBay/"
if (quarterly){
  mediaD <- paste0 (mediaD, "update/")
  currentCol <- currentCol [c(1,3,2)]
}else{

}
dir.create(mediaD, showWarnings=FALSE, recursive=TRUE)


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

source ("annualPlotFct.R")
wDB <- getNOAA (buoyID = 46108)


## is all the Augustine Island part obsolete? replicated elsewhere? XXX
## Augustine Island wind -- as covariate to KBay wind?
# Aug <- isd ("994700", wban = 99999, year = 2020)
if (0){
  tl <- try (load ("~/tmp/LCI_noaa/cache/noaa-Augustine.RData"))
  if (class (tl) == "try-error"){
    aB <- lapply (as.numeric (levels (factor (format (wDB$datetimestamp, "%Y"))))
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
  aDB$datetimestamp <- as.POSIXct (with (aDB, paste (date, time)), format = "%Y%m%d %H%M")
  aDB <- addTimehelpers(aDB)
}

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
source ("annualPlotFct.R")




# tDay <- fixGap (tDay)
tDay <- addTimehelpers (wDB)
tDayW <- prepDF (dat = tDay, varName = "wave_height"
                #, maO = maO, qntl = qntl
              , sumFct = mean
   #             , sumFct = max
              , maO = maO
              , qntl = qntl
              , currentYear=currentYear
   )


# pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-waves.pdf"), width = 9, height = 6)
png (paste0 (mediaD, "sa-waves.png"), width = 1800, height = 1200, res = 300)
par (mar = c(3,4,1,4)) # space for 2nd y-axis (feet)

aPlot (tDayW, "wave_height", ylab = "wave height [m]"
       , currentCol = currentCol
       , MA = TRUE
       #       , ylim = c (0,1.2)
       , pastYear=FALSE, ongoingYear=TRUE
)
box()
# lines (tDay$jday, tDay [,which (names (tDay) == paste0 ("y_", currentYear - 1, "_wave_height"))]
#        , lty = "dashed", lwd = 2, col = currentCol [1])
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
         , pastYear=FALSE, ongoingYear=TRUE
)

wFt <- tDayW$wave_height / 0.3048
# fAxis (c (-15, 15), mT = expression ('wave height [feet]'))
alt.ax <- pretty (wFt)
alt.at <- (alt.ax) * 0.3048
axis (side = 4, at = alt.at, labels = alt.ax, srt = 90)
mtext ("wave height [ft]", side = 4, line = 2.5)
# for (i in 2011:2018){
#   x <- try (lines (tDayW$jday
#         , tDayW [,which (names (tDayW) == paste0 ("y_", i, "_wave_height"))]
#         , lwd = 0.5, col = i))
# }
# rm (x)
# legend ("bottomleft", bty = "n", legend = 2011:2018, col = 2011:2018, lwd = 0.5)
dev.off()
rm (tDayW, wFt, alt.ax, alt.at)




pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-wavesPD.pdf", width = 9, height = 6)
# maO <- 30
tDayP <- prepDF (dat = tDay, varName = "dominant_wpd", maO = maO, currentYear=currentYear)
aPlot (tDayP, "dominant_wpd", ylab = "dominant wave period [s]"
       , currentCol = currentCol
       , MA = TRUE
       #       , ylim = c (0,1.2)
       , pastYear = FALSE, ongoingYear = TRUE
)
box()
# lines (tDay$jday, tDay [,which (names (tDay) == paste0 ("y_", currentYear - 1, "_wave_height"))]
#        , lty = "dashed", lwd = 2, col = currentCol [1])
cLegend ("bottomleft"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
         , pastYear = FALSE, ongoingYear = TRUE
)
tDayP <- prepDF (dat = tDay, varName = "average_wpd", currentYear=currentYear) #, maO = 1)
aPlot (tDayP, "average_wpd", ylab = "average wave period [s]"
       , currentCol = currentCol
       , MA = TRUE
       #       , ylim = c (0,1.2)
       , pastYear = FALSE, ongoingYear = TRUE
)
box()
cLegend ("bottomleft"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
         , pastYear = FALSE, ongoingYear = TRUE
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
require ("lubridate") # time zone conversion
wDB$localTime <- with_tz (wDB$datetimestamp, "America/Anchorage")
require ("suncalc")
wDB$sunAlt <- getSunlightPosition (date = wDB$localTime
                                   , lat = 59.643, lon = -151.526)$altitude # in radians
wDB$sunDeg <- wDB$sunAlt / pi * 180
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

wDB$onshoreWind <- ifelse ((120 < wDB$windDir) & (wDB$windDir < 270), TRUE, FALSE)

## good conditions
if (0){
  wDB$surf <- 0
wDB$surf <- ifelse (wDB$wave_height > 1.0, wDB$surf + (wDB$wave_height-1)*3, wDB$surf)
wDB$surf <- ifelse (wDB$dominant_wpd > 6, wDB$surf + (wDB$dominant_wpd - 6)/2, wDB$surf) # make this gradual?
# wDB$surf <- ifelse (wDB$wave_dir_cat == "SW", wDB$surf + 1, wDB$surf)
wDB$surf <- ifelse ((220 < wDB$mean_wave_dir) & (wDB$mean_wave_dir < 270), wDB$surf + 1, wDB$surf)
# wDB$surf <- ifelse (wDB$tideHght > 4.5, wDB$surf + 1, wDB$surf) # according to Vince, 16'
wDB$surf <- ifelse (wDB$tideHght > 2, wDB$surf + 1, wDB$surf) # according to Vince, 16'

# wDB$surf <- ifelse (wDB$windDir)
## wind: onshore / offshore
wDB$surf <- ifelse ((120 < wDB$windDir) & (wDB$windDir < 270) & (wDB$windspd > 5)
                    , wDB$surf - 5, wDB$surf)  ## onshore wind = bad
wDB$surf <- ifelse ((wDB$windDir > 270) | (wDB$windDir < 120), wDB$surf + 1, wDB$surf)


## deal-breakers
wDB$surf <- ifelse (wDB$wave_height > 0.1, wDB$surf, 0)
wDB$surf <- ifelse (wDB$sunDeg > -6.0, wDB$surf, 0)  # within civil twilight,  < 6 degree below horizon

wDB$surf <- ifelse (wDB$dominant_wpd > 5, wDB$surf, 0)
wDB$surf <- ifelse (wDB$mean_wave_dir < 190, 0, wDB$surf) # need SW-erly swell
wDB$surf <- ifelse (wDB$mean_wave_dir > 275, 0, wDB$surf)
wDB$surf <- ifelse ((120 < wDB$windDir) & (wDB$windDir < 270) & (wDB$windspd > 15)
                    , 0, wDB$surf) # onshore wind = bad

wDB$surf <- ifelse (wDB$surf < 0, 0, wDB$surf)
summary (wDB$surf) # 4,8,16
}

## start over -- keep it simple
## make it great: big waves, long period, offshore wind
wDB$sscore <- 0
wDB$sscore <- ifelse (wDB$wave_height > 2, 2, wDB$sscore)
wDB$sscore <- ifelse (wDB$dominant_wpd > 10, wDB$sscore + 1, wDB$sscore)
wDB$sscore <- ifelse ((wDB$windDir < 120) | (wDB$windDir > 270), wDB$sscore + 1, wDB$sscore)

## minimum criteria:
## waves 1 m+, SW swell, tide 5m+, daylight, no strong onshore wind
wDB$sscore <- ifelse (wDB$wave_height> 1, wDB$sscore, 0)
wDB$sscore <- ifelse (wDB$dominant_wpd > 5, wDB$sscore, 0)
wDB$sscore <- ifelse ((220 < wDB$mean_wave_dir) & (wDB$mean_wave_dir < 270), wDB$sscore, 0)
wDB$sscore <- ifelse (wDB$tideHght > 4.5, wDB$sscore, 0)
wDB$sscore <- ifelse (wDB$sunDeg > -6, wDB$sscore, 0)
wDB$sscore <- ifelse ((120 < wDB$windDir) & (wDB$windDir < 270) & (wDB$windspd > 15)
                     , 0, wDB$sscore)  ## strong onshore -- take this out?

wDB$surf <- wDB$sscore

### END of surfscore ##
#######################




pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/surf-exploration.pdf")

plot (surf~datetimestamp, wDB, ylab = "surf-score")
plot (dominant_wpd~datetimestamp, wDB)
plot (average_wpd~datetimestamp, wDB)
hist (wDB$dominant_wpd)
hist (wDB$average_wpd)
hist (wDB$tideHght)
hist (wDB$surf)
hist (subset (wDB$surf, wDB$surf > 0))

## time series of surf score
surfC <- aggregate (surf ~ jday + year, wDB # calendar -- proportion of time > 1
#                    , FUN = function (x){mean (x >= 1)}
                    , FUN = mean
)
summary (surfC$surf)
plot (surf~jday, surfC
      , pch = 19, col = ifelse (year == 2021, "red"
                                , ifelse(year == 2020, "blue", "black"))
      , ylab = "daily mean surf score")
legend ("top", bty = "n", pch = 19, col = c("red", "blue", "black"),
        legend = c("2021", "2020", "2011-2019"))

## proportion of surfable time
wDB$surfs <- ifelse (wDB$surf >= 1, 1, 0)
sTday <- prepDF(wDB, "surfs"
              , sumFct = function (x){mean (x >= 1)}
              , currentYear=currentYear
)
aPlot (sTday, "surfs", ylab = "propotion of surfable time"
       , currentCol = currentCol, MA = TRUE, main = paste ("Daily surf"))
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)


## proportion of surf days
wDB$surfs <- ifelse (wDB$surf >= 1, 1, 0)
sTday <- prepDF(wDB, "surfs"
                #                , sumFct = function (x){mean (x, na.rm = TRUE)}
                # , sumFct = function (x){sum (x >= 1) > 1 }
                , sumFct = function (x){maO * any (x >= 1)}
                , currentYear=currentYear
)
aPlot (sTday, "surfs", ylab = "days with surf"
       , currentCol = currentCol, MA = TRUE
       , main = paste ("Days per", maO, "days with surf"))
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)


## proportion of GREAT days
wDB$surfs <- ifelse (wDB$surf >= 2, 1, 0) # use maO so that mean over MA windows = N days
sTday <- prepDF(wDB, "surfs", sumFct = function (x){maO * any (x > 0)}
                , currentYear=currentYear
)
aPlot (sTday, "surfs", ylab = "days with GREAT surf"
       , currentCol = currentCol, MA = TRUE, main = paste ("Days per", maO, "days"))
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)
## mean score per day
sTday <- prepDF(wDB, "surf", currentYear=currentYear)
aPlot (sTday, "surf", ylab = "mean surf score"
       , currentCol = currentCol, MA = TRUE #, main = "Mean surf score"
       )
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)

## airtemp when surf is great
hist (subset (wDB$air_temperature, wDB$surf > 1)
      , main = "Air temperature when surf is good"
      , xlab = expression ('air'~'temperature '~'['*degree~'C'*']'))

wSpd <- wDB$windspd / 1.852
windchill <- 13.12 + 0.6215*wDB$air_temperature - 11.37 * wSpd ^ 0.16 +
  0.3965*wDB$air_temperature*wSpd^0.16
rm (wSpd)
hist (subset (windchill, wDB$surf >= 1)
      , xlab = expression ('windchill when surf is good '~'['*degree~'C'*']'))
# hist (subset (wDB$sea_surface_temperature, wDB$surf > 4)
#       , main = "Water temperature when surf is good"
#       , xlab = "temperature [C]")


dev.off()


## number of good and great surfdays
## only useful if there's a complete record for the year!
sEvent <- nEvents (wDB, "surf", thrht = c(1,4))
print (sEvent)




### the plot to keep for the report
## try to do it all in R
# pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-surf.pdf"
#      , width = 8, height = 6)
png (paste0 (mediaD, "sa-surf.png"), width = 1600, height = 1200, res = 200)
## import image, use ggplot to set background
## see https://guangchuangyu.github.io/2018/04/setting-ggplot2-background-with-ggbackground/
# require(magick)
# require (ggplot2)
# require (ggimage)  ## yes, this is what I want,
## no ggplot too complex with existing functions
wDB$surfs <- ifelse (wDB$surf > 1, 1, 0)
sTday <- prepDF(wDB, "surfs"
                , sumFct = function (x){maO * any (x >= 1)}
)
aPlot (sTday, "surfs", ylab = "days with surf"
       , currentCol = currentCol, MA = TRUE, main = paste ("Days per", maO, "days with surf"))
require ("jpeg")
# img <- readJPEG ("~/My Pictures/surf/_J5A9758-s.jpg", native = TRUE) # fall
# img <- readJPEG ("~/My Pictures/surf/_J5A9729-sc.jpg", native = TRUE)
img <- readJPEG ("pictograms/_J5A9729-sc.jpg", native = TRUE)
lim <- par()
# img <- img^0.5
# require ("magick")
# img <- image_contrast(img, sharpen = 0.5)

rasterImage (img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4]) # covers up plot above. Just add lines now
# extending limits will not plot past the plotting area, but result in crop
# on special request, only lines, no percentile polygon
box()
lines (MA_surfs~jday, data = sTday, col = "black", lwd = 3)
# lines (pcYMA_surfs~jday, data = sTday, col = currentCol [1], lwd = 3)
lines (pYMA_surfs~jday, data = sTday, col = currentCol [2], lwd = 4)
lines (ogYMA_surfs~jday, data = sTday, col = currentCol [3], lwd = 4)
lL <- legend ("top", bty = "n", legend = "")
# legend ("top", title = paste0 (maO, "day moving average")
legend (lL$rect$left - 72, lL$rect$top - 0.1
        , bty = "0"
        , box.col = "gray"
        , bg = rgb (200,200,200, max = 255, alpha = 125, names = "tgray")
        , legend = c(paste0 ("mean [", min (as.numeric (format (wDB$datetimestamp, "%Y")))
                             , "-", currentYear-1, "]"), currentYear, currentYear + 1)
        , lwd = c (3,4)
        , col = c ("black", currentCol [2], currentCol [3])
)

# addGraphs (longMean = sTday$MA_surfs
#            , percL = sTday$maL1_surfs
#            , percU = sTday$maU1_surfs
#            , current = cbind (sTday$pYMA_surfs,
#                               sTday$cYMA_surfs,
#                               sTday$pcYMA_surfs)
#            , jday = sTday$jday, maxV = NA, minV = NA
#            , currentCol = currentCol # = currentCol # "red"
#            , pastYear = FALSE, ongoingYear = TRUE
# )
# cLegend ("top"
#          , qntl = qntl, title = paste (maO, "day moving average")
#          , title.adj = 0.5, currentYear = currentYear
#          , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
#          , cYcol = currentCol
#          , text.col = "blue"
#          , pastYear = FALSE, ongoingYear = TRUE
# )
dev.off()


if (0){  ## use the one above
  # require ("gridSVG")  # could also just use svg()
  svg ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-surf4inkscape.svg") # combine with image in Inkscape
  ## in inkscape: open sa-surf-inkscape. Import sa-surf4inkscape.svg
  ## ungroup plot, delete background. group. align. Export to PDF.

  mCol <- "yellow"
  par (bg = NA
       , col.lab = mCol
       , col.axis = mCol
       , col.main = "blue"
       , fg = mCol
       , col = mCol); rm (mCol)
  wDB$surfs <- ifelse (wDB$surf > 1, 1, 0)
  sTday <- prepDF(wDB, "surfs"
                  , sumFct = function (x){maO * any (x >= 1)}
                  , currentYear=currentYear
  )
  aPlot (sTday, "surfs", ylab = "days with surf"
         , currentCol = currentCol, MA = TRUE, main = paste ("Days per", maO, "days with surf"))
  cLegend ("top"
           , qntl = qntl, title = paste (maO, "day moving average")
           , title.adj = 0.5, currentYear = currentYear
           , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
           , cYcol = currentCol
           , text.col = "blue"
  )
  dev.off()


  ## the original
  pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-surf-simple.pdf")
  ## background: surfer plunging into wave
  wDB$surfs <- ifelse (wDB$surf > 1, 1, 0)
  sTday <- prepDF(wDB, "surfs"
                  #                , sumFct = function (x){mean (x, na.rm = TRUE)}
                  , sumFct = function (x){sum (x >= 1) > 1 }
  )
  aPlot (sTday, "surfs", ylab = "chance of good surf"
         , currentCol = currentCol, MA = TRUE, main = "Days with surf")
  cLegend ("top"
           , qntl = qntl, title = paste (maO, "day moving average")
           , title.adj = 0.5, currentYear = currentYear
           , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
           , cYcol = currentCol
  )
  dev.off()
}



if (1){ ## windrose of wave height
  require ("openair")
  wRD <- with (wDB, data.frame (ws = wave_height, wd = mean_wave_dir
                                , date = datetimestamp
  ))

  pdf ("~/tmp/LCI_noaa/media/waves_windrose.pdf")
  wR <- windRose (wRD
                  #, type = "yClass"
                  , type = c("season") #, "yClass")
                  , auto.text = FALSE, paddle = FALSE, annotate = FALSE
                  , breaks = c (0, 1, 2, 3, 4)
                  , key.footer = "meters"
                  , grid.line = 10 #list (value = 10, lty = 5, col = "purple")
                  #  , statistic = "prop.mean"
                  #, max.freq = 30
  )
  # print (wR)
  dev.off()
}


## hind-cast
goodDays <- as.POSIXct (c("2021-03-06 19:40"
, "2020-11-02 16:18"
, "2018-10-28 18:32"
, "2020-11-13 13:20"
, "2021-04-30 04:30"   # big loud waves at dawn (high tide). pretty tight, short wave period
, "2021-08-28 16:00"   # check time (around high tide). 5 Surfers catching waves.
, "2021-09-24 16:40"
, "2021-09-30 11:00"  # HT
, "2021-10-01 13:00"  # HT
, "2021-10-02 13:00"  # HT
, "2021-10-03 14:00"  # HT
, "2021-10-15 11:45" # Vince out
, "2021-10-14 11:00" # Vince et al. out
, "2021-11-01 13:30"  # 4 surfers out
, "2021-11-06 15:30" # 4 surfers
, "2021-11-12 09:00" # surf's up!
, "2021-11-13 11:00" # 5 surfers , 4 feet
, "2021-11-17 11:45" # 1.2 m of beautiful surf
, "2021-11-21 14:00" # 1 surfer, 0.8 m
, "2021-11-21 14:00" # 5 surfers
, "2021-12-02 11:50" # 1 surfer, more coming
, "2021-12-07 15:00" # 1+ m surf, no surfers
, "2022-08-02 18:00" # 3 surfer in 0.8m surf (minimal, but some caught a wave)
, "2023-01-13 13:00" # 5 surfers, big loud surf, offshore wind.
, "2023-02-08 15:30" # nice 2-3 feet, offshore wind, 2 surfers
, "2023-02-20 14:45" # Vince and 2 others. Long, 1m waves
)
, tz = "America/Anchorage")
as.data.frame (approx(wDB$datetimestamp, wDB$surf, xout = goodDays))

mW <- 16 # window in min
gS <- as.data.frame (t (sapply (goodDays, function (x){
  subset (wDB, ((x - mW*60) < wDB$datetimestamp) &
                                          wDB$datetimestamp < (x + mW*60))}
  )))
print (gS [,c(1,7,8,0,10, 12, 13, 18:ncol (gS))])

rm (mW)
## calm days
as.data.frame (approx(wDB$datetimestamp, wDB$surf
                      , xout = as.POSIXct ("2020-12-31 15:00")))


getSunlightPosition(date = goodDays, lat = 59.6, lon = -151.5)



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
