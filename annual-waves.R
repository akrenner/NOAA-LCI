
################################
## wave height from bouy data ##
################################

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
## remove duplicates
wDB <- wDB [!duplicated(wDB$time),]
save.image ("~/tmp/LCI_noaa/cache/noaawaves.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/noaawaves.RData")


tm <- gsub ("T", "", wDB$time)
tm <- gsub ("Z", "", tm)
wDB$datetimestamp <- strptime (tm, format = "%F %T", tz = "UTC")
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



if (0){ ## mean daily wave height
  dailyW <- aggregate (wave_height~format (datetimestamp
                                           , "%Y-%m-%d")
                       , wDB, FUN = mean)
  names (dailyW)[1] <- "date"
  ## max daily wave height
  dailyW$maxW <- aggregate (wave_height~format (datetimestamp
                                                , "%Y-%m-%d")
                            , wDB, FUN = max)$wave_height
}





currentCol <- c("blue", "lightblue")
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
maO <- 30
qntl = 0.9

source ("annualPlotFct.R")

# tDay <- fixGap (tDay)
tDay <- addTimehelpers (wDB)
tDay <- prepDF (dat = tDay, varName = "wave_height"
                #, maO = maO, qntl = qntl
               , sumFct = mean # max
 #               , sumFct = max
                , maO = maO
                , qntl = qntl
)


pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-waves.pdf"), width = 9, height = 6)
aPlot (tDay, "wave_height", ylab = "wave height [m]"
       , currentCol = currentCol
       , MA = TRUE)
box()
cLegend ("top"
         , qntl = qntl, title = paste (maO, "day moving average")
         , title.adj = 0.5, currentYear = currentYear
         , mRange = c (min (as.numeric (format (wDB$datetimestamp, "%Y"))), currentYear-1)
         , cYcol = currentCol
)
dev.off()



## EOF 