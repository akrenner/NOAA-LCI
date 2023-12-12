## alternative to SWMP: try with NOAA airport data
# rm (list = ls())

## bug?? year shifted by 1?? data doesn't appear current??


# if (!require("pacman")) install.packages("pacman"
#                                          , repos = "http://cran.fhcrc.org/", dependencies = TRUE)
require ("rnoaa")
## see https://recology.info/2015/07/weather-data-with-rnoaa/
## GHCND          daily summaries
## PRECIP_HLY     Precipitation Hourly
## NORMAL_DLY     daily normals

## rainfall: 1979-current, res = 25 km
# cpc_prcp ()
## all historic weather data from single site: Homer Airport = USW00025507
options (noaakey = "rXArpDLHoQAkwVJpXfExOGyPgjFatTjH")

if (0){
  token = 'rXArpDLHoQAkwVJpXfExOGyPgjFatTjH'
  hom <- ghcnd (stationid = "USW00025507") #, refresh = TRUE)
  hom <- ghcnd ("AGE00147704")

  station_data <- ghcnd_stations()
  lat_lon_df <- data.frame(id = c("homer"),
                           latitude = c(59.6),
                           longitude = c(-151.49))
  nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                            station_data = station_data
                                            , radius = 10)
  ## query weather for this/these stations
  require ("dplyr")
  monitors <- nearby_stations$homer$id
  monitors <- monitors [grep ("AP", nearby_stations$homer$name),]
  all_monitors_clean <- meteo_pull_monitors(monitors,
                                            date_min = as.character(Sys.Date() - 30),
                                            date_max = as.character(Sys.Date()))  %>%
    dplyr::rename(datetimestamp = date, location = id)
  ## yes, daily data! could work with that?!

  # homerRain <-  ncdc (datasetid = "PRECIP_HLY"  # or GHCND -- also not working -- no data found
  #         , stationid = "USW00025507"
  #         , datatypeid = "HPCP"#, limit = 5
  #         , startdate = as.Date ("1999-01-01"), enddate = as.Date ("1999-02-01"))
}

require ("dplyr")
source ("annualPlotFct.R")
## meteo_pull_monitors appears to be incomplete. Start with data from manual download
## from https://www.ncei.noaa.gov/cdo-web/
if (0){
# hmr1 <- read.csv ("~/GISdata/LCI/SWMP/HomerAirport2959063.csv") %>%  ## old version to 2022-04
hmr1 <- read.csv ("~/GISdata/LCI/SWMP/HomerAirport3060741.csv") %>%
  dplyr::rename_with (tolower) %>%
  dplyr::rename (datetimestamp = date, location = station
                 ) %>%
  select (!ends_with ("_attributes"))
}



## may have to delete cache
if (.Platform$OS.type=="windows"){
  cacheD <- "C:/Users/Martin.Renner/AppData/Local/Cache/R/noaa_ghcnd/"
}else{
  cacheD <- "~/Library/Caches/R/noaa_ghcnd/"
}
cF <- list.files(cacheD, "dly", full.names=TRUE)
rm (cacheD)
## delete cache file if older than one month and computer is online
if (length (cF) > 0){
  if (max (file.info (cF)$ctime) < (Sys.time()-30*24*3600)){  # file is older than 1 month
    require (curl)
    if (curl::has_internet()){  ## only flush cache if new data can be downloaded
      unlink (cF)
    }
  }
}
hmrL <-  meteo_pull_monitors ("USW00025507"
                              # , date_min = "2022-04-18"  # goes back to 1932-09-01
#                              , date_min = "1933-01-01"  # goes back to 1932-09-01
#                              , date_max = as.character (Sys.Date())
                               ) %>%
  dplyr::rename (datetimestamp = date, location = id) # wdfg and awnd now missing

  ## merge hmr1 and hmrL
# hmr <- rbind (with (hmr1, data.frame (datetimestamp = as.POSIXct(datetimestamp)
#                                        , location, tavg, tmax, tmin
#                                        , prcp, wdf5, wsf5, wsf2, wdf2))
#                , with (hmrL, data.frame (datetimestamp = as.POSIXct(datetimestamp)
#                                          , location, tavg, tmax, tmin
#                        , prcp, wdf5, wsf5, wsf2, wdf2))) %>%
hmr <- rbind (hmrL) %>%
    addTimehelpers()
rm (hmrL)
# rm (hmr1)

## adjust units to mm/day and degrees C
hmr$totprcp <- hmr$prcp * 0.1 # PRCP = Precipitation (tenths of mm)
hmr$atemp <- hmr$tavg * 0.1
hmr$mintemp <- hmr$tmin  * 0.1
hmr$maxtemp <- hmr$tmax  * 0.1
hmr$maxwspd <- hmr$wsf2 * 0.1
hmr$wspd <- hmr$wsf5 * 0.1
hmr$wdfg <- hmr$wdf5

hmr <- as.data.frame (hmr)
hmr$medtemp <- rowMeans(cbind (hmr$maxtemp, hmr$mintemp))
if (0){
  x <- subset (hmr, !is.na (atemp))
  summary (factor (x$year))
  summary (hmr$maxtemp)
  summary (hmr$mintemp)
  summary (hmr$atemp)
  summary (hmr$medtemp)
  plot (medtemp~atemp, hmr)
}
## QAQC
is.na (hmr$atemp)[which (hmr$atemp > hmr$maxtemp)] <- TRUE
is.na (hmr$atemp)[which (hmr$atemp < hmr$mintemp)] <- TRUE


save (hmr, file = "~/tmp/LCI_noaa/cache/HomerAirport.RData")

if (0){
  tDay <- prepDF (hmr, "totprcp", maO = 7, qntl = 0.9) ## even with lots of data, maO = 7 is too short

  tDay <- prepDF (hmr, "snow", maO = 31, qntl = 0.9)
  aPlot (tDay, "snow", MA = TRUE, currentCol = "red, MA")
}

# homerNorm <-  ncdc (datasetid = "NORMAL_DLY"
#                      , stationid = "USW00025507"
#                      #      , locationid = "ZIP:99603"
#                      #  , datatypeid = "HPCP"#, limit = 5
#                     # , startdate = "1999-01-01", enddate = "2020-02-01"
# )

if(0){
  (stations <- isd_stations())
  ## plot stations
  ### remove incomplete cases, those at 0,0
  df <- stations[complete.cases(stations$lat, stations$lon), ]
  df <- df[df$lat != 0, ]
  ### make plot
  require ("leaflet")
  leaflet(data = df) %>%
    addTiles() %>%
    addCircles()
  #
  ##
  ak [grep ("HOM", ak$station_name),]


  ak <- df [grep ("AK", df$state),]
  as.data.frame (ak [grep ("AUG", ak$station_name),])

  # allStn <- ghcnd_stations()
  # AK <- allStn [grep ("AK", allStn$state),]
  # as.data.frame (AK [grep ("AUGU", AK$name),])
}


## EOF
