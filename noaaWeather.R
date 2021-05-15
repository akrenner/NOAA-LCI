## alternative to SWMP: try with NOAA airport data
# rm (list = ls())

## bug?? year shifted by 1?? data doesn't appear current??


if (!require("pacman")) install.packages("pacman"
                                         , repos = "http://cran.fhcrc.org/", dependencies = TRUE)
# pacman::p_load(package1, package2, package_n)
# pacman::p_load ("parallel")
Require <- pacman::p_load
setwd("~/myDocs/amyfiles/NOAA-LCI/")

Require ("rnoaa")
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

Require ("dplyr")
source ("annualPlotFct.R")

hmrL <-  meteo_pull_monitors ("USW00025507"
                              , date_min = "1970-01-01"  # goes back to 1932-09-01
                              , date_max = as.character (Sys.Date())) %>%
  dplyr::rename (datetimestamp = date, location = id
                 #, totprcp = prcp # wdfg not ideal equivalent
                 #, atemp = tavg
                 , wspd = awnd, maxwspd = wsfg, wdir = wdfg) %>%
  addTimehelpers ()

## adjust units to mm/day and degrees C
hmrL$totprcp <- hmrL$prcp * 0.1 # / 31  ##??
hmrL$atemp <- hmrL$tavg * 0.1
# hmrL$maxwspd <-
# hmrL$wdfg

hmr <- as.data.frame (hmrL)
save (hmr, file = "~/tmp/LCI_noaa/cache/HomerAirport.RData")
rm (hmrL)

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
  Require ("leaflet")
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