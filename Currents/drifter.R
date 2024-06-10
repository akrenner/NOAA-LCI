## analyse drifter tracks
## create animations and plots of current drifters
rm (list=ls())
# renv::restore()
print (startTime <- Sys.time())

test <- TRUE
test <- FALSE

## tasks:
## fetch all drifter data from PacificGyre.com
## also add off-shore drifters?
## clean data: identify positions over land or transported by boat
## produce two versions: raw positions and interpolated values for animation

# and animate through seasons (by jday -- or monthly plots)
## get OpenDrift to work on jupyter notebook -- check-point raw book in here.


## Define the whole drifter import and data processing as a function (it's slow).
## On re-runs, only reprocess new data

## animate both 2021 drifters on same day



## set interpolation [min] for animation
interP <- 10 # interpolation interval (min)
# interP <- 0  # no interepolation

speedTH <- 6 # max speed deemed realistic. Above which records are not plotted

resW <- 1920; resH <- 1080   ## HD+ 1920 x 1080, HD: 1280x729, wvga: 1024x576
resW <- 1024; resH <-  576   ## HD+ 1920 x 1080, HD: 1280x729, wvga: 1024x576
frameR <- 24 ## frames/s




## ----------------------------------------------------------
## set file locations
worldP <- "~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/f/GSHHS_f_L1.shp"   ## full resolution
worldP <- "~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/h/GSHHS_h_L1.shp"   ## high
# worldP <- "~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/c/GSHHS_c_L1.shp"  ## coarse for testing
# AKshape <- "GISdata/LCI/shoreline/akshape"
driftP <- "~/GISdata/LCI/drifter/"
bathyP <- "~/GISdata/LCI/bathymetry/KBL-bathymetry/KBL-bathymetry_ResearchArea_100m_EPSG3338.tiff" # large-scale bathymetry
# bingP <- "bingaddress -- find a way to get bing satellite imagery on the fly"
cacheD <- "~/tmp/LCI_noaa/cache/ggplot/"
outpath <- "~/tmp/LCI_noaa/media/drifter/"


## -----------------------------------------------------------
## load packages
# require ('ggOceanMapsLargeData') # of any use here? needed for examples only?
require ('dplyr')      ## needed for pipe
require ('RColorBrewer')
require ("sf")         ## apparently not auto-loaded by ggOceanMaps
require ("stars")
require ('oce')
require ("readr") ## for read_csv
# require ("rnaturalearth")
# require ('tidyverse')

## add snapbox for basemap? -- all seem to require a token
# require ("snapbox")


## set projection for output map
projection <- st_crs (3338) ## Alaska Albers EA  ## can't be 4326 I guess :(
# projection <- st_crs (4326)  # "+init=epsg:4326" ## WGS84
# projection <- st_crs (32605) # UTM 5N


## ----------------------------------------------------------
## animation options:
# gganimate: good for ggplot2, but doesn't work with ggOceanMaps (yet). 2024
# animation: 2.7 2021 , github same age. Requires ImageMagick or GraphicsMagick or avconv or FFmpeg
# plotly: mainly for interactive web graphics. Version 4.x, 2024
# gifski in R: convert video frames to GIF. Requires Rust. (2023) -- maybe that's the ticket!
# tmap: too limited options for animation

## may need the flexibility of package animation??
## need to interpolate positions to make better animation


## tmaps: works, but animation potential quite limited
## ggplot2 -- need to adapt reading in world polygon to existing examples (sub borders) -- do that!
## go back to base graphics -- what I know? Want nices graticules.


# use ggOceanMaps to make map (has nice cartography)
# gganimate to animate it -- don't work together :(
#
# products:
#   1. animation of drifter track (n concurrent drifters)
#   2. static map of all drifters combined


## ggplot map:  https://bookdown.org/nicohahn/making_maps_with_r5/docs/ggplot2.html
## ggOceanMaps https://mikkovihtakari.github.io/ggOceanMaps/ (nice cartography, will need external bathymetry)
##             https://aen-r-workshop.github.io/4-ggOceanMaps/ggOceanMaps_workshop.html#1
## animation: https://gganimate.com/
## fancy background for land: https://jakob.schwalb-willmann.de/basemaps/



## ----------------------------------------------------------
## set up directories
dir.create (outpath, showWarnings=FALSE, recursive=TRUE)
dir.create (cacheD, showWarnings=FALSE, recursive=TRUE) ## I set .ggOceanMapsenv in .Rprofile:
dir.create ("~/tmp/LCI_noaa/cache/drifter/", showWarnings=FALSE, recursive=TRUE)
dir.create (driftP, showWarnings=FALSE, recursive=TRUE) ## for drifter downloads
# {.ggOceanMapsenv <- new.env(); .ggOceanMapsenv$datapath <- "~/tmp/LCI_noaa/cache/ggplot/"} ## not worth the trouble?
## increase time-out limit from 60 s -- better to get local bathymetry?
options(timeout=600) ## double timeout limit
# if (!file.exists (AKshape)){
#   dir.create (AKshape, showWarnings=FALSE, recursive=TRUE)
#   download.file (url="https://dev.nceas.ucsb.edu/knb/d1/mn/v2/object/urn%3Auuid%3Aaceaecb2-1ce0-4d41-a839-d3607d32bb58"
#                  , destfile=AKshape)
# }




## ----------------------------------------------------------
## load needed GIS data

# bbox <- read.csv("~/GISdata/LCI/KBL_ResearchArea/KBL_ResearchArea.csv") %>%
#   st_as_sf (coords=c("lon", "lat"), crs=4326) %>%
#   st_bbox() %>%
#   st_as_sfc() %>%
#   st_transform(projection) %>%
#   st_as_sfc()

## bathymetry/topography
mar_bathy <- stars::read_stars (bathyP); rm (bathyP)  # , crs=3338) ; rm (bathyP)
if (projection != st_crs (3338)){
  mar_bathy <- st_warp(mar_bathy, crs=projection)
}
names (mar_bathy) <- "topo"
# depth <- st_as_stars(ifelse (mar_bathy$topo > 0, NA, mar_bathy$topo * -1)
#                      , dimensions = attr(mar_bathy, "dimensions"))

## bounding box -- redundant?
bbox <- mar_bathy %>%  ## extended Research Area
  st_bbox() %>%
  st_as_sfc()


## coastline
worldM <- sf::st_read (worldP, quiet=TRUE, crs=4326) %>%
  st_geometry()
worldM <- subset (worldM, st_is_valid (worldM)) %>% ## polygon 2245 is not valid
  st_crop (c(xmin=-160, xmax=-140, ymin=55, ymax=62)) %>%   ## or could use bbox above
  sf::st_transform(projection)
rm (worldP)
## somehow, polygon 2245 is not valid and cannot be made valid
## it's at Lon:43.83, Lat:-69.75 -- ideally fixed in gshhs source!
# summary (st_is_valid(st_make_valid(worldM)))
seaA <- st_difference(bbox, st_union (worldM))[1,]  ## why was this so hard?!?






## ----------------------------------------------------------------------------
## prepare drifter data:
## download/update to latest data from PacificGyre.com
## select drifter
## define deployment bouts
## interpolate within bouts to standardize time intervals
## turn into geographic sf and project
## could manually get drifter data from https://data.pacificgyre.com/data#data-download-tab
## see http://api.pacificgyre.com/ for API syntax


## migrate from .csv file to .zip for direct download  XXX

## build up API-url to download drifters from PacificGyre.com
key="6A6BEAD8-4961-4FE5-863A-721A16E54C1C"
startDate="2010-01-01%2000:00"
endDate="2023-12-31%2023:59"
endDate="2023-12-31"
# FieldList="DeviceName,DeviceDateTime,AgeInSeconds,BatteryVoltage,CommId,Latitude,Longitude,SubmergedPercent,Temperature0cm"
FieldList="DeviceName,DeviceDateTime,CommId,Latitude,Longitude"  ## make sure all fields are covered by all devices
driftF <- paste0 (driftP, "drifter-data_", endDate, ".zip")
updateFN <- gsub ("2023-12-31", "latest", driftF)
dM <- ifelse (.Platform$OS.type=="unix", "w", "wd")


## could do annual downloads?

if (!file.exists(driftF)){
  urlC <- paste0 ("https://api.pacificgyre.com/api2/getData.aspx?apiKey=", key,
                  "&FieldList=", FieldList,"&startDate=", startDate,
                  "&endDate=", endDate, "&fileFormat=csv&compression=zip&download=Yes"
  )
  options(timeout=300)
  download.file(url=urlC, destfile=driftF, mode=dM); rm (urlC)
}
## update to the latest data
if (file.exists (updateFN) & (difftime (Sys.time(), file.info (updateFN)$ctime, units="days") < 7)){
  message ("Drifter data downloaded within the last week: skipping update")
}else{
  options(timeout=180)
  download.file (url=paste0 ("https://api.pacificgyre.com/api2/getData.aspx?apiKey=", key,
                             "&FieldList=", FieldList,
                             "&startDate=", as.character (as.Date(endDate)+1), "%2000:00",
                             "&fileFormat=csv&compression=zip&download=Yes")## endDate defaults to now
                 , destfile=updateFN, mode=dM)
}
rm (key, startDate, endDate, FieldList, dM)


## combine archive and latest drifter download
readC <- function (x){read_csv (x, show_col_types=FALSE, lazy=TRUE)}  ## XXX read from zip file
drift <- purrr::map_df (c(driftF, updateFN) #list.files (path=driftP, pattern="\\.csv$", full.names=TRUE)
                        , readC) %>%
  filter (Longitude < -149) %>%            # remove SE Alaska (here to get bbox right) -- and AI
  filter (Latitude < 70) %>%               # remove Arctic Ocean
  arrange (DeviceName, DeviceDateTime) %>% # test that this working -- crash on Windows
  # mutate (DeviceDateTime = DeviceDateTime %>% as.character %>% as.POSIXct(tz = "GMT")) %>% # already recognized as UTC
  arrange (DeviceName, DeviceDateTime) %>%
  mutate (IDn=1:nrow (.)) %>%
  filter()
rm (readC, driftF, updateFN, driftP)


## remove duplicates (about 80 in contiguous download)
drift <- drift %>%
  filter (!duplicated (drift [,which (names (drift) %in%
                                        c("DeviceName", "DeviceDateTime") )])) %>%
  filter()
## get more drifter data from NOAA global drifter program
if (0) {
  ## See https://osmc.noaa.gov/erddap/tabledap/index.html?page=1&itemsPerPage=1000
  df = read.csv('http://osmc.noaa.gov/erddap/tabledap/gdp_interpolated_drifter.csvp?ID%2Clongitude%2Clatitude%2Ctime%2Cve%2Cvn&longitude%3E=-70&longitude%3C=-50&latitude%3E=35&latitude%3C=50&time%3E=2018-01-01&time%3C=2019-01-01')
  df = read_csv(paste0 ('http://osmc.noaa.gov/erddap/tabledap/gdp_interpolated_drifter.csvp?"
, "ID%2Clongitude%2Clatitude%2Ctime%2Cve%2Cvn&longitude%3E="
                      , -70, "&longitude%3C=", -50, "&latitude%3E=", 35, "&latitude%3C=", 50,
                      "&time%3E=2018-01-01&time%3C=2019-01-01'))

  # ERDDAP "https://erddap.aoml.noaa.gov/"
  # https://erddap.aoml.noaa.gov/gdp/erddap/index.html
}




## ----------------------------------------------------------
## add additional information to drifter
## include speed between positions? XXX
## interpolated positions = potentially dangerous (on land)? But, is a more accurate position for the speed

# ## new dataframe for interpolated positions -- move down?
# dI <- with (drift, data.frame (CommId [-1], DeviceName[-1], IDn [-1],
#                                DeviceDateTime=DeviceDateTime[-1]-diff(DeviceDateTime),
#                                Latitude=Latitude[-1]-diff (Latitude),
#                                Longitude=Longitude[-1]-diff (Longitude),
#                                dT_min=diff (DeviceDateTime) %>% as.numeric()/60,
#                                distance_m=diff (oce::geodDist (Longitude, Latitude, alongPath=TRUE)*1e3)
# )) %>%
#   mutate (speed_ms=distance_m/(dT_min*60)) %>%
#   st_as_sf (coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
#   st_transform(projection)
# ## to replace drift, would still need direction of shift (velocity)

drift$dT_min <- c (0, diff (drift$DeviceDateTime)/60)  ## in min
drift$distance_m <- c (0, diff (oce::geodDist(drift$Longitude, drift$Latitude, alongPath=TRUE)*1e3))
drift$speed_ms <- with (drift, distance_m / (dT_min*60)) ## filter out speeds > 6 (11 knots) -- later

# consider: using dI here instead of drift
drift <- st_as_sf (drift, coords=c("Longitude", "Latitude")   ### why not keep if for drift?
                , dim="XY", remove=FALSE, crs=4326) %>%
  st_transform(projection)
## use morph..
drift$topo <- st_extract(mar_bathy, at=drift)$topo

## using LandDistance_m <- st_distance(worldM, dx) %>% apply (2, min) is slow by orders of magnitude
drift$LandDistance_m <- worldM %>%
  st_union() %>%
  st_distance(drift, by_element=FALSE) %>%
  as.numeric()
# drift$onLand <- !is.na (st_intersects(dx, st_union (worldM)) |> as.numeric()) ## not pretty, not reliable. Skip for now
drift$onLand <- st_intersects(drift, worldM) %>% as.numeric () ## not pretty, not reliable. Skip for now
# dx$onLand <- st_join (dx, st_sf (worldM), join=st_within)
# dx <- st_filter (dx, seaA)  ## supposed to filter out points on land
# rm (dx)

## ice wave rider and MicroStar are surface devices
drift$deployDepth <- ifelse (seq_len(nrow (drift)) %in% grep ("UAF-SVP", drift$DeviceName), 15, 0)
drift$year <- format (drift$DeviceDateTime, "%Y") |> factor()  ## above should be piped and mapped XXX


if (test){save.image ("~/tmp/LCI_noaa/cache/drifter/drifter3.Rdata")}
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifter3.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")







## -------------------------------------------------------------------------------------------
## calculate state of the tide to subset data for maps, separating flood/slack/ebb

## XXX can't currently reproduce results of tide_slack_data XXXXX -- try different harmonics
## tide test
if (0){
  # tide_slack_data_station (tide_station ("Seldovia*", Sys.time()))
  # tide_slack_data_datetime (as.POSIXct ("2020-05-01 15:00", tz= "GMT"), tide_station("Seldovia*"))
  # hist ((runif (1e6) - runif (1e6)))
  require ('rtide')
  tide_slack_data (as.POSIXct ("2020-05-01 15:00", tz= "GMT"), tide_stations("Kasitsna*"))

  times <-  tide_datetimes(
    minutes = 60L,
    from = as.Date("2015-01-01"),
    to = as.Date("2015-01-01"),
    #    tz="-0800"
    #    tz = "PST8PDT"
  )
  tide_height()  # tide heigth at station and times to-from
  tide_height_data(data.frame (DateTime=as.POSIXct("2015-01-01 00:00"), Station=tide_stations ("Monterey Harbor*")))
  tide_slack_data(data.frame (DateTime=as.POSIXct("2015-01-01 12:00"), Station=tide_stations ("Monterey Harbor*")))

  tide_slack_data(data.frame (DateTime=as.POSIXct("2015-01-01 12:00"), Station=tide_stations ("Seldovia*")))


  ## for testing:  drift <- slice_sample(drift, n=1000)
  # n=5000 on dell, serial: 215 usr = 3.5 min
  # n=1000 on dell, serial:  46 usr = s
  # n=5000 on dell, parall: 131 usr = 2 min
  # n=10e3 on dell, parall: 209 usr = 3.5 min
  # 345635  -- expect: 2 h   209/10e3*345635/3600 -- for all
  # 38105   (tu) -- a rather modest 209/10e3*38105/60 = 15 min

  tDF <- data.frame (Station=tide_stations("Kasitsna*"), DateTime=as.POSIXct(c("2015-01-01 00:00", "2015-01-01 6:00"
                                                                               , "2015-01-01 12:00", "2015-01-01 18:00"
                                                                               , "2015-01-01 23:00"), tz="GMT"))
  tide_slack_data (tDF)

  tide_slack_data (data.frame (Station=tide_stations ("The Battery, New York Harbor, New York")
                               , DateTime=(as.POSIXct (c("2015-01-01 00:00", "2015-01-01 6:00"
                                                         , "2015-01-01 12:00", "2015-01-01 18:00"
                                                         , "2015-01-01 23:00"), tz="GMT"))))
}

## pull fresh tide data from NOAA API
## https://api.tidesandcurrents.noaa.gov/mdapi/prod/
## https://tidesandcurrents.noaa.gov/web_services_info.html

station=9455517  # Kasitsna Bay
yL <- levels (drift$year)

if (file.exists("~/tmp/LCI_noaa/cache/drifter/tideCache.csv")){
  tide <- read_csv ("~/tmp/LCI_noaa/cache/drifter/tideCache.csv", show_col_types=FALSE)
  tide$Date.Time <- tide$Date.Time |> as.POSIXct(tz="GMT")
  ## fetch only the missing ones
  nL <- tide$Date.Time |> format ("%Y") |> factor () |> levels()
  yL <- yL [-which (yL %in% nL)]; rm (nL)
}

if (length (yL) > 0){
  for (i in seq_along(yL)){
    begin_date=paste0 (yL [i], "0101")
    end_date=paste0 (yL [i], "1231")
    url <- paste0 ("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date="
                   , begin_date, "&end_date=", end_date, "&station=", station,
                   "&product=predictions&datum=MLLW&time_zone=gmt",
                   "&interval=hilo&units=metric&application=DataAPI_Sample&format=csv")
    tideT <- read.csv (url)
    tideT$Date.Time <- as.POSIXct (tideT$Date.Time, tz="GMT")
    if (!exists ("tide")){
      tide <- tideT
    }else{
      tide <- rbind (tide, tideT)
    }
  }
  rm (tideT, station, end_date, begin_date, url, i)
  ## find closest date for each moment
  write.csv (tide, file="~/tmp/LCI_noaa/cache/drifter/tideCache.csv", row.names=FALSE)
}
rm (yL, station)

## find better solution to the nearest point problem? -- divide and conquer
## https://www.statology.org/r-find-closest-value/
## using mclapply takes minutes. Using cut is instantaneous!
# sapply:  227 s elapsed, 153 user
# mclapply: 62 s elapsed, 347 user
# cut:     0.1 s elapsed, 0.1 user

# drift <- drift [order (drift$DeviceDateTime),]
tide <- tide [order (tide$Date.Time),]
v1 <- as.numeric(drift$DeviceDateTime)
v2 <- as.numeric (tide$Date.Time)
cuts <- c(-Inf, v2 [-1]-diff (v2)/2, Inf)
tIdx <- cut (v1, breaks=cuts, labels=1:length (v2)) %>% as.numeric (levels (.))[.]  # conv factor after pipe
rm (v1, v2, cuts)
# system.time({
#   require ("parallel")  ## keep for reference. Brute-force method to nearest point problem. Slow!
#   if (.Platform$OS.type=="unix"){
#     tIdx <- mclapply (1:nrow (drift), function (i){
#       which.min (abs (drift$DeviceDateTime [i] - tide$Date.Time))
#     }
#     , mc.cores=detectCores()-1)
#     tIdx <- do.call ("rbind", tIdx)
#   }else{
#     tIdx <- sapply (1:nrow (drift), function (i){
#       which.min (abs (drift$DeviceDateTime [i] - tide$Date.Time))
#     })
#   }
# })

## parse tide data back into drift
drX <- data.frame (
  DeviceDateTime=drift$DeviceDateTime,
  slackTide=tide$Date.Time [tIdx],
  tideLevel=tide$Prediction [tIdx],
  hilo=tide$Type [tIdx]
)
drX$dT <- difftime (drX$DeviceDateTime, drX$slackTide) |> as.numeric()/3600
drX$tide <- ifelse (abs (drX$dT) < 1.5, "slack", NA)
drX$tide <- ifelse (is.na (drX$tide) & (drX$hilo == "H") & (drX$dT < 0), "flood", drX$tide)
drX$tide <- ifelse (is.na (drX$tide) & (drX$hilo == "L") & (drX$dT > 0), "flood", drX$tide)
drX$tide [is.na (drX$tide)] <- "ebb"
drift$tide <- factor (drX$tide)
## add more tide information to drifter tracks??
rm (drX, tide, tIdx)
# barplot (summary (drift$tide))

## now obsolete (and currently unreliable) tide calculations using package rtide. Revive?
if (0){
  ## tides
  require ('rtide')  ## calculates tides from harmonics
  ## find closest station: Seldovia/Kasitsna, Nikiski, Kodiak, Anchorage, Seward
  #  tStn <- tide_stations("Seldovia*")
  tStn <- "Anchor Point, Cook Inlet, Alaska"
  tSn <- "Bear Cove, Kachemak Bay, Cook Inlet, Alaska"
  tSn <- "Kasitsna Bay, Kachemak Bay, Alaska"
  ttbl <- data.frame (# Station=tide_stations ("Seldovia*") ## find better harmonics?!
    Station=tSn, DateTime = round (drift$DeviceDateTime, units="hours"))  ## imperative to parallelize. Enough?
  tu <- unique (ttbl)

  system.time ({
    if (1){  ## parallel (or serial)
      require(parallel)
      if (.Platform$OS.type=="unix"){
        pT <- mclapply (1:nrow (tu), function (i){tide_slack_data(tu [i,])}
                        , mc.cores=detectCores()-1)
      }else{
        require(parallel)
        ## need to pre-allocate to cores -- don't use parLapplyLB
        nCores <- detectCores()-1
        cl <- makeCluster(nCores)
        clusterExport (cl, varlist=c("tu"))
        clusterEvalQ(cl, require ("rtide"))
        pT <- parLapply (cl, 1:nrow (tu), function (i){tide_slack_data (tu [i,])})
        stopCluster (cl); rm (cl)
      }
      tSlack <- as.data.frame (do.call (rbind, pT))
    }else{
      ## serial processing
      tSlack <- tide_slack_data(tu)  ## must parallelize
    }
  })
  if (test){save.image ("~/tmp/LCI_noaa/cache/drifter/drifterTide.Rdata")}  ## checkpoint for safety
  ##  rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifterTide.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")

  ttbl <-  cbind (DeviceDatetime=drift$DeviceDateTime
                  # , DateTimeT=ttbl$DateTime
                  , tSlack [match (ttbl$DateTime, tSlack$DateTime),c(2,3,5)])

  ## categorize tide: within 1.5 h: high/low -- incorrect XXX see above
  ttbl$dT <- difftime (ttbl$DeviceDatetime, ttbl$SlackDateTime) |> as.numeric()/3600  # time in hours
  ttbl$tide <- ifelse (abs (ttbl$dT) < 1.5, "slack", NA)
  # ttbl$tide <- ifelse (is.na (ttbl$tide), ifelse (ttbl$SlackType=="high", "ebb", "flood"), ttbl$tide)

  if (0){
    hist (ttbl$dT, main = "", xlab="hours from slack tide")
    hist (abs (ttbl$dT), main = "", xlab = "|hours| from slack tide")
    hist (subset (ttbl, SlackType=="high")$dT)
    hist (format (ttbl$DeviceDatetime, "%H") |> as.numeric(), xlab="hour of day")
    # hist (sin (runif(10e3)*2*pi))
  }

  drift <- cbind (drift, ttbl [, c(3,4,6)])
  rm (tu, ttbl, tSlack)
}
## move tide functions into function script, load that
if (test){save.image ("~/tmp/LCI_noaa/cache/drifter/drifterTide2.Rdata")}
#  rm (list = ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifterTide2.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")




## -------------------------------------------------------------------------------------------
## build reference map to identify human-transported drifter positions

# https://towardsdatascience.com/building-kriging-models-in-r-b94d7c9750d8
# https://gis.stackexchange.com/questions/411556/how-to-set-up-a-target-grid-for-kriging-in-r
# parallel:
# https://gis.stackexchange.com/questions/237672/how-to-achieve-parallel-kriging-in-r-to-speed-up-the-process
# https://github.com/guzmanlopez/parallelizingAndClusteringInR/blob/master/parallel-kriging-function.R

## kriging a map
# require ('automap')
# require ("gstat")


## filter == only for drifter speed map

drift_sf <- drift %>%
  filter (!is.na (speed_ms)) %>%   ## uncertain why there are NAs, but they have to go
  filter (speed_ms > 0.0001) %>%   ## negatives -- go
  filter (speed_ms < 10) %>%    ## 15 m/s approx 30 knots -- generous to avoid it messing with mean
  #  filter (distance_m > 0.1) %>%  # no effect?
  #  filter (is.na (onLand)) %>%   ## trouble -- cuts down nrow dramatically
  filter (dT_min != 0) %>%     ## some zero-values
  #  slice_sample (n=10e3) %>% #, order_by=speed_ms, na_rm=TRUE  ## balance spatially?
  #  st_as_sf (coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
  #  st_transform(projection) %>%  ## or UTM? -- sf_to_rast requires projection
  # st_transform (32605) %>% # UTM zone 5N
#  select (speed_ms, IDn) %>%  ## only needed to match extra points from shoreline below
  filter()
# nrow (drift_sf)

if (test){save.image ("~/tmp/LCI_noaa/cache/drifter/drifterSpeedMap.Rdata")}
## rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifterSpeedMap.Rdata")





## -----------------------------------------------------------------------------------
## spatial interpolation/aggregation

## make grid
## aggregate quantile speed over grid, then look-up aggregated values
## for data filtering, aggregate over grid (rather than IDW, etc) -- see:
# https://stackoverflow.com/questions/66907012/aggregate-values-in-raster-using-sf
## later make prediction using kriging


## define grid
seaAEx <- st_bbox (drift_sf) %>%
  st_as_sfc() # %>%
grid_spacing <- 10e3  ## 10 km seems to make sense -- go to 20 km?
pgon <- st_make_grid(seaAEx, square=TRUE, cellsize=rep (grid_spacing, 2)) %>%
  st_sf () %>%
  mutate (ID=row_number())
A <- st_intersection (pgon, seaAEx)  ## grid -- suppress warning
rm (seaAEx)

# do this for classes: tide==flood, tide==ebb, deployDepth=0, depolyDepth==15
## ok to lump deployDepth here -- only for filtering. Separate them later
pointsID <- drift_sf %>%
#  group_split (tide, deployDepth) %>%
  filter (tide!="slack") %>%
  st_join (A) %>%
  as.data.frame() %>%
  group_by (ID) %>%
  summarize (mapped_speed=quantile (speed_ms, 0.5))
A <- left_join(A, pointsID, by="ID")
# plot (A ["mapped_speed"])
rm (pointsID, pgon)

## extract mapped_speed from A
## need to subset to drifters within study area
### no longer needed if useing seaAEx?
# drift_sf$inSpeedcov <- st_intersects(drift_sf, A) %>% apply (1, sum)
# drift_sf <- filter (drift_sf, inSpeedcov==1) ## subset to within grid or st_extract fails

sdTh <- 3

drift_sf$mapped_speed <- st_rasterize (A ["mapped_speed"]) %>%
  st_extract (drift_sf) %>%
  st_drop_geometry() %>%
  pull (mapped_speed)
## flag records more than 2 SD from predicted grid speed (only above, not below)
rSp <- with (drift_sf, speed_ms-mapped_speed)

pdf (paste0(outpath, "speedResiduals.pdf"))
hist (rSp, breaks=500, xlab="speed residual [m/s]", main="")
abline (v=sdTh*sd (rSp, na.rm=TRUE))
axis (3, tick=FALSE, at=sdTh*sd (rSp, na.rm=TRUE), labels=paste (sdTh,"SD"))

drift_sf %>%
  filter (speed_ms < 20) %>%
  st_drop_geometry() %>%
  plot (speed_ms ~ speed_ms-mapped_speed, data=.)  ## residual vs predicted
abline (a=0, b=1)
dev.off()

png (paste0 (outpath, "Speed-Aggregate.png"), width=resW, height=resH)
## map of A -- speed aggregate
require ("viridisLite")
plot (seaA, col="white", border="white")
A ["mapped_speed"] %>% st_rasterize() %>% plot(add=TRUE, col=plasma (100))
plot (worldM, add=TRUE, col="gray")
dev.off()

drift_sf$badSpeed <- ifelse (rSp > sdTh*sd (rSp, na.rm=TRUE), TRUE, FALSE)
rm (grid_spacing, A, rSp, sdTh)


## ---------------------------------------------------------------------------------
## merge drift_sf with drift

if (1){ ## revert to drift or stay with drift_sf (which is restricted to study area)
  ds <- st_drop_geometry(drift_sf)
  drift <- cbind (drift, ds [match (drift$IDn, ds$IDn)
                             , which (!names (ds) %in% names (drift))])
  rm (ds)
}else{
  dr <- st_drop_geometry(drift)
  drift_sf <- cbind (drift_sf, dr [match (drift_sf$IDn, dr$IDn),
                                      which (!names (dr)%in%names (drift_sf))])
  rm (dr)
}
rm (drift_sf)



## filter out unrealistic speeds and records too close to shore/on land? XXX
driftX <- drift %>%
  # st_drop_geometry() %>%  ## drop spatial part
  #  filter (speed_ms < speedTH) %>%  ## not much effect here? still need to filter again after interpolation?
  filter ((badSpeed != TRUE) | (is.na (badSpeed)) ) %>%
  filter (topo < 1) %>%         ## to make sure none are on land  XXX
  #  filter (LandDistance_m > 50) %>%
  filter()
## retains 21k out of 28 k
nrow (drift)
nrow (driftX)
# drift <- driftX
rm (driftX)



if (test){save.image ("~/tmp/LCI_noaa/cache/drifter/driftSped.RData")}
## rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/driftSped.RData")





# ----------------------------------------------------------------------------
## define individual drifter deployments

## redundant, but better safe
# is.unsorted(drift$DeviceName)
drift <- drift [order (drift$DeviceName, drift$DeviceDateTime),]

## define new deployment XXXX  review!!! XXXX
newDeploy <- drift$dT_min > 240 | !duplicated (drift$DeviceName) ## dT_min in min. mark new deployments XXX test!! 240 min = 4 h
# newDeploy <- drift$dT_min > 60*14 | !duplicated (drift$DeviceName) ## dT_min in min. mark new deployments   14 h (840 min)
## mark new deployments
x <- 0; depIdx <- character(nrow (drift)) # declare variables
for (i in 1:length (newDeploy)){
  if (newDeploy [i]){
    x <- x + 1
    x <- drift$DeviceDateTime [i]
  }
  depIdx [i] <- paste0 (drift$DeviceName [i], "-", x)
}
drift$deploy <- factor (depIdx)
# head (summary (drift$deploy))
rm (newDeploy, x, depIdx, i)



if (test){
  drift <- subset (drift, deploy %in% sample (levels (drift$deploy), 5))
  drift$deploy <- factor (drift$deploy)  ## reset levels
}


# plot (Latitude~Longitude, drift)
# summary (drift$dT_min)
# for (i in 1:5) alarm()


# ----------------------------------------------------------------------------
## interpolate within bouts
if (exists ("iDF")){rm (iDF)} ## in case of reruns of code


dInt <- function (i){
  require ("sf")
  require ("dplyr")
  df <- subset (drift, deploy == levels (drift$deploy)[i])
  if (nrow (df)>1){
    if (interP > 0){
      newDF <- with (df, data.frame(DeviceName=df$DeviceName[1], deploy=df$deploy[1]
                                    , DeviceDateTime=seq.POSIXt(from=min (DeviceDateTime)
                                                                , to=max (DeviceDateTime)
                                                                , by=paste (interP, "min"))
      ))
      newDF$Longitude <- approx (df$DeviceDateTime, df$Longitude, xout=newDF$DeviceDateTime)$y
      newDF$Latitude <- approx (df$DeviceDateTime, df$Latitude, xout=newDF$DeviceDateTime)$y
      newDF$dT_min <- interP
      ## fill in the rest -- interpolate if numeric, find nearest if categorical
      newDF$distance_m <- c (0, diff (oce::geodDist(newDF$Longitude, newDF$Latitude, alongPath=TRUE)*1e3))
      newDF$speed_ms <- newDF$distance_m / (newDF$dT_min*60) ## convention to use m/s, not knots
      newDF$topo <- df$topo [1]
      newDF$LandDistance_m <- df$LandDistance_m [1]
      # newDF$onLand
      newDF$deployDepth <- df$deployDepth [1]
      newDF$year <- format (newDF$DeviceDateTime, "%Y") %>% as.numeric() %>% as.factor()
      newDF$tide <- df$tide [1]  ## quick and dirty -- ok for short intervals
      newDF$mapped_speed <- df$mapped_speed [1]  ## XXX dirty, but fine here
      # newDF$badSpeed
    }else{
      newDF <- df # %>% select (c("DeviceName", "DeviceDateTime", "Longitude", "Latitude", "deploy", "dT_min"))
    }
    ## interpolate all other variables; closest for categoricals
    newDF$days_in_water <- with (newDF, difftime(DeviceDateTime, min (DeviceDateTime), units="days"))|> as.numeric()
    ## trim newDF XXXX  -- cut bad stuff front and back
    ## at the very least: first and last
    newDF <- newDF [2:(nrow (newDF)-1),]
  }}

if (1){
  require ("parallel")
  require ('data.table')
  nCores <- detectCores()-1

  if (.Platform$OS.type=="unix"){
    iDF <- mclapply(seq_along (levels (drift$deploy)), FUN=dInt, mc.cores=nCores) %>%
      rbindlist()
  }else{
    cl <- makeCluster(nCores, type="PSOCK")
    clusterExport(cl, varlist=c("drift", "interP", "dInt"))
    clusterEvalQ(cl, require ("dplyr"))
    iDF <- parLapply (cl, seq_along (levels (drift$deploy)), fun=dInt) %>%
      rbindlist()
    stopCluster(cl); rm (cl)
  }
  rm (nCores)
}else{
  for (i in seq_along (levels (drift$deploy))){               ## serial=slow! cache?; use dplyr split/group
    dInt (i)
  }
}
rm (dInt)


# iDF <- subset (iDF, speed_ms < speedTH)  ## apply again --- any way to get pre/post deploy more thorough?

## project positions -- don't move earlier to allow interpolations
if (1){   #interP > 0){
  drift <- iDF %>%
    filter (!is.na (Longitude)) %>%  ## not sure where they came from, but can't have it XXX
    st_as_sf (coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
    st_transform(projection)
}
rm (interP, iDF)



# ### -- old -- did this already
# # drift$distance_m <- c (0, d <- st_distance (drift, by_element=TRUE))
# ## ice wave rider and MicroStar are surface devices
# drift$deployDepth <- ifelse (seq_len(nrow (drift)) %in% grep ("UAF-SVP", drift$DeviceName), 15, 0)
# drift$year <- format (drift$DeviceDateTime, "%Y") |> as.numeric()  ## above should be piped and mapped XXX
# drift$topo <- st_extract(mar_bathy, at=drift)$topo



## testing
if (0){
  sort (summary (drift$deploy), decreasing=TRUE) |> head()
  x <- subset (drift, deploy=="UAF-SVPI-0047-2022-07-21 18:30:10")
  plot (x)
  summary (x$DeviceDateTime)
  summary (x$days_in_water)
  levels (factor (x$DeviceName))
}



# driftC <- st_intersection(drift, bbox)

## get more drifter data from NOAA global drifter program
if (0) {
  ## See https://osmc.noaa.gov/erddap/tabledap/index.html?page=1&itemsPerPage=1000
  df = read.csv('http://osmc.noaa.gov/erddap/tabledap/gdp_interpolated_drifter.csvp?ID%2Clongitude%2Clatitude%2Ctime%2Cve%2Cvn&longitude%3E=-70&longitude%3C=-50&latitude%3E=35&latitude%3C=50&time%3E=2018-01-01&time%3C=2019-01-01')
  df = read_csv(paste0 ('http://osmc.noaa.gov/erddap/tabledap/gdp_interpolated_drifter.csvp?"
, "ID%2Clongitude%2Clatitude%2Ctime%2Cve%2Cvn&longitude%3E="
                      , -70, "&longitude%3C=", -50, "&latitude%3E=", 35, "&latitude%3C=", 50,
                      "&time%3E=2018-01-01&time%3C=2019-01-01'))

  # ERDDAP "https://erddap.aoml.noaa.gov/"
  # https://erddap.aoml.noaa.gov/gdp/erddap/index.html
}


## summarise reporting interval ?? still needed? -- move up if to be used
if (0){
  ## set reporting interval ?
  ## tricky. Some deployments at 4 h intervals, mostly < 1

  ## standardize by interpolating to xx min time intervals
  ## summary of reporting intervals:
  dRep <- difftime(drift$DeviceDateTime [2:nrow (drift)]
                   , drift$DeviceDateTime [1:(nrow (drift)-1)]
                   , units="mins")
  dRep <- subset (dRep, abs (dRep) < 60*24)
  summary (as.numeric (dRep))
  sort (summary (factor (round (dRep/5)*5)), decreasing=TRUE)  ## most common: 10min, 30 min, 60 min, 240 min
  ## => interpolate to 10 min
  rm (dRep)
}





if (test){save.image ("~/tmp/LCI_noaa/cache/drifter/drifter0.Rdata")}
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifter0.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")












if (0){
  if (0){  ## fast IDW or gstat?
    ## fast IDW
    # https://geobrinkmann.com/post/iwd/
    ## not accelerated under windows -- worth the installation trouble?
    ## should use kriging for data-product output
    ## ideally, grid would be 95%-ile -- revert to gstat?

    # shoreline -- assume and force shoreline speed to be 0  -- only needed for interpolation, not for averaging
    # add shoreline to drifter speeds
    if (0){
      drift_sf <- worldM %>%
        st_sf () %>%
        st_simplify(dTolerance=0.01) %>%  ## in meters
        st_cast ("MULTIPOLYGON") %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        st_as_sf (coords=c("X", "Y"), dim="XY", remove=TRUE, crs=4326) %>%
        st_transform(projection) %>%
        mutate (speed_ms = 0) %>%
        mutate (IDn = -1) %>%
        select (speed_ms, IDn) %>%
        rbind (drift_sf)
    }


    p <- require ('GVI')  ## for sf_to_rast
    if (!p){
      require ("remotes")
      remotes::install_github("STBrinkmann/GVI")
    }; rm (p)

    # save.image ("~/tmp/LCI_noaa/cache/drifter/drifterSpeedMap0.Rdata")
    require ('parallel')
    nCores <- detectCores()-1

    speed1 <- sf_to_rast (observer=drift_sf, v="speed_ms"
                          , aoi=st_sf (seaA)
                          , max_distance=10e3
                          , raster_res=5e3
                          , beta=3
                          , progress=TRUE
                          , cores=nCores  # no effect on windows? uses openMP?
    ) %>%
      st_as_stars () %>%  ## terra raster to stars
      st_warp(crs=projection)
  }else{
    ## consider using gstat to specify aggregating function = max
    require ("gstat")
    ## see https://mgimond.github.io/Spatial/interpolation-in-r.html
    ## create an empty grid see https://michaeldorman.github.io/starsExtra/reference/make_grid.html
    require ("starsExtra")
    grd <- starsExtra::make_grid (drift_sf, res=20e3)  # 1 km grid -- takes a while

    if (0){ ## serial processing?
      speedO <- gstat::idw (speed_ms~1, drift_sf, newdata=grd, idp=2.0
                            # , nmax=10e3
                            ,
      )
      ## clip to seaA
    }else{
      ## parallel interpolation
      ## see https://gis.stackexchange.com/questions/237672/how-to-achieve-parallel-kriging-in-r-to-speed-up-the-process
      vg <- variogram (speed_ms~1, drift_sf)
      mdl <- fit.variogram (vg, model=vgm (1, "Exp", 90, 1))
      # plot (vg, model=mdl)

      require ("parallel")
      nCores <- detectCores ()-1
      parts <- split (x=1:length (grd), f=1:nCores)

      if (.Platform$OS.type=="unix"){
        speedP <- mclapply(1:nCores, FUN=function (x){
          # gstat::idw (speed_ms~1, drift_sf, newdata=grd [parts[[x]],], idp=2.0, nmax=10e3)  ## requires sp class?
          krige (formula=speed_ms~1, locations=drift_sf, newdata=grd [parts[[x]],], model=mdl)
        })
      }else{
        cl <- makeCluster (nCores)
        clusterExport(cl=cl, varlist = c("grd", "drift_sf"), envir = .GlobalEnv)
        clusterEvalQ (cl=cl, expr = c(library ('sf'), library ('gstat'), library ('stars')))
        pX <- parLapply(cl=cl, X=1:nCores, FUN=function (x){
          krige (formula=speed_ms~1, locations=drift_sf, newdata=grd [parts[[x]],], model=mdl)
        })
        stopCluster (cl)
      }
      ## rbind grid--stars version of maptools
    }
  }
}



if (0){  ## redundant -- more kriging??
  ## variogram = excruciatingly slow -- subsample input?!  Parallelize
  v_mod_OK <- autofitVariogram(speed_ms~1, as(drift_sf, "Spatial"))$var_model
  # plot (v_mod_OK)

  # Create grid
  grid <- st_as_stars(st_bbox(st_buffer(drift_sf, 0.001)))  ## set resolution ?

  # Interpolation model
  g = gstat(formula = speed_ms~1, model = v_mod_OK$var_model, data = drift_sf)
  # plot (v_mod_OK, g) ## parameter cutoff needs to be specified


  if(0){
    require ('parallel')
    nCores <- detectCores()-1
    cl <- makeCluster (nCores)
    parts <- split (x=1:length (grid), f=1:nCores)
    clusterExport (cl=cl, varlist=c("drift_sf", "grid", "v_mod_OK", "g"), envir = .GlobalEnv)
    clusterEvalQ(cl = cl, expr = c(library('sf'), library('gstat')))
    parallelX <- parLapply(cl = cl, X = 1:no_cores, fun = function(x){
      krige(formula=log(zinc)~1, locations = drift_sf, newdata=grid[parts[[x]],], model = g)
    })
    stopCluster(cl)
  }

  # Interpolate
  z = predict(g, grid)  ## slow -- go back to parallel version?
  ## just do an IDW from the start -- for simplicity!

  # Plot
  plot(z, col = hcl.colors(12, "Spectral"), reset = FALSE)
  plot(st_geometry(drift_sf), add = TRUE)
  # text(st_coordinates(kerpensample_sf), as.character(round(kerpensample_sf$Z, 1)), pos = 3, add = TRUE)

  # grd <- drift %>%
  #   st_as_sf (coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
  #     st_bbox () |>
  #   st_as_stars (dx=1000) |>
  #   st_crop (drift)
  #
  # # https://r-spatial.org/book/12-Interpolation.html
  # require ('gstat')
  # v <- variogram (speed_ms~1, drift)
  save.image("~/tmp/LCI_noaa/cache/drifter/drifterKrige.RData")
  # load ("~/tmp/LCI_noaa/cache/drifter/drifterKrige.RData")
}







## -----------------------------------------------------------------------------
## select drifters to plot

## deployment summary table showing start dates and deployment length
# deployment <- aggregate (.~deploy, data=drift, FUN=function (x){x[1]})

## sort short to long deployments to plot short ones first
depL <- aggregate (DeviceDateTime~deploy, data=drift, FUN=function (x){
  length(x)
  #  difftime (max (x), min(x), units="hours")
}) %>%
  #  arrange (desc (DeviceDateTime))
  arrange (DeviceDateTime)
drift$depOrder <- (1:nrow (depL))[match (drift$deploy, depL$deploy)]
rm (depL)

## subset drifter database
## Port Graham to Cook Inlet: SVPI-0047
drift <- drift %>%
  dplyr::arrange (depOrder, DeviceDateTime) %>%
  #  dplyr::filter (DeviceName %in% c("UAF-SVPI-0046", "UAF-SVPI-0047", "UAF-MS-0066")) %>%
  ###  dplyr::filter (DeviceName %in% c("UAF-SVPI-0046", "UAF-SVPI-0047")) %>%
  #  dplyr::filter (DeviceName %in% c("UAF-SVPI-0048")) %>%  # many deploys?
  #  dplyr::filter (DeviceName == "UAF-SVPI-0046") %>%
  # dplyr::filter (DeviceDateTime < as.POSIXct("2022-07-30 00:00::00")) %>%
  # dplyr::filter (DeviceDateTime > as.POSIXct("2022-07-22 07:00")) %>%
  #  dplyr::filter (DeviceName == "UAF-MS-0066") # %>%
  #  dplyr::filter (speed_ms < speedTH) %>%
  dplyr::filter (Latitude > 58.7) %>%        ## restrict it to within Cook Inlet
  # dplyr::filter (DeviceDateTime > as.POSIXct("2020-01-01 12:00")) %>%
  dplyr::filter()

## need to set these after final drifter selection (days_in_water already per deploy)
drift <- drift %>%
  mutate (DeviceName=factor (DeviceName)) %>%
  mutate (deploy = factor (deploy)) %>%
  mutate (col=brewer.pal (8, "Set2")[drift$DeviceName]) %>% # 8 is max of Set2
  mutate (year=as.numeric (format (DeviceDateTime, "%Y"))) %>%
  dplyr::filter()
# drift$DeviceName <- factor (drift$DeviceName)
# drift$deploy <- factor (drift$deploy)
# drift$col <- brewer.pal (8, "Set2")[drift$DeviceName] # 8 is max of Set2
# as.numeric (drift$DeviceDateTime) - min (as.numeric (drift$DeviceDateTime))/3600 # in hrs





## -----------------------------------------------------------------------------
## interactive mapping
## can/should supply own basemap?
require ('mapview')  ## also see rMaps on GitHub
require ('webshot')
mymap <- drift %>%
  filter (year==2022) %>%
  filter (DeviceName %in% c("UAF-SVPI-0046", "UAF-SVPI-0047")) %>%  #, "UAF-MS-0066")) %>%
  filter (DeviceDateTime < as.POSIXct("2022-08-20 07:00")) %>%
  filter (DeviceDateTime > as.POSIXct("2022-07-21 18:00")) %>%
  #  filter (CommId != "300534060052710") %>%
  filter ((DeviceDateTime < as.POSIXct ("2022-08-10") | DeviceName == "UAF-SVPI-0047")) %>%  ## remove redeployment
  mutate (month=as.numeric (format (DeviceDateTime, "%m"))) %>%
  mutate (depth=topo * -1) %>%
  # group_by (DeviceName) %>%
  # summarize(m=mean(attr_data)) %>% st_cast ("LINESTRING") %>%  ## making a string from this?? not working
  mapview::mapview (zcol= c (# "DeviceName",
                             "days_in_water", "speed_ms", "depth")
                    , col.regions=colorRampPalette(heat.colors(20))
                    , map.types = c("Esri.WorldImagery", "Esri.WorldShadedRelief", "CartoDB.Positron")
  )
# mymap
mapshot (mymap, url=paste0 (outpath, "mapview.html"))
## zip up mapview.htlm + folder
rm (mymap)
## save for others  (ggplotly)
## saveWidget(object_name, file="map.html")





## -----------------------------------------------------------------------------
## define some colors

## from https://www.magesblog.com/post/2013-04-30-how-to-change-alpha-value-of-colours-in/
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}


## -----------------------------------------------------------------------------
## revert to R base-graphics -- keep it simple and flexible


plotBG <- function(downsample=0, dr=drift){
  ## start background details for plot
  nbox <- st_bbox (dr)
  pA <- projection == st_crs (4326)

  ## static plot of drifter tracks, colored by deployment
  par(mar=c(3,3,4,1))
  plot (st_geometry (worldM), col="beige", border=NA
        , xlim=nbox[c(1,3)], ylim=nbox[c(2,4)]
        , axes=pA
        , main="")
  # rm (pA, nbox)

  ## add google/bing map background -- ggmap an option?? needs token :(

  ## add bathymetry/topography
  if (0){
    depth <- st_as_stars(ifelse (mar_bathy$topo > 0, NA, mar_bathy$topo * -1)
                         , dimensions = attr(mar_bathy, "dimensions"))  ## move up if actually using!
    plot (depth, add=TRUE
          , nbreaks=100
          , compact=TRUE  ## still smothers everyting
          , col=colorRampPalette(c("lightblue", "darkblue"), interpolate="spline", bias=1)
          , main=""
    )
    ## add land back on
    plot (st_geometry (worldM), add=TRUE, col="beige") ## find a brewer color -- or satelite BG
  }else{
    ## see https://www.benjaminbell.co.uk/2019/08/bathymetric-maps-in-r-colour-palettes.html
    ## use topo colors for mar_bathy
    plot (mar_bathy, add=TRUE
          , downsample=downsample
          , col=c(colorRampPalette (c("darkblue", "lightblue"))(80)
                  , add.alpha (terrain.colors(100), 0.15))
          , breaks=c(seq (-400, 0, by=5), seq (0, 1000, by=10))
          , main=""
    )
    ## add land back on -- to tone down land
    # plot (st_geometry (worldM), add=TRUE, col=add.alpha("beige", 0.8), border=NA) ## find a brewer color -- or satelite BG
  }

  ## add bathymetry contours -- don't
  if (0){
    plot (st_contour (mar_bathy, contour_lines=TRUE
                      , breaks=seq (-500, -50, by=50))
          , add=TRUE, col="blue")
  }
}



cat ("Total time passed from startTime:", difftime(Sys.time(), startTime), "\n")
rm (startTime)
save.image ("~/tmp/LCI_noaa/cache/drifter/drifterSetup.Rdata")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifterSetup.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")


## call plotting code here?
# source ("Currents/plotDrifter.R")


for (i in 1:5) alarm()

## -----------------------------------------------------------------------------
## EOF
