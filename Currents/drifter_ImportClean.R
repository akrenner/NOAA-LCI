## analyse drifter tracks
## create animations and plots of current drifters
rm (list=ls())
# renv::restore()
print (startTime <- Sys.time())

test <- TRUE
test <- FALSE


## data-density map by tide cycle: slack, flood, ebb (3 h each)
## clean raw data: compare to modeled speeds



## tasks:
## fetch all drifter data from PacificGyre.com
## also add off-shore drifters?
## clean data: identify positions over land or transported by boat
## produce two versions: raw positions and interpolated values for animation

## review need for drift_sf -- remove?? driftX  XXX

# and animate through seasons (by jday -- or monthly plots)
## get OpenDrift to work on jupyter notebook -- check-point raw book in here.


## Define the whole drifter import and data processing as a function (it's slow).
## On re-runs, only reprocess new data

## animate both 2021 drifters on same day



# XXX interpolation: add interpolated records, mix them with raw, marking both for easy subsetting



speedTH <- 6 # max speed deemed realistic. Above which records are not plotted
ciofsF <- 1.5

## set interpolation [min] for animation
interP <- 10 # interpolation interval (min)
resolu <- c (resW=1080, resH=576, frameR=24) ## HD+ 1920 x 1080, HD: 1280x729, wvga: 1024x576
# resolu <- c (resW=1920, resH=1080, frameR=30)

## ----------------------------------------------------------
## set file locations
worldP <- "~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/f/GSHHS_f_L1.shp"   ## full resolution
# worldP <- "~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/h/GSHHS_h_L1.shp"   ## high
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
require ('oce')        ## for oce::geodist
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
worldMb <- sf::st_read (worldP, quiet=TRUE, crs=4326) %>%
  st_geometry()
worldM <- subset (worldMb, st_is_valid (worldMb)) %>% ## polygon 2245 is not valid
  st_crop (c(xmin=-154, xmax=-149, ymin=58, ymax=61.5)) %>%   ## or could use bbox above
  sf::st_transform(projection)
worldMb <- subset (worldMb, st_is_valid (worldMb)) %>%
  st_crop (c(xmin=150, xmax=-115, ymin=35, ymax=70)) %>%
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
if (1){  ## for the time being -- download updates manually
  message ("Download updated drifter data manually for the time being")
  # if (file.exists (updateFN) & (difftime (Sys.time(), file.info (updateFN)$ctime, units="days") < 7)){
  # message ("Drifter data downloaded within the last week: skipping update")
}else{
  options(timeout=180)
  download.file (url=paste0 ("https://api.pacificgyre.com/api2/getData.aspx?apiKey=", key,
                             "&FieldList=", FieldList,
                             "&startDate=", as.character (as.Date(endDate)+1), "%2000:00",
                             "&fileFormat=csv&compression=zip&download=Yes")## endDate defaults to now
                 , destfile=updateFN, mode=dM, cacheOK=TRUE  ## still having issues on Windows. Go manual.
                 )
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
  mutate (FileName="PacificGyre") %>%
  filter()
rm (readC, driftF, updateFN)



## BIG CHANGE:
## Use files from Research Workspace, uploaded by Scott Pegau
## These may be cleaned? And have more detailed drogue depth information
## downloaded from https://researchworkspace.com/file/41810436/CIDrifter0013Y2012_SubsurfaceDrogueAt15M_data.csv
require ("readr")
driftF <- paste0 (driftP, "drifter-researchworkspace.zip")
# driftF <- "~/GISdata/LCI/drifter-researchworkspace.zip"
fileL <- unzip(driftF, list = TRUE)$Name
fileL <- grep(".csv$", fileL, value = TRUE)
# for (i in 1:length (fileL)){
#   cat (i, "\n")
#   d1 <- read_csv (unz (driftF, fileL [i]))
#   d1$fn <- fileL [i]
# }
driftWS <- dplyr::bind_rows(lapply(fileL, function(fn){
  suppressMessages ({
  readr::read_csv(unz(driftF, fn)) %>%
      mutate (drogue_depth=strsplit(fn, "m/")[[1]][1] %>%
                as.numeric()) %>%
      mutate(DeviceName=strsplit (fn, "/")[[1]][2]) %>%
      mutate (FileName=fn) %>%
      arrange (FileName, DeviceName, Year, Month, Day, Hour, Minute) %>%
filter()
  })
}
))
rm (fileL, driftF)
# rm (driftP)


## merge WorkSpace files with PacificGyre downloads
spd <- function (x){
  require ("stringr")
  str_pad(x, width=2, side="left", pad="0")
}

driftWS <- driftWS %>%
  # mutate (DeviceDateTimeTxt=paste0 (Year, "-", spd (Month), "-", spd (Day), " ", spd (Hour), ":", spd (Minute), ":00")) %>%
  mutate (DeviceDateTime = as.POSIXct(paste0 (Year, "-", spd (Month), "-", spd (Day), " "
                                              , spd (Hour), ":", spd (Minute), ":00")
                                      , tz="GMT")) %>%
  mutate (Latitude=Lat) %>%
  mutate (Longitude=Long)
names (driftWS) <- gsub ("-", "_", names (driftWS), fixed = TRUE)
rm (spd)

# ## QAQC
# head (driftWS$DeviceDateTime)  # if (any (is.na ()))
# summary (driftWS$Hour%%1)      # all integr?
# summary (driftWS$Minute)
# for (i in 1:nrow (driftWS)){
#   x <- with (driftWS [i,], as.POSIXct (paste0 (Year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":00"), tz="GMT"))
#   #  if (format (x, "%H") == "00"){with (driftWS [i,], print (paste (i, x, Year, Month, Day, Hour, Minute)))}
#   if (nchar (as.character (driftWS$DeviceDateTime [i])) < 10){
#   #  if (nchar (as.character (x)) < 10){
#       print (with (driftWS[i], paste (i, x, Year, Month, Day, Hour, Minute)))
#   }
# }
# head (format (driftWS$DeviceDateTime, "%H %M"))
# x <- driftWS [which (driftWS$Minute==00),]
# y <- driftWS [which (driftWS$Minute!=00),]
# nrow (driftWS [which (driftWS$Hour==00),]) / nrow (driftWS)
#
# inshoreD <- driftWS %>%
#   filter (Hour != 0)
# inshoreFile <- levels (factor (inshoreD$FileName))
# inshoreDevice <- levels (factor(inshoreD$Drifter))
#
# ## only use drifters that report more than once per day
# driftWSx <- subset (driftWS, Drifter %in% inshoreDevice)
# driftWSx <- subset (driftWS, FileName %in% inshoreFile)
# ## no effect to subset by these -- some intervals are 24 h -> need to ensure that > 23 h is classified as a new deployment



drift <- with (drift, data.frame(Drifter=DeviceName,
                                 Year=as.numeric (format (drift$DeviceDateTime, "%Y")),
                                 Month=as.numeric (format (drift$DeviceDateTime, "%m")),
                                 Day=as.numeric (format (drift$DeviceDateTime, "%d")),
                                 Hour=as.numeric (format (drift$DeviceDateTime, "%H")),
                                 Minute=as.numeric (format (drift$DeviceDateTime, "%M")),
                                 Lat=Latitude, Long=Longitude,
                                 U_Vel=rep (NA, nrow (drift)),
                                 V_Vel=rep (NA, nrow (drift)),
                                 drogue_depth=rep (NA, nrow (drift)),
                                 DeviceName,                                           ## redundant!!
                                 FileName,
                                 Deployment = rep (NA, nrow (drift)),
                                 Temperature = rep (NA, nrow (drift)),
                                 Salinity = rep (NA, nrow (drift)),
                                 s = rep (NA, nrow (drift)),
                                 DeviceDateTime, Latitude, Longitude))

## summary of annual coverages
if(0){
  aggregate (DeviceName~Year, data=driftWS, function (x){length(x)})
  aggregate (DeviceName~Year, data=drift, function (x){length(x)})
  plot (Latitude~Longitude, data=driftWS, subset=Year < 2012)
  plot (Latitude~Longitude, data=drift, subset=Year > 2020)
}

## append latest records from PacificGyre to records already processed by Scott Pegau
drift <- rbind (driftWS, subset (drift, Year > max (driftWS$Year)))
# drift <- rbind (subset (driftWS, Year < min (drift$Year)), drift)
rm (driftWS)








## remove duplicates (about 80 in contiguous download)
# drift <- drift %>%
#   filter (!duplicated (drift [,which (names (drift) %in%
#                                         c("DeviceName", "DeviceDateTime") )])) %>%
#   filter()
Nd <- nrow (drift)
drift <- subset (drift, !duplicated (paste (drift$DeviceName, drift$DeviceDateTime)))
cat (paste0 ("N drifter points: ", Nd, ". Removed ", Nd - nrow (drift), " duplicates ("
     , (Nd-nrow(drift))/nrow(drift), " %)\n\n")); rm (Nd)
# drift2 <- subset (drift, !duplicated (paste (drift$Latitude, drift$Longitude, drift$DeviceDateTime)))

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

drift$dT_sec <- c (0, diff (drift$DeviceDateTime, units="secs"))
drift$dT_min <- drift$dT_sec / 60

drift$distance_m <- c (0, diff (oce::geodDist(drift$Longitude, drift$Latitude, alongPath=TRUE)*1e3))  ## verified and correct -- sqrt (u2 + v2) is the same
drift$speed_ms <- drift$distance_m / drift$dT_sec  ## filter out speeds > 6 (11 knots) -- later

## QAQC
head (drift$DeviceDateTime)
head (drift$dT_min)

## QAQC
if (0){
  summary (drift$speed_ms)
  head (drift$DeviceDateTime)
  head (drift$dT_min)
}


# consider: using dI here instead of drift
drift <- st_as_sf (drift, coords=c("Longitude", "Latitude")   ### why not keep if for drift?
                , dim="XY", remove=FALSE, crs=4326) %>%
  st_transform(projection)

crds <- st_coordinates(drift) |> as.data.frame()
drift$u_vel2 <- c (0, (diff (crds$X))) / drift$dT_sec
drift$v_vel2 <- c (0, (diff (crds$Y))) / drift$dT_sec
rm (crds)

## use morph..
drift$topo <- st_extract(mar_bathy, at=drift)$topo

## using LandDistance_m <- st_distance(worldM, dx) %>% apply (2, min) is slower by orders of magnitude
drift$LandDistance_m <- worldM %>%
  st_union() %>%
  st_distance(drift, by_element=FALSE) %>%
  as.numeric()
## drift$onLand <- !is.na (st_intersects(dx, st_union (worldM)) |> as.numeric()) ## not pretty, not reliable. Skip for now
drift$onLand <- st_intersects(drift, worldM) %>% as.numeric () ## not pretty, not reliable. Skip for now
# dx$onLand <- st_join (dx, st_sf (worldM), join=st_within)
# dx <- st_filter (dx, seaA)  ## supposed to filter out points on land
# rm (dx)

## ice wave rider and MicroStar are surface devices
drift$drogue_depth <- ifelse (is.na (drift$drogue_depth)
                              , ifelse (seq_len (nrow (drift)) %in% grep ("UAF-SVP", drift$DeviceName), 15, 0)
                              , drift$drogue_depth
                              ) |> factor ()
# drift$deployDepth <- ifelse (seq_len(nrow (drift)) %in% grep ("UAF-SVP", drift$DeviceName), 15, 0) |> factor()
# drift$Year <- format (drift$DeviceDateTime, "%Y") |> factor()  ## above should be piped and mapped XXX









## -------------------------------------------------------------------------------------------
## rtide to retire -- use NOAA server
## calculate state of the tide to subset data for maps, separating flood/slack/ebb

## XXX can't currently reproduce results of tide_slack_data XXXXX -- try different harmonics
## tide test
if (0){
  # tide_slack_data_station (tide_station ("Seldovia*", Sys.time()))
  # tide_slack_data_datetime (as.POSIXct ("2020-05-01 15:00", tz= "GMT"), tide_station("Seldovia*"))
  # hist ((runif (1e6) - runif (1e6)))

#  require ('rtide')
  tide_slack_data (as.POSIXct ("2020-05-01 15:00", tz= "GMT"), tide_stations("Kasitsna*"))

  times <-  tide_datetimes(
    minutes = 60L,
    from = as.Date("2015-01-01"),
    to = as.Date("2015-01-01"),
    #    tz="-0800"
    #    tz = "PST8PDT"
  )
  tide_height()  # tide heigth at station and times to-from
  tide_height_data(data.frame (DateTime=as.POSIXct("2015-01-01 00:00", tz="GMT"), Station=tide_stations ("Monterey Harbor*")))
  tide_slack_data(data.frame (DateTime=as.POSIXct("2015-01-01 12:00", tz="GMT"), Station=tide_stations ("Monterey Harbor*")))

  tide_slack_data(data.frame (DateTime=as.POSIXct("2015-01-01 12:00", tz="GMT"), Station=tide_stations ("Seldovia*")))


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
yL <- levels (drift$Year)

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

## move tide functions into function script, load that
# if (test){save.image ("~/tmp/LCI_noaa/cache/drifter/drifterTide2.Rdata")}
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

## interval between samples to gauge new deployments
drift <- drift %>%
#  st_drop_geometry() %>%
#  filter (speed_ms > 0.0001) %>%
#  filter (speed_ms < 20) %>%
#  filter (dT_min < 7*24*60) %>%
  mutate (dT_f=5*round (dT_min/5)) %>%
  mutate (dT_f=ifelse(dT_f > 24*60, 24*60, dT_f)) %>%
  mutate (dT_f=factor (dT_f))
invlSum <- summary (drift$dT_f, maxsum=1e3)
invlSum [order (as.numeric (names (invlSum)))]
drift$natIntvl <- (drift$dT_min > 4 & drift$dT_min < 40) |
  (drift$dT_min > 55 & drift$dT_min < 70) |
  # (drift$dT_min > 0 & drift$dT_min < 70) |  ## take this out?? XXX
  (drift$dT_min > 235 & drift$dT_min < 255)

if (0){
  hx <- hist (subset (drift, (dT_min > 30) & (dT_min < 60*7))$dT_min |> log()
              , breaks=200, axes=FALSE)  # there are 3 peaks only
  axis (1, at=pretty (hx$breaks)
        , labels = round (exp (pretty (hx$breaks))))
  rm (hx)
}
## intervals found: 10, 30, 60[], 240[]
##
# invlSum/nrow (drift)
drift$ivlNew <- drift$dT_f %in% c(10, 20, 25, 30, 35, 55, 60, 65  ## intervals around 30, 50, 240
                                  , 120, 235, 240, 245, 250)




## -------------------------------------------------------------
## match with max speed from CIOFS (produced by ciofs_maxCurrent.R)

require ("stars")
load ("~/tmp/LCI_noaa/cache/ciofs_maxspeed.RData") # speedS -- stars
drift$ciofs_sp <- speedS %>%
  st_extract (drift) %>%
  st_drop_geometry() %>%
  pull (maxSpeed)

if (0){
  plot (ciofs_sp~speed_ms, drift)
  abline (b=1)
}

# pU <- stars::read_stars ("~/tmp/LCI_noaa/data-products/CIOFS/coifs_maxspeeds_prU_bc.tif")
# pV <- stars::read_stars ("~/tmp/LCI_noaa/data-products/CIOFS/coifs_maxspeeds_prV_bc.tif")






## -------------------------------------------------------------

## criteria by which to weed out positions
# over land
# speeds over max speed * 1.1 -- flag those
# first of bout
# last of bout ?
# wrong direction: u, v
# long time after previous (more than 1 h) ????  -- that's a bout



if (any (is.na (drift$ciofs_sp))){ ## replace with spatial interpolation (voronoi) XXX
  drift$ciofs_sp [which (is.na (drift$ciofs_sp))] <- max (drift$ciofs_sp, na.rm=TRUE)
}

## better than filter: mark bad, then cut chunks of bad positions?
drift$off_deploy <- ifelse (is.na (drift$speed_ms), TRUE, FALSE)                # NA speed -- only 1st record
drift$off_deploy <- ifelse (drift$dT_min > 60*12, TRUE, drift$off_deploy)       # 12 h is always too long
drift$off_deploy <- ifelse (drift$speed_ms < 0.0000001, TRUE, drift$off_deploy) # standing too still
drift$off_deploy <- ifelse (drift$speed_ms > drift$ciofs_sp * ciofsF, TRUE, drift$off_deploy) # too fast, 10% faster than max -- way too many?? XX
drift$off_deploy <- ifelse (drift$dT_min < 0.00000001, TRUE, drift$off_deploy)  # same time
drift$off_deploy <- ifelse (is.na (drift$topo), drift$off_deploy,
                            ifelse (drift$topo > 1, TRUE, drift$off_deploy))  # depth > 1 m => over land

if (any (is.na (drift$off_deploy))){stop ("1")}
## distinguish: 1 bad GPS signal vs boat transport -- possible?


## ----------------------------------------------------------------------------
## define individual drifter deployments

## important definition! Don't interpolate between deployments
## missed satellite?

newDeploy <- (drift$dT_min > 250) | (!duplicated (drift$DeviceName)) ## 700 -- dT_min in min. mark new deployments XXX test!! 240 min = 4 h
# newDeploy <- drift$natIntvl | !duplicated (drift$DeviceName)     ## 300k -- too much
# print (summary (newDeploy))

# cbind (newDeploy, drift)[275215:275227,
#                          c(1,1+which (names (drift) %in% c("DeviceName", "DeviceDateTime", "dT_min")))]


## mark new deployments
x <- 0; depIdx <- character(nrow (drift)) # declare variables
for (i in 1:length (newDeploy)){
  if (newDeploy [i]){
    # x <- x + 1  ## consecutive numbers
    x <- drift$DeviceDateTime [i]
  }
  depIdx [i] <- paste0 (drift$DeviceName [i], "-", x)
}
drift$deploy <- factor (depIdx)
# head (summary (drift$deploy))
rm (newDeploy, x, depIdx, i)

cat ("\n\n### N deployments: ####\n")
print (length (levels (drift$deploy)))


if (test){
  drift <- subset (drift, deploy %in% sample (levels (drift$deploy), 5))
  drift$deploy <- factor (drift$deploy)  ## reset levels
}






if (test){save.image ("~/tmp/LCI_noaa/cache/drifter/driftSped.RData")}
save.image ("~/tmp/LCI_noaa/cache/drifter/driftSped.RData")
## rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/driftSped.RData")



# ----------------------------------------------------------------------------
## interpolate within bouts
## trim bad speeds at start and end

if (exists ("iDF")){rm (iDF)} ## in case of reruns of code
unlink(paste0 (outpath, "deployment/"), recursive=TRUE)
dir.create(paste0 (outpath, "deployment/"), showWarnings=FALSE, recursive=TRUE)


dInt <- function (i){
  ## step-by-step re-implementation to work with new dataset
  require ("sf")
  require ("dplyr")
  dfd <- subset (drift, deploy == levels (drift$deploy)[i])

  ## less than 4 positions in deployment
  if (nrow (dfd) < 4){dfd$off_deploy <- TRUE}

  ## bad GPS: addressed by CIOFS?

  ## no movement in 2-h blocks -- XXX unfinished
  tBlock <- 20
  buf <- nrow (dfd) %% tBlock

#  ## first and last (not enough!) -- some clean?
#  dfd$off_deploy [1] <- TRUE
#  dfd$off_deploy [nrow (dfd)] <- TRUE


  ## only consider the end 1/4 of deployment for cutting?

  ## no bad data points
  # dfd$off_deploy [c(1, nrow (dfd))] <- FALSE
fN <- gsub (":", "_", paste0 (outpath, "deployment/", i, levels (drift$deploy)[i]))

  ## output plots and individual CSV files
  write.csv(dfd, file=paste0 (fN, ".csv"), row.names = FALSE)

  ## plot deployments
  png (paste0 (fN, ".png"), width=6*300, height=3*300)
  par (mfrow=c(1,2))
  plot (Lat~Long, dfd, type="n")
  plot (worldM %>% st_transform(crs=4326), add=TRUE, col="beige")
  lines (Lat~Long, dfd)
  points (Lat~Long, dfd
          , col = ifelse (dfd$off_deploy, "red", "blue")
          , pch=ifelse (dfd$off_deploy, 19, 1))

  plot (speed_ms ~ DeviceDateTime, data=dfd, type = "l", col = "green", lwd=2)
  lines (ciofs_sp ~ DeviceDateTime, data=dfd, col = "blue", lwd=2)
  legend ("topleft", col=c("green", "blue"), lwd=2, legend=c("drift", "ciofs"))
  dev.off()



  ## interpolations
  newDF <- dfd

  newDF$days_in_water <- with (newDF, difftime(DeviceDateTime, min (DeviceDateTime), units="days"))|> as.numeric()
  newDF
}

dIntX <- function (i){
  require ("sf")
  require ("dplyr")
  df <- subset (drift, deploy == levels (drift$deploy)[i])

  if (nrow (df) > 4){  # discard deployments shorter than this off-hand

    if (1){   ## cut out bad positions -- {} to fold code
      ### cut-out long stationary periods, in case those are longer than deployments
      ## evaluate 2-h blocks -- should move at least 100 m from start to finish
      tBlock <- 20
      buf <- nrow (df) %% tBlock

      if (0){  # nrow (df) > 40){    ## can produce NAs
        df$blockSpeed_ms <- sapply (nrow (df):buf, function (m){
          oce::geodDist(df$Longitude [m], df$Latitude [m]
                        , df$Longitude [m-tBlock], df$Latitude [i-tBlock]) * 1e3 #/ (df$DeviceDateTime [m] - df$DeviceDateTime [m-tBlock])
        }) %>%
          c (rep (NA, buf-1)) %>%
          rev()
        # plot (blockSpeed_ms~distance_m, df)
        # plot (df$speed_ms, type="l")
        # plot (df$blockSpeed_ms, type="l")
        # plot (df$blockSpeed_ms, col=ifelse (df$blockSpeed_ms < 100, "red", "black"))
      }
      if (!"blockSpeed_ms" %in% names (df)){df$blockSpeed_ms <- 5000}

      badGPS <- c (rep (FALSE, 3), sapply (3:nrow (df), FUN=function (k){
        ## single bad GPS position: previous speed is fine, next is bad, next+1 is good
        if (k < (nrow (df)-2) &
            (df$speed_ms [k-1] < speedTH) &
            (df$speed_ms [k] > speedTH) &
            (df$speed_ms [k+2] < speedTH)){
          out <- TRUE
        }else{
          out<- FALSE
        }
        out
      }))

      if (any (badGPS)){  ## remove these separately, as to not jeapardize ID of boat tracks
        df <- subset (df, subset=!badGPS)
        df$dT_min <- c (0, diff.POSIXt (df$DeviceDateTime, units="min"))
        df$distance_m <- c (0, diff (oce::geodDist(df$Longitude, df$Latitude, alongPath=TRUE)*1e3))
        df$speed_ms <- with (df, distance_m / (dT_min*60)) ## filter out speeds > 6 (11 knots) -- later
      }


      resx <- with (df, speed_ms - mapped_speed)
      sdTH <- median (df$speed_ms, na.rm=TRUE) + 3 * sd (df$speed_ms, na.rm=TRUE)
      sdMT <- mean (resx, na.rm=TRUE) + 5*sd (resx, na.rm=TRUE)

      bad <- c (which (df$topo > 10), # which (df$onLand > 0),
                which (resx > sdTH),
                which (df$speed_ms > sdTH),
                which (df$speed_ms > sdMT),
                which (df$speed_ms > speedTH)  # can still have sequence of points > speedTH
      ) %>%
        unique()

      ## only consider the end 1/4 of deployment for cutting
      suppressWarnings({trimb <- c (max (subset (bad, bad < (nrow (df)*0.20)))+1,
                                    min (subset (bad, bad > (nrow (df)*0.85)))-1)
      })
      ## in case there are no bad data points
      trimb <- ifelse (trimb == -Inf, 1, trimb)
      trimb <- ifelse (trimb == Inf, nrow (df), trimb)
      df$trimBoat <- TRUE
      df$trimBoat [seq (trimb[1], trimb[2])] <- FALSE
      df$trimBoat [bad] <- TRUE


      png (paste0 ("~/tmp/LCI_noaa/media/drifter/deployment/trim_", i, ".png")
           , height=resolu [2], width=resolu [1])
      par (mfrow=c(1,2))
      plot (df$speed_ms, col=ifelse (df$trimBoat, "red", "black"), pch=19)
      abline (h=sdTH, lty="dashed")
      abline (h=speedTH, lty="dotted", col="green")
      abline (v=trimb, lty="dotted")

      pDF <- df %>%
        st_as_sf (coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
        st_transform(projection) %>%
        st_geometry()
      plot (pDF, col=ifelse (df$trimBoat, "red", "black"), pch=19, cex=0.7)
      plot (worldM, add=TRUE, col="lightgray")
      pC <- st_coordinates(pDF)
      for (j in 1:(nrow (df)-1)){
        segments (pC [j,1], pC [j,2], pC [j+1,1]
                  , pC [j+1,2], col=ifelse (df$trimBoat, "red", "black")[j]
                  , lwd=2)
      }
      dev.off()
      rm (trimb, sdTH, bad, resx)


      if (1){
        ## set NAs in bad points to prevent interpolation across them
        is.na (df$Latitude [which (df$trimBoat)]) <- TRUE
        is.na (df$Longitude [which (df$trimBoat)]) <- TRUE
        is.na (df$DeviceDateTime [which (df$trimBoat)]) <- TRUE
      }

      ## reset time in the water
      # df <- df [seq (trimb[1], trimb[2]),]  ## trim the ends off
      # df$days_in_water <- with (df, difftime(DeviceDateTime, min (DeviceDateTime), units="days"))|> as.numeric()
    }


    ## trim newDF XXXX  -- cut bad stuff front and back
    ## at the very least: first and last
    ## from start to 1/2 nrow: trim 0 to last bad
    ## from 1/2 nrow to end: trim first bad to nrow

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
    newDF$Year <- format (newDF$DeviceDateTime, "%Y") %>% as.numeric() %>% as.factor()
    newDF$tide <- df$tide [1]  ## quick and dirty -- ok for short intervals
    newDF$mapped_speed <- df$mapped_speed [1]  ## XXX dirty, but fine here
    newDF$interp <- TRUE

    df$interp <- FALSE
    newDF <- rbind (df, newDF)
    ## interpolate all other variables; closest for categoricals
    newDF$days_in_water <- with (newDF, difftime(DeviceDateTime, min (DeviceDateTime), units="days"))|> as.numeric()

  }else{
    newDF <- df
    newDF$off_deploy <- TRUE
    # newDF <- NULL
  }
  newDF
}


# x <- dInt (1)




if (0){   ## parallel processing
  require ("parallel")
  require ('data.table')
  nCores <- detectCores()-1

  if (.Platform$OS.type=="unix"){
    iDF <- mclapply(seq_along (levels (drift$deploy)), FUN=dInt, mc.cores=nCores) %>%
      rbindlist()
  }else{
    cl <- makeCluster(nCores, type="PSOCK")
    clusterExport(cl, varlist=c("drift", "interP", "outpath", "dInt", "speedTH", "projection", "resolu", "worldM"))
    clusterEvalQ(cl, require ("dplyr"))
    iDF <- parLapply (cl, seq_along (levels (drift$deploy)), fun=dInt) %>%
      rbindlist()
    stopCluster(cl); rm (cl)
  }
  rm (nCores)
}else{
  for (i in seq_along (levels (drift$deploy))){               ## serial=slow! cache?; use dplyr split/group
    x <- dInt (i)
    cat (i, "\n")
    if (i == 1){
      iDF <- x
    }else{
      iDF <- rbind (iDF, x)
    }
  }
}
rm (dInt)





# iDF <- subset (iDF, speed_ms < speedTH)  ## apply again --- any way to get pre/post deploy more thorough?

## project positions -- don't move earlier to allow interpolations  ???
drift <- iDF %>%
  filter (!is.na (Longitude)) %>%  ## not sure where they came from, but can't have it XXX
  st_as_sf (coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
  st_transform(projection)
rm (interP, iDF)

if (any (is.na (drift$off_deploy))){stop ("2")}




## testing
if (0){
  sort (summary (drift$deploy), decreasing=TRUE) |> head()
  x <- subset (drift, deploy=="UAF-SVPI-0047-2022-07-21 18:30:10")
  plot (x)
  summary (x$DeviceDateTime)
  summary (x$days_in_water)
  levels (factor (x$DeviceName))
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





## --------------------- manually fix bad positions ------------------------- ##
## screening video plots
save.image ("~/tmp/LCI_noaa/cache/drifterScreen.RData")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifterScreen.RData")

## generate external file of deployments

x <- as.character ("
# 16 2012-08-13 17:00 to 2012-08-14 00:00
# 24 start to 2012-08-27 00:00
# 25 2013-04-14 07:00 to end
# 47 2013-04-21 18:00 to 2013-04-22 00:00
# 70 2012-08-15 19:00 to 2012-08-15 23:00
# 71 start to 2012-08-26 20:00
# 78 start to 2014-11-07 07:00
# 112 start to 2005-12-25 01:00
# 144 2003-08-14 20:00 to 2003-08-15 10:00
# 145 2003-08-19 20:00 to 2003-08-20 22:00 and 2003-08-24 03:00 to 2003-08-24 17:00
# 145 2003-08-26 00:00 to 2003-08-27 09:00 and 2003-08-28 04:00 to 2003-08-30 17:00
# 145 2003-08-31 06:00 to end
# 157 2019-06-03 21:40 to end
# 159 2019-06-06 14:24 to end
# 161 2019-07-04 13:30 to end
# 162 2020-07-04 19:30 to end
# 163 2019-07-06 23:00 to 2019-07-07 01:10
# 164 2020-07-03 23:10 to end
# 165 2019-07-30 21:50 to end
# 167 2019-08-01 23:30 to 2019-08-02 01:10 and 2019-08-03 18:40 to end
# 168 2020-07-05 12:00 to end
# 170 2020-07-06 20:20 to end
# 171 2020-07-08 18:20 to end
# 174 2020-08-02 18:10 to end
# 175 2020-08-03 19:30 to 2020-08-04 02:00 and 2020-08-04 21:30 to end
# 176 2020-08-06 00:50 to end
# 177 2020-08-06 12:50 to end
# 179 2020-09-19 20:20 to end
# 180 2020-06-07 07:30 to end
# 185 2020-06-20 22:00 to end
# 187 2020-07-30 01:30 to 2020-07-30 05:50
# 188 2012-09-13 18:20 to end
# 195 start to end
# 196 start to end
# 197 start to end
# 198 start to end
# 199 start to end
# 200 start to 2021-03-29 00:00
# 200 2021-04-03 20:10 to end
# 201 start to 2021-04-24 23:00
# 201 2021-04-27 18:40 to 2021-04-28 02:00 and 2021-05-02 19:10 to end
# 202 start to 2021-05-24 01:00 and 2021-05-26 15:30 to 2021-05-27 01:40
# 204 start to end
# 205 start to 2021-07-21 23:00 and 2021-07-25 23:40 to 2021-07-26 01:30 and 2021-07-27 18:50 to end
# 206 start to 2021-08-19 23:50 and 2021-08-21 21:30 to 2021-08-23 01:10
# 206 2021-08-23 20:10 to 2021-08-23 01:10 and 2021-08-25 18:10 to end
# 207 start to 2021-09-20 00:30
# 208 start to end
# 209 start to 2021-05-24 01:20 and 2021-05-26 15:30 to 2021-05-27 01:40 and 2021-05-29 15:10 to end
# 210 start to 2021-06-23 00:00 and 2021-06-24 18:00 to 2021-06-24 21:10 and 2021-06-27 15:30 to end
# 211 start to 2021-07-21 22:30 and 2021-07-25 23:20 to 2021-07-26 01:20 and 2021-07-27 17:00 to end
# 212 start to 2021-08-20 00:10 and 2021-08-21 20:50 to end
# 213 start to 2021-03-29 00:20 and 2021-04-01 04:40 to end
# 214 start to 2021-04-25 00:00 and 2021-05-02 19:00 to end
# 215 start to 2021-05-23 23:30 and 2021-05-26 20:10 to 2021-05-27 03:30 and 2021-05-29 21:50 to end
# 216 start to 2021-06-22 23:00 and 2021-06-24 18:40 to 2021-06-24 22:10 and 2021-06-27 16:00 to end
# 217 start to 2021-07-21 22:30 and 2021-07-24 04:10 to 2021-07-26 05:10 and 2021-07-27 19:30 to end
# 218 start to 2021-08-19 23:50 and 2021-08-20 22:00 to 2021-08-23 00:20 and 2021-08-23 21:30 to 2021-08-24 02:00 and 2021-08-25 17:10 to end
# 219 start to 2021-09-20 01:10
# 220 start to 2021-03-29 00:00 and 2021-03-31 12:20 to end
# 221 start to 2021-04-24 22:40 and 2021-04-27 20:40 to 2021-04-28 01:50 and 2021-04-30 10:50 to end
# 222 start to 2021-05-24 01:10 and 2021-05-26 18:30 to 2021-05-27 04:30 and 2021-05-29 11:10 to end
# 223 start to 2021-06-22 22:50 and 2021-06-24 18:30 to 2021-06-24 21:50 and 2021-06-27 16:00 to end
# 224 start to 2021-07-21 22:20 and 2021-07-26 00:10 to 2021-07-26 01:40 and 2021-07-27 17:50 to end
# 225 start to 2021-08-19 22:10 and 2021-08-20 07:20 to 2021-08-23 00:50 and 2021-08-23 21:30 to 2021-08-24 01:20 and 2021-08-25 17:30 to end
# 226 start to 2021-09-20 00:10 and 2021-09-21 22:00 to 2021-09-23 00:10
# 227 start to end
# 228 start to end
# 229 start to end
# 230 start to end
# 231 start to 2022-04-16 23:20 and 2022-04-18 15:40 to 2022-04-18 20:40 and 2022-04-20 11:40 to end
# 232 start to 2022-05-14 21:20 and 2022-05-16 19:20 to 2022-05-17 14:40 and 2022-05-18 15:50 to 2022-05-18 18:20 and 2022-05-20 21:10 to end
# 233 start to 2022-06-12 23:50 and 2022-06-13 17:10 to 2022-06-14 22:00 and 2022-06-16 01:20 to end
# 234 start to 2022-07-11 18:30 and 2022-07-13 18.10 to 2022-07-14 23:50 and 2022-07-15 19:30 to end
# 235 start to 2022-08-10 22:30 and 2022-08-12 19:40 to 2022-08-14 00:50 and 2022-08-14 19:10 to end
# 236 start to 2022-09-08 18:40 and 2022-09-11 09:50 to end
# 238 start to 2021-03-29 00:20 and 2021-04-03 19:30 to end
# 239 start to 2021-04-24 22:50 and 2021-04-27 16:40 to 2021-04-28 01:10 and 2021-05-02 18:20 to end
# 240 start to end
# 241 start to 2021-06-22 23:10 and 2021-06-24 17:40 to 2021-06-25 00:20 and 2021-06-27 15:30 to end
# 242 start to end
# 243 start to 2021-09-20 00:20
# 244 start to end
# 245 start to end
# 246 start to end
# 247 start to end
# 248 start to 2022-04-16 23:30 and 2022-04-18 14:40 to 2022-04-18 20:50 and 2022-04-20 01:10 to end
# 249 start to 2022-05-14 20:40 and 2022-05-16 18:30 to 2022-05-17 15:00 and 2022-05-18 15:50 to 2022-05-18 18:30 and 2022-05-20 18:20 to end
# 250 start to 2022-06-12 17:50 and 2022-06-13 17:10 to 2022-06-14 20:50 and 2022-06-16 21:40 to end
# 251 start to 2022-07-11 17:20 and 2022-07-13 18:30 to 2022-07-14 20:10 and 2022-07-17 19:40 to end
# 252 start to 2022-08-10 19:00 and 2022-08-12 19:50 to 2022-08-14 01:10 and 2022-08-14 19:30 to end
# 253 start to 2022-09-08 19:20 and 2022-09-11 00:20 to end
# 254 start to 2022-04-16 20:00 and 2022-04-18 14:30 to 2022-04-18 20:40 and 2022-04-21 01:50 to end
# 254 start to 2022-04-16 23:30 and 2022-04-18 14:40 to 2022-04-18 20:30 and 2022-04-21 04:40 to end
# 256 start to 2022-04-16 22:40 and 2022-04-18 15:10 to 2022-04-18 20:50 and 2022-04-20 15:00 to end
# 257 start to 2022-05-14 20:10 and 2022-05-16 19:40 to 2022-05-17 14:40 and 2022-05-18 16:50 to 2022-05-18 18:40 and 2022-05-20 22:10 to end
# 258 start to 2022-06-12 23:30 and 2022-06-13 17:40 to 2022-06-15 00:20 and 2022-06-16 01:00 to end
# 259 start to 2022-07-11 16:30 and 2022-07-13 19:10 to 2022-07-14 20:00 and 2022-07-15 20:30 to end
# 260 start to 2022-08-10 18:30 and 2022-08-12 18:40 to 2022-08-14 01:00 and 2022-08-14 19:30 to end
# 261 start to 2022-09-08 19:10 and 2022-09-11 16:50 to end
# 262 start to 2022-04-19 01:50 and 2022-04-20 15:10 to end
# 263 start to 2022-05-15 00:40 and 2022-05-16 19:40 to 2022-05-16 22:00
# 264 start to 2022-04-17 01:40 and 2022-04-18 14:50 to 2022-04-19 01:30 and 2022-04-20 15:10 to end
# 265 start to 2022-05-14 19:30 and 2022-05-16 20:00 to 2022-05-17 15:00 and 2022-05-18 16:10 to 2022-05-18 18:30 and 2022-05-20 18:50 to end
# 266 start to 2022-06-12 18:10 and 2022-06-13 16:50 to 2022-06-14 19:50 and 2022-06-15 17:50 to end
# 267 start to 2022-07-11 18:10 and 2022-07-13 19:20 to 2022-07-14 19:50 and 2022-07-15 20:00 to end
# 268 start to 2022-08-10 19:00 and 2022-08-12 04:20 to 2022-08-14 01:00 and 2022-08-14 19:30 to end
# 269 start to 2022-09-08 19:50 and 2022-09-10 10:30 to end
# 270 start to 2022-09-22 19:10
# 271 start to end
# 272 start to end
# 273 start to 2022-04-17 01:30 and 2022-04-18 14:30 to 2022-04-19 01:20 and 2022-04-20 15:00 to end
# 274 start to 2022-05-15 00:20 and 2022-05-18 15:20 to 2022-05-19 14:40 and 2022-05-20 21:50 to end
# 275 start to 2022-06-12 23:50 and 2022-06-13 17:50 to 2022-06-15 00:20 and 2022-06-16 00:40 to end
# 276 start to 2022-07-11 22:50 and 2022-07-12 18:30 to 2022-07-13 00:00 and 2022-07-13 15:50 to 2022-07-14 00:40 and 2022-07-15 21:00 to end
# 277 start to 2022-08-10 18:40 and 2022-08-12 09:40 to 2022-08-13 21:20 and 2022-08-14 19:20 to end
# 278 start to 2022-09-08 22:40 and 2022-09-11 05:00 to end
# 279 start to 2022-09-22 19:10
# 280 start to 2022-04-17 01:40 and 2022-04-18 15:20 to 2022-04-19 01:10 and 2022-04-20 15:20 to end
# 281 start to 2022-05-15 00:20 and 2022-05-18 15:10 to 2022-05-19 15:10 and 2022-05-20 02:10 to end
# 282 start to 2022-06-12 17:40 and 2022-06-13 16:00 to 2022-06-14 19:40 and 2022-06-16 22:00 to end
# 283 start to 2022-07-11 23:00 and 2022-07-12 17:00 to 2022-07-13 00:20 and 2022-07-13 16:20 to 2022-07-14 01:30 and 2022-07-15 02:20 to end
# 284 start to 2022-08-10 22:40 and 2022-08-12 18:50 to 2022-08-13 20:50 and 2022-08-14 19:20 to end
# 285 start to 2022-09-08 22:50 and 2022-09-11 11:20 to end
# 286 start to 2022-05-15 00:30 and 2022-05-16 17:20 to 2022-05-17 04:00 and 2022-05-18 15:10 to 2022-05-19 15:00 and 2022-05-20 00:20 to end
")
### comments

# 90 -- NP gyre circumnavigation -- cool!
# 52 beached at times
# 280 2022-04-19 07:50 -- eddy-street behind 60-foot rock




## ----------------------------- process above text into deployment database -----------
x2 <- gsub ("# ", "", x) |>
  strsplit("\n") |>
  unlist()
x3 <- x2 [2:length (x2)] # first blank line
x3s <- strsplit (x3, split=" ")
lvN <- sapply (seq_along (x3s), function (i){x3s [[i]][1]|> as.numeric() })
cT <- sapply (seq_along (x3s), function (i){Ts <- x3s [[i]]
  tout <- paste (Ts [2:length (Ts)], collapse = " ")
})
dfix <- data.frame(level=lvN
                   , text=cT
)
# and
for (i in 1:nrow (dfix)){
  if (length (grep ("and", dfix$text [i])) > 0){
    nR <- strsplit (dfix [i,2], split=" and ")[[1]]
  }else{
    nR <- dfix$text [i]
  }
  dNand <- data.frame (level=rep (dfix$level [i], length (nR))
                                     , text=nR)
  if (i == 1){dOut <- dNand}else{dOut <- rbind (dOut, dNand)}
}
tL <- strsplit(dOut$text, split=" to ")
SEtimes <- sapply (seq_along (tL), function (i){  ## translate "end" and "start" into times
# for (i in seq_along (tL)){
  if (length (tL[[i]]) != 2){stop (dOut [i,], " is bad")}
  dT <- subset (drift, deploy == levels (drift$deploy) [i])
  if (tL [[i]][1] == "start"){
    sT <- format (min (dT$DeviceDateTime), "%Y-%m-%d %H:%M")
  }else{
    sT <- tL[[i]][1]
  }
  if (tL[[i]][2] == "end"){
    eT <- format (max(dT$DeviceDateTime), "%Y-%m-%d %H:%M")
  }else{
    eT <- tL[[i]][2]
  }
  c (sT, eT)
}
)
dOut <- cbind (dOut, t (SEtimes))
names (dOut) <- c("level", "text", "start", "end")
dOut$start <- paste (dOut$start, "GMT")
dOut$end <- paste (dOut$end, "GMT")
dOut$startT <- as.POSIXct(dOut$start)
dOut$endT <- as.POSIXct(dOut$end)

drift$ISOtime <- as.POSIXct(drift$DeviceDateTime)

## testing
# dOut$start [1:20]
# format (dOut$start, "%H")
# which (nchar (dOut$start) < 16)
# which (nchar (dOut$end) < 16)
# if (any (as.numeric (format (dOut$startT, "%H")) != 0)){stop ("times got dropped")}
# if (any (as.numeric (format (dOut$endT, "%H")) != 0)){stop ("times got dropped")}

rm (x, x2, x3, x3s, lvN, cT, dfix, dNand, i, nR, SEtimes)
# dOut$deploy <- levels ()

## apply new dOut
## cut-out manually marked boat times and redefine drifter deployments

for (i in seq_along(levels (drift$deploy))){
  dT <- subset (drift, deploy == levels (deploy)[i])
  bT <- rep (FALSE, nrow (dT))
  if (i %in% dOut$level){ ## cut out boats
    boats <- which (dOut$level %in% i)
    for (j in seq_along (boats)){
      bT <- ifelse ((dOut$startT [j] >= dT$ISOtime) & (dOut$endT [j] <= dT$ISOtime)
                    , TRUE, bT)
    }

    ## still all messed up! iterate by deployments or boat cut-outs? don't duplicate records!
XXXX      dTN$deploy <- paste0 (dTN$deploy, "-", j)  # kmeans clusters?
dDeploy <- diff (bT)

   dTN <- dT [which (!bT),]
  }else{
    dTN <- dT
  }
  if (i == 1){
    nDrift <- dTN
  }else{
    nDrift <- rbind (nDrift, dTN)
  }
}
nDrift$deploy <- factor (nDrift$deploy)
dim (drift)
dim (nDrift)
drift <- nDrift
rm (nDrift, dT, boats, bT, dTN, i)

## -------------------------------------------------------------------------------------







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
driftP <- drift %>%
  dplyr::arrange (depOrder, DeviceDateTime) %>%
  #  dplyr::filter (DeviceName %in% c("UAF-SVPI-0046", "UAF-SVPI-0047", "UAF-MS-0066")) %>%
  ###  dplyr::filter (DeviceName %in% c("UAF-SVPI-0046", "UAF-SVPI-0047")) %>%
  #  dplyr::filter (DeviceName %in% c("UAF-SVPI-0048")) %>%  # many deploys?
  #  dplyr::filter (DeviceName == "UAF-SVPI-0046") %>%
  # dplyr::filter (DeviceDateTime < as.POSIXct("2022-07-30 00:00::00")) %>%
  # dplyr::filter (DeviceDateTime > as.POSIXct("2022-07-22 07:00")) %>%
  #  dplyr::filter (DeviceName == "UAF-MS-0066") # %>%
  #  dplyr::filter (speed_ms < speedTH) %>%
#  dplyr::filter (Latitude > 58.7) %>%        ## restrict it to within Cook Inlet
  # dplyr::filter (DeviceDateTime > as.POSIXct("2020-01-01 12:00")) %>%
  dplyr::filter()




## need to set these after final drifter selection (days_in_water already per deploy)
drift <- drift %>%
  mutate (DeviceName=factor (DeviceName)) %>%
  mutate (deploy = factor (deploy)) %>%
  mutate (col=brewer.pal (8, "Set2")[drift$DeviceName]) %>% # 8 is max of Set2
  mutate (Year=as.numeric (format (DeviceDateTime, "%Y"))) %>%
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
  filter (Year==2022) %>%
  filter (DeviceName %in% c("UAF-SVPI-0046", "UAF-SVPI-0047")) %>%  #, "UAF-MS-0066")) %>%
  filter (DeviceDateTime < as.POSIXct("2022-08-20 07:00", tz="GMT")) %>%
  filter (DeviceDateTime > as.POSIXct("2022-07-21 18:00", tz="GMT")) %>%
  #  filter (CommId != "300534060052710") %>%
  filter ((DeviceDateTime < as.POSIXct ("2022-08-10", tz="GMT") | DeviceName == "UAF-SVPI-0047")) %>%  ## remove redeployment
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






## ----------------------------------------------------------------------------
## plot sampling effort by stage in tide cycle

seaAEx <- st_bbox (drift) %>%
  st_as_sfc() # %>%
grid_spacing <- 10e3  ## 10 km seems to make sense -- go to 20 km?
pgon <- st_make_grid(seaAEx, square=TRUE, cellsize=rep (grid_spacing, 2)) %>%
  st_sf () %>%
  mutate (ID=row_number())
A <- st_intersection (pgon, seaAEx)  ## grid -- suppress warning
rm (grid_spacing, pgon)


for (ti in levels (drift$tide)){
  #  ti <- "slack"
  #  ti <- "flood"
  ## aggregate drift over grid
  pointsID <- drift %>%
    #  group_split (tide, deployDepth) %>%
    filter (tide==ti) %>%
    st_join (A) %>%
    as.data.frame() %>%
    group_by (ID) %>%
    summarize (Ndrift=length (speed_ms))
  #  summarize (mapped_speed=quantile (speed_ms, 0.5))
  B <- left_join(A, pointsID, by="ID")
  # A$Ndrift <- ifelse(is.na (A$Ndrift), 0, A$Ndrift)
  # plot (A ["mapped_speed"])
  rm (pointsID)
  # print (ti)
  # print (max (B$Ndrift, na.rm=TRUE))

  # breaks <- 19571 # max slack
  mbreaks <- seq (0, 19571, by=19571/100)  # 19571 = max (slack)

  png (paste0 (outpath, "Sampling-", ti, ".png"), width=resolu [1], height=resolu [2])
  require ("viridisLite")
  ##

  plot (seaA, col="white", border="white", main=paste0 ("N positions ", ti, "-tide"))
  B ["Ndrift"] %>% st_rasterize() %>% plot(add=TRUE, col=plasma (100), breaks=mbreaks, main="") #"equal")
  # Aimg <- A ["Ndrift"] %>% st_rasterize()
  # %>% plot(add=TRUE, col=plasma (100))
  plot (worldM, add=TRUE, col="gray")
  rm (B)
  dev.off()
}
rm (A, ti)








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



## clean-up
rm (speedS, i, dIntX, ciofsF, speedTH)
# rm (speedS, pU, pV, i, dIntX, ciofsF, speedTH)



## -----------------------------------------------------------------------------
## save data dump for plotting

cat ("\nTotal time passed from startTime:"
     , round (difftime(Sys.time(), startTime, "minutes"),1), " min\n")
rm (startTime)
save.image ("~/tmp/LCI_noaa/cache/drifter/drifterSetup.Rdata")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifterSetup.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")

write.csv (drift %>% st_drop_geometry() %>%
             filter (off_deploy==FALSE) %>%
             # filter (trimBoat==FALSE) %>%
#             select (CommId, DeviceName, DeviceDateTime, Latitude, Longitude, IDn)
           select (Drifter, Year, Month, Day, Hour, Minute, Lat, Long,
                   # U-Vel, V-Vel
                   drogue_depth, DeviceName, FileName #, Deployment
                   )
           , file=gzfile ("~/tmp/LCI_noaa/data-products/drifter_cleaned.csv.gz")
           , row.names=FALSE)

## call plotting code here?
# source ("Currents/plotDrifter.R")


## -----------------------------------------------------------------------------
## EOF
