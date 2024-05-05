## analyse drifter tracks
## create animations and plots of current drifters
rm (list=ls())
# renv::restore()
print (startTime <- Sys.time())

## tasks:
## animate both 2021 drifters on same day
## fetch all off-shore drifters and animate through seasons (by jday -- or monthly plots)

## get OpenDrift to work on jupyter notebook -- check-point raw book in here.


## Define the whole drifter import and data processing as a function (it's slow).
## On re-runs, only reprocess new data



## set interpolation [min] for animation
interP <- 10 # interpolation interval (min)
interP <- 0  # no interepolation

speedTH <- 6 # max speed deemed realistic. Above which records are not plotted

wRes <- 1920; hRes <- 1080   ## HD+ 1920 x 1080, HD: 1280x729, wvga: 1024x576
wRes <- 1024; hRes <-  576   ## HD+ 1920 x 1080, HD: 1280x729, wvga: 1024x576
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
# require ("ggplot2")
# require ("ggspatial")  ## for spatial points in ggplot
# require ("gganimate")  ## seems to be a convenient package for animation
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
mar_bathy <- stars::read_stars (bathyP, crs=3338) ; rm (bathyP)
if (projection != st_crs (3338)){
  mar_bathy <- st_warp(mar_bathy, crs=projection)
}
names (mar_bathy) <- "topo"
depth <- st_as_stars(ifelse (mar_bathy$topo > 0, NA, mar_bathy$topo * -1)
                     , dimensions = attr(mar_bathy, "dimensions"))

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
## somehow, polygon 2245 is not valid and cannot be made valid
## it's at Lon:43.83, Lat:-69.75 -- ideally fixed in gshhs source!
# summary (st_is_valid(st_make_valid(worldM)))
seaA <- st_difference(bbox, st_union (worldM))[1,]  ## why was this so hard?!?






## ----------------------------------------------------------------------------
## prepare drifter data:
## download/update to latest data
## select drifter
## define deployment bouts
## interpolate within bouts to standardize time intervals
## turn into geographic sf and project
## could manually get drifter data from https://data.pacificgyre.com/data#data-download-tab
## see http://api.pacificgyre.com/ for API syntax


## migrate from .csv file to .zip for direct download  XXX

## build up API-url
key="6A6BEAD8-4961-4FE5-863A-721A16E54C1C"
startDate="2010-01-01%2000:00"
endDate="2023-12-31%2023:59"
endDate="2023-12-31"
# FieldList="DeviceName,DeviceDateTime,AgeInSeconds,BatteryVoltage,CommId,Latitude,Longitude,SubmergedPercent,Temperature0cm"
FieldList="DeviceName,DeviceDateTime,CommId,Latitude,Longitude"  ## make sure all fields are covered by all devices
driftF <- paste0 (driftP, "drifter-data_", endDate, ".csv")
updateFN <- gsub ("2023-12-31", "latest", driftF)

if (!file.exists(driftF)){
  urlC <- paste0 ("https://api.pacificgyre.com/api2/getData.aspx?apiKey=", key,
                  "&FieldList=", FieldList,"&startDate=", startDate,
                  "&endDate=", endDate, "&fileFormat=csv&download=Yes"
  )
  options(timeout=300)
  download.file(url=urlC, destfile=driftF); rm (urlC)
}
## update to the latest data
if (file.exists (updateFN) & (difftime (Sys.time(), file.info (updateFN)$ctime, units="days") < 7)){
  message ("Drifter data downloaded within the last week: skipping update")
}else{
  options(timeout=180)
  download.file (url=paste0 ("https://api.pacificgyre.com/api2/getData.aspx?apiKey=", key,
                             "&FieldList=", FieldList,
                             "&startDate=", as.character (as.Date(endDate)+1), "%2000:00",
                             "&fileFormat=csv&download=Yes")## endDate defaults to now
                 , destfile=updateFN)
}
rm (key, startDate, endDate, FieldList)


## combine archive and latest drifter download
readC <- function (x){read_csv (x, show_col_types=FALSE, lazy=TRUE)}  ## XXX read from zip file
drift <- purrr::map_df (c(driftF, updateFN) #list.files (path=driftP, pattern="\\.csv$", full.names=TRUE)
                        , readC) %>%
  filter (Longitude < -149) %>%            # filter out arctic and SE Alaska (here to get bbox right) -- and AI
  arrange (DeviceName, DeviceDateTime) %>% # test that this working -- crash on Windows
  mutate (DeviceDateTime = DeviceDateTime %>% as.character %>% as.POSIXct(tz = "GMT")) %>% # all drifter data are in UTC
  filter()
rm (readC, driftF, updateFN)

if (0){
  ## read from zip file
require ('zip')
dFs <- zip::zip_list (paste0 (driftP, "UAF_Data.zip/"))
readC <- function (fn){
  require ('readr')
#  read_csv(unzip("my_data.zip", "data1.csv"))
  con <- unz (paste0 (drifP, "UAF_Data.zip"))
  readr::read_csv (con, fn) %>%
    select (as.expression (FieldList))
  close (con)
  readr::read_csv (utils::unzip (paste0 (driftP, "UAF_Data.zip"), fn)
            , show_col_types=FALSE) %>%
    select (as.expression (FieldList))
}

  ## remove duplicates
  fl <- list.files (driftP, pattern="\\.csv$", full.names=TRUE)
  for (i in 1:length (fl)){cn <-  read.csv (fl[i]) %>% colnames(); print (cn)}
  ddrift <- duplicated (drift [,which (names (drift) %in%
                                         c("DeviceName", "DeviceDateTime"
                                           # , "Latitude", "Longitude"
                                         ) )])
  driftc <- subset (drift, !ddrift) %>%
    arrange (DeviceName, desc (DeviceDateTime)) %>% ## test that it's working
    filter()
  rm (ddrift)
}




## ----------------------------------------------------------
## add additional information to drifter
## include speed between positions? XXX
drift$dT <- c (0, diff (drift$DeviceDateTime)/60)  ## in min
drift$distance_m <- c (0, diff (oce::geodDist(drift$Longitude, drift$Latitude, alongPath=TRUE)*1e3))
# dx [,which (names (dx)%in%c("distance_m","oceDdist", "oceDist"))] %>% st_drop_geometry() %>% head(n=30)
drift$speed_ms <- with (drift, distance_m / (dT*60)) ## filter out speeds > 6 (11 knots) -- later

dx <- st_as_sf (drift
                , coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
  st_transform(projection)
## use morph..
dx$topo <- st_extract(mar_bathy, at=dx)$topo
dx$LandDistance_m <- worldM %>%
  st_union() %>%
  st_distance(dx, by_element=FALSE) %>%
  as.numeric()
# dx$onLand <- st_join (dx, st_sf (worldM), join=st_within)
dx$onLand <- st_intersects(dx, worldM) |> as.numeric()
# dx <- st_filter (dx, seaA)  ## supposed to filter out points on land

save.image ("~/tmp/LCI_noaa/cache/drifter3.Rdata")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter3.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")







## -------------------------------------------------------------------------------------------
## calculate state of the tide to subset data for maps, separating flood/slack/ebb

## XXX can't currently reproduce results of tide_slack_data XXXXX -- try different harmonics

if (1){
## built intermediate map to filter out human-assisted positions
  ## tides
  require ('rtide')  ## calculates tide height--not quite what's needed here
  ## for testing:  drift <- slice_sample(drift, n=1000)
  # n=5000 on dell, serial: 215 usr = 3.5 min
  # n=1000 on dell, serial:  46 usr = s
  # n=5000 on dell, parall: 131 usr = 2 min
  # n=10e3 on dell, parall: 209 usr = 3.5 min
  # 345635  -- expect: 2 h   209/10e3*345635/3600 -- for all
  # 38105   (tu) -- a rather modest 209/10e3*38105/60 = 15 min


#  tStn <- tide_stations("Seldovia*")
  ttbl <- data.frame (Station=tide_stations ("Seldovia*") ## find better harmonics?!
                           , DateTime = round (drift$DeviceDateTime, units="hours"))  ## imperative to parallelize. Enough?
   tu <- unique (ttbl)

   system.time ({
 if (1){
   require(parallel)
    ## need to pre-allocate to cores -- don't use parLapplyLB
    nCores <- detectCores()-1
    cl <- makeCluster(nCores)
    clusterExport (cl, varlist=c("tu"))
    clusterEvalQ(cl, require ("rtide"))
    pT <- parLapply (cl, 1:nrow (tu), function (i){tide_slack_data (tu [i,])})
    stopCluster (cl); rm (cl)
    tSlack <- as.data.frame (do.call (rbind, pT))
  }else{
    ## serial processing
    tSlack <- tide_slack_data(tu)  ## must parallelize
  }
   })
  save.image ("~/tmp/LCI_noaa/cache/drifterTide.Rdata")  ## checkpoint for safety
  # rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifterTide.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")

  ttbl <-  cbind (DeviceDatetime=drift$DeviceDateTime
                  # , DateTimeT=ttbl$DateTime
                  , tSlack [match (ttbl$DateTime, tSlack$DateTime),c(2,3,5)])

  ## categorize tide: within 1.5 h: high/low,
  ttbl$dT <- difftime (ttbl$DeviceDatetime, ttbl$SlackDateTime) |> as.numeric()/3600  # time in min
  ttbl$tide <- ifelse (abs (ttbl$dT) < 1.5, "slack", NA)
  ttbl$tide <- ifelse (is.na (ttbl$tide), ifelse (ttbl$SlackType=="high", "ebb", "flood"), ttbl$tide)

  if (0){
    hist (ttbl$dT, main = "", xlab="hours from slack tide")
    hist (abs (ttbl$dT), main = "", xlab = "|hours| from slack tide")
    hist (subset (ttbl, SlackType=="high")$dT)
    hist (format (ttbl$DeviceDatetime, "%H") |> as.numeric(), xlab="hour of day")
    # hist (sin (runif(10e3)*2*pi))
  }

  drift <- cbind (drift, ttbl [, c(3,4,6)])

  drift$dT_flood <- difftime(drift$SlackDateTime, drift$DeviceDateTime) |> as.numeric()*3600 * 1.5
  drift$tide <- factor (ttbl$tide)
  rm (tu, ttbl, tSlack)
}
## move tide functions into function script, load that
## combine with TideTables ?? (TideTables needs raw data? as does oce)




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

drift_sf <- dx %>%
  filter (!is.na (speed_ms)) %>%   ## uncertain why there are NAs, but they have to go
  filter (speed_ms > 0.01) %>%
  filter (distance_m > 0.1) %>%
  filter (!is.na (onLand)) %>%
  filter (dT > 1) %>%     ## some zero-values!
  #  slice_sample (n=10e3) %>% #, order_by=speed_ms, na_rm=TRUE  ## balance spatially?
#  st_as_sf (coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
#  st_transform(projection) %>%  ## or UTM? -- sf_to_rast requires projection
  # st_transform (32605) %>% # UTM zone 5N
  select (speed_ms) %>%
  filter()

# shoreline -- assume and force shoreline speed to be 0
# add shoreline to drifter speeds
drift_sf <- worldM %>%
  st_sf () %>%
  st_simplify(dTolerance=0.00001) %>%
  st_cast ("MULTIPOLYGON") %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  st_as_sf (coords=c("X", "Y"), dim="XY", remove=TRUE, crs=4326) %>%
  st_transform(projection) %>%
  mutate (speed_ms = 0) %>%
  select (speed_ms) %>%
  rbind (drift_sf)

## fast IDW
# https://geobrinkmann.com/post/iwd/
## not accelerated under windows -- worth the installation trouble?
## revert to gstat IDW?
## should use kriging for data-product output
p <- require ('GVI')  ## for sf_to_rast
if (!p){
  require ("remotes")
  remotes::install_github("STBrinkmann/GVI")
}; rm (p)


# save.image ("~/tmp/LCI_noaa/cache/drifterSpeedMap0.Rdata")

require ('parallel')
nCores <- detectCores()-1

## consider using gstat to specify aggregating function = max
speedO <- sf_to_rast (observer=drift_sf, v="speed_ms"
                      , aoi=st_sf (seaA)
                      , max_distance=100e3
                      , raster_res=5e3
                      , progress=TRUE
                      , cores=nCores
) %>%
  st_as_stars () %>%  ## terra raster to stars
  st_warp(crs=projection)
save.image ("~/tmp/LCI_noaa/cache/drifterSpeedMap.Rdata")

## look-up drift


if (0){
  plot (mar_bathy)
  plot (speedO, add=TRUE)
  plot (worldM, add=TRUE, col="beige", alpha=0.5)
  plot (drift_sf %>% st_transform(projection), add=TRUE)
  # speedO == worldM and drifter_sf > st_trans, but not mar_bathy
}







if (0){
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
  save.image("~/tmp/LCI_noaa/cache/drifterKrige.RData")
  # load ("~/tmp/LCI_noaa/cache/drifterKrige.RData")
}


## ----------------------------------------------------------------------------
## filter out unrealistic speeds and records too close to shore/on land? XXX
drift <- dx %>%
  st_drop_geometry() %>%  ## drop spatial part
  filter (speed_ms < speedTH) %>%  ## not much effect here? still need to filter again after interpolation?
  filter (topo < 3) %>%         ## to make sure none are on land
  filter (LandDistance_m > 50) %>%
  filter()
## retains 21k out of 28 k
# dim (drift)
# dim (dx)
rm (dx)


## redundant, but better safe
# is.unsorted(drift$DeviceName)
drift <- drift [order (drift$DeviceName, drift$DeviceDateTime),]

## define new deployment XXXX  review!!! XXXX
newDeploy <- drift$dT > 120 | !duplicated (drift$DeviceName) ## dT in min. mark new deployments XXX test!! 2h
# newDeploy <- drift$dT > 60*14 | !duplicated (drift$DeviceName) ## dT in min. mark new deployments   14 h (840 min)
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

# plot (Latitude~Longitude, drift)
# summary (drift$dT)
# for (i in 1:5) alarm()


## interpolate within bouts
if (exists ("iDF")){rm (iDF)} ## in case of reruns of code

for (i in seq_along (levels (drift$deploy))){               ## very slow! parallelize? cache?
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
      newDF$dT <- interP
    }else{
      newDF <- df %>% select (c("DeviceName", "DeviceDateTime", "Longitude", "Latitude", "deploy", "dT"))
    }
    newDF$days_in_water <- with (newDF, difftime(DeviceDateTime, min (DeviceDateTime), units="days"))|> as.numeric()
    newDF$dist_m <- c (0, diff (oce::geodDist(newDF$Longitude, newDF$Latitude, alongPath=TRUE)*1e3))
    newDF$speed_ms <- newDF$dist_m / (newDF$dT*60) ## convention to use m/s, not knots
    ## trim newDF XXXX  -- cut bad stuff front and back
    ## at the very least: first and last
    newDF <- newDF [2:(nrow (newDF)-1),]

    if (exists ("iDF")){
      iDF <- rbind (iDF, newDF)
    }else{
      iDF <- newDF
    }
  }
}


rm (df, i, newDF, interP)
iDF <- subset (iDF, speed_ms < speedTH)  ## apply again --- any way to get pre/post deploy more thorough?

## project positions -- don't move earlier to allow interpolations
drift <- iDF %>%
  st_as_sf (coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
  st_transform(projection)
rm (iDF)


# drift$distance_m <- c (0, d <- st_distance (drift, by_element=TRUE))
## ice wave rider and MicroStar are surface devices
drift$deployDepth <- ifelse (seq_len(nrow (drift)) %in% grep ("UAF-SVP", drift$DeviceName), 15, 0)
drift$year <- format (drift$DeviceDateTime, "%Y") |> as.numeric()  ## above should be piped and mapped XXX
drift$topo <- st_extract(mar_bathy, at=drift)$topo



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






save.image ("~/tmp/LCI_noaa/cache/drifter0.Rdata")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter0.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")






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
drift$DeviceName <- factor (drift$DeviceName)
drift$deploy <- factor (drift$deploy)
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
  mapview::mapview (zcol= c ("days_in_water", "speed_ms", "depth")
    , col.regions=colorRampPalette(heat.colors(20))
    , map.types = c("Esri.WorldImagery", "Esri.WorldShadedRelief", "CartoDB.Positron")
  )
# mymap
mapshot (mymap, url="~/tmp/LCI_noaa/media/drifter/mapview.html")
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



save.image ("~/tmp/LCI_noaa/cache/drifterSetup.Rdata")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifterSetup.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")

cat ("Total time passed from startTime:", difftime(Sys.time(), startTime), "\n")

## call plotting code here?
# source ("Currents/plotDrifter.R")


for (i in 1:5) alarm()

## -----------------------------------------------------------------------------
## EOF
