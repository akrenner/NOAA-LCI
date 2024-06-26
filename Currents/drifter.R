## analyse drifter tracks
## create animations and plots of current drifters
rm (list=ls())
# renv::restore()


## tasks:
## animate both 2021 drifters on same day
## fetch all off-shore drifters and animate through seasons (by jday -- or monthly plots)

## get OpenDrift to work on jupyter notebook -- check-point raw book in here.




## set interpolation [min] for animation
interP <- 10 # interpolation interval (min)
interP <- 0

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
driftP <- "~/GISdata/LCI/drifter/drifter-04_05_23-18_23.csv"  ## smaller, only local data
# driftP <- "~/GISdata/LCI/drifter/drifter-03_14_24-21_58.csv"  ## full drifter archive
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
# require ("readr") ## for read_csv
# require ("rnaturalearth")
# require ('tidyverse')

## add snapbox for basemap? -- all seem to require a token
# require ("snapbox")


## set projection for output map
# projection <- st_crs (3338) ## Alaska Albers EA  ## can't be 4326 I guess :(
projection <- st_crs (4326)  # "+init=epsg:4326" ## WGS84



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
mar_bathy <- stars::read_stars (bathyP) ; rm (bathyP)
if (projection != st_crs (3338)){
  mar_bathy <- st_warp(mar_bathy, crs=projection)
}
names (mar_bathy) <- "topo"
depth <- st_as_stars(ifelse (mar_bathy$topo > 0, NA, mar_bathy$topo * -1)
                     , dimensions = attr(mar_bathy, "dimensions"))

## bounding box -- redundant?
bbox <- mar_bathy %>%  ## extended Research Area
  st_bbox() %>%
  st_as_sfc() %>%
  st_transform(projection)

## coastline
worldM <- sf::st_read (worldP, quiet=TRUE) %>%
  st_geometry()
worldM <- subset (worldM, st_is_valid (worldM)) %>% ## polygon 2245 is not valid
  st_crop (c(xmin=-160, xmax=-140, ymin=55, ymax=62)) %>%   ## or could use bbox above
  sf::st_transform(projection)
## somehow, polygon 2245 is not valid and cannot be made valid
## it's at Lon:43.83, Lat:-69.75 -- ideally fixed in gshhs source!
# summary (st_is_valid(st_make_valid(worldM)))



## ----------------------------------------------------------
## prepare drifter data:
## select drifter
## define deployment bouts
## interpolate within bouts to standardize time intervals
## turn into geographic sf and project


save.image ("~/tmp/LCI_noaa/cache/drifter2.Rdata")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter2.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")


# drift <- read.csv (unz (driftP, "drifter-03_14_24-21_58.csv"), colClasses=c(DeviceDateTime="POSIXct"
#                                         , DeviceName="factor")) %>%
drift <- read.csv (driftP, colClasses=c(DeviceDateTime="POSIXct", DeviceName="factor")) %>%
  #  arrange (DeviceName, DeviceDateTime) %>%   ## test that this is working!
  dplyr::filter (Longitude < -149)   # filter out arctic and SE Alaska (here to get bbox right)
drift <- drift [order (drift$DeviceName, drift$DeviceDateTime),]
rm (driftP)
## define deployment bouts
## include speed between positions? XXX
drift$dT <- c (0, diff (drift$DeviceDateTime, units="mins")) |> as.numeric()
drift$distance_m <- c (0, diff (oce::geodDist(drift$Longitude, drift$Latitude, alongPath=TRUE)*1e3))
# dx [,which (names (dx)%in%c("distance_m","oceDdist", "oceDist"))] %>% st_drop_geometry() %>% head(n=30)
drift$speed_ms <- with (drift, distance_m / (dT*60)) ## filter out speeds > 6 (11 knots) -- later

dx <- st_as_sf (drift, coords=c("Longitude", "Latitude"), dim="XY"  # sf points
                , remove=FALSE, crs=4326) %>%
  st_transform(projection)
## use morph..
# dx$depth <- st_extract (mar_bathy, at=dx)$topo * -1 # depth
dx$topo <- st_extract(mar_bathy, at=dx)$topo
dx$LandDistance_m <- st_distance(worldM, dx) %>%  ## slow. Extract the min of each column. Is there a shortcut?
  apply (2, min)


save.image ("~/tmp/LCI_noaa/cache/drifter3.Rdata")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter3.Rdata"); require ("stars"); require ("RColorBrewer"); require ("dplyr")

## ----------------------------------------------------------------------------
## filter out unrealistic speeds and records too close to shore/on land? XXX
drift <- dx %>%
  dplyr::filter (speed_ms < speedTH) %>%  ## not much effect here? still need to filter again after interpolation?
  dplyr::filter (topo < 3) %>%         ## to make sure none are on land
  dplyr::filter (LandDistance_m > 50) %>%
  st_drop_geometry()  ## drop spatial part
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

for (i in seq_along (levels (drift$deploy))){
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
      newDF <- df
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

# plot (Latitude~Longitude, iDF)


## project positions -- don't move earlier to allow interpolations
drift <- iDF %>%
  st_as_sf (coords=c("Longitude", "Latitude"), dim="XY"
            , remove=FALSE
            , crs=4326
  ) %>% st_transform(projection)
rm (iDF)


# drift$distance_m <- c (0, d <- st_distance (drift, by_element=TRUE))
## ice wave rider and MicroStar are surface devices
drift$deployDepth <- ifelse (seq_len(nrow (drift)) %in% grep ("UAF-SVP", drift$DeviceName), 15, 0)


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
  dplyr::filter (speed_ms < speedTH) %>%
  dplyr::filter (speed_ms > 0.00001) %>%      ## remove stationary positions
  dplyr::filter (Latitude > 58.7) %>%        ## restrict it to within Cook Inlet
  dplyr::filter()

## need to set these after final drifter selection (days_in_water already per deploy)
drift$DeviceName <- factor (drift$DeviceName)
drift$deploy <- factor (drift$deploy)
drift$col <- brewer.pal (8, "Set2")[drift$DeviceName] # 8 is max of Set2
# as.numeric (drift$DeviceDateTime) - min (as.numeric (drift$DeviceDateTime))/3600 # in hrs




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



wRes <- 800

## color by device -- simple
png (filename=paste0 (outpath, "drifterPlot_byID.png")
     , width=wRes, height=hRes)
# , width=wRes, height=hRes)
# pdf (filename=paste0 (outpath, "drifterPlot_byID.png")
#      , width=16, height=9)
plotBG(2)
# plot (st_geometry (drift), add=TRUE, type="l", lwd=2, col="yellow")  ## lines -- trouble between deploys
## cross/x: pch=4,  plus+: pch=3
plot (st_geometry (drift), add=TRUE, pch=19, cex=0.5
      # , col=add.alpha ("#FFBB00", 0.4)
      , col=add.alpha (drift$col, 0.5)
)
dev.off()




## -----------------------------------------------------------------------------
## color by speed (or age of deployment)
png (filename=paste0 (outpath, "drifterPlot_speed.png")
     , width=wRes, height=hRes)
par (mfrow =c(1,2))
plotBG()

x <- subset (drift, (speed_ms < 3))$speed_ms
brks <- seq (min (x), max (x), length.out=20)
x <- sqrt (x)
# brks <- (seq (min (x), max (x), length.out=20))^2

drift %>%
  dplyr::filter (speed_ms < 3) %>%
  dplyr::filter (speed_ms > 0) %>%
  dplyr::filter (deployDepth < 5) %>%
  dplyr::select (speed_ms) %>%
  plot (add=TRUE
        , pch=19  ## by deployDepth
        , type="p"
        , alpha=0.5
        , breaks=brks
        ## key for colors
        # ,graticule=TRUE
  )
title (main = "speed: surface")

plotBG()
drift %>%
  dplyr::filter (speed_ms < 3) %>%
  dplyr::filter (speed_ms > 0) %>%
  dplyr::filter (deployDepth > 5) %>%
  select (speed_ms) %>%
  plot (add=TRUE
        , pch=19  ## by deployDepth
        , type="p"
        , alpha=0.5
        , breaks=brks
        ## key for colors
        # ,graticule=TRUE
  )
title (main = "speed: 15 m depth")
dev.off()
rm (brks, x)


## age of deployment
png (filename=paste0 (outpath, "drifterPlot_age.png")
     , width=wRes, height=hRes)
plotBG()
drift %>%
  dplyr::filter (speed_ms < 3) %>%
  #  dplyr::filter (deployDepth < 5) %>%
  select (days_in_water) %>%
  plot (add=TRUE
        , pch=19  ## by deployDepth
        , cex=1
        , type="p"
        , alpha=0.5
        , nbreaks=100
        #        , key.pos=
        #        , breaks=brks
  )
title (main="time")
dev.off()
# rm (brks)




## test whether a legend can be shown
if (0){
#  require ("ggmap")
# m <- get_map ("Alaska" #, zoom=1
#               , source="osm"
#               , color="color")
drift %>% select (days_in_water) %>%
  plot(axes=TRUE
       , graticule=TRUE
       , col_graticule="gray"
       #, bgMap= ## class ggmap, e.g. googlex
       )
}

## plot each individual deployment
dir.create(paste0 (outpath, "/deployment/"), recursive=TRUE, showWarnings=FALSE)

plotDrift <- function (i){
  png (filename=paste0 (outpath, "/deployment/", levels (drift$deploy)[i], ".png")
       , width=wRes, height=hRes)
  dR <- drift %>%
    #  dplyr::filter (speed_ms < 3) %>%
    dplyr::filter (deploy == levels (drift$deploy)[i]) %>%
    # select (days_in_water)
    select (speed_ms)

  plotBG(dr=dR)
  plot (dR, add=TRUE
        , pch=19  ## by deployDepth
        , cex=1, type="p"
        , alpha=0.5
        , nbreaks=100
        #        , key.pos=
        #        , breaks=brks
  )
  title (main=levels (drift$deploy)[i])
  dev.off()
}


for (i in seq_along(levels (drift$deploy))){
  print (i)
  plotDrift (i)
}

# library (parallels)
# require ('parallelly')


if (0){
plot (speed_ms~topo, drift, pch=19, col = add.alpha("black", 0.1))
lS <- loess(speed_ms~topo, drift)
nD <- data.frame (topo=seq(-200, 0, length.out=100))
lines (nD$topo, predict (lS, newdata=nD), col = "red")
}

## plot drifter
# for (i in 1:seq_along (levels (drift$deploy))){
#   dr <- subset (drift, deploy==levels (drift$deploy)[i])
#   if (nrow (dr)>2){
#   plot (st_geometry (dr), add=TRUE, type="l", lwd=3, col=dr$col)
#   plot (st_geometry (dr), add=TRUE, pch=20, cex=0.1)
#   }
# }



## animation of drifter tracks
# see https://hansenjohnson.org/post/animate-movement-in-r/

## plotly: interactive, but restrictive
## gganimate: easy, but restrictive
## googleVis: interactive? Needs flash
## animate:  flexible, but web-based. Not it! https://kcf-jackson.github.io/animate/
# require ("animate") ## simply combine frames -- most flexible
## output to HTML (with controls), mp4, gif, LaTeX

## animation tasks:
# 1. plot point by point
# 2. point by point, building up a tail from start
# 3. point by point with x-day long tail

# device <- animate$new()  ## real-time animation on html?

## or require ("gifski") (part of animate?) or require ("av")

## see https://cran.r-project.org/web/packages/animate/vignettes/introduction.html -- NO



## subset to min example
## start with small test! This takes time.




# setTimeLimit (elapsed=60*10
#               , transient=TRUE) # 10 min limit

if (0){
tD <- tempdir()
png (paste0 (tD, "/frame%04d.png"), width = wRes, height=hRes, res=120)
par (ask=FALSE)
for (i in 1:nrow (drift)){
  plotBG()
  plot (subset (drift, 1:nrow (drift) <= i), add=TRUE)
  # arrows (x, y, ...)
}
dev.off()
png_files <- list.files(tD, "*.png")
gif_file <- paste0 (outpath, "driftAnimation.gif")
gifski (png_files, gif_file, width=wRes, height=hRes
        , delay=1/frameR, loop=0)
unlink(tD, recursive=TRUE)
utils::browseURL(gif_file)

require ("av")
av_encode_video (input=gif_file
                 , output=paste0 (outpath, "driftAnimation.mp4")
                 , framerate=frameR)




## bare av
video_file <- paste0 (outpath, "driftAnimationAV.mp4")
av::av_capture_graphics({
  par (ask=FALSE)
  for (i in 1:nrow (drift)){
    plotBG()
    plot (subset (drift, 1:nrow (drift) <= i), add=TRUE)
    # arrows (x, y, ...)
  }
}, video_file, wRes, hRes, res=144, vfilter='framerate=fps=10')
utils::browseURL(video_file)

}

for (i in 1:5) alarm()

## -----------------------------------------------------------------------------
## EOF
