## European Green Crab, Carcinus maenas
## invasive species in the Pacific and South America. Native to the North Atlantic.
## first detected in Alaska in 2022. Will they ever reach Kachemak Bay?
## East Coast of US: since 1817
## Australia: since 1800s
## Placentia Bay, Newfoundland: 2007
## San Francisco: 1989
## Patagonia, Chile: 2003

## iNaturalist.org positions

## 10 km buffer on global coastline
rm (list = ls())

## bugs and missing features
## distance to coast
## predict on non-linear model
## use 90% quantiles/box of habitat data
## test and examine output

##################
## native range ##
##################

natLat <- c(0,89)
natLon <- c(-25,36)
natLon <- c(-80, 36) ## include East-coast -- Australia not much use to define cold end
# natLat <- c(-80,89)
# natLon <- c (-170,170)

coastal <- TRUE
distX <- 20   ## in km
myspecies <- c("Carcinus maenas")

## for testing only
# natLat <- c(30,60)
# natLon <- c(-5,20)

### end of user-defined parameters



###################
## finish set-up ##
###################

if (!require("pacman")) install.packages("pacman")
Require <- pacman::p_load
Require ("sf")
Require ("stars")
units (distX) <- "km"


## GBIF code from https://www.r-bloggers.com/2021/03/downloading-and-cleaning-gbif-data-with-r/
## make this a shiny app?

## caviats: competition from local species may halt the advance of introduced species. Lots of unknowns.
##

#######################################
## DOWNLOAD AND CLEAN DATA FROM GBIF ##
#######################################
Require ("rgbif")
# library(scrubr)
Require ("maps")
Require ("readr")

# IF YOU HAVE ONLY ONE SPECIES ----
# download GBIF occurrence data for this species; this takes time if there are many data points!
## check cache first
cF <- paste0 ("~/tmp/LCI_noaa/cache/speciesDistribution/", myspecies, ".RData")
if (file.exists(cF)){
  load (cF)
}else{
  gbif_data <- try (occ_data(scientificName=myspecies, hasCoordinate=TRUE, limit=20000))
  if (class (gbif_data)=="try-error"){
    gbif_data <- try (occ_data(scientificName=myspecies, hasCoordinate=TRUE, limit=20000))
    if (class (gbif_data)=="try-error"){
      gbif_data <- occ_data(scientificName=myspecies, hasCoordinate=TRUE, limit=20000)
    }} ## try three times, if time-out on first attempt
  # take a look at the downloaded data:
#  gbif_data
  dir.create("~/tmp/LCI_noaa/cache/speciesDistribution/", recursive=TRUE, showWarnings=FALSE)
  save (gbif_data, file=cF)
  ## keep all other data for later cross-validation
}
rm (cF)
## cleanup: remove southern hemisphere, zeros, anything West of Azores -25 long
gbifP <- subset (gbif_data$data, decimalLongitude > -25) %>%    # include Australia -- established
  subset (individualCount > 0) %>%
  subset (decimalLongitude < 36) %>%  # for testing only XXXXXXXXXXXXXX
  st_as_sf (coords=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")


##############################################
## get seascape data from World Ocean Atlas ##
##############################################
## https://www.ncei.noaa.gov/products/world-ocean-atlas  (downside: files are 3d = big)


Require ("oceanexplorer")  # works with stars
if (1){
  gN <- function (...){
    Require ("readr")
    get_NOAA (..., spat_res=1, cache=TRUE) %>%
      filter_NOAA (depth=0)  ##
  }
}else{
  ## manual download to ~/GISdata/data/world-ocean-atlas/
  # https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa/salinity/decav/0.25/woa18_decav_s00_04.nc
  gN <- function (var="temperature", av_period="January"){
    Require ("stars")
    if (var=="temperature"){
      dCub <- st_read (paste0 ("~/GISdata/data/world-ocean-atlas/temperature/woa18_decav_t"
                               , sprintf ("%02i", which (month.name %in% av_period))
                               , "_04.nc")) %>%
        filter_NOAA (depth=0)
    }
    if (var=="salinity"){
      dCub <- st_read ("~/GISdata/data/world-ocean-atlas/salinity/woa18_decav_s00_04.nc") %>%
        filter_NOAA (depth=0)
    }
  }
}

for (m in month.name){
  assign (paste0 ("T", m), gN ("temperature", av_period=m))
}
tStr <- paste0 ("T", month.name, collapse=", ")
getT <- paste ("c (", tStr, ", along='t_an')")
temp <- eval (str2lang (getT))
#  names (temp) <- "degC"
tMin <- st_apply (c (temp), 1:2, min)
tMax <- st_apply (c (temp), 1:2, max)
tMean <- st_apply (c (temp), 1:2, mean)
## cleanup
eval (str2lang (paste0 ("rm (", tStr, ")")))
rm (getT, tStr, temp)

sss <- gN ("salinity", av_period="annual")  # split up by month and look for range?
rm (gN)

sea <- c (sss, tMin, tMax, tMean, nms=c("sal", "Tmin", "Tmax", "Tmean"))
rm (sss, tMin, tMax, tMean)
## stars/raster stack for point extraction and prediction
# sea$sal


#####################
## setup coastline ##
#####################

## shoreline
Require ("maptools")
Require ("zip")
tD <- tempdir()
unzip ("~/GISdata/data/coastline/gshhg-shp-2.3.7.zip"
       , junkpaths = TRUE, exdir = tD)
Require ("sf")
coastG <- st_read (dsn = tD, layer = "GSHHS_l_L1") ## select f, h, i, l, c  ---  doesn't need to be fine-scale here

## clip to bounding box: NW Atlantic
b <- st_bbox (coastG)
b[c(1,3)] <- natLon
b[c(2,4)] <- natLat
rm (natLon, natLat)

bP <- st_as_sfc (b, crs=st_crs (coastG))
coastE <- st_intersection (coastG, bP)
unlink (tD, TRUE); rm (tD)
rm (b)
# coastSF)


###########################################
## generate pseudo-negative observations ##
###########################################

## sample from an off-shore buffer around coast
## could also use st_difference?  -- this is slow

save.image ("~/tmp/LCI_noaa/cache/specDist1.RData")
# rm (list = ls()); load  ("~/tmp/LCI_noaa/cache/specDist1.RData"); require (sf)


if (coastal){
  cb <- st_buffer (coastE, dist=distX) %>%
    st_union() %>%
    st_sf() %>%
    st_make_valid

  cbX <- cb
  for (i in 1:length (coastE)){ # cut out every individual polygon
    cbX <- st_sym_difference(cbX, coastE [i,])
  }
}else{
  cbX <- bP
}
rPt <- st_sample(cbX, nrow (gbifP), type = "random") ## stuck/extremely slow?
rm (distX, cbX, cb)
## XXX still to be done: remove points along bounding-box line (far from coast) XXX
## subset to avoid bounding-box line


## lookup environmental values at location of observation and at pseudo-locations
## expand environmental values into coast NAs
# randPt <- st_extract (sea, rPt)
# plot (subset (randPt, !is.na (sal)))

if (1){
  ## interpolate NA values from neighbours
  seax <- sea
  Require ("zoo") ## use na.approx -- not ideal, but does the trick for now. ideal: r.neighbors
  for (i in 1:length (seax)){
    seax [[i]] <- na.approx (seax [[i]])
  }
  # image (na.approx (mtx))
  observed <- st_extract (seax, gbifP)  ## old version -- mostly NAs
  rm (seax)
}else{
  ## idea: apply buffer to gbifP, then find sea values in that buffer to avoid NAs
  ## for unknown reasons, it's actually getting worse. Also tried with terra::buffer
  observed <- aggregate (sea
                         , by = st_buffer (gbifP, nQuadSegs=4, dist=100e3) # 100 km may be reasonable with current raster
                         , FUN=function (x){mean (x, na.rm=TRUE)}
                         # , FUN=mean, na.rm=TRUE
                         # , as.points=FALSE
  )
  if (sum (is.na (observed$sal)) > 40){stop ("still busted")}
  ## XXXXXXXXXXXXXX
}

pnts <- st_extract(sea, rPt) %>%
  rbind (observed)
pnts$status <- c (rep (0, length (rPt)), rep (1, nrow (gbifP)))
pnts <- subset (pnts, !is.na (sal))
mDF <- st_drop_geometry(pnts)
rm (observed)


##################################################################
## model species distribution using resource selection function ##
##################################################################

save.image ("~/tmp/LCI_noaa/cache/specDist2.RData")
# rm (list = ls()); load  ("~/tmp/LCI_noaa/cache/specDist2.RData"); require (sf); require (stars)

# simulatedUsedAvail ()


### fudge Tmin and Tmax to reflect the idea behind them
## high Tmin in tropics should not drive the function
mMean <- mean (subset (mDF$T$mean, mDF$status==1))
mDF$Tmin <- ifelse (mDF$status==1 & mDF$Tmin > mMean, mMean, mDF$Tmin)
mDF$Tmax <- ifelse (mDF$status==1 & mDF$Tmax < mMean, mMean, mDF$Tmax)


## scale predictive variables
scale2 <- function (var, ref=NULL){
  if (length (ref) < 1){
    ref <- var
  }
  (var - mean (ref, na.rm=TRUE))/sd (ref, na.rm=TRUE)
}
mDF$TminS <- scale2 (mDF$Tmin)
mDF$TmaxS <- scale2 (mDF$Tmax)
mDF$salS <- scale2 (mDF$sal)

sea$TminS <- scale2 (sea$Tmin, mDF$Tmin)
sea$TmaxS <- scale2 (sea$Tmax, mDF$Tmax)
sea$TmeanS <- scale2 (sea$Tmean, mDF$Tmean)
sea$salS <- scale2 (sea$sal, mDF$sal)


## logistic regression
m1 <- glm (factor (status)~TminS+TmaxS+salS, mDF, family=binomial(link="logit"))
summary (m1)
# plot (m1)

sea$pred <- predict (m1, newdata=sea, type="response")  # type= "link"/"response"/"terms"
Require ("viridis")
png ("~/tmp/LCI_noaa/media/GreenCrab.png", width=1600, height=900, res=100)
plot (sea ['pred'], col = inferno(12), breaks="equal")
dev.off()
# image (sea$pred)
write_stars (sea, dsn="~/tmp/LCI_noaa/data-products/EuroGreenCrab.tif", layer="pred")




Require ("ResourceSelection")
m1 <- ResourceSelection::rsf (status~Tmin+Tmax+Tmean+sal, mDF, m=0, B=999)
m2 <- ResourceSelection::rsf (status~Tmin+Tmax, mDF, m=0, B=999)
m3 <- ResourceSelection::rsf (status~Tmin+sal, mDF, m=0, B=999)
CAIC (m1, m2, m3) # m1 is better

summary(m1)
plot (m1)
mep (m1)
kdepairs (m1)

sea$rsf <- predict (m1, newdata=sea) #, type="response")

## apply RSF to global dataset
# predict (m1, )


## rsf -- same as "ResourceSelection" ??
Require ("rsf")
m1 <- rsf (status~Tmin+Tmax+Tmean+sal, mDF, m=0, B=99)

## maxent
# Require ("dismo")
# maxent ()


## project 50 years of global warming

