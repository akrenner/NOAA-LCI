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


##################
## native range ##
##################

natLat <- c(0,89)
natLon <- c(-25,35)
coastal <- TRUE
distX <- 100   ## in km
myspecies <- c("Carcinus maenas")

## for testing only
natLat <- c(30,60)
natLon <- c(-5,20)

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
Require ("tidyverse")

# IF YOU HAVE ONLY ONE SPECIES ----
# download GBIF occurrence data for this species; this takes time if there are many data points!
## check cache first
cF <- paste0 ("~/tmp/LCI_noaa/cache/speciesDistribution/", myspecies, ".RData")
if (file.exists(cF)){
  load (cF)
}else{
  gbif_data <- occ_data(scientificName=myspecies, hasCoordinate=TRUE, limit=2000)
  # take a look at the downloaded data:
  gbif_data
  ## cleanup: remove southern hemisphere, zeros, anything West of Azores -25 long
  gbif <- subset (gbif_data$data, decimalLongitude > -25) %>%    # include Australia -- established
    subset (individualCount > 0)
  # plot (gbif$decimalLongitude, gbif$decimalLatitude)
  dir.create("~/tmp/LCI_noaa/cache/speciesDistribution/", recursive=TRUE, showWarnings=FALSE)
  save (gbif, file=cF)
}
rm (cF)
gibfP <- st_as_sf (gbif, coords=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")

##############################################
## get seascape data from World Ocean Atlas ##
##############################################
## https://www.ncei.noaa.gov/products/world-ocean-atlas  (downside: files are 3d = big)
Require ("oceanexplorer")  # works with stars
gN <- function (...){
  x <- get_NOAA (..., spat_res=1, cache=TRUE)
  filter_NOAA (x, depth=0)  ##
}

sstW <- gN ("temperature", av_period="winter")
sstS <- gN ("temperature", av_period="summer")
sss <- gN ("salinity", av_period="annual")  # cache about 12 MB each
rm (gN)

tMin <- st_apply (c(c(sstW, sstS, along="t_an")), 1:2, min)
tMax <- st_apply (c(c(sstW, sstS, along="t_an")), 1:2, max)
sea <- c (sss, tMin, tMax, nms=c("sss", "Tmin", "Tmax"))

## stars/raster stack for point extraction and prediction
# sea$sss


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
coastG <- st_read (dsn = tD, layer = "GSHHS_c_L1") ## select f, h, i, l, c  ---  doesn't need to be fine-scale here

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
rPt <- st_sample(cbX, nrow (gbif), type = "random")
rm (distX, cbX, cb)
## XXX still to be done: remove points along bounding-box line (far from coast) XXX
## subset to avoid bounding-box line


## lookup environmental values at location of observation and at pseudo-locations
## expand environmental values into coast NAs
randPt <- st_extract (sea, rPt)
observed <- st_extract (sea, gibfP)


crab <- rbind (exEnv (gbif)
               , exEnv (rPt))
crab$status <- c (rep (1, nrow (gbif)), rep (0, nrow (rPt)))
## XX end of tests


##################################################################
## model species distribution using resource selection function ##
##################################################################

Require ("rsf")
m1 <- rspf (status~Tmin+Tmax+sal, crab, m=0, B=999)
m2 <- rspf (status~Tmin+Tmax, crab, m=0, B=999)
CAIC (m1, m2)

summary(m1)
plot (m1)
mep (m1)
kdepairs (m1)


## apply RSF to global dataset

# pArea <- sstW
# pArea ['sstS'] <- sstS ['t_an']
# pArea ['']


## project 50 years of global warming

