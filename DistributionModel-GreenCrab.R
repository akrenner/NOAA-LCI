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



if (!require("pacman")) install.packages("pacman")
Require <- pacman::p_load



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
myspecies <- c("Carcinus maenas")
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


## get seascape data from World Ocean Atlas
## https://www.ncei.noaa.gov/products/world-ocean-atlas  (downside: files are 3d = big)
Require ("oceanexplorer")  # works with stars
sstwin <- get_NOAA ("temperature", spat_res=1, av_period="winter", cache=TRUE)
sstsum <- get_NOAA ("temperature", spat_res=1, av_period="summer", cache=TRUE)
sss <- get_NOAA ("salinity", spat_res=1, av_period="annual", cache=TRUE)  # cache about 12 MB each

## slice out the surface layer
sstwin <- filter_NOAA(sstwin, depth = 0)
sstsum <- filter_NOAA(sstsum, depth = 0)
sss <- filter_NOAA(sss, depth = 0)


## shoreline
Require ("maptools")
Require ("zip")
tD <- tempdir()
unzip ("~/GISdata/data/coastline/gshhg-shp-2.3.7.zip"
       , junkpaths = TRUE, exdir = tD)
Require ("sf")
coastG <- read_sf (dsn = tD, layer = "GSHHS_h_L1") ## select f, h, i, l, c
## clip to bounding box: NW Atlantic
b <- st_bbox (coastG)
b[c(1,3)] <- c(-25,35)
b[c(2,4)] <- c(0,89)
bP <- as (st_as_sfc (b), "Spatial") # get spatial polygon for intersect
coast <- st_intersects (coastG, bP)
unlink (tD, TRUE); rm (tD)
rm (b, bP)
# coastSF)


## generate pseudo-negative observations
## distance from shoreline and buffer to constrain pseudo-locations

## list of ocean grid-points
b <- st_bbox (coast)
rPt <- vect (cbind (lon=runif (n=1e4, b[1], b[3])
                     , lat=runif (n=1e4, b[2], b[4]))
               , crs="+proj=longlat")

Require ("geosphere")
cDist <- geosphere::dist2Line (rPt, coast)
## subset close to shore
rPt <- rPt [which (cDist < 20e3),]

## faster to find points within coastal buffer?
## buffer around coast
Require ("terra")
cB <- terra::buffer (coast, 20* 1000)


## sample at random
rPt <- rPt [sample (nrow (gbif)),]



## lookup environmental values at location of observation and at pseudo-locations
## expand environmental values into coast NAs
exEnv <- function (pt){
  Require ("terra")
  sstW <- terra::extract (sstwin, pt)
  sstS <- terra::extract (sstsum, pt)
  crab <- data.frame (
    Tmin=apply (cbind (sstW, sstS), 2, min)
    , Tmax=apply (cbind (sstW, sstS), 2, max)
    , sal=terra::extract (sss, pt)
  )
  rm (sstW, sstS)
  crab
}

crab <- rbind (exEnv (gbif)
               , exEnv (rPt))
crab$status <- c (rep (1, nrow (gbif)), rep (0, nrow (rPt)))

## resource selection function, covering 90 % of observations
Require ("rsf")
m1 <- rspf (status~Tmin+Tmax+sal, crab, m=0, B=999)
m2 <- rspf (status~Tmin+Tmax, crab, m=0, B=999)
CAIC (m1, m2)

summary(m1)
plot (m1)
mep (m1)
kdepairs (m1)


## apply RSF to global dataset

## project 50 years of global warming
