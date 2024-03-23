## analyse drifter tracks
## create animations and plots of current drifters
rm (list=ls())

## file locations
worldP <- "~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/h/GSHHS_h_L1.shp"
driftP <- "~/GISdata/LCI/drifter/drifter-04_05_23-18_23.csv"
bathyP <- "~/GISdata/LCI/bathymetry/CookInletETOPO-bathymetry-ncei.noaa.tif" # getter to graph from tiling server -- use ETOPO 2022
bingP <- "bingaddress -- find a way to get bing satellite imagery on the fly"
cacheD <- "~/tmp/LCI_noaa/cache/ggplot/"
outpath <- "~/tmp/LCI_noaa/media/drifter/animation.gif"


# use ggplot to make map
# gganimate to animate it
#
# products:
#   1. animation of drifter track (n concurrent drifters)
#   2. static map of all drifters combined


## ggplot map:  https://bookdown.org/nicohahn/making_maps_with_r5/docs/ggplot2.html
## fancy background for land: https://jakob.schwalb-willmann.de/basemaps/
## animation: https://gganimate.com/
## ggOceanMaps https://mikkovihtakari.github.io/ggOceanMaps/ (nice cartography, will need external bathymetry)




## load packages
require ('tmap')  ## for bathymetry scale?
# require ('ggpattern')
if (1){
  require ('rnaturalearth')  # -- not hi-res enough; use gshhs
  if (!require ('rnaturalearthhires')){
    require ('devtools')
    devtools::install_github("ropensci/rnaturalearthhires")
  }
}
require ('sf')
require ("RColorBrewer")
require ("tidyverse")
require ("gganimate")  ## seems to be a convenient package for animation

##
require ("basemaps")  # see https://jakob.schwalb-willmann.de/basemaps/ -- backgrounds!
require ("mapedit")   # for draw_ext()



options("sp_evolution_status"=2)  ## suppress old frameworks
require (sp) # need to call this
require ("terra") # this or stars??
## do this with oce??

require ("stars")








## set up directories
dir.create (cacheD, showWarnings=FALSE, recursive=TRUE)
dir.create (outpath, showWarnings=FALSE, recursive=TRUE)



## select drifter data
dr <- read.csv (driftP)
dr$depth <- ifelse (1:nrow (dr) %in% grep ("UAF-SVP", dr$DeviceName)
                    , 15, 0) # ice wave rider and MicroStar are surface devices
## filter out arctic and SE Alaska
dr <- subset (dr, Longitude < -149)
levels (factor (dr$DeviceName))
dr$DeviceDateTime <- as.POSIXct(dr$DeviceDateTime)


## Port Graham to Cook Inlet: SVPI-0047
drP <- dr %>%
  filter (DeviceName %in% c("UAF-SVPI-0046", "UAF-SVPI-0047", "UAF-MS-0066")) %>% #, "UAF-SVPI-0048")) %>%
  #  filter (DeviceName == "UAF-SVPI-0046") %>%
  #  filter (DeviceDateTime < as.POSIXct("2022-07-30 00:00::00")) %>%
  #  filter (DeviceDateTime > as.POSIXct("2022-07-22 07:00"))
  filter (DeviceName == "UAF-MS-0066") # %>%

drP$DeviceName <- factor (drP$DeviceName)
drP$col <- brewer.pal (3, "Set2")[drP$DeviceName]
## to projected sf object




## map
# require ("marmap")  ## requires sp, raster
# oce:plot,coastline ?
# oceanmap -- uses base plotting



## get shoreline and define bounding box
## map using sf
world <- sf::st_read (worldP)
bbox_new <- st_sf (a=1:2, geom=st_sfc (st_point (c (-150, 58.7)), # E S
                                       st_point (c (-154.3, 60.3)))  # W N
                   , crs=4326) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_transform (3338) # AK Alberts EA
rm (worldP)


if (0){
  plot (ne_states (geounit="alaska"))
  ak <- ne_download (scale=110, type="countries")
}

if (0){
  ## NC4 version -- gives trouble with projection?
  ## st_mosaic not working for this. Try nc4 files again?

  ## trouble: blending the two rasters into one

  bathyZ <- read_stars ("~/GISdata/LCI/bathymetry/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf")  ## define NA_value ?
  bathyZ2 <- read_stars ("~/GISdata/LCI/bathymetry/CGOA_bathymetry_grid/cgoa_bathy/w001001.adf")
  ## combine with st_mosaic (if same offset and delta), or st_wrap
  ba <- st_mosaic (bathyZ, bathyZ2)

  bathCont <- rasterToContour (bath, levels = c(50, 100, 200, 500))
  # bathCont <- st_contour (bathyZ, contour_lines=TRUE, na.rm=TRUE, breaks=c(-50, -100, -200, -500))
}



## do some plotting
## base on Murre_Figure.R -- ggplot

# CRS:  3338: NAD83/Alaska Albers
#       4326: WGS 1984

## set boundaries interactively:
# or use draw_ext()

# view available maptypes
get_maptypes()
## set defaults for basemap
# set_defaults (map_service="osm", map_type="topographic")
set_defaults(map_service="osm_thunderforest", map_type="atlas") ## needs token
set_defaults (map_service="esri", map_type="world_ocean_base")
set_defaults (map_service="esri", map_type="natgeo_world_map")

## can now use any of these:
# basemap_plot(ext)
# basemap_ggplot(ext)

basemap_ggplot (bbox_new)

# require (ggplot2)
# ggplot()+
# basemap_gglayer(ext) +
#   scale_fill_identity() +
#   coord_sf()

# basemap_stars(ext)
# basemap_mapview(ext)  ## life map!


tmap_mode('plot')
dev.new(width=16, height=12, unit="in")
m <- tm_shape (world, projection=3338, bbox=bbox_new)

## plot drifter onto map
m <- m +
  geom_point (data=transform (drP, proj.out=4326)
              , aes (x=Longitude, y=Latitude))
## .... more plotting

tmap_save(m, outpath, scale=1.5)




q()



## could work, but still not there with these plots and adding elements -- revisit
## ggOceanMaps -- sounds good, but puzzled -- uses natrual earth, etopo 2022
require ("ggOceanMaps")  ## uses sf and stars
.ggOceanMapsenv <- new.env()
.ggOceanMapsenv$datapath <- '~/tmp/LCI_noaa/cache/ggOceanMapsLargeData'
## get ggOceanMapsLargeData ??

dt <- data.frame (lon=rep (range (drP$Longitude), each=2)
                  , lat=c(range (drP$Latitude), rev (range (drP$Latitude)))
)
basemap (data=dt, crs=4326, bathymetry=FALSE, projection.grid=TRUE, glaciers=TRUE) +
  geom_point(data=transform (drP, proj.out=4326), aes(x=Longitude, y=Latidue)
)
#  geom_line(data=transform (drP, proj.out=4326), aes(x=Longitude, y=Latitude)

  # geom_polygon(data=transform_coord(dt, proj.out=4326)
  #              , mappping aes(x=lon,y=lat)
  #              , color="red", fill=NA
  #              )
rm (dt)







# lines (Latitude~Longitude, drP)

plot (Latitude~Longitude, drP
      , col = drP$col)





# world <- ne_countries (scale="large", returnclass="sf", type="countries")

plot (world)
# image (world)


## base map

## - Zimmermann bathymetry
## - coastline
## - tracks
