## analyse drifter tracks
## create animations and plots of current drifters
rm (list=ls())



## ----------------------------------------------------------
## file locations
worldP <- "~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/h/GSHHS_h_L1.shp"
# worldP <- "~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/c/GSHHS_c_L1.shp"  ## coarse for testing
# AKshape <- "GISdata/LCI/shoreline/akshape"
driftP <- "~/GISdata/LCI/drifter/drifter-04_05_23-18_23.csv"
bathyP <- "~/GISdata/LCI/bathymetry/CookInletETOPO-bathymetry-ncei.noaa.tif" # getter to graph from tiling server -- use ETOPO 2022
# could also try Zimmerman bathymetry here
# bingP <- "bingaddress -- find a way to get bing satellite imagery on the fly"
cacheD <- "~/tmp/LCI_noaa/cache/ggplot/"
outpath <- "~/tmp/LCI_noaa/media/drifter/"


## set reporting interval ?
## tricky. Some deployments at 4 h intervals, mostly < 1


## ----------------------------------------------------------
## set projection
projection <- "+init=epsg:3338" ## Alaska Albers EA
# projection <- "+init=epsg:4326" ## WGS84


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

## load packages
# require ('ggOceanMapsLargeData') # of any use here? needed for examples only?
require ("gganimate")  ## seems to be a convenient package for animation
require ("dplyr")      ## needed for pipe
require ("RColorBrewer")
require ("sf")         ## apparently not auto-loaded by ggOceanMaps
require ("stars")
require ("ggplot2")
require ("ggspatial")  ## for spatial points in ggplot



## ----------------------------------------------------------
## select drifter and manage data
drift <- read.csv (driftP, colClasses=c(DeviceDateTime="POSIXct"
                                        , DeviceName="factor")) %>%
  .[order (.$DeviceName, .$DeviceDateTime),] %>% ## ensure data is sorted right
  st_as_sf (coords=c("Longitude", "Latitude"), dim="XY"
            , remove=FALSE
            ## add GpsQuality
            # , group=deploy    ## define group later, under lines
            # , agr=c(DeviceName="identity")
            , crs=4326
  ) %>%
  st_transform(projection) %>%
  # filter out arctic and SE Alaska (here to get bbox right)
  filter (Longitude < -149)
rm (driftP)

## mark individual drifter deployments
deploy <- paste (drift$DeviceName, drift$DeviceDateTime)
dTime <- c (0, difftime(drift$DeviceDateTime [2:nrow (drift)]
                        , drift$DeviceDateTime [1:(nrow (drift)-1)]
                        , units="hours"))
for (i in 2:nrow (drift)){
  ## copy deploy from prev record if it's in the same deployment
  if ((dTime [i] < 5) & # (dTime [i] > 0) &   ## tricky. Some deployments at 4 h intervals, mostly < 1
      (drift$DeviceName [i] == drift$DeviceName [i-1])){
    deploy [i] <- deploy [i-1]
  }# to break lines, insert a blank row at the end with appropriate DeviceDateTime + 1s an DeviceName ?
}
drift$deploy <- factor (deploy)
# levels (factor (drift$DeviceName))
# drift$deploy |> factor() |> levels() #|> length()
rm (dTime, deploy)

## deployment summary table showing start dates and deployment length
# deployment <- aggregate (.~deploy, data=drift, FUN=function (x){x[1]})
if (0){
  aggregate (DeviceDateTime~deploy, data=drift, FUN=function (x){
    length(x)
    #  difftime (max (x), min(x), units="hours")
  })
}

## subset drifter database
## Port Graham to Cook Inlet: SVPI-0047
drift <- drift %>%
#  dplyr::filter (DeviceName %in% c("UAF-SVPI-0046", "UAF-SVPI-0047", "UAF-MS-0066")) %>%
  dplyr::filter (DeviceName %in% c("UAF-SVPI-0046", "UAF-SVPI-0047")) %>%
#  dplyr::filter (DeviceName %in% c("UAF-SVPI-0048")) %>%  # many deploys?
#  dplyr::filter (DeviceName == "UAF-SVPI-0046") %>%
    # dplyr::filter (DeviceDateTime < as.POSIXct("2022-07-30 00:00::00")) %>%
    # dplyr::filter (DeviceDateTime > as.POSIXct("2022-07-22 07:00")) %>%
  #  dplyr::filter (DeviceName == "UAF-MS-0066") # %>%
  dplyr::filter()

## ice wave rider and MicroStar are surface devices
drift$depth <- ifelse (seq_along(nrow (drift)) %in% grep ("UAF-SVP", drift$DeviceName), 15, 0)
## need to set these after final drifter selection
drift$DeviceName <- factor (drift$DeviceName)
drift$col <- brewer.pal (8, "Set2")[drift$DeviceName] # 8 is max of Set2
drift$age <- as.numeric (drift$DeviceDateTime) - min (as.numeric (drift$DeviceDateTime))/3600 # in hrs



## ----------------------------------------------------------
## set-up background map layers and define bounding box
## set bbox manually
if (0){
  # bbox_new <- st_sf (a=1:2, geom=st_sfc (st_point (c (-150,   58.7)),  # E S
  #                                        st_point (c (-154.3, 60.3)))  # W N
  bbox_new <- st_sf (a=1:2, geom=st_sfc (st_point (c (-150.5, 59.2)),  # E S
                                         st_point (c (-152.5, 59.8)))  # W N
                     , crs=4326) %>%
    st_transform (projection) %>%  # transform first to keep straight lines in projection
    st_bbox() %>%
    st_as_sfc()
  ## use drifter range to set bbox
}else{
  bbox_new <- st_bbox (drift) %>%
    st_as_sfc() %>%
    st_transform(projection)
}


## get shoreline and clip with bbox_new
## for crop, see https://datascience.blog.wzb.eu/2019/04/30/zooming-in-on-maps-with-sf-and-ggplot2/
world <- sf::st_read (worldP, quiet=TRUE) %>%
  sf::st_transform(projection)  %>%
 st_crop (bbox_new)

## should get this to work to expand polygon past bbox of plot
## or set xlim and ylim on plot
# st_crop (st_coordinates (bbox_new)[c(1,3), c(1,2)]*c(0.9,1.1,0.9,1.1) %>%  # expand past bbox_new
#          st_sfc (st_point (.[1,])
#                  , st_point(.[2,]), crs=projection) %>%
#            st_bbox() %>%
#            st_as_sfc()
#          ) # suppress warning

## also see st_wrap_dateline !!  (for murres!)
# ak <- sf::st_read ("~/GISdata/LCI/AKregions/ak_regions_simp.shp")  ## may not need it any longer?

## test crop ok
# ggplot2::ggplot (world) + geom_sf()



## ----------------------------------------------------------
## plot and animate with ggplot2 and gganimate
## https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate/

basemap <- ggplot2::ggplot (data=bbox_new) +
#  coord_cartesian(xlim=st_coordinates(bbox_new)[1:2,1]*rev (c(1.2, 0.8))) +  # no effect?
  ggplot2::geom_sf() +
  ggplot2::geom_sf (data=world, fill = "goldenrod") +
#  ggplot2::labs(title="") +
  ggplot2::theme(  ## minor fixes to do here: plot land the way to the edge
    panel.background=ggplot2::element_blank(),
    legend.position="none"
  )
## add bathymetry and decorations
## add graticules
#  sf::st_graticule()  # transform? -- or at the end?


## separate map per animation/drifter
## plot dimensions
Hi <- 800; Wi <- 800

for (i in seq_along(levels (drift$deploy))){
  drP <- subset (drift, deploy == levels (drift$deploy)[i])
  ## catch error if there's nothing to animate
  if (nrow (drP) > 5){
    fN <- gsub (":", "_", levels (drift$deploy), fixed=TRUE)
    drL <- dplyr::summarise(drP, do_union=FALSE) %>% st_cast ("LINESTRING")
    map <- basemap +
      ggplot2::geom_sf (data=drL, color="yellow", lwd=0.7) +
      ggplot2::geom_sf (data=drP, size=1, pch=3)
    ggsave (paste0 ("s", fN, ".gif"), map, device="png", path=outpath)

    map3 <- map +
      transition_time(DeviceDateTime) +
      # transition_time(age) +
      shadow_wake(wake_length=0.5, alpha=FALSE) +   ## or add trails
      ggtitle("time: {frame_time}")

    anim <- gganimate::animate (map3, nframes = 100, fps=2  # calc nframes!
                                , height=Hi, width=Wi
                                , rewind=FALSE)
    anim_save (paste0 (fN, ".gif"), anim, path=outpath)
  }
}

## add drifters, make lines
drP <- drift# %>% filter (DeviceName==levels (DeviceName)[1])
drL <- dplyr::summarize (drP, do_union=FALSE) %>%
  st_cast("LINESTRING")

map <- basemap +
  ggplot2::geom_sf (data=drL, color="yellow", lwd=0.7) +  # animate line somehow
  ggplot2::geom_sf (data=drP, cex=2, pch=3)  # mark positions with a cross
# map
ggsave (paste0 ("aMap.gif"), map, device="png", path=outpath)


## animate
map3 <- map +
  transition_time(DeviceDateTime) +
 # transition_time(age) +
  shadow_wake(wake_length=0.5, alpha=FALSE) +   ## or add trails
  ggtitle("time: {frame_time}")

anim <- gganimate::animate (map3, nframes = 100, fps=4  # calc nframes!
                            , height=Hi, width=Wi
                            , rewind=FALSE)
anim_save ("animation.gif", anim, path=outpath)

rm (Hi, Wi, drL, drP)



## end of current code








## refine animation
## add bathymetry

## alternative: plot it all in base plots with sf


## end of working code









## tmap -- fallback
if(0){
  tmap_mode ('plot')
  dev.new(width=16, height=12, unit="in")

  m <- tm_shape (world, projection=projection, bbox=bbox_new) +
    tm_polygons(col='goldenrod', alpha=0.3, border.col='black', lwd=0.75)

  m <- m +
    tm_shape (drP) +
    tm_dots (col="red", size=.2)  # add legend
  # tm_graticules(x=seq(-180,180,by=15),y=seq(40,80,by=5), labels.size = 0.4, lwd = 0.25, ticks=FALSE) +

  tmap_save(m, paste0 (outpath, "test.png"), scale=1.5)

  ## animate the beast
  # use tm_facets
  m <- m +
    tm_facets(along="age")

  tmap_animation(m, filename=paste0 (outpath, "animation.gif")
                 , width=16, height=8, dpi=100)

  rm (bbox_new, world, worldP)
}




##Create animation with points showing up one by one
plot_anim <- B +
  transition_states(states = Y, state_length = 0, wrap = FALSE) +
  enter_recolor(fill = "#f0f5f9") +
  shadow_mark(past = TRUE, alpha = 1, fill = "#3a6589")

##Render animation
animate(plot_anim, end_pause = 60,
        height = 200, width = 400) # a higher res img would not upload here :(


require (ggplot2)
require (maps)
require (ggthemes)



## load bathymetry
# bathyP
mar_bathy <- read_stars (bathyP)


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

require ("ggplot2")
require ("ggthemes")
require ("maps")
require ("gganimate")
require ("dplyr")

## ggmaps == disabled by NOAA

ggplot () +
  borders ("world", "usa:alaska", colour="gray90", fill="gray85")+
  theme_map() +
  geom_point (data=drP, aes (x=Longitude, y=Latitude
                   # , color=depth
                   , size=2)) +
  labs (title="Date: {frame_time}") +
  transition_time(age) +
  shadow_wake(wake_length=0.5, alpha=FALSE) +
  ease_aes ("linear")





require ('ggOceanMaps')## uses sf and stars
require ("basemaps")   ## see https://jakob.schwalb-willmann.de/basemaps/ -- backgrounds!

## 1. Use ggOceanMaps for bathymetry and drifter
## 2. add basemaps::esri usa_topo_maps for land, once above is working
map <- ggOceanMaps::basemap(drP
                     # , bathymetry=TRUE, bathy.style='rcb' # use better bathymetry
                     # , glaciers=TRUE
                     , legends=TRUE
                     # , downsample=2
                     # , base_size=....?
                       , projection.grid=FALSE
                     , expand.factor=1.2 # expand map based tracks
                     , rotate=TRUE
                     )
# map <- map + stars::geom_stars(data=mar_bathy)+
#   ggplot2::scale_fill_gradientn(
#     name="Depth [m]"
#     # , breaks=seq(0,5e3,1e2), limits=c(0,NA)
#     , colors=colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(8)
#   )
#   reorder_layers (map)

map <- map +
  geom_spatial_point (data=drP, aes (x=Longitude, y=Latitude, color=depth, size=2)
                      , crs=4326, show.legend=FALSE)
# map + transition_time(DeviceDateTime) +
map + transition_time(age) +
  shadow_wake(wake_length=0.5, alpha=FALSE)
  # shadow_mark (alpha=0.3, size=0.5)
map



## experimentail
###
gganimate::animate (map, nframes=200, fps=5)  ## calculate this better -- not working
# anim_save()
# view_step.... ?


## remaining issues:
# low-res of bathymetry
# low-res of shoreline
# replace land with satellite image (basemaps)
# animate by time steps






if (0){
map <- map +
  stars::geom_stars(data=mar_bathy) +
  ggplot2::scale_fill_gradientn(
    name="Depth [m]"
    # , breaks=seq(0,5e3,1e2), limits=c(0,NA)
    , colors=colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(8)
  )
# + drP here still fails
reorder_layers(map)
}

## add drifter -- and animate
map +
  geom_spatial_point(data=drP, aes(x=x,y=y,color=col))


  geom_path (data=drP,
                 aes(x=long, y=lat, group=group, color=type)
)


# map +
#   geom_point()
#   geom_line(drP)
#
#
#   geom_line(data=transform_coord(drP))
#
# ggplot2() +
#   map +
#   geom_line(drP)



## for basemaps
## set boundaries interactively:
# or use draw_ext()

# view available maptypes
get_maptypes()
## set defaults for basemap
## osm, carto, and esri are free without token
# set_defaults (map_service="osm", map_type="topographic")  ## nice dierke-like topography
set_defaults (map_service="esri", map_type="world_ocean_base")    ## too pale
# set_defaults (map_service="esri", map_type="natgeo_world_map") ## like usa_topo_maps but labled :(
set_defaults (map_service="esri", map_type="world_shaded_relief")    ## simple, non-distracting
set_defaults (map_service="esri", map_type="world_imagery")    ## like google (a bit dark?)
set_defaults (map_service="esri", map_type="usa_topo_maps")    ## like shaded relief, but color

## can now use any of these:
# basemap_plot(ext)
# basemap_ggplot(ext)
basemap_ggplot (bbox_new)
# +
#   geom_point (data=transform (drP, proj.out=4326)
#               , aes (x=Longitude, y=Latitude))
# transition_states .....


require (ggplot2)
ggplot()+
basemap_gglayer(bbox_new) +
   scale_fill_identity() +
   coord_sf() +
  drP

# basemap_stars(ext)
# basemap_mapview(ext)  ## life map!

## ggOceanMaps
p <- basemap (limits=bbox_new
              , shapefiles=list(land=bs_land, glacier=bs_glacier, bathy=bs_bathy)
              , bathymetry=TRUE, glaciers=TRUE, legends=FALSE
              )
p

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
