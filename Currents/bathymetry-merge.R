## generate unified high-resolution bathymetry for cook inlet and northern GoA
## merge Zimmermann and GEBCO bathymetries onto a common raster

## use Cook Inlet raster parameters. Upsample other rasters.
## fill NAs from Cook Inlet with GOA, result with GEBCO
## smooth edges?
## add DEM?
## output: topo-values or depth values?


## QGIS: no scripting -- this is too complex
## GrassGIS: may be better alternative? use rgrass


rm (list=ls())
# renv::load("renv/activate.R")

interAct <- FALSE

## -----------------------------------------------------------------------------
## set area for the grid

## optimistic and big
area <- "ResearchAreaBig"
bbox <- c (-156, -143.5, 56, 62) ## rRes 200 is too much

## KBL research area -- running a large grid like this at 100 or 50 m needs > 16 GB RAM
area <- "ResearchArea"  ## MacBook with 32 GB RAM can handle up to 100 m, but not 50
bbox <- c(-155.2, -143.9, 57.1, 60.7)

if (0){
## reduced research area
area <- "ResearchArea_small"
bbox <- c(-154, -150, 58.5, 61) ## restricted to stay within memory limits

area <- "GWA-area"
bbox <- c(-153.5, -150.8, 58.7, 60.1) ## restricted to stay within memory limits

# area <- "quarterly"
# bbox <- c(-152.2, -150.9, 59.1, 59.85) ## restricted to stay within memory limits
#
# area <- "KachemakBay"
# bbox <- c(-151.95, -151, 59.38, 59.8) ## restricted to stay within memory limits
}


## ----------------------------------------------------------------------------

ciF <- "~/GISdata/LCI/bathymetry/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf" ## bad numbers, good crs
gaF <- "~/GISdata/LCI/bathymetry/CGOA_bathymetry_grid/cgoa_bathy/w001001.adf"
gcF <- "~/GISdata/LCI/bathymetry/GEBCO_26_Mar_2024_b7838035e5db/gebco_2023_n62.0_s55.0_w-157.0_e-143.0.tif" ## no CRS
# gcF <- "~/GISdata/LCI/bathymetry/gebco_2022/GEBCO_2022.nc" ## has crs
gcF <- "~/GISdata/LCI/bathymetry/GEBCO_17_Apr_2024_fc045996e23b/gebco_2023_n62.0_s54.0_w-159.0_e-143.0.tif" ## no CRS
## add terrestrial DEM: best=2m


require ("stars")
require ("magrittr")



# for (gRes in c(200, 100, 50)){
gRes <- 50


# gc <- read_stars(gcF, package="stars")
# rm (ciF, gaF, gcF)

## fix bad NA values -- this takes care of those odd edges in Zimmermann files!
ci <- read_stars (ciF, package="stars")*-1  ## NA_value = ?
ci [ci > 0] <- NA ## refine? still cutting off useful info in upper Cook Inlet?

if (interAct){
  plot (ci)
  ci
}


## generate a new all-inclusive raster first -- st_warp will otherwise use ci grid
## port all of this to GrassGIS?
## require ("rgrass")



  require ("starsExtra")
#  if (exists ("bbox")){
  if (0){
    ## use gebco as template:  -157 -143 55 62
    ## hand-tool a smaller grid
    nG <- starsExtra::make_grid (st_bbox(gc) %>%  ## res=50 easts up > 8 GB RAM, 100 takes 4GB -- use GRASS GIS?!
                                   st_as_sfc () %>%
                                   st_transform (st_crs(ci))
                                 , res=gRes, buffer=10e3
    )
  }else{
    ## use manually defined bounding box (study region)
    nG <- data.frame (lon=c(bbox[1], bbox[2]), lat=c(bbox[3], bbox[4])) %>%  ## gebco is -157 -143 55 62
      st_as_sf (coords=c("lon", "lat"), crs=4326) %>%
      st_transform(st_crs (ci)) %>%
      starsExtra::make_grid(res=gRes)
  }
  ## transform grids to new grid and fill in the blanks hierachically
  ci2 <- stars::st_warp(ci, nG)
  rm (ci)
  ga <- read_stars(gaF, package="stars")*-1
  ga [ga > 0] <- NA
  ga2 <- stars::st_warp(ga, ci2, method="cubic", use_gdal=TRUE) # or bilinear -- deal with NA!
  rm (ga)
  ## use raster::cover, terra::cover equivalent to merge the rasters
  ci2 [[1]] <- ifelse (is.na (ci2 [[1]]), ga2 [[1]], ci2 [[1]])
  rm (ga2)
  ## check-point
  save.image ("~/tmp/LCI_noaa/cache/bathymetry2.RData")
  gc2 <- read_stars (gcF, package="stars") %>%
    stars::st_warp(gc, ci2, method="cubic", use_gdal=TRUE)
  rm (gcF)
  ci2 [[1]] <- ifelse (is.na (ci2 [[1]]), gv2 [[1]], ci2 [[1]])
  tm (gc2)

  # zm <- ci2; rm (ci2)
  # zm [[1]] <- ifelse (is.na (zm [[1]]), ga2 [[1]], zm [[1]])
  # zm [[1]] <- ifelse (is.na (zm [[1]]), gc2 [[1]], zm [[1]])

  # zm2 <- st_crop (zm, )

  ## save results
  write_stars (ci2, paste0 ("~/GISdata/LCI/bathymetry/KBL-bathymetry_", area, "_"
                           , gRes, "m_epsg3338.tiff"))
# }



# require ("raster")
# comp <- raster::cover (ci2, ga2, gc2)

## crop final output:  st_crop
if (0){
  ## cut out suitable chunck from gebco!
  ## old -- for global gebco
  gc2 <- st_crop(gc
                 , data.frame (lon=c(-153, -150), lat=c(52, 60)) %>%
                   st_as_sf (coords=c("lon", "lat"), crs=st_crs(gc))) %>%
    st_transform(st_crs (gc)) %>%  ## correct for raster??
    st_warp (ci)  ## this part is slow!!
}


## EOF

