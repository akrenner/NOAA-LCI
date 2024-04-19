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

## over-sample research area, just in case
area <- "ResearchAreaBig"
bbox <- c (-156, -143.5, 55, 61.5) ## rRes 200 is too much for Dell

## KBL research area -- running a large grid like this at 100 or 50 m needs > 16 GB RAM
area <- "ResearchArea"  ## MacBook with 32 GB RAM can handle up to 100 m, but not 50
# bbox <- c(-155.3, -143.9, 55.6, 60.72) # as specified

if (0){
  ## reduced research area
  area <- "GWA-area"
  bbox <- c(-154, -150, 58.5, 61) ## restricted to stay within memory limits
}
## output projection (Alaska AEA: 3338)
epsg <- 3338



## -----------------------------------------------------------------------------
## file locations
demF <- "~/GISdata/LCI/bathymetry/Kachemak_Bay_DEM_1239/kachemak_bay_ak.asc"
ciF <- "~/GISdata/LCI/bathymetry/Cook_bathymetry_grid/ci_bathy_grid/w001001.adf" ## bad numbers, good crs
gaF <- "~/GISdata/LCI/bathymetry/CGOA_bathymetry_grid/cgoa_bathy/w001001.adf"
gcF <- "~/GISdata/LCI/bathymetry/GEBCO_26_Mar_2024_b7838035e5db/gebco_2023_n62.0_s55.0_w-157.0_e-143.0.tif" ## no CRS
# gcF <- "~/GISdata/LCI/bathymetry/gebco_2022/GEBCO_2022.nc" ## has crs
gcF <- "~/GISdata/LCI/bathymetry/GEBCO_17_Apr_2024_fc045996e23b/gebco_2023_n62.0_s54.0_w-159.0_e-143.0.tif" ## no CRS
# gcF <- "/vsizip/~/GISdata/LCI/bathymetry/GEBCO_17_Apr_2024_fc045996e23b.zip/GEBCO_17_Apr_2024_fc045996e23b/gebco_2023_n62.0_s54.0_w-159.0_e-143.0.tif"


## add terrestrial DEM: best=2m





require ("stars")
require ("magrittr")


# for (gRes in c(200, 100, 50)){
## set gRes depending on available memory
require ("memuse")
ramL <- memuse::Sys.meminfo()$totalram |> as.numeric ()
if (area == "GWA-area" && ramL > 17e9){
  rRes <- 50 # ok on Mac
} else if (ramL |> as.numeric() < 17e9 ){        # have less than 17 GB RAM?
  gRes <- 400                                    # for testing. Works on Dell.
  gRes <- 1000
}else if (ramL |> as.numeric() > 32e9){
  gRes <- 100                             # max on Mac (32 GB RAM)
}else{
  gRes <- 200
}


# gc <- read_stars(gcF, package="stars")
# rm (ciF, gaF, gcF)



## generate a new all-inclusive raster first -- st_warp will otherwise use ci grid
## port all of this to GrassGIS?
## require ("rgrass")


## -----------------------------------------------------------------------------
## set-up/define output grid

require ("starsExtra")
## use manually defined bounding box (study region)
nG <- data.frame (lon=c(bbox[1], bbox[2]), lat=c(bbox[3], bbox[4])) %>%  ## gebco is -157 -143 55 62
  st_as_sf (coords=c("lon", "lat"), crs=4326) %>%
  st_transform(crs=epsg) %>%     # st_crs (ci)) %>%
  starsExtra::make_grid(res=gRes)
## transform grids to new grid and fill in the blanks hierachicaly



## -----------------------------------------------------------------------------
## read in grids

dm <- stars::read_stars(demF, package="stars") %>%
stars::st_warp(nG)
rm (demF, nG)

## eventually, consider adding USGS DEM here at some stage. But it's BIG.

## fix bad NA values -- this takes care of those odd edges in Zimmermann files!
ci <- read_stars (ciF, package="stars")  ## NA_value = ?

if (interAct){
  plot (ci)
  ci
}
ci [[1]][ci[[1]] < 0] <- NA ## refine? XXX
ci2 <- stars::st_warp(ci, dm)
rm (ci)
dm [[1]] <- ifelse (is.na (dm[[1]]), ci2[[1]]*-1, dm[[1]])  ## zero-out weird data-blocks in Zimmermann files

ga <- read_stars(gaF, package="stars")
ga [[1]][ga[[1]] < 0.01] <- NA
ga2 <- stars::st_warp(ga, dm, method="cubic", use_gdal=TRUE, no_data_value=9999) # or bilinear -- deal with NA!
rm (ga)
## use raster::cover, terra::cover equivalent to merge the rasters
dm [[1]] <- ifelse (is.na (dm [[1]]), ga2 [[1]]*-1, dm [[1]])
rm (ga2)
## check-point
save.image ("~/tmp/LCI_noaa/cache/bathymetry2.RData")

gc2 <- read_stars (gcF, package="stars") %>%
  stars::st_warp(dm, method="cubic", use_gdal=TRUE, no_data_value=9999)
dm [[1]] <- ifelse (is.na (dm [[1]]), gc2 [[1]], dm [[1]])
rm (gc2, gcF)

# zm2 <- st_crop (zm, )

## save results
write_stars (dm, paste0 ("~/GISdata/LCI/bathymetry/KBL-bathymetry_", area, "_"
                        , gRes, "m_EPSG", epsg, ".tiff"))
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

