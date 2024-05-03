## generate unified high-resolution bathymetry for cook inlet and northern GoA
## merge DEM, Zimmermann and GMRT bathymetries onto a common raster

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
area <- "ResearchAreaXL"
bbox <- c (-156, -143.5, 55, 61.5) ## rRes 200 is too much for Dell

## KBL research area -- running a large grid like this at 100 or 50 m needs > 16 GB RAM
area <- "ResearchArea"  ## MacBook with 32 GB RAM can handle up to 100 m, but not 50
# bbox <- c(-155.3, -143.9, 55.6, 60.72) # as specified

if (1){
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
## GEBCO_2023 has issues. The next are all similar, but GMTv4_2 preserves highest resolution
gcF <- "~/GISdata/LCI/bathymetry/gebco_2022/GEBCO_2022.nc" ## has crs
gcF <- "~/GISdata/LCI/bathymetry/ETOPO/ETOPO_2022_(iceSurface-15arcS).tif"  ## use ETOPO instead of GEBCO. Better DEM?
gcF <- "~/GISdata/LCI/bathymetry/ETOPO_2022_v1_15s_surface_SCAK.tiff"
gcF <- "~/GISdata/LCI/bathymetry/GMRTv4_2topo/GMRTv4_2_20240412topo_max_WGS84_EPSG4326.grd"
gcF <- "~/GISdata/LCI/bathymetry/GMRTv4_2_20240423topo.tif"
gcF <- "~/GISdata/LCI/bathymetry/GMRTv4_2/GMRTv4_2_20240423topo.tif"
## add terrestrial DEM: best=2m AK from USGS



## ensure needed files are present
  # curl::curl_download(url="https://gis.ngdc.noaa.gov/arcgis/rest/services/DEM_mosaics/DEM_all/ImageServer/exportImage?bbox=-160.00000,55.00000,-140.00000,63.00000&bboxSR=4326&size=4800,1920&imageSR=4326&format=tiff&pixelType=F32&interpolation=+RSP_NearestNeighbor&compression=LZ77&renderingRule={%22rasterFunction%22:%22none%22}&mosaicRule={%22where%22:%22Name=%27ETOPO_2022_v1_15s_surface_elev%27%22}&f=image"
  #                     , destfile=gcF)
mF <- !file.exists (demF, ciF, gaF, gcF)
if (any (mF)){ # any files missing?
  sites <- c(DEM="https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:707/html"
             , Zimmermann="http://web.archive.org/web/20180821131853/http://www.afsc.noaa.gov/RACE/groundfish/bathymetry/ (CI)"
             , Zimmermann="http://web.archive.org/web/20180821131853/http://www.afsc.noaa.gov/RACE/groundfish/bathymetry/ (CGOA)"
             , gmrt="https://www.gmrt.org/"
  )
  stop (paste0 ("Missing files. Get the necessary bathymetry file(s) from: \n"
  , sites [which (mF)])
  , "\nand place it in ", c(demF, ciF, gaF, gcF)[mF], " respectively.")
}
rm (mF)



require ("stars")
require ("magrittr")


## set gRes depending on available memory
require ("memuse")
ramL <- memuse::Sys.meminfo()$totalram |> as.numeric ()
if (area == "GWA-area" && ramL > 17e9){
  gRes <- 50                                     # ok on 32 GB Mac
} else if (ramL |> as.numeric() < 17e9 ){        # have less than 17 GB RAM?
  gRes <- 400                                    # for testing. Works on Dell.
  gRes <- 1000
}else if (ramL |> as.numeric() > 24e9){
  gRes <- 100                                    # max on Mac (32 GB RAM)
}else{
  gRes <- 200
}
rm (ramL)
# gRes <- 100



## generate a new all-inclusive raster first
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
rm (ci2)

ga <- read_stars(gaF, package="stars")
ga [[1]][ga[[1]] < 0.01] <- NA
ga2 <- stars::st_warp(ga, dm, method="cubic", use_gdal=TRUE, no_data_value=9999) # or bilinear -- deal with NA!
rm (ga)
## use raster::cover, terra::cover equivalent to merge the rasters
dm [[1]] <- ifelse (is.na (dm [[1]]), ga2 [[1]]*-1, dm [[1]])
rm (ga2)
## check-point
save.image ("~/tmp/LCI_noaa/cache/bathymetry2.RData")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/bathymetry2.RData")

## add terrestrial DEM. Using stars::st_mosaic

gc2 <- read_stars (gcF, package="stars") %>%
   stars::st_warp(dm, method="cubic", use_gdal=TRUE, no_data_value=9999)
#  stars::st_warp(dm)
dm [[1]] <- ifelse (is.na (dm [[1]]), gc2 [[1]], dm [[1]])
rm (gc2, gcF)
# zm2 <- st_crop (zm, )

## save results
write_stars (dm, paste0 ("~/GISdata/LCI/bathymetry/KBL-bathymetry_", area, "_"
                        , gRes, "m_EPSG", epsg, ".tiff"))
## add metadata, e.g.: gdal_edit.py -mo TIFFTAG_ARTIST="It was me" in.tif
## or gdal_translate in.tif out.tif -mo TIFFTAG_ARTIST="It was me" -mo TIFFTAG_IMAGEDESCRIPTION="This data my layer"
## ...: passed to gdal_write
## options: passed to GDAL
## there's a function: gdal_metadata -- only for reading

## EOF

