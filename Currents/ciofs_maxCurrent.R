## map maximal currents during biggest tide from CIOFS
rm (list=ls())


## biggest tide:   8.242 m,       2014-01-31 21:23:00
## max tide speed: 0.0003626 m/s, 2015-02-19 21:16:00
# format (as.POSIXct("2014-01-31"), "0%d")



## -----------------------------------------------------------
## read NoteBook-exported ncdf file and extract coordinates
## -----------------------------------------------------------


## 2024-10-30, status:
## can export netcdf to raster, but stretched grid results in gaps
## using larger grid to fill-in gaps causes some ugly artifacts
## not much luck with bicubic interpolation so far, apparently
##
## exporting directions from u and w -- would need to figure out cardinal
## directions first (from change in lat-lon in cell above and to the right)

## 2024-11-06: interpolated raster
## still need to test: bicubic, linear, vs nearest neighbour interpolation
## still to do: windy-like plot of vector field




###-----  use OpenDrift.R instead???  ------


## u, v, w, rho
# u: east-west    lat_u, lon_u, s_rho, ocean_time
# v: north-south
# w: up movement?
# rho: grid-centers
# s: depth
# zeta = ?
# xi_rho and eta_rho:  simple counters -- not needed




## --------------------- define parameters ------------------- ##

grid_spacing <- 100

prjct <- 3338


## any way to download these automatically? maybe not
ncF <- "~/GISdata/LCI/OpenDrift/max_speed_2014-1.nc"     ## max tide picked by Kristen Thyng
ncF3 <- "~/GISdata/LCI/OpenDrift/ciofs_bigtide_KT_b.nc"  ## big file with 30 depth, 30 time steps
worldP <- "~/GISdata/data/coastline/gshhg-shp/GSHHS_shp/f/GSHHS_f_L1.shp"   ## full resolution


## ------------------------- processing ----------------------- ##



## options to access ROM netCDF data
## stars::read_ncdf Simple, nice documentation
## netcdf4 raw. More complicated. But avoids some stars hassle.

## use Kristen's max_speed_2014-1.nc file
## visit ciofs_bigtide_KT.nc later (extracting u,v vectors, filtering by direction as well)


## use stars::read_ncdf(ncF3, var="u") -- not that simple; abandon this effort. Trouble extracting data




require ("ncdf4")
require ("stars")
dir.create("~/tmp/LCI_noaa/data-products/CIOFS/", showWarnings=FALSE, recursive=TRUE)


if (0){
nc <- ncdf4::nc_open(ncF)
#print (nc)
#names (nc$dim)
#names (nc$var)
nX <- function (vid){
  v <- ncvar_get (nc, varid=vid)
  v <- as.numeric(v)
  v
}

require("sf")
require ("dplyr")
# s_rho=depth -- constant in this nc
maxS <- data.frame (lon = nX ("lon_rho"),
                      lat = nX ("lat_rho"),
                      speed=nX ("speed")) %>%
  filter (!is.na (speed)) %>%
  st_as_sf (coords=c("lon", "lat"), crs=4326) %>%
  st_transform(crs=prjct)
ncdf4::nc_close (nc)
rm (nc)

# COUlD use ## st_rasterize ## (effectively reproject) or aggregate over grid
# bb <- st_bbox (maxS)
# spStr <- st_rasterize (maxS
#                        # , template=st_as_stars (st_bbox (maxS),
#                        #                       nx = (bb$xmax-bb$xmin) %% grid_spacing,
#                        #                       ny = (bb$ymax-bb$ymin) %% grid_spacing)
# )
# rm (bb)


## define grid
seaAEx <- st_bbox (maxS) %>%
  st_as_sfc()
pgon <- st_make_grid(seaAEx, square=TRUE, cellsize=rep (grid_spacing, 2)) %>%
  st_as_sf() %>%
   mutate (ID=row_number())
A <- st_intersection (pgon, seaAEx)  ## grid -- suppress warning
rm (seaAEx, pgon)


pointsID <- maxS %>%
  st_join (A) %>%
  as.data.frame() %>%
  group_by (ID) %>%
  summarize (maxSpeed=max (speed, na.rm=TRUE))
speedPol <- left_join(A, pointsID, by="ID")
rm (pointsID)

speedS <- st_rasterize (speedPol ["maxSpeed"], dx=grid_spacing, dy=grid_spacing)



## aggregate CIOFS data over new grid
if (0){  ## seems super slow -- needs testing
  gridPts <- function (pts, grid_spacing, var="speed"){
    seaAx <- st_bbox (pts) %>%
      st_as_sfc()
    pgon <- st_make_grid(seaAx, square=TRUE, cellsize = rep (grid_spacing, 2)) %>%
      st_as_sf() %>%
      mutate(ID=row_number())
    Ax <- st_intersection(pgon, seaAx)
    # generalize variable name
    pts$speedX <- pts [,which (names (pts)==var)]
    pointsID <- pts %>%
      st_join(Ax) %>%
      as.data.frame() %>%
      group_by (ID) %>%
      summarize (maxSpeed=max (speedX, na.rm=TRUE))  ## need to generalize this!
    speedPol <- left_join(A, pointsID, by=ID)
    speedS <- st_rasterize(speedPol ["maxSpeed"], dx=grid_spacing, dy=grid_spacing)
    speedS
  }

  speedS <- gridPts (maxS, 50)
 }




dir.create("~/tmp/LCI_noaa/media/CIOFS/", showWarnings=FALSE, recursive=FALSE)
png ("~/tmp/LCI_noaa/media/CIOFS/ciofs_maxspeed.png")
plot (speedS)
dev.off()

save (speedS, file="~/tmp/LCI_noaa/cache/ciofs_maxspeed.RData")
write_stars (speedS, dsn = paste0 ("~/tmp/LCI_noaa/data-products/maxSpeed_CIOFS"
             , grid_spacing, ".tif"))

rm (speedPol, ncF)
}







## ------------------ new start with u v w ----------------------

## set depth = 0
## calc speeds per time and position
## find greatest speed for each position
## export speed, u, v, and w

## should use proper u_lat, v_lat, etc, instead of truncating arrays (dimLim1, dimLim2)


## start over
if (0){
  rm (list=ls())
  ncF3 <- "~/GISdata/LCI/OpenDrift/ciofs_bigtide_KT_b.nc"  ## big file with 30 depth, 30 time steps
  grid_spacing <- 10e3  ## 10 km seems to make sense -- go to 20 km?
  grid_spacing <- 5e3
  grid_spacing <- 1e3
  # grid_spacing <- 500
  prjct <- 3338
  require ("ncdf4")
}


## see https://rpubs.com/cyclemumner/roms0
## tools made for ROMS -- but useing old sp/raster framework
## using angstroms, ncdump, tabularaster, rworldmap....  not substantial gain
## do not pursue this further




### u v movement
ncF <- ncF3   ## big file with 30 depth, 30 time steps
nc <- ncdf4::nc_open(ncF)

wV <- ncvar_get (nc, "w")  # dim: xi_v, eta_v, s_rho, ocean_time - 391 187 30 30
vV <- ncvar_get (nc, "v")  ## dim 2 is already short
uV <- ncvar_get (nc, "u")  ## u is 1 short of v



## dimLimit -- ensure all dimensions are of same length
dR <- rbind (dim (wV), dim (vV), dim (uV)) %>%
  apply (MARGIN=2, FUN=min) %>%
  lapply (FUN=function (x, sv=1){seq(sv, x)})

## 0: easy: max w
wV <- wV[dR[[1]],dR[[2]],,dR[[4]]]   ## use any depth for w, surface for v and u
vV <- vV [dR[[1]],dR[[2]],1,dR[[4]]]
uV <- uV [dR[[1]],dR[[2]],1,dR[[4]]]


wU <- array (dim=dim(wV)[1:2])
wD <- wU # wU: up, wD: down
for (i in 1:dim (wU)[1]){
  for (j in 1:dim (wU)[2]){
    if (!any (!is.na (wV [i,j,,]))){  # all values are NA/NaN
      wU [i,j] <- NA
      wD [i,j] <- NA
    }else{
      # XXXX  add: average upwelling, upwelling in top layer
      wU [i,j] <- max (wV [i,j,,], na.rm=TRUE)
      wD [i,j] <- min (wV [i,j,,], na.rm=TRUE)
      maxT <- which.max (wV [i,j,1,]) # time is last
      minT <- which.min (wV [i,j,1,]) # other end of tide
      meanUP <- mean  (subset (wV [i,j,,maxT], wV [i,j,,maxT] > 0), na.rm=TRUE)  ## all NAs
    }
  }
}

wDF <- data.frame (lon = as.numeric (ncvar_get (nc, varid="lon_rho")[dR[[1]],dR[[2]] ]),
                   lat = as.numeric (ncvar_get (nc, varid="lat_rho")[dR[[1]],dR[[2]] ]),
                   wu = as.numeric (wU), # already applied dR
                   wd = as.numeric (wD)  # already applied dR
#                   wuM = as.numeric (meanUP)  # mean upwelling in the watercolumn at time of max upwelling  ## XXX not working yet -- all NA
)         # cut off first or last??


## 1: calculate speed
# vV <- ncvar_get (nc, "v")[,,1,]  ## dim 2 is already short
# uV <- ncvar_get (nc, "u")[,dimLim2,1,] ## u is 1 short of v
speed <- sqrt (as.numeric (vV)^2 + as.numeric(uV)^2) |>     ## -180 to 180, 0 = north
  array (dim = dim (vV))

## calculate direction -- in ROMS and then in projected coordinates
alphaR <- atan (uV/vV)  #   0 = north; -pi to pi     /pi*360  ## N = 0 degrees, E: 90 degrees -- check XXX -- N/S correct?
## calculate angle between ROMS cells in projected coordinates
## transform into projected coordinates
lon <- ncvar_get(nc, varid="lon_rho")[dR[[1]], dR[[2]]]
lat <- ncvar_get(nc, varid="lat_rho")[dR[[1]], dR[[2]]]
## find angle between current cell and cell to the north in ROMS-grid
lA <- sapply (1:ncol (lon), function (i){ ## process column by column. lat goes from south to north
  # or use gear::angle2d?
  require ("useful")
  lldf <- data.frame (lon=lon [,i], lat=lat [,i]) %>%
    st_as_sf (coords=c("lon", "lat"), crs=4326) %>%
    st_transform(crs=prjct) %>%
    st_coordinates()
  dC <- diff (lldf) %>% as.data.frame()
  angl <- (cart2pol (dC$X, dC$Y, degrees=FALSE)$theta) + pi/2 ## 0-2pi, 0 degrees = N -- VERIFY XXXX
  angl
})

# working so far 2024-10-09
ncdf4::nc_close (nc)


## combine uv-angle with lA chart curvature
aR <- array (dim = dim (alphaR))
for (i in 1:dim (alphaR)[3]){
  aR [,,i] <- (alphaR [,,i] + lA [c(1:nrow (lA), nrow (lA)),]) %% (pi/2) ## duplicate last row to make lA conformal
}
# aR <- ifelse (aR > )
rm (alphaR, lon, lat, lA, i)





## extract max speed within day
## anywhere in water column or only at the surface?
topAr <- array (dim = c (dim (speed)[1:2], 5))  # last dimension: max speed, mtheta, surface speed, stheta
speedDepth <- rep (NA, dim (speed)[1])
for (i in 1:dim (speed)[1]){
  for (j in 1:dim (speed)[2]){
    if (all (is.na (speed [i,j,]))){
      topAr [i,j,] <- rep (NA, dim (topAr)[3])
    }else{
      mIJ <- which.max (speed [i,j,])
      speedDepth [i] <- mIJ
      topAr [i,j,1] <- speed [i,j,mIJ]
      topAr [i,j,2] <- aR [i,j,mIJ]
      topAr [i,j,3] <- speed [i,j,1]
      topAr [i,j,4] <- aR [i,j,1]
      topAr [i,j,5] <- mean (speed [i,j,], na.rm=TRUE)
    }
  }
}
## assemple big DF for export
wDF <- cbind (wDF,
              speedM = as.numeric (topAr [,,1]),
              thetaM = as.numeric (topAr [,,2]),
              speedS0 = as.numeric (topAr [,,3]),
              thetaS0 = as.numeric (topAr [,,4]))
## add projected u and v
wDF$prU <- sin (wDF$thetaM) * wDF$speedM
wDF$prV <- cos (wDF$thetaM) * wDF$speedM

## clean-up
rm (nc, topAr, speed, uV, vV, wV, wU, wD, ncF, dR, i,j, mIJ)


## trim to original export domain (unknown why there is more data than that)
wDF <- subset (wDF, (-153.6 < lon) & (lon < -150.0) &
                 (58.7 < lat) & (lat < 60.8))







## --------------- interpolate and save output ------------------------------##

save.image ("~/tmp/LCI_noaa/cache/maxCurrentCIOFS3.RData")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/maxCurrentCIOFS3.RData"); require ("stars"); require ("dplyr")
## modify things here to interpolate NAs?



wDFsf <- st_as_sf (wDF, coords=c("lon", "lat"), crs=4326) %>%
  st_transform(crs=prjct)
rm (wDF)

require ("interp")
for (i in seq_len(length (names (wDFsf))-1)){
  tDF <- data.frame (x=st_coordinates(wDFsf)[,1], y=st_coordinates(wDFsf)[,2]
                     , z=st_drop_geometry(wDFsf [,i])[,1]) %>%
    dplyr::filter (!is.na (z))
  wdS <- interp::interp (x=tDF$x, y=tDF$y, z=tDF$z
                           , input="points", output="grid"
                           # , linear=TRUE  ## bi-cubic can result in negative speeds!!
                           , linear=FALSE, baryweight=TRUE
                           , na.rm=TRUE
                           , extrap=FALSE, duplicate="error"
                           , nx = diff (range (st_coordinates (wDFsf)[,1])) / grid_spacing + 1
                           , ny = diff (range (st_coordinates (wDFsf)[,2])) / grid_spacing + 1
  ) %>%
    interp::interp2xyz(data.frame=TRUE) %>%   ## assemble stars object
    st_as_sf (coords=c("x", "y"), crs=prjct)
  ## interp::interp2grid () -- not applicable, at least not until stars version available


  ## cut-out land
  if (!exists ("worldM")){
    worldM <- sf::st_read (worldP, quiet=TRUE, crs=4326) %>% st_geometry()
    worldM <- subset (worldM, st_is_valid (worldM)) %>% ## polygon 2245 is not valid
      st_crop (c(xmin=-154, xmax=-149, ymin=58, ymax=61.5)) %>%   ## or could use bbox above
      sf::st_transform(prjct)
    rm (worldP)
  }
  if (0){
    onLand <- st_intersects(wdS, worldM) %>% as.numeric () ##
    wdS <- subset (wdS, isFALSE(onLand)); rm (onLand)
  }



  ## plot
  png (paste0 ("~/tmp/LCI_noaa/media/CIOFS/", names (wDFsf)[i], ".png")
       , width = 6*300, height=8*300, res=300)
  plot (wdS)
  plot (worldM, add=TRUE, col = "beige")
  dev.off()


  ## save to geoTIFF
  write_stars (wdS %>% st_rasterize()
               , dsn=paste0 ("~/tmp/LCI_noaa/data-products/CIOFS/coifs_maxspeeds_"
                             , names (wDFsf)[i], ".tif"))
  rm (wdS)
  cat (i, "\n")
}


## turn it into stars object (earlier?) and export
save.image ("~/tmp/LCI_noaa/cache/maxCurrentCIOFS2.RData")

# image (wDFsf, band=3)


## ------------------ end of u v w ----------------------






## move to a new file?

## read in transformed u and v vectors
## make windy-like current map for flood and slack tides

## read geotiffs
## plot as in https://github.com/milos-agathon/wind_map/blob/main/R/wind_map.R
## also see https://r-graphics.org/recipe-miscgraph-vectorfield (more traditional)

rm (list = ls()); load ("~/tmp/LCI_noaa/cache/maxCurrentCIOFS2.RData")


require ('ggfields')
require ('ggplot2')
require ('ggspatial')


## unified data frame with u, v, speed, and theta
tDFx <- data.frame (x=st_coordinates(wDFsf)[,1], y=st_coordinates(wDFsf)[,2]
                    , speedS0 = wDFsf$speedS0, thetaS0 = wDFsf$thetaS0
                    , u = wDFsf$prU, v = wDFsf$prV)
for (i in 3:ncol (tDF)){
  tDF <- tDFx %>%
    dplyr::filter (!is.na (tDFx [,i]))
  wDFbc <- interp::interp (x=tDF$x, y=tDF$y, z=tDF[,i]
                           , input="points", output="grid"
                           , linear=TRUE  ## bi-cubic can result in negative speeds!!
                           # , linear=FALSE, baryweight=TRUE
                           , na.rm=TRUE
                           , extrap=FALSE, duplicate="error"
                           , nx = diff (range (st_coordinates (wDFsf)[,1])) / grid_spacing + 1
                           , ny = diff (range (st_coordinates (wDFsf)[,2])) / grid_spacing + 1
  ) %>%
    interp::interp2xyz(data.frame=TRUE)
  if (i == 3){
    wDS <- wDFbc
  }else{
    wDS <- cbind (wDS, wDFbc$z)
  }
}
names (wDS) <- names (tDFx)

wDS <- st_as_sf (wDFbc, coords=c("x", "y"), crs=prjct)
wdS <- interp::interp2xyz(wDFbc, data.frame=TRUE) %>%
  st_as_sf (coords=c("x", "y"), crs=prjct)
rm (tDF)

## assemble stars object



ggplot() +
  ggspatial::annotation_map_tile(
    alpha      = 0.25,
    cachedir   = tempdir()) +
  geom_fields(
    data       = seawatervelocity,
    aes(radius = as.numeric(v),
        angle  = as.numeric(angle),
        colour = as.numeric(v)),
    max_radius = grid::unit(0.7, "cm")) +
  labs(colour  = "v[m/s]",
       radius  = "v[m/s]") +
  scale_radius_binned() +
  scale_colour_viridis_b(guide = guide_bins())









### code in progress -- or to be deleted
if (0){


## the hard part: get data from AOOS/Axiom server -- failed so far
if (0){
## 1. Thrreds -- server has been down
## 2. zarr -- also flaky at times. Preferred, but python/R interaction non-trivial


## THRREDs

## server may still be done -- couldn't resolve host name
if (0){
  require ('RNetCDF')
  # nc <- open.nc ("https://thredds.aoos.org/thredds/ncss/grid/AXIOM_CIOFS_HINDCAST.nc")
  # nc <- open.nc ("https://thredds.aoos.org/thredds/ncss/grid/AXIOM_CIOFS_HINDCAST.nc/dataset.html")

  ## from Chang -- could not resolve host name
  nc <- RNetCDF::open.nc ("http://thredds.aoos.org/thredds/dodsC/ciofs_hindcast/hindcast-products/fields/2018/axiom.ciofs.fields.hindcast.2014_0031.nc")


  # THREDDS NetCDF subset
  # https://thredds.aoos.org/thredds/ncss/grid/AXIOM_CIOFS_HINDCAST.nc/dataset.html

  ## THREDDS OPeNDAP
  # https://thredds.aoos.org/thredds/dodsC/AXIOM_CIOFS_HINDCAST.nc.html
  file.inq.nc (nc)

  close.nc (nc)


  ## from Chang, Axiom
  require ("ncdf4")
  # nc <- ncdf4::nc_open("http://thredds.aoos.org/thredds/dodsC/ciofs_hindcast/hindcast-products/fields/2018/axiom.ciofs.fields.hindcast.2018_0001.nc")
  nc <- ncdf4::nc_open("http://thredds.aoos.org/thredds/dodsC/ciofs_hindcast/hindcast-products/fields/2018/axiom.ciofs.fields.hindcast.2014_0031.nc")

  #  lon_u, lat_u, lon_psi, h, mask_u, ocean_time  ## u = eastwards
  u <- ncvar_get(nc, "u")
  nc_close (nc)
}




if (0){

  ## use zarr -- access through python
  require ("reticulate")
  # os <- import ('os')
  # use_condaenv ("base")
  # reticulate::install_python(version="3.12:latest")  ## use only once?
  reticulate::install_python()  ## use only once?
  # use_python()
  if (virtualenv_exists("zarrDL")){
    use_virtualenv("zarrDL")
  }else{
    ## automatically install seabirdscientific using pip into virtual environment
    # virtualenv_create("zarrDL", version="3.12")
    virtualenv_create("zarrDL")
    virtualenv_install (envname="zarrDL"
                        , packages= c("xarray", "zarr", "fsspec", "requests", "aiohttp")
                        , ignore_installed=TRUE)
  }
  ## set up fathom processing
  # reticulate::import ("xarray", delay_load=TRUE)





  if (0){
    xr <- reticulate::import ("xarray", as="xr", delay_load=TRUE)
    ds <- xr$open_zarr ("http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr/")
    ds <- ds.sel (ocean_time=slice ("2014-01-01 18:00", "2014-02-01 06:00"))
    #  ds <- ds [["u", 'v', "w"]]

    # lon = np.array ([-152, -150.5])
    # lat = np.array ([58, 59.5])
    #
    # box = ((lon[0] < ds.lon_rho) & (ds.lon_rho < lon [1])
    #         & (lat[0] < ds.lat_rho) & (ds.lat_rho < lat [1])).compute()

    # ds=ds ["u"].isel (s_rho=-1).where(box, drop=True)

    ds$variables

    # ds <- xr$open_dataset ("http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr/"
    #                        , engine="zarr", cache=True, decode_times=True)



    vN <- py_run_string ('
import numpy as np

loc = "http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr/"
ds = xr.open_dataset (loc, engine="zarr", cache=True, decode_times=True)
ds = ds.sel (ocean_time=slice ("2014-01-01 18:00", "2014-02-01 06:00"))

lon = np.array ([-152, -150.5])
lat = np.array ([58, 59.5])

box = ((lon[0] < ds.lon_rho) & (ds.lon_rho < lon [1])
       & (lat[0] < ds.lat_rho) & (ds.lat_rho < lat [1])).compute()

ds=ds ["u"].isel (s_rho=-1).where(box, drop=True)
print (ds)
')


    vN2 <- py_to_r (vN)
    #  print (ds.variables.keys)
    # print (ds.coord_names)
    # print (ds)
    # print (ds.variables)
  }else{
    ## or run script -- still need to import ncdf
    py_run_file ("Currents/dlZarr.py")
  }



  ci <- py_to_r ("ds", convert=TRUE)
}

# virtualenv_remove("zarrDL")


if (0){
# # see https://ptaconet.github.io/modisfast/
# if (!require (modisfast)){
#   if(!require(devtools)){renv::install("devtools")}
#   devtools::install_github("ptaconet/modisfast")
#   require (modisfast)
# }  ## formerly opendapr

## ROI and time range
roi <- st_as_sf (data.frame (id="any", geom="POLOYGON ((-156 58, -156 61, -149 61, -149 58, -156 58))",
                             wkt="geom", crs=4326))
time_range <- as.Date (c("2020-05-05", "2020-05-06"))




# https://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/axiom.ciofs.fields.hindcast.2014_0031.nc
download.file(url="https://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/axiom.ciofs.fields.hindcast.2014_0031.nc"
              , destfile="test.nc")

## as per Kristen's email: access CIOFS model output (hindcast) with zarr
# xr.open_zarr ("http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr/")
# Dimensions:
#  s_rho: 30s_w: 31eta_rho: 1044xi_rho: 724eta_u: 1044xi_u: 723eta_v: 1043xi_v: 724eta_psi: 1043xi_psi: 723ocean_time: 210384)

require ("stars")
# dsn = 'ZARR:"/vsicurl/https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr"'
# dsn <- 'ZARR:"vsicurl/http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr/"
axlnk <- "http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr/"
dsn <- paste0 ('ZARR:/vsicurl/"', axlnk, 'gpcp.zarr"')
dsn <- paste0 ('ZARR:/vsicurl/"', axlnk, '/:psl.zarr"')


dsn <- 'ZARR:/vsicurl/"http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr"'  ## not all wrong
(r = read_mdim (dsn #, bounds=bounds
                , count = c(NA, NA, 10)))

r <- read_mdim (dsn, variable="?", count=c(5,5,5,5,5,5,5,5,5,5,5))

dsn <- 'ZARR:/vsicurl/"http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr"'  ## not all wrong
bounds <- c(longitude="lon_bounds", latitude="lat_bounds")
(r = read_mdim (dsn, bounds=bounds, count = c(NA, NA, 10)))





## example
dsn = 'ZARR:"/vsicurl/https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr"'
bounds = c(longitude = "lon_bounds", latitude = "lat_bounds")
r = read_mdim(dsn, bounds = bounds)
r
}

}


}
