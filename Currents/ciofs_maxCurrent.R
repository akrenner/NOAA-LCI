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

## any way to download these automatically? maybe not
ncF <- "~/GISdata/LCI/OpenDrift/max_speed_2014-1.nc"     ## max tide picked by Kristen Thyng
ncF3 <- "~/GISdata/LCI/OpenDrift/ciofs_bigtide_KT_b.nc"  ## big file with 30 depth, 30 time steps


grid_spacing <- 10e3  ## 10 km seems to make sense -- go to 20 km?
grid_spacing <- 5e3
grid_spacing <- 1e3
# grid_spacing <- 500

prjct <- 3338


## ------------------------- processing ----------------------- ##



## options to access ROM netCDF data
## stars::read_ncdf Simple, nice documentation
## netcdf4 raw. More complicated. But avoids some stars hassle.

## use Kristen's max_speed_2014-1.nc file
## visit ciofs_bigtide_KT.nc later (extracting u,v vectors, filtering by direction as well)


if (0){
  require ("stars")
  ncU <- stars::read_ncdf(ncF3, var="u")  ## not this simple -- as-is = no values? -- abandone this effort
  ncU  ## dimensions: xi_u (lon), eta_u (lat), s_rho (1-30, offset -1, delta 0.03333), ocean_time (1-25, 2014-01-31 18:00 delta 1 hours)
  ncV <- stars::read_ncdf(ncF3, var="v")
  ncW <- stars::read_ncdf(ncF3, var="w")
}



require ("ncdf4")
require ("stars")

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



## voronoi interpolation
# voroi <- st_voronoi(maxS)
# speedS <- maxS %>%
#   st_voronoi() %>%
#   st_rasterize(dx=grid_spacing, dy=grid_spacing)

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
  speedS <- ifelse (is.na (speedS), st_extract(gridPts (maxS, 100), speedS), speedS)
  speedS <- ifelse (is.na (speedS), st_extract(gridPts (maxS, 500), speedS), speedS)
  speedS <- ifelse (is.na (speedS), st_extract(gridPts (maxS, 1000), speedS), speedS)
  speedS <- ifelse (is.na (speedS), st_extract(gridPts (maxS, 5000), speedS), speedS)
  speedS <- ifelse (is.na (speedS), st_extract(gridPts (maxS, 10000), speedS), speedS)
  speedS <- ifelse (is.na (speedS), st_extract(gridPts (maxS, 50000), speedS), speedS)
}




png ("~/tmp/LCI_noaa/media/drifter/ciofs_maxspeed.png")
plot (speedS)
dev.off()

save (speedS, file="~/tmp/LCI_noaa/cache/ciofs_maxspeed.RData")
write_stars (speedS, dsn = paste0 ("~/tmp/LCI_noaa/data-products/maxSpeed_CIOFS"
             , grid_spacing, ".tif"))

rm (speedPol, ncF)








## ------------------ new start with u v w ----------------------

## set depth = 0
## calc speeds per time and position
## find greatest speed for each position
## export speed, u, v, and w

## should use proper u_lat, v_lat, etc, instead of truncating arrays (dimLim1, dimLim2)


## start over
rm (list=ls())
ncF3 <- "~/GISdata/LCI/OpenDrift/ciofs_bigtide_KT_b.nc"  ## big file with 30 depth, 30 time steps
grid_spacing <- 10e3  ## 10 km seems to make sense -- go to 20 km?
grid_spacing <- 5e3
grid_spacing <- 1e3
# grid_spacing <- 500
prjct <- 3338



## ----- snip -- try a different, more canned approach ------- ##
## see https://rpubs.com/cyclemumner/roms0
## tools made for ROMS -- but useing old sp/raster framework

if (0){
  ## tested -- same result as below (maybe less flexible??)
roms_path <- file.path (ncF3)
                       ## require ("devtools")
require (angstroms)    ## devtools::install_github("AustralianAntarcticDivision/angstroms")
require (ncdump)       ## devtools::install_github("r-gris/ncdump")
require (tabularaster) ## devtools::install_github("r-gris/tabularaster")
require (rworldmap)
ncd <- NetCDF(roms_path)
ncd$variable$name

vname <- "v"
dd <- romsdata(roms_path, varname = vname, slice = c(1L, 1L), transpose = TRUE)
plot (dd)

longlat <- romscoords(roms_path, transpose = TRUE)
contour(longlat[[1]], add = TRUE, lty = 2)
bound <- boundary(longlat)
projection(bound) <- "+init=epsg:4326"
extent(bound)
}
## ---- snip -- end of a different, more canned approach ------ ##



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
      wU [i,j] <- max (wV [i,j,,], na.rm=TRUE)
      wD [i,j] <- min (wV [i,j,,], na.rm=TRUE)
    }
  }
}

wDF <- data.frame (lon = as.numeric (ncvar_get (nc, varid="lon_rho")[dR[[1]],dR[[2]] ]),
                   lat = as.numeric (ncvar_get (nc, varid="lat_rho")[dR[[1]],dR[[2]] ]),
                   wu = as.numeric (wU), # already applied dR
                   wd = as.numeric (wD)  # already applied dR
)         # cut off first or last??


## 1: calculate speed
# vV <- ncvar_get (nc, "v")[,,1,]  ## dim 2 is already short
# uV <- ncvar_get (nc, "u")[,dimLim2,1,] ## u is 1 short of v
speed <- sqrt (as.numeric (vV)^2 + as.numeric(uV)^2) |>
  array (dim = dim (vV))

## calculate direction -- in ROMS and then in projected coordinates
alphaR <- atan (uV/vV)  /pi*360  ## N = 0 degrees, E: 90 degrees -- check XXX
## calculate angle between ROMS cells in projected coordinates
## transform into projected coordinates
lon <- ncvar_get(nc, varid="lon_rho")[dR[[1]], dR[[2]]]
lat <- ncvar_get(nc, varid="lat_rho")[dR[[1]], dR[[2]]]
## find angle between current cell and cell to the north in ROMS-grid
lA <- sapply (1:ncol (lon), function (i){
  require ("useful")
  lldf <- data.frame (lon=lon [,i], lat=lat [,i]) %>%
    st_as_sf (coords=c("lon", "lat"), crs=4326) %>%
    st_transform(crs=prjct) %>%
    st_coordinates()
  dC <- diff (lldf) %>% as.data.frame()
  angl <- 2*pi - cart2pol (dC$X, dC$Y, degrees=FALSE)$theta
})


aR <- array (dim = dim (alphaR))
for (i in 1:dim (alphaR)[3]){
  aR [,,i] <- alphaR [,,i] + lA [c(1,1:nrow (lA)),]  ## duplicate first row to make lA conformal
}
rm (alphaR, lon, lat, lA)


# working so far 2024-10-09
ncdf4::nc_close (nc)



## extract max speed within day
## anywhere in water column or only at the surface?
topAr <- array (dim = c (dim (speed)[1:2], 5))  # last dimension: max speed, mtheta, surface speed, stheta
topSpeed <- rep (NA, dim (speed)[1])
for (i in 1:dim (speed)[1]){
  for (j in 1:dim (speed)[2]){
    if (all (is.na (speed [i,j,]))){
      topAr [i,j,] <- rep (NA, dim (topAr)[3])
    }else{
      mIJ <- which.max (speed [i,j,])
      topSpeed [i] <- mIJ
      topAr [i,j,1] <- speed [i,j,mIJ]
      topAr [i,j,2] <- aR [i,j,mIJ]
      topAr [i,j,3] <- speed [i,j,1]
      topAr [i,j,4] <- aR [i,j,1]
      topAr [i,j,5] <- mean (speed [i,j,], na.rm=TRUE)
      # topAr [i,j,5] <- uV [i,j,mIJ]    ## cannot easily calculate a vector in projected space from these
      # topAr [i,j,6] <- vV [i,j,mIJ]
    }
  }
}
## assemple big DF for export
wDF <- cbind (wDF,
              speedM = as.numeric (topAr [,,1]),
              theatM = as.numeric (topAr [,,2]),
              speedS = as.numeric (topAr [,,3]),
              theatS = as.numeric (topAr [,,4])
)
## clean-up
rm (nc, topAr, speed, uV, vV, wV, wU, wD, ncF, dR)


## trim to original export domain (unknown why there is more data than that)
wDF <- subset (wDF, (-153.6 < lon) & (lon < -150.0) &
                 (58.7 < lat) & (lat < 60.8))

save.image ("~/tmp/LCI_noaa/cache/maxCurrentCIOFS3.RData")
# rm (list=ls()); load ("~/tmp/LCI_noaa/cache/maxCurrentCIOFS3.RData"); require ("stars"); require ("dplyr")
## modify things here to interpolate NAs?



wDFsf <- st_as_sf (wDF, coords=c("lon", "lat"), crs=4326) %>%
  st_transform(crs=prjct)
require ("interp")
for (i in seq_len(length (names (wDFsf))-1)){
  tDF <- data.frame (x=st_coordinates(wDFsf)[,1], y=st_coordinates(wDFsf)[,2]
                     , z=st_drop_geometry(wDFsf [,i])[,1]) %>%
    dplyr::filter (!is.na (z))
  wDFbc <- interp::interp (x=tDF$x, y=tDF$y, z=tDF$z
                           , input="points", output="grid"
                           , linear=TRUE  ## bi-cubic can result in negative speeds!!
                           # , linear=FALSE, baryweight=TRUE
                           , na.rm=TRUE
                           , extrap=FALSE, duplicate="error"
                           # , nx = length (unique (st_coordinates(speedS)[,1]))  ## better to have a meaningful pixel size
                           # , ny = length (unique (st_coordinates(speedS)[,2]))
                           , nx = diff (range (st_coordinates (speedS)[,1])) / grid_spacing + 1
                           , ny = diff (range (st_coordinates (speedS)[,2])) / grid_spacing + 1
  )
  rm (tDF)

  # ## apply interp or akima a 2nd time?
  # x <- interp::interp2xyz(wDFbc, data.frame=TRUE) %>% filter (!is.na (z))
  # wDFbc <- interp::bicubic (x$x, x$y, x$z, x0=x$x, y0=x$y)
  #                               , dx=grid_spacing, dy=grid_spacing)


  ## assemble stars object
  wdS <- interp::interp2xyz(wDFbc, data.frame=TRUE) %>%
    st_as_sf (coords=c("x", "y"), crs=prjct) %>%
    st_rasterize ()  ## specify grid size?? template=maxS causes trouble!
  ## interp::interp2grid () -- not applicable, at least not until stars version available


  ## cut-out land


  ## save to geoTIFF
  write_stars (wdS, dsn=paste0 ("~/tmp/LCI_noaa/data-products/ciofs_maxspeeds_"
                                , names (wDFsf)[i], "_", grid_spacing, ".tif"))
  rm (wdS)
  cat (i, "\n")
}
rm (wDFsf)






## turn it into stars object (earlier?) and export
save.image ("~/tmp/LCI_noaa/cache/maxCurrentCIOFS2.RData")


# image (wDFsf, band=3)

## ------------------ end of u v w ----------------------

















### code in progress -- or to be deleted
if (0){
## work with u, v, w vectors -- using stars
if (0){
  ## does not work with speed, only with bigTide file!
  ncF <- "~/GISdata/LCI/drifter/ciofs_bigtide_KT.nc"

  readSpeed <- function (var){
    require (stars)
    velo <- stars::read_ncdf(ncF, var=var)
    st_crs (velo) <- 4326
    ## subset surface [s_rho>-0.16669]
    # velo <-
    ## reproject onto grid A
    velo
  }


  u <- stars::read_ncdf (ncF, var="u", crs=4326)
  v <- stars::read_ncdf (ncF, var="v")
  w <- stars::read_ncdf (ncF, var="w")
  st_crs (u) <- 4326
  u <- st_transform(u, crs=3338)

## polygon now? should be a point cloud
# um <- apply (u, MARGIN=c(1,2), FUN=max, na.rm=TRUE)  ## use for u, v -- not max speed
}



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
