## map maximal currents during biggest tide from CIOFS
rm (list=ls())


## biggest tide:   8.242 m,       2014-01-31 21:23:00
## max tide speed: 0.0003626 m/s, 2015-02-19 21:16:00
# format (as.POSIXct("2014-01-31"), "0%d")





## -----------------------------------------------------------
## read NoteBook-exported ncdf file and extract coordinates
## -----------------------------------------------------------

###-----  use OpenDrift.R instead???  ------


## u, v, w, rho
# u: east-west    lat_u, lon_u, s_rho, ocean_time
# v: north-south
# w: up movement?
# rho: grid-centers
# s: depth
# zeta = ?
# xi_rho and eta_rho:  simple counters -- not needed


ncF <- "~/GISdata/LCI/drifter/ciofs_bigtide_KT.nc"  ## big file with 30 depth, 30 time steps
ncF <- "~/GISdata/LCI/drifter/max_speed_2014-1.nc"  ## max tide picked by Kristen Thyng

## options to access ROM netCDF data
## stars::read_ncdf Simple, nice documentation
## netcdf4 raw. More complicated. But avoids some stars hassle.

## use Kristen's max_speed_2014-1.nc file
## visit ciofs_bigtide_KT.nc later (extracting u,v vectors, filtering by direction as well)


# grid_spacing <- 10e3  ## 10 km seems to make sense -- go to 20 km?
grid_spacing <- 1e3
grid_spacing <- 500
prjct <- 3338






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
rm (nX, nc)

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


## aggregate CIOFS data over new grid
pointsID <- maxS %>%
  st_join (A) %>%
  as.data.frame() %>%
  group_by (ID) %>%
  summarize (maxSpeed=max (speed, na.rm=TRUE))
speedPol <- left_join(A, pointsID, by="ID")
rm (pointsID)

speedS <- st_rasterize (speedPol ["maxSpeed"], dx=grid_spacing, dy=grid_spacing)


png ("~/tmp/LCI_noaa/media/drifter/ciofs_maxspeed.png")
plot (speedS)
dev.off()

save (speedS, file="~/tmp/LCI_noaa/cache/ciofs_maxspeed.RData")
write_stars (speedS, dsn = paste0 ("~/tmp/LCI_noaa/data-products/maxSpeed_CIOFS"
             , grid_spacing, ".tif"))

rm (speedS, speedPol, ncF, maxS)








### code in progress -- or to be deleted

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


## work with u, v, w vectors -- using ncdf4
if (0){
  ## export netCDF with jupyter notebook https://researchworkspace.com/file/44648890/CIOFS_currents_MR_2.ipynb
  ## then import netCDF -- or use csv exported by Kristen Thyng

  ### dimensions of u, v, and w don't perfectly match
  ## => extract each variable with associated lat,lon
  ## subset surface layer (s_rho = -0.01666666-ish)  [30]
  ## reproject to a new raster grid
  ## stack those rasters


  maxCurrent <- function (ncF, varid, maxv=TRUE){
    ## dimensions of u, v, w don't match -- use lat_u, lat_v, lat_rho!
    u <- ncvar_get (nc, varid=varid)

    ## extract surface or max value?
    if (maxv){
      um <- apply (u, MARGIN=c(1,2), FUN=max, na.rm=TRUE) ## suppress
    }else{
      um <- apply (u [,,30,], MARGIN=c(1,2), FUN=max, na.rm=TRUE) # surface, any time
    }
    um2 <- ifelse (is.finite (um), um, NA)
    as.numeric (um2)
  }




  ncF <- "~/GISdata/LCI/drifter/ciofs_bigtide_KT.nc"
  require ("ncdf4")
  nc <- ncdf4::nc_open(ncF)
  # print (nc)
  # names (nc$dim)
  # names (nc$var)


  ## dimensions are close but don't match yet XXXX
  maxDF <- data.frame (lon=as.numeric (ncvar_get (nc, varid="lon_rho")),
                       lat=as.numeric (ncvar_get (nc, varid="lon_rho")),
                       u=maxCurrent (nc, varid="u"),
                     v=maxCurrent (nc, varid="v"),
                     w=maxCurrent (nc, varid="w"))
nc_close(nc)
rm (nc, maxCurrent)


maxDF <- data.frame (lon=ncvar_get (nc, varid="lon_rho"))
maxDF <- data.frame (lon=ncvar_get (nc, varid="lon_rho"))


nc <- ncdf4::nc_open("~/GISdata/LCI/drifter/ciofs_bigtide_KT.nc")


## extract u with lon_u and lat_u and v with lon_v and lat_v
## only uppermost layer (s_rho=-1), all times (ocean_time=0)
u <- ncvar_get (nc, varid="u")
u_lat <- ncvar_get (nc, varid="lat_u")
u_lon <- ncvar_get (nc, varid="lon_u")

## find max -- any time, any depth -- or always surface
um <- apply (u [,,30,], MARGIN = c(1,2), FUN=function (y){max (y, na.rm=TRUE)})
um2 <- ifelse (is.finite(um), um, NA)
uDF <- data.frame (u=as.numeric (um2), lat=as.numeric (u_lat), lon=as.numeric (u_lon))
rm (u, u_lat, u_lon, um, um2)

u <- ncvar_get (nc, varid="v")
u_lat <- ncvar_get (nc, varid="lat_v")
u_lon <- ncvar_get (nc, varid="lon_v")

## find max -- any time, any depth -- or always surface
um <- apply (u [,,30,], MARGIN = c(1,2), FUN=function (y){max (y, na.rm=TRUE)})
um2 <- ifelse (is.finite(um), um, NA)
vDF <- data.frame (u=as.numeric (um2), lat=as.numeric (u_lat), lon=as.numeric (u_lon))
rm (u, u_lat, u_lon, um, um2)





nc_close (nc)
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
# see https://ptaconet.github.io/modisfast/
if (!require (modisfast)){
  if(!require(devtools)){renv::install("devtools")}
  devtools::install_github("ptaconet/modisfast")
  library(modisfast)
}  ## formerly opendapr

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



