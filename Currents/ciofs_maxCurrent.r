## map maximal currents during biggest tide from CIOFS
rm (list=ls())


## biggest tide:   8.242 m,       2014-01-31 21:23:00
## max tide speed: 0.0003626 m/s, 2015-02-19 21:16:00
# format (as.POSIXct("2014-01-31"), "0%d")


## the hard part: get data from AOOS/Axiom server

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


# virtualenv_remove("zarrDL")


## or (more familiar) use
require ("ncdf4")
nc <- ncdf4::nc_open("~/GISdata/LCI/OpenDrift/Data/ciofs_bigtide.nc")
print (nc)
names (nc$var)  # 22
## extract u and v with relevant coordinates
## only uppermost layer, all times


nc_close (nc)



require ('RNetCDF')
nc <- RNetCDF::open.nc ("~/GISdata/LCI/OpenDrift/Data/ciofs_bigtide.nc")
file.inq.nc (nc)
close.nc (nc)









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









