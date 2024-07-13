
## create virtual environment
# python3 -m venv .venv
# source .venv/bin/activate

## make sure zarr is installed -- e.g.
# pip install xarray zarr fsspec requests aiohttp

import numpy as np 
import xarray as xr

loc = "http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr/"
ds = xr.open_dataset (loc, engine="zarr", cache=True, decode_times=True)

## turn on chunking to activate dask and parallelize read/write
# ds = ds.chunk ({'ocean_time': 1})


## range to extract: Kachemak Bay, 0-20 m, 2018-xx-xx
# lon:
# lat:
# time:
# s_rho: surface to 10 m:  > -0.02  (?? scaled -1.0 to 0.0)

print (ds)

lon = np.array ([-152, -150.5])
lat = np.array ([58, 59.5])
box = ((lon[0] < ds.lon_rho) & (ds.lon_rho < lon [1])
       & (lat[0] < ds.lat_rho) & (ds.lat_rho < lat [1])).compute()
ds = ds.isel (s_rho=-1).where(box,drop=True)
print (ds)

ds = ds.sel (ocean_time=slice ("2014-01-31 18:00", "2014-02-01 06:00"))
print (ds)

# ds = ds.isel (((ocean_time=slice ("2014-01-31 18:00, 2014-0201 06:00")) 
#               & (s_rho=-1).where(box,drop=True)))
print (ds)

## Pick out some of the variables that will be included as coordinates -- XXX review ??
# ds = ds.set_coords(['Cs_r', 'Cs_w', 'hc', 'h', 'Vtransform'])


## Select a subset of variables.
# variables = ['x_v', 'y_u'']
ds = ds [['u', 'v', 'w']]

# drop_variables=...
# ds = ds.drop_vars (['rdrg', 'rdrg2', 'rho0', 'Vtransform', 'Vstretching', 'Cs_r', 'Cs_w', 'Pair', 'y_rho', 'Uwind', 'Vwind', 'f', 'x_rho', 'zeta'])

## drop all other than u, v, w




## coordinates (14)
# * ocean_time:   1999-01-01T01:00.0 -- time sinze initialization

## data variables
## lots --- extract:  u (velocity east), v (north), w (up)
# u: ocean_time, s_rho, eta_u, xi_u
# v: ocean_time, s_rho, eta_v, xi_v
# w: ocean_time, s_w, eta_rho, xi_rho


# print (ds.variables.keys)

print (ds)

## write to netcdf
ds.to_netcdf(path='ciofs_bigtide', mode='w', format='NETCDF4')



# deactivate+

