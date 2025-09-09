## activate virtual environment

## get data with xarray
# using conda
# conda install -c conda-forge xarray dask netCDF4 bottleneck

## alternative: using pip
python -m pip install xarray
## add optional packages





## OpenDrift simulation using CIOFS / UAF model

## 1. Install OpenDrift

%%Time
## provide tag
# pip install -q git+https://github.com/OpenDrift/opendrift.git@1.10.4
pip install -q git+https://github.com/OpenDrift/opendrift.git@v1.9.0 coloredlogs git+https://github.com/OpenDrift/opendrift-landmask-data.git

import datetime
from pathlib import Path

import numpy as np
import xarray as xr
import opendrift
from opendrift.readers import reader_ROMS_native, reader_netCDF_CF_generic, reader_global_landmask
from opendrift.models.oceandrift import OceanDrift

## 2. Prepare forcing data

## Best available nowcast and forecast CIOFS model data -- or hindcast
data_dir='~/GISdata/LCI/OpenDrift/Data/'
!ls $data_dir


## this fails in jupyter
multi = False
## can't find Cook_Inlet_grid_1.nc
# Or, use many files
if multi:
    uaf_reader = reader_ROMS_native.Reader(
        filename=Path(data_dir) / '1999/**/nwgoa_1999-*.nc' 
        #, gridfile=Path(data_dir) / 'Cook_Inlet_grid_1.nc'
    )
else :
   # Use a single file
   uaf_reader = reader_ROMS_native.Reader(
       filename=Path(data_dir) / '1999/nwgoa_1999-12-31.nc' 
       #, gridfile=Path(data_dir) / 'Cook_Inlet_grid_1.nc'
  )
print(uaf_reader)



## alternative: get CIOFS hindcast from xarray axiom server

xr.open_zarr (http://xpublish-ciofs.srv.axds.co/datasets/ciofs_hindcast/zarr/)

