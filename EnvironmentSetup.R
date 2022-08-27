#!/usr/bin/env Rscript

## setup working environment
dir.create("~/GISdata/LCI/", recursive=TRUE, showWarnings=FALSE)
dir.create("~/GISdata/data/coastline", recursive=TRUE, showWarnings=FALSE)
tO <- getOption ("timeout")


options (timeout=max (1200, getOption("timeout")))


## fetch bathymetry, shoreline, weather, swmp, CTD hex/aggregated files

## bathymetry: gebco and AOOS
## AOOS http://thredds/ncss/CI_BATHY.nc?var=Band1&disableLLSubset=on&disableProjSubset=on&horizStride=1&time_start=2009-01-01T00%3A00%3A00Z&time_end=2009-01-01T00%3A00%3A00Z&timeStride=1&accept=netcdf4
## hi-resolution Kachemak Bay DEM
outF <- "~/GISdata/LCI/kachemak_bay_13_mhr_2010.nc"
# unlink (outF)
if (!file.exists(outF)){
  download.file (url="https://www.ngdc.noaa.gov/thredds/fileServer/regional/kachemak_bay_13_mhw_2010.nc"
                 , destfile=outF, method="auto") ## 269.4 MB
}
rm (outF)
## gebco



## GSHHG shoreline
outF <- "~/GISdata/data/coastline/gshhg-shp-2.3.7.zip"
if (!file.exists (outF)){
  download.file (url="https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-2.3.7.zip"
                 , destfile=outF, method="wget") ## 149 MB
}
rm (outF)


## CTD aggregated and SWMP -- get from GoogleDrive or from GitHub
## zooplankton, phytoplankton, etc.

## seabirds


options (timeout=tO)
# EOF
