#! /usr/bin/env Rscript

## ------------------- install required packages ----------------
r = getOption("repos")
r["CRAN"] = "https://cloud.r-project.org/"
options(repos = r)


if (!require("renv")) {
  install.packages("renv")
  require ("renv")
}
renv::status()
renv::init(bioconductor = TRUE) ## for ConsennsusClusterPlus
# renv::init(bioconductor = "3.21")

renv::install (repos = "https://cloud.r-project.org/", prompt = FALSE)
# detach ("package:renv", unload=TRUE) ## detach to avoid renv::load masking base::load
renv::install("remotes", prompt = FALSE)
remotes::install_github("NOAA-EDAB/buoydata")
renv::install("terra", prompt = FALSE)
remotes::install_git("https://github.com/STBrinkmann/GVI")
renv::restore()
require ("conflicted")
conflicted::conflicts_prefer(base::load())
unloadNamespace("renv")  ## detach to avoid renv::load masking base::load





## --------------- ensure data is available locally ---------------
bDir <- "~/GISdata/LCI/bathymetry/"

if (!file.exists(paste0 (bDir, "/KBL-bathymetry/KBL-bathymetry_GWA-area_50m_EPSG3338.tiff"))) {
  cat ("\n##\n## Downloading more required datasets for initial setup\n##\n")
  ## use KBL bathymetry, rather than recreate it
  dir.create (paste0 (bDir, "KBL-bathymetry/"), showWarnings = FALSE, recursive = TRUE)
  if (1) {
    require ('googledrive')
    drive_download (file = "https://drive.google.com/file/d/1_XEEX9UcYFZeK-Q2j8WNiZr76loUOzof/view?usp=drive_link"
      , path = paste0 (bDir, "KBL-bathymetry/KBL-bathymetry_GWA-area_50m_EPSG3338.tiff")
      , overwrite = TRUE)
    drive_download (file = "https://drive.google.com/file/d/1W8ie9YHEoneJne75d2flTT5QJqDKhapp/view?usp=drive_link"
      , path = paste0 (bDir, "KBL-bathymetry/KBL-bathymetry_ResearchArea_100m_EPSG3338.tiff")
      , overwrite = TRUE)
  } else {
    ## these were supposed to be on NOAA servers, but NOAA dropped the ball. Credit: Mark Zimmermann
    ## alternative: NC4 from https://portal.aoos.org/old/cibw#module-metadata/f1844c0d-11da-40ab-b963-9b6b222fe647/82284b16-a985-4233-86cd-242071a290c1
    options (timeout = max (300, getOption ("timeout"))) # allow up to 5 minutes for downloads
    ch <- download.file ("http://web.archive.org/web/20170513133945/https://www.afsc.noaa.gov/RACE/groundfish/Bathymetry/Cook_bathymetry_grid.zip"
      , destfile = paste0 (bDir, "Cook_bathymetry_grid.zip"), quiet = FALSE)
    ci <- download.file ("http://web.archive.org/web/20170513133945/https://www.afsc.noaa.gov/RACE/groundfish/Bathymetry/CGOA_bathymetry_grid.zip"
      , destfile = paste0 (bDir, "CGOA_bathymetry_grid.zip"), quiet = FALSE)
    if (ch != 0 || ci != 0) {
      stop ("\nOne of the downloads were unsuccessful. Try again or download the bathymetry files manually.\n")
    }
    ## expand zip files
    unzip (paste0 (bDir, "Cook_bathymetry_grid.zip"), exdir = paste0 (bDir, "Cook_bathymetry_grid/"))
    unzip (paste0 (bDir, "CGOA_bathymetry_grid.zip"), exdir = paste0 (bDir, "CGOA_bathymetry_grid/"))
    source ("Currents/bathymetry-merge.R")
  }
}


## get data from GitHub
# .... still need to add this here. rsync would be ideal.

dir.create("~/tmp/LCI_noaa/cache/", showWarnings = FALSE, recursive = TRUE)
dir.create("~/tmp/LCI_noaa/media/StateOfTheBay/", showWarnings = FALSE, recursive = TRUE)
dir.create("~/tmp/LCI_noaa/media/CTDcasts/", showWarnings = FALSE, recursive = TRUE)
dir.create("~/tmp/LCI_noaa/media/CTDsections/", showWarnings = FALSE, recursive = TRUE)
dir.create("~/tmp/LCI_noaa/data-products/", showWarnings = FALSE, recursive = TRUE)


## more GIS files
## setup working environment
dir.create("~/GISdata/data/coastline", recursive = TRUE, showWarnings = FALSE)
tO <- getOption ("timeout")

options (timeout = max (1200, getOption("timeout")))

## fetch bathymetry, shoreline, weather, swmp, CTD hex/aggregated files
## bathymetry: gebco and AOOS
## AOOS http://thredds/ncss/CI_BATHY.nc?var=Band1&disableLLSubset=on&disableProjSubset=on&horizStride=1&time_start=2009-01-01T00%3A00%3A00Z&time_end=2009-01-01T00%3A00%3A00Z&timeStride=1&accept=netcdf4
## hi-resolution Kachemak Bay DEM
# outF <- "~/GISdata/LCI/kachemak_bay_13_mhr_2010.nc"
# # unlink (outF)
# if (!file.exists(outF)){
#   download.file (url="https://www.ngdc.noaa.gov/thredds/fileServer/regional/kachemak_bay_13_mhw_2010.nc"
#                  , destfile=outF, method="auto") ## 269.4 MB
# }
# rm (outF)

# ## Zimmermann Cook Inlet Bathymetry
# outF <- "~/GISdata/LCI/Cook_Bathymetry_Grid.zip"
# # unlink (outF)
# if (!file.exists(outF)){
#   download.file(url="http://www.afsc.noaa.gov/RACE/groundfish/bathymetry/Cook_bathymetry_grid.zip"
#               , destfile=outF, method="auto")
# }
#
# ## gebco


## GSHHG shoreline
outF <- "~/GISdata/data/coastline/gshhg-shp-2.3.7.zip"
if (!file.exists (outF)) {
  download.file (url = "https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-2.3.7.zip"
                 , destfile = outF, method = "wget") ## 149 MB
}
rm (outF)

## CTD aggregated and SWMP -- get from GoogleDrive or from GitHub
## zooplankton, phytoplankton, etc.

## seabirds


options (timeout = tO)



# EOF
