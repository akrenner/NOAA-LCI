#! /usr/bin/env Rscript

## ------------------- install required packages ----------------
r = getOption("repos")
r["CRAN"] = "https://cloud.r-project.org/"
options(repos = r)


if (!require("renv")){
  install.packages("renv")
  require ("renv")
}
renv::status()
renv::init(bioconductor = TRUE) ## for ConsennsusClusterPlus
# renv::init(bioconductor = "3.17")

renv::install (repos="https://cloud.r-project.org/")
# detach ("package:renv", unload=TRUE) ## detach to avoid renv::load masking base::load
require ("conflicted")
conflicted::conflicts_prefer(base::load())
unloadNamespace("renv")  ## detach to avoid renv::load masking base::load



## --------------- ensure data is available locally ---------------
bDir <- "~/GISdata/LCI/bathymetry/"

if (!file.exists(paste0 (bDir, "/KBL-bathymetry/KBL-bathymetry_GWA-area_50m_EPSG3338.tiff"))){
  cat ("\n##\n## Downloading more required datasets for initial setup\n##\n")
  ## use KBL bathymetry, rather than recreate it
  dir.create (paste0 (bDir, "KBL-bathymetry/"), showWarnings=FALSE, recursive=TRUE)
  if (1){
    require ('googledrive')
    drive_download (file="https://drive.google.com/file/d/1_XEEX9UcYFZeK-Q2j8WNiZr76loUOzof/view?usp=drive_link"
                    , path=paste0 (bDir, "KBL-bathymetry/KBL-bathymetry_GWA-area_50m_EPSG3338.tiff")
                    , overwrite=TRUE)
    drive_download (file="https://drive.google.com/file/d/1W8ie9YHEoneJne75d2flTT5QJqDKhapp/view?usp=drive_link"
                    , path=paste0 (bDir, "KBL-bathymetry/KBL-bathymetry_ResearchArea_100m_EPSG3338.tiff")
                    , overwrite=TRUE)
  }else{
    ## these were supposed to be on NOAA servers, but NOAA dropped the ball. Credit: Mark Zimmermann
    ## alternative: NC4 from https://portal.aoos.org/old/cibw#module-metadata/f1844c0d-11da-40ab-b963-9b6b222fe647/82284b16-a985-4233-86cd-242071a290c1
    options (timeout=max (300, getOption ("timeout"))) # allow up to 5 minutes for downloads
    ch <- download.file ("http://web.archive.org/web/20170513133945/https://www.afsc.noaa.gov/RACE/groundfish/Bathymetry/Cook_bathymetry_grid.zip"
                         , destfile=paste0 (bDir, "Cook_bathymetry_grid.zip"), quiet=FALSE)
    ci <- download.file ("http://web.archive.org/web/20170513133945/https://www.afsc.noaa.gov/RACE/groundfish/Bathymetry/CGOA_bathymetry_grid.zip"
                         , destfile=paste0 (bDir, "CGOA_bathymetry_grid.zip"), quiet=FALSE)
    if (ch != 0 || ci != 0){
      stop ("\nOne of the downloads were unsuccessful. Try again or download the bathymetry files manually.\n")
    }
    ## expand zip files
    unzip (paste0 (bDir, "Cook_bathymetry_grid.zip"), exdir=paste0 (bDir, "Cook_bathymetry_grid/"))
    unzip (paste0 (bDir, "CGOA_bathymetry_grid.zip"), exdir=paste0 (bDir, "CGOA_bathymetry_grid/"))
    source ("Currents/bathymetry-merge.R")
  }
}


## get data from GitHub
# .... still need to add this here. rsync would be ideal.

dir.create("~/tmp/LCI_noaa/cache/", showWarnings=FALSE, recursive=TRUE)
dir.create("~/tmp/LCI_noaa/media/StateOfTheBay/", showWarnings=FALSE, recursive=TRUE)
dir.create("~/tmp/LCI_noaa/media/CTDcasts/", showWarnings=FALSE, recursive=TRUE)
dir.create("~/tmp/LCI_noaa/media/CTDsections/", showWarnings=FALSE, recursive=TRUE)
dir.create("~/tmp/LCI_noaa/data-products/", showWarnings=FALSE, recursive=TRUE)


#EOF
