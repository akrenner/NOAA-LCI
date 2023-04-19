#! /usr/bin/env Rscript


bDir <- "~/GISdata/LCI/bathymetry/"

if (!dir.exists(bDir)){
  cat ("\n##\n## Downloading more required datasets for initial setup\n##\n")
  dir.create(bDir, showWarnings=FALSE, recursive=TRUE)

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
}

#EOF