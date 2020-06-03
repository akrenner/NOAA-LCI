#!/bin/sh

## run all files for NOAA LCI project

## BUGS:
## bathymetry is read in as a raster. This creates a non-portable reference to the original file.
## Breaking this reference would be desirable, although it would increase file size of all temporary
## cache files.  



## clear tmp directory first? CAREFUL with this! 
# rm -r ~/tmp/LCI_noaa/

# Rscript metaExtraction.R && \
Rscript SeldoviaTemp.R && \
	Rscript dataSetup.R && \
	Rscript anaCTD.R && \
	Rscript ecoAn.R && \
	Rscript plotMaps.R && \
	Rscript commMap.R && \
	exit 0


