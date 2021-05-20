#! /usr/bin/env Rscript

## execute all Kachemak Bay/Cook Inlet scripts, 2020
setwd ("~/myDocs/amyfiles/NOAA-LCI/")
rm (list = ls())

print (sT <- Sys.time())


## State of the Bay Report 2019

## plot SWMP weather data for annual state of the bay report
source ("SeldoviaTemp.R")
source ("annual-wind.R")

# source ("precipSalinity.R")  # calls the scripts below and makes a combined multi-panel PDF
source ("annual-rainy.R")
source ("annual-salinity.R")
source ("annual-airTemp.R")

source ("annual-snowpack.R")
source ("annual-stratification.R")


cat ("all done\n")
print (Sys.time())
print (difftime(Sys.time(), sT, units = NULL)) ## not going to work here because of saved dumps

## EOF

